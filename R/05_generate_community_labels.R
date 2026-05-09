#' Sample and format user posts for labeling (JSON string)
#'
#' Samples posts per user and returns a compact JSON-style character string per user.
#' Character fields are cleaned and truncated to \code{max_chars}. Empty fields are omitted.
#'
#' @param groups_data Named list with \code{post_data} and \code{node_list} (both \code{data.table}).
#'   \code{post_data} must contain: \code{post_id}, \code{account_id}, \code{account_name}, \code{content}, \code{community}.
#' @param user_vars Character vector of user-level columns to include (besides user name). Default \code{NULL}.
#' @param user_vars_rename Named character vector mapping JSON field names to existing user columns.
#'   If \code{NULL}, names in \code{user_vars} are used as JSON names. Default \code{NULL}.
#' @param post_vars Character vector of post-level columns to include per post (besides content). Default \code{NULL}.
#' @param post_vars_rename Named character vector mapping JSON field names to existing post columns.
#'   If \code{NULL}, names in \code{post_vars} are used as JSON names. Default \code{NULL}.
#' @param min_chars Minimum characters required for a cleaned post content to be eligible. Default \code{1}.
#' @param max_chars Maximum characters for truncating all cleaned character fields. Use \code{NULL} for no truncation. Default \code{1000}.
#' @param clean_name If \code{TRUE}, clean and truncate \code{account_name} before use as user name. Default \code{TRUE}.
#' @param min_n_posts Minimum number of posts to sample per user. Default \code{1}.
#' @param max_n_posts Maximum number of posts to sample per user. Default \code{30}.
#' @param sampling_ratio_posts Proportion of available posts to sample per user before bounding by min/max. Default \code{0.5}.
#' @param min_n_users Minimum number of users to sample per community; communities below this are dropped. Default \code{1}.
#' @param max_n_users Maximum number of users to sample per community. Default \code{30}.
#' @param sampling_ratio_users Proportion of eligible users to sample per community before bounding by min/max. Default \code{0.5}.
#' @param strat_sample If \code{TRUE}, sample users within community with probability proportional to post count; if \code{FALSE}, pick most active. Default \code{TRUE}.
#' @param seed Integer seed for reproducibility. Default \code{42}.
#'
#' @return The input \code{groups_data} with an added \code{user_labels} \code{data.table}
#'   (one row per sampled \code{account_id}) with columns:
#'   \itemize{
#'     \item \code{account_id}, \code{community}
#'     \item \code{account_name_clean} (used as user name)
#'     \item \code{text} (compact JSON-style character string with user fields and a list of posts)
#'     \item \code{sampled_post_ids} (list column)
#'     \item \code{approx_tokens} (approximate token count)
#'     \item \code{user_share_comm} (user's posting share in community)
#'   }
#'
#' @details All character fields in \code{user_vars} and \code{post_vars} are cleaned and truncated
#'   before inclusion. Fields that are \code{NA} or empty after cleaning are omitted from the JSON string.
#'
#' @export
sample_user_text <- function(groups_data,
                             user_vars = NULL,
                             user_vars_rename = NULL,
                             post_vars = NULL,
                             post_vars_rename = NULL,
                             min_chars = 1,
                             max_chars = 1000,
                             clean_name = TRUE,
                             min_n_posts = 1,
                             max_n_posts = 30,
                             sampling_ratio_posts = 0.5,
                             min_n_users = 1,
                             max_n_users = 30,
                             sampling_ratio_users = 0.5,
                             strat_sample = TRUE,
                             seed = 42) {
  
  if (!"node_list" %in% names(groups_data) || !"post_data" %in% names(groups_data)) {
    stop("'groups_data' must contain 'node_list' and 'post_data'.")
  }
  
  post_data <- data.table::copy(groups_data$post_data)
  stopifnot("data.table" %in% class(post_data))
  set.seed(seed)
  
  required <- c("post_id", "account_id", "account_name", "content", "community")
  all_vars <- unique(c(required, user_vars, post_vars))
  if (!all(all_vars %in% names(post_data))) {
    stop("Missing required variables in post_data: ", paste(setdiff(all_vars, names(post_data)), collapse = ", "))
  }
  
  if(is.null(max_chars)) max_chars = Inf
  
  clean_text <- function(x, max_chars) {
    if (!is.character(x)) x <- as.character(x)
    x |>
      stringr::str_replace_all("([[:punct:]])\\1{1,}", "\\1") |>
      stringr::str_remove_all("\\b[a-fA-F0-9]{32,}\\b") |>
      stringr::str_replace_all("[\\r\\n\\t]+", " ") |>
      stringr::str_squish() |>
      stringr::str_trunc(width = max_chars, ellipsis = "..")
  }
  
  # clean main content
  post_data[, content_clean := clean_text(content, max_chars = max_chars)]
  
  # clean user vars
  if (!is.null(user_vars)) {
    for (v in user_vars) {
      post_data[[paste0(v, "_clean")]] <- clean_text(post_data[[v]], max_chars = max_chars)
    }
  }
  
  # clean post vars
  if (!is.null(post_vars)) {
    for (v in post_vars) {
      post_data[[paste0(v, "_clean")]] <- clean_text(post_data[[v]], max_chars = max_chars)
    }
  }
  
  # clean account name safely (no copy warning)
  # remove if exists
  if ("account_name_clean" %in% names(post_data)) {
    post_data[, account_name_clean := NULL]
  }
  
  # create clean copy first to avoid shallow-copy metadata
  post_data <- data.table::copy(post_data)
  
  # assign by reference safely
  post_data[, account_name_clean := if (clean_name) {
    clean_text(account_name, max_chars = max_chars)
  } else {
    account_name
  }]
  
  # calculate user share within community
  user_shares <- post_data[, .N, by = .(community, account_id)][
    , user_share_comm := N / sum(N), by = community
  ][, .(community, account_id, user_share_comm)]
  
  post_data <- post_data[nchar(content_clean) >= min_chars]
  
  # sample posts per user
  sampled_dt <- post_data[
    , {
      n_total <- .N
      n_sample <- min(max(min_n_posts, ceiling(n_total * sampling_ratio_posts)), max_n_posts)
      .SD[sample(.N, min(.N, n_sample))]
    },
    by = account_id
  ]
  
  keep_users <- sampled_dt[, .N, by = account_id][N >= min_n_posts]
  sampled_dt <- sampled_dt[account_id %in% keep_users$account_id]
  
  user_comm_map <- unique(sampled_dt[, .(account_id, community)])
  eligible_communities <- user_comm_map[
    , .(n_users = data.table::uniqueN(account_id)), by = community
  ][n_users >= min_n_users, community]
  sampled_dt <- sampled_dt[community %in% eligible_communities]
  
  user_stats <- sampled_dt[, .N, by = .(account_id, community)]
  selected_users <- user_stats[
    , {
      n_eligible <- .N
      n_sample <- min(max(min_n_users, ceiling(n_eligible * sampling_ratio_users)), max_n_users)
      if (strat_sample) {
        probs <- N / sum(N)
        .SD[sample(.N, min(.N, n_sample), prob = probs)]
      } else {
        .SD[order(-N)][1:min(.N, n_sample)]
      }
    },
    by = community
  ]
  
  sampled_dt <- sampled_dt[account_id %in% selected_users$account_id]
  
  # rename logic
  if (!is.null(user_vars_rename)) {
    user_map <- user_vars_rename
  } else {
    user_map <- stats::setNames(user_vars, user_vars)
  }
  if (!is.null(post_vars_rename)) {
    post_map <- post_vars_rename
  } else {
    post_map <- stats::setNames(post_vars, post_vars)
  }
  
  out <- sampled_dt[
    , {
      # construct the list of columns to extract safely
      user_cols <- c("account_name_clean", "community")
      if (!is.null(user_vars)) user_cols <- c(user_cols, paste0(user_vars, "_clean"))
      
      # extract user metadata safely (no .. lookup!)
      user_meta <- unique(.SD[, user_cols, with = FALSE])[1L]
      
      # build user JSON object
      json_user <- list()
      json_user$user_name <- user_meta$account_name_clean
      
      if (!is.null(user_vars)) {
        for (nm in names(user_map)) {
          val <- user_meta[[paste0(user_map[[nm]], "_clean")]]
          if (!is.null(val) && !is.na(val) && nzchar(val)) json_user[[nm]] <- val
        }
      }
      
      # build posts array safely
      posts <- list()
      for (i in seq_len(.N)) {
        p <- list(content = .SD$content_clean[i])
        if (!is.null(post_vars)) {
          for (nm in names(post_map)) {
            val <- .SD[[paste0(post_map[[nm]], "_clean")]][i]
            if (!is.null(val) && !is.na(val) && nzchar(val)) p[[nm]] <- val
          }
        }
        if (nzchar(p$content)) posts[[length(posts) + 1L]] <- p
      }
      json_user$posts <- posts
      
      json_str <- jsonlite::toJSON(json_user, auto_unbox = TRUE, null = "null", pretty = FALSE)
      
      list(
        community = user_meta$community,
        account_name_clean = user_meta$account_name_clean,
        text = json_str,
        sampled_post_ids = list(.SD$post_id),
        approx_tokens = as.integer(round(length(strsplit(json_str, "\\s+")[[1]]) * 1.3))
      )
    },
    by = account_id
  ]
  
  
  out <- merge(out, user_shares[, community := NULL], by = "account_id", all.x = TRUE)
  data.table::setorder(out, community, -user_share_comm)
  groups_data$user_labels <- out
  return(groups_data)
}




#' Convert JSON Description to Plain Text
#'
#' Unpacks a description column stored as JSON text (from keep_description_json = TRUE) 
#' into concatenated plain text by extracting and joining sentence_1 through sentence_5.
#'
#' @param description_json Character vector containing JSON-formatted descriptions
#'
#' @return Character vector with concatenated sentence text
#'
#' @examples
#' \dontrun{
#' # Convert a data frame column
#' df$description <- unpack_description_json(df$description)
#' 
#' # Or use with dplyr
#' df <- df |> dplyr::mutate(description = unpack_description_json(description))
#' }
#'
#' @export
unpack_description_json <- function(description_json) {
  sapply(description_json, function(desc) {
    if (is.na(desc) || desc == "") return(NA_character_)
    
    tryCatch({
      # Parse JSON
      parsed <- jsonlite::fromJSON(desc)
      
      # Extract sentences
      sentence_names <- paste0("sentence_", 1:5)
      sentences <- sapply(sentence_names, function(s) {
        parsed[[s]] %||% ""
      }, USE.NAMES = FALSE)
      
      # Remove empty sentences and concatenate
      sentences <- sentences[sentences != ""]
      paste(sentences, collapse = " ")
      
    }, error = function(e) {
      # If not valid JSON or parsing fails, return as-is
      desc
    })
  }, USE.NAMES = FALSE)
}





#' Slice Community-Level User Descriptions for LLM Annotation
#'
#' This function prepares community-level text slices for LLM-based labeling by
#' aggregating user-level descriptions sampled via [sample_user_text()]. It cleans,
#' truncates, and filters user descriptions, inserts the user's name into the JSON
#' structure, and aggregates the resulting JSON objects into compact community-level
#' text slices for annotation.
#'
#' @param groups_data A list that must include the element `user_labels`, typically
#'   created by [sample_user_text()]. This table must include at least the columns
#'   `community`, `user_share_comm`, `response`, and `account_name_clean`.
#' @param max_n_per_slice Integer. Maximum number of user descriptions included
#'   per text slice (per community). Default is `10`.
#' @param min_chars Integer. Minimum number of characters required in a user
#'   description to be retained. Default is `20`. *(Currently not enforced in filtering,
#'   but reserved for future use.)*
#' @param max_chars Integer. Maximum number of characters retained per description
#'   after cleaning. Default is `500`. *(Currently not applied in truncation, but included
#'   for consistency with [sample_user_text()].)*
#' @param min_share Optional numeric. Minimum cumulative `user_share_comm` required
#'   for a slice to be retained. Default is `NULL` (no filtering).
#' @param drop_fields Character vector of JSON fields to exclude from each user’s
#'   description (e.g., `"confidence"`, `"emotion_valence"`). Default is `c("confidence", "emotion_valence")`.
#' @param verbose Logical. If `TRUE`, prints the number of returned community slices.
#'   Default is `TRUE`.
#' @param seed Integer. Random seed for reproducibility. Default is `42`.
#'
#' @return A `data.table` with one row per community–slice combination containing:
#' \itemize{
#'   \item \code{community} – Community identifier.
#'   \item \code{slice} – Slice index within the community.
#'   \item \code{n} – Number of user descriptions included in the slice.
#'   \item \code{sum_user_share_comm} – Sum of user shares within the slice.
#'   \item \code{text_slice} – JSON-formatted string of concatenated user descriptions,
#'         including user names and excluding dropped fields.
#' }
#'
#' @details
#' This function aggregates user-level JSON outputs (e.g., from local LLM-based
#' annotations) into structured, slice-level JSON documents suitable for community
#' labeling. It automatically embeds user names into the JSON under `"name"`, and
#' drops unwanted fields defined in `drop_fields`. Each slice corresponds to the
#' top `max_n_per_slice` users within a community, ranked by `user_share_comm`.
#'
#' @seealso [sample_user_text()], [label_communities()], [jsonlite::toJSON()], [stringr::str_replace()]
#'
#' @export
slice_community_text <- function(groups_data,
                                 max_n_per_slice = 10,
                                 min_chars = 20,
                                 max_chars = 500,
                                 min_share = NULL,
                                 drop_fields = c("confidence", "emotion_valence"),
                                 verbose = TRUE,
                                 seed = 42) {
  
  if (!"user_labels" %in% names(groups_data)) {
    stop("No 'user_labels' found. Call 'sample_user_text'!")
  }
  
  set.seed(seed)
  
  # Ensure a clean, modifiable copy
  user_labels <- data.table::copy(groups_data$user_labels)
  data.table::setDT(user_labels)  # Coerce to data.table just in case
  
  # Order and slice
  data.table::setorder(user_labels, community, -user_share_comm)
  user_labels[, slice := ceiling(seq_len(.N) / max_n_per_slice), by = community]
  
  user_labels[
    nchar(account_name_clean) > 0 & !is.na(account_name_clean),
    response_wname := stringr::str_replace(
      response,
      pattern = "^\\{",  # replace only the first opening brace
      replacement = sprintf("{\"name\": \"%s\" , ", account_name_clean)
    )
  ]
  
  # If name is empty or NA, just copy response
  user_labels[is.na(response_wname), response_wname := response]
  
  comm_labels <- user_labels[, .(
    n = .N,
    sum_user_share_comm = sum(user_share_comm, na.rm = TRUE),
    
    text_slice = jsonlite::toJSON(
      lapply(response_wname, function(x) {
        obj <- jsonlite::fromJSON(x)
        
        # Drop unwanted fields at the top level
        obj[setdiff(names(obj), drop_fields)]
      }),
      auto_unbox = TRUE,
      pretty = FALSE
    )
  ), by = .(community, slice)]
  
  if(!is.null(min_share)){
    comm_labels <- comm_labels[sum_user_share_comm >= min_share]
  }
  
  if (verbose) {
    cli::cli_inform("Returning {nrow(comm_labels)} community texts to annotate.")
  }
  
  return(comm_labels)
}



#' Reconstruct a 'description' value from the flat desc_* fields (v0.5 schema)
#'
#' Reads desc_lang_topics / desc_named_entities / desc_patterns /
#' desc_tone_style / desc_striking from a parsed JSON list and returns either
#' a JSON string (keep_json = TRUE) or a space-joined sentence string
#' (keep_json = FALSE), matching the output contract of .process_comm_description().
#'
#' The mapping to the canonical sentence numbering is:
#'   sentence_1 <- desc_lang_topics
#'   sentence_2 <- desc_named_entities
#'   sentence_3 <- desc_patterns
#'   sentence_4 <- desc_tone_style
#'   sentence_5 <- desc_striking
#'
#' @param parsed_json list. A parsed JSON response from the v0.5 schema.
#' @param keep_json logical. If TRUE returns a JSON object string; else plain text.
#' @return character(1)
.reconstruct_description_v2 <- function(parsed_json, keep_json = FALSE) {
  s1 <- parsed_json$desc_lang_topics    %||% ""
  s2 <- parsed_json$desc_named_entities %||% ""
  s3 <- parsed_json$desc_patterns       %||% ""
  s4 <- parsed_json$desc_tone_style     %||% ""
  s5 <- parsed_json$desc_striking       %||% ""

  if (keep_json) {
    desc_obj <- list(
      sentence_1 = s1,
      sentence_2 = s2,
      sentence_3 = s3,
      sentence_4 = s4,
      sentence_5 = s5
    )
    return(jsonlite::toJSON(desc_obj, auto_unbox = TRUE))
  }

  sentences <- c(s1, s2, s3, s4, s5)
  sentences <- sentences[nzchar(sentences)]
  paste(sentences, collapse = " ")
}




#' Flatten a JSON Description String to Plain Text
#'
#' Parses a JSON string of the form `\{"sentence_1": "...", "sentence_2": "...", ...\}`
#' and concatenates the values into a single space-separated string. Non-JSON strings
#' and `NA` values are returned as-is.
#'
#' @param x Character vector of JSON strings (or plain text) to flatten.
#'
#' @return A character vector of the same length as \code{x}.
#'
#' @export
flatten_description <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || !jsonlite::validate(s)) return(s)
    parsed <- tryCatch(jsonlite::fromJSON(s), error = function(e) NULL)
    if (is.null(parsed)) return(s)
    sentences <- unlist(parsed, use.names = FALSE)
    sentences <- sentences[nzchar(sentences)]
    paste(sentences, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}