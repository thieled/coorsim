
#' Sample and format user posts for classification or labeling tasks
#'
#' This function samples posts from social media users based on customizable constraints 
#' and formats them into a markdown-style text block suitable for input to classification 
#' or labeling models. It supports inclusion of user-level and post-level metadata, 
#' optional name cleaning, and stratified or top-down user sampling per community.
#'
#' @param groups_data A named list containing `post_data` and `node_list`, both of which must be `data.table` objects.
#' @param user_vars Character vector of user-level variables to include (in addition to `account_name`).
#' @param user_vars_short Optional character vector with short labels corresponding to `user_vars` for display.
#' @param post_vars Character vector of post-level variables to include (in addition to `content`).
#' @param post_vars_short Optional character vector with short labels corresponding to `post_vars` for display.
#' @param min_chars Minimum number of characters required for post content (after cleaning). Default is 20.
#' @param max_chars Maximum number of characters for cleaned content or metadata fields. Default is 250.
#' @param clean_name Logical. If `TRUE`, clean `account_name` to remove noise and truncate length. Default is `TRUE`.
#' @param min_n_posts Minimum number of posts per user to be included. Default is 2.
#' @param max_n_posts Maximum number of posts per user to be included. Default is 10.
#' @param sampling_ratio_posts Proportion of posts to sample per user. Default is 0.25.
#' @param min_n_users Minimum number of users to sample per community. Default is 2.
#' @param max_n_users Maximum number of users to sample per community. Default is 10.
#' @param sampling_ratio_users Proportion of users to sample per community. Default is 0.25.
#' @param strat_sample Logical. If `TRUE`, performs stratified sampling of users by post volume within community. 
#'   If `FALSE`, selects most active users. Default is `TRUE`.
#' @param seed Integer seed for reproducible sampling. Default is 42.
#'
#' @return A list identical to `groups_data` with an added `user_labels` element. 
#'   This element is a `data.table` containing:
#'   \itemize{
#'     \item \code{community}: Community identifier.
#'     \item \code{account_name_clean}: Cleaned display name.
#'     \item \code{text}: A markdown-style string combining the user's posts and metadata.
#'     \item \code{sampled_post_ids}: List column of sampled post IDs.
#'     \item \code{approx_tokens}: Approximate token count (assuming 1.3x whitespace word count).
#'     \item \code{user_share_comm}: Relative posting activity of the user in their community.
#'   }
#'
#' @export
sample_user_text <- function(groups_data,
                             user_vars = NULL,
                             user_vars_short = NULL,
                             post_vars = NULL,
                             post_vars_short = NULL,
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
  
  clean_text <- function(text, min_chars) {
    text |>
      stringr::str_replace_all("([[:punct:]])\\1{1,}", "\\1") |>
      stringr::str_remove_all("\\b[a-fA-F0-9]{32,}\\b") |>
      stringr::str_replace_all("[\\r\\n\\t]+", " ") |>
      stringr::str_squish() |>
      stringr::str_trunc(width = max_chars, ellipsis = "..")
  }
  
  post_data[, content_clean := clean_text(content, max_chars)]
  
  if (!is.null(user_vars)) {
    for (var in user_vars) {
      data.table::set(post_data, j = paste0(var, "_clean"), value = clean_text(post_data[[var]], max_chars))
    }
  }
  
  if (!is.null(post_vars)) {
    for (var in post_vars) {
      data.table::set(post_data, j = paste0(var, "_clean"), value = clean_text(post_data[[var]], max_chars))
    }
  }
  
  if ("account_name_clean" %in% names(post_data)) {
    post_data[, "account_name_clean" := NULL]
  }
  data.table::set(
    post_data,
    j = "account_name_clean",
    value = if (clean_name) clean_text(post_data$account_name, max_chars) else post_data$account_name
  )
  
  # Calculate user shares
  user_shares <- post_data[, .N, by = .(community, account_id)][
    , user_share_comm := N / sum(N), by = community
  ][, .(community, account_id, user_share_comm)]
  
  post_data <- post_data[nchar(content_clean) >= min_chars]
  
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
  
  post_cols <- c("content_clean", if (!is.null(post_vars)) paste0(post_vars, "_clean"))
  
  out <- sampled_dt[
    , {
      user_meta <- unique(.SD[, c("account_name_clean", "community", if (!is.null(user_vars)) paste0(user_vars, "_clean")), with = FALSE])[1L]
      user_info <- if (!is.null(user_vars)) {
        short_names <- if (!is.null(user_vars_short)) user_vars_short else user_vars
        values <- unlist(user_meta[, paste0(user_vars, "_clean"), with = FALSE])
        non_empty <- !is.na(values) & nzchar(values)
        if (any(non_empty)) paste0("[", paste(paste0(short_names[non_empty], ": ", values[non_empty]), collapse = ", "), "]") else ""
      } else ""
      
      post_lines <- apply(
        .SD[, post_cols, with = FALSE],
        1,
        function(row) {
          if (!is.null(post_vars) && length(post_vars) > 0) {
            short <- if (!is.null(post_vars_short)) post_vars_short else post_vars
            vals <- row[-1]
            non_empty <- !is.na(vals) & nzchar(vals)
            meta <- if (any(non_empty)) paste0(" [", paste(paste0(short[non_empty], ": ", vals[non_empty]), collapse = ", "), "]") else ""
          } else {
            meta <- ""
          }
          paste0(row[[1]], meta)
        }
      )
      
      md <- paste0(user_meta$account_name_clean,
                   if (nchar(user_info) > 0) paste0(" ", user_info),
                   ": ", paste(post_lines, collapse = " | "))
      
      list(
        community = user_meta$community,
        account_name_clean = user_meta$account_name_clean,
        text = md,
        sampled_post_ids = list(.SD$post_id),
        approx_tokens = as.integer(round(length(strsplit(md, "\\s+")[[1]]) * 1.3))
      )
    },
    by = account_id
  ]
  
  out <- merge(out, user_shares[, community := NULL], by = "account_id", all.x = T)
  
  data.table::setorder(out, community, -user_share_comm)
  
  groups_data$user_labels <- out
  return(groups_data)
}




#' Label Users Using a Local LLM
#'
#' This function uses a locally hosted large language model (LLM) through the 
#' \pkg{rollama} interface to automatically generate structured annotations for users 
#' based on their sampled posts. It applies a few-shot prompting setup with examples 
#' and a predefined schema to produce machine-readable JSON output describing each user.
#'
#' The model should run via an active \emph{Ollama} Docker container with GPU (CUDA) support.
#' Use `system("docker ps")` to verify that Ollama is running, and ensure that the 
#' desired model (e.g., `"llama3.2:3b"`) has been pulled via Ollama before executing this function.
#'
#' @param groups_data A list containing at least the element \code{user_labels}, 
#' typically produced by [sample_user_text()]. This element must include a column \code{text} 
#' with sampled user content and an \code{account_id} column.
#' @param model Character string. Name of the local LLM model to be queried via Ollama. 
#' Default is \code{"llama3.2:3b"}.
#' @param prompt Optional character string providing user-facing task instructions 
#' (defaults to the internal \code{prompts$prompt_user}).
#' @param system Optional character string defining the system-level instruction to guide model behavior 
#' (defaults to the internal \code{prompts$system_user}).
#' @param example Optional character vector containing example input texts 
#' (defaults to \code{examples$example_user_text}).
#' @param answer Optional character vector containing corresponding example model outputs 
#' (defaults to \code{examples$example_user_answer}).
#' @param schema Optional JSON-like list defining the expected output structure 
#' (defaults to \code{schemata$schema_user}).
#' @param retries Integer. Maximum number of re-query attempts for responses that fail JSON validation. 
#' Default is \code{3}.
#' @param retry_trunc Integer. Character limit for truncating user text during retry attempts to prevent 
#' context overflow. Default is \code{4000}.
#' @param seed Integer. Random seed used for model sampling; incremented by 1 on each retry. Default is \code{42}.
#' @param temp Numeric. Sampling temperature for model generation (0 = deterministic). Default is \code{0.0}.
#' @param verbose Logical. Whether to display progress messages and retry information. Default is \code{TRUE}.
#'
#' @return 
#' The input \code{groups_data} list with the \code{user_labels} element updated to include:
#' \describe{
#'   \item{\code{description}}{Concise summary generated by the model.}
#'   \item{\code{lang}}{Predicted language code (ISO format).}
#'   \item{\code{emotion_valence}}{Overall emotional tone ("positive", "negative", "neutral").}
#'   \item{\code{incivility}}{Presence of uncivil or offensive language ("yes", "no").}
#'   \item{\code{elaborate}}{Linguistic elaboration category ("simple", "moderate", or "elaborate").}
#'   \item{\code{confidence}}{Model-assessed confidence (0–1 scale).}
#'   \item{\code{topic_1}–\code{topic_5}}{Detected main topics.}
#'   \item{\code{pattern_1}–\code{pattern_3}}{Repeated emojis, slogans, or stylistic markers.}
#'   \item{\code{named_entity_1}–\code{named_entity_3}, \code{sentiment_1}–\code{sentiment_3}}{Named entities 
#'         and their corresponding sentiment labels.}
#'   \item{\code{is_valid_json}}{Logical flag indicating valid JSON structure in model output.}
#'   \item{\code{response}}{Raw model output (JSON string).}
#'   \item{\code{model}, \code{model_queried_at}, \code{model_total_duration}}{Metadata for reproducibility.}
#' }
#'
#' @details
#' The function constructs few-shot prompts by combining examples and instructions, sends them to the 
#' locally hosted model using [rollama::query()], and validates the returned JSON using \pkg{jsonlite}.
#' If a response fails validation, it retries up to \code{retries} times with shorter input text 
#' (\code{retry_trunc}) and incremented seed values.
#'
#' The schema guides the model to output structured JSON containing linguistic, emotional, and stylistic annotations 
#' for each user. Common fields include `description`, `lang`, `topic`, `named_entities`, `repetitive_patterns`, 
#' `emotion_valence`, `incivility`, `elaborate`, and `confidence`.
#'
#' Ollama must be accessible locally (e.g., via Docker). Use [rollama::ping_ollama()] to confirm connectivity.  
#'
#' @seealso 
#' [sample_user_text()], [rollama::query()], [rollama::make_query()], 
#' [jsonlite::fromJSON()], [data.table::as.data.table()]
#'
#' @export
label_users <- function(groups_data, 
                        model = "llama3.2:3b",
                        prompt = NULL,
                        system = NULL,
                        example = NULL,
                        answer = NULL,
                        schema = NULL,
                        retries = 3, 
                        retry_trunc = 4000, 
                        seed = 42,
                        temp = 0.0,
                        verbose = TRUE) {
  
  if (!"user_labels" %in% names(groups_data)) {
    stop("No 'user_labels' found. Please first sample content by 'sample_user_text'.")
  }
  
  # Set prompts and examples
  if(is.null(prompt)){
    prompt_user <- prompts$prompt_user
  }else{
    prompt_user <- prompt
  }
  
  if(is.null(system)){
    system_user <- prompts$system_user
  }else{
    system_user <- system
  }
  
  if(is.null(example)){
    example_user_text <- examples$example_user_text
  }else{
    example_user_text <- example
  }
  
  if(is.null(answer)){
    example_user_answer <- examples$example_user_answer
  }else{
    example_user_answer <- answer
  }
  
  if(is.null(schema)){
    schema_user <- schemata$schema_user
  }else{
    schema_user <- schema
  }
  
  user_labels <- data.table::copy(groups_data$user_labels)
  
  if (!requireNamespace("rollama", quietly = TRUE)) {
    stop("Package 'rollama' is required for this function. Please install it.")
  }
  
  ping_res <- try(rollama::ping_ollama(), silent = TRUE)
  if (!ping_res[[1]]) stop("Ollama is not running.")
  
  example_resp <- tibble::tibble(
    text = paste0("Annotate:", example_user_text), 
    answer = example_user_answer
  )
  
  retry <- 0
  res_all <- list()
  user_current <- user_labels
  
  for (retry in 0:retries) {
    
    if (verbose) cli::cli_inform("Retry round {retry}. Querying {nrow(user_current)} users...")
    
    queries <- rollama::make_query(
      text = paste0("\n### EXAMPLES END ###\n\n", ### Clarify example boundaries to prevent context contamination
                    "### NEW INPUT ###\n",
                    "Annotate: \n",
                    user_current$text),
      template = "\n{text}\n{prompt}",
      prompt = prompt_user,
      system = paste0(system_user, 
                      "\n### EXAMPLES START ###\n"),
      example = example_resp
    )
    
    responses <- rollama::query(
      queries,
      model = model,
      screen = TRUE,
      model_params = list(seed = seed + retry, temperature = temp),
      verbose = TRUE,
      format = schema_user
    )
    
    res_df <- user_current |>
      dplyr::mutate(
        response = purrr::map_chr(responses, ~ purrr::pluck(.x, "message", "content")),
        model = purrr::map_chr(responses, ~ purrr::pluck(.x, "model")),
        model_queried_at = purrr::map_chr(responses, ~ purrr::pluck(.x, "created_at")),
        model_total_duration = purrr::map_dbl(responses, ~ purrr::pluck(.x, "total_duration"))
      ) |>
      dplyr::mutate(
        parsed_json = purrr::map(
          response,
          ~ tryCatch(jsonlite::fromJSON(.x), error = function(e) NULL)
        ),
        is_valid_json = purrr::map_lgl(response, jsonlite::validate),
        
        # --- Scalar fields ---
        description = purrr::map_chr(parsed_json, ~ .x$description %||% NA_character_),
        lang = purrr::map_chr(parsed_json, ~ .x$lang %||% NA_character_),
    #    emotion_valence = purrr::map_chr(parsed_json, ~ .x$emotion_valence %||% NA_character_),
        incivility = purrr::map_chr(parsed_json, ~ .x$incivility %||% NA_character_),
        elaborate = purrr::map_chr(parsed_json, ~ .x$elaborate %||% NA_character_),
        confidence = purrr::map_dbl(parsed_json, ~ .x$confidence %||% NA_real_)
      ) |>
      dplyr::mutate(
        # --- Topics ---
        topic = purrr::map(parsed_json, ~ base::unlist(.x$topic) %||% character()),
        topic_1 = purrr::map_chr(topic, ~ .x[1] %||% NA_character_),
        topic_2 = purrr::map_chr(topic, ~ .x[2] %||% NA_character_),
        topic_3 = purrr::map_chr(topic, ~ .x[3] %||% NA_character_),
        topic_4 = purrr::map_chr(topic, ~ .x[4] %||% NA_character_),
        topic_5 = purrr::map_chr(topic, ~ .x[5] %||% NA_character_),
        
        # --- Repetitive patterns ---
        repetitive_patterns = purrr::map(parsed_json, ~ base::unlist(.x$repetitive_patterns) %||% character()),
        pattern_1 = purrr::map_chr(repetitive_patterns, ~ .x[1] %||% NA_character_),
        pattern_2 = purrr::map_chr(repetitive_patterns, ~ .x[2] %||% NA_character_),
        pattern_3 = purrr::map_chr(repetitive_patterns, ~ .x[3] %||% NA_character_),
        
        # --- Named entities ---
        named_entities = purrr::map(parsed_json, ~ base::as.data.frame(.x$named_entities) %||% base::data.frame()),
        named_entity_1 = purrr::map_chr(named_entities, ~ .x$entity[1] %||% NA_character_),
        sentiment_1     = purrr::map_chr(named_entities, ~ .x$sentiment[1] %||% NA_character_),
        named_entity_2 = purrr::map_chr(named_entities, ~ .x$entity[2] %||% NA_character_),
        sentiment_2     = purrr::map_chr(named_entities, ~ .x$sentiment[2] %||% NA_character_),
        named_entity_3 = purrr::map_chr(named_entities, ~ .x$entity[3] %||% NA_character_),
        sentiment_3     = purrr::map_chr(named_entities, ~ .x$sentiment[3] %||% NA_character_),
        named_entity_4 = purrr::map_chr(named_entities, ~ .x$entity[4] %||% NA_character_),
        sentiment_4     = purrr::map_chr(named_entities, ~ .x$sentiment[4] %||% NA_character_),
        named_entity_5 = purrr::map_chr(named_entities, ~ .x$entity[5] %||% NA_character_),
        sentiment_5     = purrr::map_chr(named_entities, ~ .x$sentiment[5] %||% NA_character_)
        
      ) |>
      dplyr::select(
        -parsed_json,
        -topic,
        -repetitive_patterns,
        -named_entities
      )
    
    res_all[[retry + 1]] <- res_df
    
    if (all(res_df$is_valid_json)) {
      if (verbose) cli::cli_inform("All answers parsed successfully.")
      break
    }
    
    # Prepare failed users for retry
    failed_ids <- dplyr::filter(res_df, !is_valid_json)$account_id
    if (length(failed_ids) == 0 || retry == retries) {
      if (verbose) cli::cli_inform("Stopping after retry {retry}.")
      break
    }
    
    user_current <- user_labels |>
      dplyr::filter(account_id %in% failed_ids) |>
      dplyr::mutate(text = stringr::str_trunc(text, retry_trunc))
  }
  
  
  res_df <- dplyr::bind_rows(res_all)
  
  # De-duplicate (for parsing errors)
  res_df <- res_df |>
    dplyr::group_by(account_id) |>
    dplyr::slice_max(order_by = is_valid_json, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |> 
    dplyr::arrange(community, -user_share_comm)
  
  res_dt <- data.table::as.data.table(res_df)
  
  groups_data$user_labels <- res_dt
  
  return(groups_data)
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



#' Label Communities Using a Local LLM
#'
#' This function uses a locally hosted large language model (LLM) via the \pkg{rollama} interface
#' to generate structured labels and descriptive summaries for user communities. It first labels
#' each community (or, if large, its individual slices) based on user-level annotations and text,
#' then optionally aggregates slice-level results in a second LLM pass to produce one coherent
#' community-level label and description.
#'
#' If malformed or incomplete JSON responses are returned, the function retries the query up to
#' a user-specified number of attempts. It supports both single-slice and multi-slice processing,
#' automatically detecting and aggregating communities that have been split during preprocessing.
#'
#' @param groups_data A list containing at least a \code{user_labels} element,
#' typically produced by [sample_user_text()]. This element must include community membership
#' and sampled post text for each user.
#' @param max_n_per_slice Integer. Maximum number of users per slice when splitting large
#' communities. Default is \code{10}.
#' @param min_chars Integer. Minimum character length required for a valid slice input.
#' Default is \code{1}.
#' @param max_chars Integer. Maximum allowed character length per slice (longer input will be truncated).
#' Default is \code{1000}.
#' @param min_share Optional numeric. Minimum cumulative posting share (\code{user_share_comm})
#' per slice. Default is \code{NULL} (no filter).
#' @param model Character string. The local LLM model to query via Ollama
#' (e.g., \code{"llama3.2:3b"}). Default is \code{"llama3.2:3b"}.
#' @param retries Integer. Maximum number of re-query attempts for invalid or unparseable
#' JSON responses. Default is \code{3}.
#' @param retry_trunc Integer. Character limit for truncating text inputs in retry rounds.
#' Default is \code{4000}.
#' @param seed Integer. Random seed for reproducibility. Default is \code{42}.
#' @param temp Numeric. Sampling temperature for model generation
#' (\code{0} = deterministic). Default is \code{0.0}.
#' @param prompt Character. User-level prompt text used to instruct the model when labeling
#' communities (overrides internal \code{prompts$prompt_comm}). Optional.
#' @param system Character. System-level instruction guiding the model’s behavior
#' (overrides internal \code{prompts$system_comm}). Optional.
#' @param system_slices Character. Optional override for \code{prompts$system_comm_slices},
#' used for the aggregation step that combines slice-level labels into one community summary.
#' @param example Character vector containing few-shot example inputs for community-level labeling.
#' Defaults to \code{examples$example_comm_text}.
#' @param example_slices Character vector containing few-shot example inputs for aggregated
#' slice-level labeling. Defaults to \code{examples$example_comm_slices_text}.
#' @param answer Character vector of corresponding example model outputs for community-level labeling.
#' Defaults to \code{examples$example_comm_answer}.
#' @param answer_slices Character vector of example outputs for aggregated slice-level labeling.
#' Defaults to \code{examples$example_comm_slices_answer}.
#' @param schema Optional JSON-like list defining the expected structured output format.
#' Defaults to \code{schemata$schema_comm}.
#' @param verbose Logical. Whether to print progress messages, retry information,
#' and aggregation updates. Default is \code{TRUE}.
#'
#' @return 
#' The same \code{groups_data} list, updated with a new \code{community_labels} element.
#' This is a \pkg{data.table} with one row per labeled community containing:
#' \describe{
#'   \item{\code{community}}{Community identifier.}
#'   \item{\code{label}}{LLM-generated label for the community.}
#'   \item{\code{description}}{Concise LLM-generated summary of the community’s tone, style, and main topics.}
#'   \item{\code{lang}}{Predominant language inferred by the model.}
#'   \item{\code{topic_1–topic_5}}{Up to five thematic categories assigned by the model.}
#'   \item{\code{named_entity_1–named_entity_5}, \code{sentiment_1–sentiment_5}}{Named entities and associated sentiments.}
#'   \item{\code{pattern_1–pattern_3}}{Recurring emojis, slogans, or stylistic markers.}
#'   \item{\code{incivility}}{Whether the community often uses uncivil or offensive language.}
#'   \item{\code{elaborate}}{Average linguistic elaboration level ("simple", "moderate", or "elaborate").}
#'   \item{\code{confidence}}{Model-estimated confidence score between 0 and 1.}
#'   \item{\code{text}}{Input text provided to the model (either slice or aggregated JSON).}
#'   \item{\code{is_valid_json}}{Logical flag indicating successful JSON validation.}
#'   \item{\code{model}, \code{model_queried_at}, \code{model_total_duration}}{Metadata about the model run.}
#' }
#'
#' @details
#' The function executes in two main stages:
#' \enumerate{
#'   \item \strong{Slice-level labeling:} If a community exceeds \code{max_n_per_slice} users, it is divided
#'   into smaller slices using [slice_community_text()]. Each slice is labeled independently by the LLM.
#'   \item \strong{Community-level aggregation:} For multi-slice communities, the slice-level labels and
#'   descriptions are combined into a JSON array and passed to the model again with
#'   \code{system_comm_slices} to generate a single coherent label and summary.
#' }
#'
#' JSON responses are validated using \pkg{jsonlite}; malformed responses trigger retries with truncated text
#' up to \code{retries} times. The function expects a locally running Ollama instance with the specified
#' model available. Verify setup using [rollama::ping_ollama()].
#'
#' @seealso
#' [sample_user_text()], [slice_community_text()], [rollama::query()], [rollama::make_query()], [jsonlite::fromJSON()]
#'
#' @export
label_communities <- function(groups_data,
                              max_n_per_slice = 10,
                              min_chars = 1,
                              max_chars = 1000,
                              min_share = NULL,
                              model = "llama3.2:3b",
                              retries = 3, 
                              retry_trunc = 4000, 
                              seed = 42,
                              temp = 0.0,
                              prompt = NULL,
                              system = NULL,
                              system_slices = NULL,
                              example = NULL,
                              example_slices = NULL,
                              answer = NULL,
                              answer_slices = NULL,
                              schema = NULL,
                              verbose = TRUE) {
  
  if (!"user_labels" %in% names(groups_data)) {
    stop("No 'user_labels' found. Please first sample content by 'sample_user_text'.")
  }
  
  # Set prompts and examples
  if(is.null(prompt)){
    prompt_comm <- prompts$prompt_comm
  }else{
    prompt_comm <- prompt
  }
  
  if(is.null(system)){
    system_comm <- prompts$system_comm
  }else{
    system_comm <- system
  }
  
  if(is.null(system_slices)){
    system_comm_slices <- prompts$system_comm_slices
  }else{
    system_comm_slices <- system_slices
  }
  
  if(is.null(example)){
    example_comm_text <- examples$example_comm_text
  }else{
    example_comm_text <- example
  }
  
  if(is.null(example_slices)){
    example_comm_slices_text <- examples$example_comm_slices_text
  }else{
    example_comm_slices_text <- example_slices
  }
  
  if(is.null(answer)){
    example_comm_answer <- examples$example_comm_answer
  }else{
    example_comm_answer <- answer
  }
  
  if(is.null(answer_slices)){
    example_comm_slices_answer <- examples$example_comm_slices_answer
  }else{
    example_comm_slices_answer <- answer_slices
  }
  
  if(is.null(schema)){
    schema_comm <- schemata$schema_comm
  }else{
    schema_comm <- schema
  }
  
  # Calculate comm stats
  post_data <- data.table::copy(groups_data$post_data)
  comm_stats <- post_data[, .(
    n_posts = data.table::uniqueN(post_id),
    n_accs = data.table::uniqueN(account_id)
  ), by = community]
  
  user_labels <- data.table::copy(groups_data$user_labels)
  top_lang <- user_labels[, .N, by = .(community, lang)][
    order(-N), .SD[1], by = community
  ][, .(community, lang)]
  
  comm_stats <- top_lang[comm_stats, on = "community"]
  
  # Bind user texts to community texts
  comm_texts <-  slice_community_text(
    groups_data = groups_data,
    max_n_per_slice = max_n_per_slice,
    min_chars = min_chars,
    max_chars = max_chars,
    min_share = min_share,
    verbose = verbose,
    seed = seed)
  
  
  # Ensure a clean, modifiable copy
  comm_labels <- data.table::copy(comm_texts)
  data.table::setDT(comm_labels)  # Coerce to data.table just in case
  
  # Create slice id
  comm_labels <- comm_labels |> dplyr::mutate(id = paste0(community, "-", slice))
  
  if (!requireNamespace("rollama", quietly = TRUE)) {
    stop("Package 'rollama' is required for this function. Please install it.")
  }
  
  ping_res <- try(rollama::ping_ollama(), silent = TRUE)
  if (!ping_res[[1]]) stop("Ollama is not running.")
  
  example_resp <- tibble::tibble(
    text = example_comm_text, 
    answer = example_comm_answer
  )
  
  retry <- 0
  res_all <- list()
  comm_current <- comm_labels
  
  for (retry in 0:retries) {
    
    if (verbose) cli::cli_inform("Retry round {retry}. Querying {nrow(comm_current)} communities...")
    
    queries <- rollama::make_query(
      text = comm_current$text_slice,
      template = "{prefix}\n{text}\n{prompt}",
      prompt = prompt_comm,
      prefix = "Annotate:",
      system = system_comm,
      example = example_resp
    )
    
    responses <- rollama::query(
      queries,
      model = model,
      screen = TRUE,
      model_params = list(seed = seed + retry, 
                          temperature = temp + retry *(2e-2)),
      format = schema_comm,
      verbose = TRUE
    )
    
    # Plucking
    res_df <- comm_current |>
      dplyr::mutate(
        response = purrr::map_chr(responses, ~ purrr::pluck(.x, "message", "content")),
        model = purrr::map_chr(responses, ~ purrr::pluck(.x, "model")),
        model_queried_at = purrr::map_chr(responses, ~ purrr::pluck(.x, "created_at")),
        model_total_duration = purrr::map_dbl(responses, ~ purrr::pluck(.x, "total_duration"))
      ) |>
      dplyr::mutate(
        parsed_json = purrr::map(
          response,
          ~ tryCatch(jsonlite::fromJSON(.x), error = function(e) NULL)
        ),
        is_valid_json = purrr::map_lgl(response, jsonlite::validate),
        
        # --- Scalar fields ---
        label = purrr::map_chr(parsed_json, ~ .x$label %||% NA_character_),
        description = purrr::map_chr(parsed_json, ~ .x$description %||% NA_character_),
        lang = purrr::map_chr(parsed_json, ~ .x$lang %||% NA_character_),
        #    emotion_valence = purrr::map_chr(parsed_json, ~ .x$emotion_valence %||% NA_character_),
        incivility = purrr::map_chr(parsed_json, ~ .x$incivility %||% NA_character_),
        elaborate = purrr::map_chr(parsed_json, ~ .x$elaborate %||% NA_character_),
        confidence = purrr::map_dbl(parsed_json, ~ .x$confidence %||% NA_real_)
      ) |>
      dplyr::mutate(
        # --- Topics ---
        topic = purrr::map(parsed_json, ~ base::unlist(.x$topic) %||% character()),
        topic_1 = purrr::map_chr(topic, ~ .x[1] %||% NA_character_),
        topic_2 = purrr::map_chr(topic, ~ .x[2] %||% NA_character_),
        topic_3 = purrr::map_chr(topic, ~ .x[3] %||% NA_character_),
        topic_4 = purrr::map_chr(topic, ~ .x[4] %||% NA_character_),
        topic_5 = purrr::map_chr(topic, ~ .x[5] %||% NA_character_),
        
        # --- Repetitive patterns ---
        repetitive_patterns = purrr::map(parsed_json, ~ base::unlist(.x$repetitive_patterns) %||% character()),
        pattern_1 = purrr::map_chr(repetitive_patterns, ~ .x[1] %||% NA_character_),
        pattern_2 = purrr::map_chr(repetitive_patterns, ~ .x[2] %||% NA_character_),
        pattern_3 = purrr::map_chr(repetitive_patterns, ~ .x[3] %||% NA_character_),
        
        # --- Named entities ---
        named_entities = purrr::map(parsed_json, ~ base::as.data.frame(.x$named_entities) %||% base::data.frame()),
        named_entity_1 = purrr::map_chr(named_entities, ~ .x$entity[1] %||% NA_character_),
        sentiment_1     = purrr::map_chr(named_entities, ~ .x$sentiment[1] %||% NA_character_),
        named_entity_2 = purrr::map_chr(named_entities, ~ .x$entity[2] %||% NA_character_),
        sentiment_2     = purrr::map_chr(named_entities, ~ .x$sentiment[2] %||% NA_character_),
        named_entity_3 = purrr::map_chr(named_entities, ~ .x$entity[3] %||% NA_character_),
        sentiment_3     = purrr::map_chr(named_entities, ~ .x$sentiment[3] %||% NA_character_),
        named_entity_4 = purrr::map_chr(named_entities, ~ .x$entity[4] %||% NA_character_),
        sentiment_4     = purrr::map_chr(named_entities, ~ .x$sentiment[4] %||% NA_character_),
        named_entity_5 = purrr::map_chr(named_entities, ~ .x$entity[5] %||% NA_character_),
        sentiment_5     = purrr::map_chr(named_entities, ~ .x$sentiment[5] %||% NA_character_)
      ) |>
      dplyr::select(
        -parsed_json,
        -topic,
        -repetitive_patterns,
        -named_entities
      )
    
    
    res_df <- res_df |> dplyr::mutate(l_d = sapply(description, length),
                                      l_l = sapply(label, length),
                                      is_valid_json = ifelse(l_d + l_l != 2, FALSE, is_valid_json)) |> 
      dplyr::select(-l_d, -l_l)
    
    res_all[[retry + 1]] <- res_df
    
    if (all(res_df$is_valid_json)) {
      if (verbose) cli::cli_inform("All answers parsed successfully.")
      break
    }
    
    # Prepare failed users for retry
    failed_ids <- dplyr::filter(res_df, !is_valid_json)$id
    if (length(failed_ids) == 0 || retry == retries) {
      if (verbose) cli::cli_inform("Stopping after retry {retry}.")
      break
    }
    
    comm_current <- comm_labels |>
      dplyr::filter(id %in% failed_ids) |>
      dplyr::mutate(text_slice = stringr::str_trunc(text_slice, retry_trunc))
  }
  
  
  res_df <- dplyr::bind_rows(res_all)
  
  # De-duplicate (for parsing errors)
  res_df <- res_df |>
    dplyr::group_by(id) |>
    dplyr::slice_max(order_by = is_valid_json, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |> 
    dplyr::arrange(community)
  
  
  res_dt <- data.table::as.data.table(res_df)
  
  # Split sliced and unsliced communities
  sliced_dt <- res_dt[res_dt[, .N, by = community][N > 1], on = "community"]
  unsliced_dt <- res_dt[res_dt[, .N, by = community][N == 1], on = "community"]
  
  # Process sliced data
  if(nrow(sliced_dt) > 0){
    
    if(verbose) cli::cli_inform("Aggregating {nrow(sliced_dt)} slices.")
    
    sliced_dt <- sliced_dt[nchar(text_slice) >= min_chars]
    
    # Aggregate
    sliced_dt_agg <- sliced_dt[
      , .(
        sum_user_share_comm_total = sum(sum_user_share_comm, na.rm = TRUE),
        
        # Build a proper JSON array of slice summaries
        text = jsonlite::toJSON(
          lapply(seq_len(.N), function(i) {
            list(
              slice = slice[i],
              share = round(sum_user_share_comm[i] * 100, 1),
              label = label[i],
              description = description[i]
            )
          }),
          auto_unbox = TRUE,
          pretty = FALSE
        )
      ),
      by = community
    ]
    
    example_slices_resp <- tibble::tibble(
      text = example_comm_slices_text, 
      answer = example_comm_slices_answer
    )
    
    retry <- 0
    res_all <- list()
    comm_current <- sliced_dt_agg
    
    for (retry in 0:retries) {
      
      if (verbose) cli::cli_inform("Retry round {retry}. Aggregating labels for {nrow(comm_current)} communities...")
      
      queries <- rollama::make_query(
        text = comm_current$text,
        template = "{prefix}\n{text}\n{prompt}",
        prompt = prompt_comm,
        prefix = "Annotate:",
        system = system_comm_slices,
        example = example_slices_resp
      )
      
      responses <- rollama::query(
        queries,
        model = model,
        screen = TRUE,
        model_params = list(seed = seed + retry, 
                            temperature = temp),
        format = schema_comm,
        verbose = TRUE
      )
      
      # Plucking
      res_df <- comm_current |>
        dplyr::mutate(
          response = purrr::map_chr(responses, ~ purrr::pluck(.x, "message", "content")),
          model = purrr::map_chr(responses, ~ purrr::pluck(.x, "model")),
          model_queried_at = purrr::map_chr(responses, ~ purrr::pluck(.x, "created_at")),
          model_total_duration = purrr::map_dbl(responses, ~ purrr::pluck(.x, "total_duration"))
        ) |>
        dplyr::mutate(
          parsed_json = purrr::map(
            response,
            ~ tryCatch(jsonlite::fromJSON(.x), error = function(e) NULL)
          ),
          is_valid_json = purrr::map_lgl(response, jsonlite::validate),
          
          # --- Scalar fields ---
          label = purrr::map_chr(parsed_json, ~ .x$label %||% NA_character_),
          description = purrr::map_chr(parsed_json, ~ .x$description %||% NA_character_),
          lang = purrr::map_chr(parsed_json, ~ .x$lang %||% NA_character_),
          #    emotion_valence = purrr::map_chr(parsed_json, ~ .x$emotion_valence %||% NA_character_),
          incivility = purrr::map_chr(parsed_json, ~ .x$incivility %||% NA_character_),
          elaborate = purrr::map_chr(parsed_json, ~ .x$elaborate %||% NA_character_),
          confidence = purrr::map_dbl(parsed_json, ~ .x$confidence %||% NA_real_)
        ) |>
        dplyr::mutate(
          # --- Topics ---
          topic = purrr::map(parsed_json, ~ base::unlist(.x$topic) %||% character()),
          topic_1 = purrr::map_chr(topic, ~ .x[1] %||% NA_character_),
          topic_2 = purrr::map_chr(topic, ~ .x[2] %||% NA_character_),
          topic_3 = purrr::map_chr(topic, ~ .x[3] %||% NA_character_),
          topic_4 = purrr::map_chr(topic, ~ .x[4] %||% NA_character_),
          topic_5 = purrr::map_chr(topic, ~ .x[5] %||% NA_character_),
          
          # --- Repetitive patterns ---
          repetitive_patterns = purrr::map(parsed_json, ~ base::unlist(.x$repetitive_patterns) %||% character()),
          pattern_1 = purrr::map_chr(repetitive_patterns, ~ .x[1] %||% NA_character_),
          pattern_2 = purrr::map_chr(repetitive_patterns, ~ .x[2] %||% NA_character_),
          pattern_3 = purrr::map_chr(repetitive_patterns, ~ .x[3] %||% NA_character_),
          
          # --- Named entities ---
          named_entities = purrr::map(parsed_json, ~ base::as.data.frame(.x$named_entities) %||% base::data.frame()),
          named_entity_1 = purrr::map_chr(named_entities, ~ .x$entity[1] %||% NA_character_),
          sentiment_1     = purrr::map_chr(named_entities, ~ .x$sentiment[1] %||% NA_character_),
          named_entity_2 = purrr::map_chr(named_entities, ~ .x$entity[2] %||% NA_character_),
          sentiment_2     = purrr::map_chr(named_entities, ~ .x$sentiment[2] %||% NA_character_),
          named_entity_3 = purrr::map_chr(named_entities, ~ .x$entity[3] %||% NA_character_),
          sentiment_3     = purrr::map_chr(named_entities, ~ .x$sentiment[3] %||% NA_character_),
          named_entity_4 = purrr::map_chr(named_entities, ~ .x$entity[4] %||% NA_character_),
          sentiment_4     = purrr::map_chr(named_entities, ~ .x$sentiment[4] %||% NA_character_),
          named_entity_5 = purrr::map_chr(named_entities, ~ .x$entity[5] %||% NA_character_),
          sentiment_5     = purrr::map_chr(named_entities, ~ .x$sentiment[5] %||% NA_character_)
        ) |>
        dplyr::select(
          -parsed_json,
          -topic,
          -repetitive_patterns,
          -named_entities
        )
      
      
      res_df <- res_df |> dplyr::mutate(l_d = sapply(description, length),
                                        l_l = sapply(label, length),
                                        is_valid_json = ifelse(l_d + l_l != 2, FALSE, is_valid_json)) |> 
        dplyr::select(-l_d, -l_l)
      
      res_all[[retry + 1]] <- res_df
      
      if (all(res_df$is_valid_json)) {
        if (verbose) cli::cli_inform("All answers parsed successfully.")
        break
      }
      
      # Prepare failed users for retry
      failed_ids <- dplyr::filter(res_df, !is_valid_json)$community
      if (length(failed_ids) == 0 || retry == retries) {
        if (verbose) cli::cli_inform("Stopping after retry {retry}.")
        break
      }
      
      comm_current <- sliced_dt_agg |>
        dplyr::filter(community %in% failed_ids) |>
        dplyr::mutate(text = stringr::str_trunc(text, retry_trunc))
    }
    
    res_df <- dplyr::bind_rows(res_all)
    
    # De-duplicate (for parsing errors)
    res_df <- res_df |>
      dplyr::group_by(community) |>
      dplyr::slice_max(order_by = is_valid_json, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |> 
      dplyr::arrange(community)
    
    sliced_dt <- data.table::as.data.table(res_df)
    
    # Harmonize variables
    sliced_dt[, sum_user_share_comm  := sum_user_share_comm_total][, c("sum_user_share_comm_total") := NULL]
    unsliced_dt[, text := text_slice][, c("text_slice", "id", "slice", "n", "N") := NULL]
    
    ## Bind together
    res_dt <- data.table::rbindlist(list(sliced_dt, unsliced_dt), use.names = T, fill = T, ignore.attr = T)
    
  }else{
    res_dt <- unsliced_dt[, text := text_slice][, c("text_slice", "id", "slice", "n", "N") := NULL]
  }
  
  res_dt <- merge(res_dt, comm_stats, by = "community", all.x = T)
  
  # Define your preferred column order
  main_cols <- c(
    "community", "n_accs", "n_posts", "sum_user_share_comm",
    "label", "description"
  )
  end_cols <- c(
    "is_valid_json", "response", "text",
    "model", "model_queried_at", "model_total_duration"
  )
  
  # Compute the remaining columns dynamically
  other_cols <- setdiff(names(res_dt), c(main_cols, end_cols))
  
  # Combine into final order
  final_col_order <- c(main_cols, other_cols, end_cols)
  
  # Reorder the data.table
  res_dt <- res_dt[, ..final_col_order]
  
  groups_data$community_labels <- res_dt
  
  return(groups_data)
}







