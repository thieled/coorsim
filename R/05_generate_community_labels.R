
#' Sample cleaned and formatted post data per community for classification
#'
#' This function samples and cleans posts from social media community members,
#' returning one row per community with a markdown-style text summary, list of post IDs,
#' and estimated token count.
#'
#' @param groups_data A list containing `post_data` and `node_list` as data.tables.
#' @param additional_text_vars Character vector of additional text variables to include.
#' @param sampling_ratio Proportion of total posts to sample per community.
#' @param min_n Minimum number of posts per community.
#' @param max_n Maximum number of posts per community.
#' @param max_n_per_account Maximum number of posts per individual account.
#' @param min_chars Minimum number of characters for content to be retained.
#' @param max_text_length Maximum character length for each text field after cleaning.
#' @param clean_name Logical. Whether to clean account names.
#' @param verbose Logical. If TRUE, prints summary messages.
#' @param seed Integer seed for reproducible sampling.
#'
#' @return `groups_data`, with additional data.table `community_content` with columns: `community`, `text`, `sampled_post_ids`, `approx_tokens`.
#' @import data.table
#' @export
sample_community_text <- function(groups_data,
                                  additional_text_vars = NULL,
                                  sampling_ratio = 0.1,
                                  min_n = 20,
                                  max_n = 200,
                                  max_n_per_account = 10,
                                  min_chars = 20,
                                  max_text_length = 500,
                                  clean_name = FALSE,
                                  verbose = TRUE,
                                  seed = 42) {
  if (!"node_list" %in% names(groups_data) || !"post_data" %in% names(groups_data)) {
    stop("'groups_data' must contain 'node_list' and 'post_data'.")
  }
  
  set.seed(seed)
  post_data <- data.table::copy(groups_data$post_data)
  
  required_vars <- c("post_id", "account_id", "account_name", "content")
  all_vars <- unique(c(required_vars, additional_text_vars))
  
  if (!all(all_vars %in% names(post_data))) {
    stop("Missing required variables in 'post_data': ",
         paste(setdiff(all_vars, names(post_data)), collapse = ", "))
  }
  
  # Clean text function
  clean_text <- function(text, max_length) {
    text |>
      stringr::str_replace_all("([[:punct:]])\\1{1,}", "\\1") |>
      stringr::str_remove_all("\\p{Extended_Pictographic}") |>
      stringr::str_remove_all("\\b[a-fA-F0-9]{32,}\\b") |>
      stringr::str_remove_all("\\b\\d+\\b") |>
      stringr::str_replace_all("(?<!#)\\d", "") |>
      stringr::str_replace_all("[\r\n\t]+", " ") |>
      stringr::str_squish() |>
      stringr::str_trunc(width = max_length, ellipsis = "...")
  }
  
  # Clean content + additional text vars
  text_vars <- c("content", additional_text_vars)
  clean_var_lookup <- paste0(text_vars, "_clean")
  for (i in seq_along(text_vars)) {
    post_data[[clean_var_lookup[i]]] <- clean_text(post_data[[text_vars[i]]], max_text_length)
  }
  
  # Optionally clean account_name
  if (clean_name) {
    post_data[["account_name_clean"]] <- clean_text(post_data[["account_name"]], max_text_length)
  } else {
    post_data[["account_name_clean"]] <- post_data[["account_name"]]
  }
  
  # Filter short posts
  post_data <- post_data[nchar(content_clean) >= min_chars]
  
  # Sample posts per community and account
  sampled_dt <- post_data[
    , {
      acc_limited <- .SD[, utils::head(.SD, max_n_per_account), by = account_id]
      n_total <- nrow(acc_limited)
      n_sample <- min(max(min_n, ceiling(n_total * sampling_ratio)), max_n)
      acc_limited[sample(.N, min(.N, n_sample))]
    },
    by = community
  ]
  
  # Create markdown-style output per community
  dt_out <- sampled_dt[
    , {
      post_subset <- .SD[, c("account_name_clean", clean_var_lookup), with = FALSE]
      rows <- apply(post_subset, 1, function(row) {
        paste0("- ", row[["account_name_clean"]], ": ", paste(row[-1], collapse = " | "))
      })
      text_block <- paste(rows, collapse = " | ")
      token_estimate <- as.integer(round(length(strsplit(text_block, "\\s+")[[1]]) * 1.3))
      list(
        text = text_block,
        sampled_post_ids = list(.SD[["post_id"]]),  # now stored as list column
        approx_tokens = token_estimate
      )
    },
    by = community
  ]
  
  if (verbose) {
    cli::cli_inform("Returning {nrow(dt_out)} communities with sampled posts and metadata.")
  }
  
  groups_data$community_content <- dt_out
  
  return(groups_data)
}



#' Clean LLM-Generated JSON Strings
#'
#' This function attempts to clean and standardize LLM-generated JSON-like strings for valid parsing.
#' It extracts the first JSON-like object enclosed in curly braces, replaces single quotes with
#' double quotes where appropriate, and escapes backslashes as required for proper JSON formatting.
#'
#' @param json_strings A character vector of raw JSON-like strings returned by an LLM.
#'
#' @return A character vector of cleaned JSON strings suitable for parsing with `jsonlite::fromJSON()`.
#'
#' @examples
#' # Example 1: Fix single quotes
#' raw_json <- "{'label':'Test','description':'A test case','lang':'en'}"
#' clean_json_strings(raw_json)
#'
#' # Example 2: Input with backslashes
#' messy_json <- "{'label':'Path','description':'Folder path is C:\\\\Users\\\\','lang':'en'}"
#' clean_json_strings(messy_json)
#'
#' @export
clean_json_strings <- function(json_strings) {
  # Extract JSON-like content: start with the first '{' and include everything up to the first matching '}'
  json_cleaned <- gsub(".*?(\\{.*?\\}).*", "\\1", json_strings)
  
  # Replace single quotes around JSON keys/values with double quotes
  json_cleaned <- gsub("(?<=[:{,\\s])'(.*?)'", "\"\\1\"", json_cleaned, perl = TRUE)
  
  # Properly escape unescaped single quotes within JSON values
  json_cleaned <- gsub("(?<!\\\\)\\'", "'", json_cleaned, perl = TRUE)
  
  # Double-escape backslashes (needed for valid JSON parsing)
  json_cleaned <- gsub("\\\\", "\\\\\\\\", json_cleaned)
  
  return(json_cleaned)
}




#' Label Communities Using LLM
#'
#' This function takes sampled community content and uses a local LLM model via the `rollama` interface
#' to generate JSON-formatted labels, descriptions, and language tags for each community. If any results
#' fail JSON validation, the function retries with truncated input up to a specified number of times.
#'
#' @param groups_data A list containing `community_content` as returned by `sample_community_text()`.
#' @param model Character. The name of the local LLM model (must be available in Ollama).
#' @param retries Integer. Number of retry attempts for communities with invalid JSON.
#' @param retry_trunc Integer. Maximum character length to truncate input text during retries.
#'
#' @return The input `groups_data`, with `community_content` updated to include model-generated
#' columns: `label`, `description`, `lang`, `is_valid_json`, `response`, and metadata.
#' @export
label_communities <- function(groups_data, 
                              model = "llama3.2:3b-instruct-q8_0",
                              retries = 3, 
                              retry_trunc = 4000) {
  if (!"community_content" %in% names(groups_data)) {
    stop("No 'communities_content' found. Please first sample content by 'sample_community_text'.")
  }
  
  community_content <- data.table::copy(groups_data$community_content)
  
  ping_res <- try(rollama::ping_ollama(), silent = TRUE)
  if (!ping_res[[1]]) stop("Ollama is not running.")
  
  example_resp <- tibble::tribble(
    ~text, ~answer,
    example_text, example_answer
  )
  
  retry <- 0
  res_all <- list()
  community_current <- community_content
  
  repeat {
    queries <- rollama::make_query(
      text = community_current$text,
      template = "{prefix}\n{text}\n{prompt}",
      prompt = example_prompt,
      prefix = "Posts to summarize:",
      system = example_system,
      example = example_resp
    )
    
    responses <- rollama::query(
      queries,
      model = model,
      screen = TRUE,
      model_params = list(seed = seed),
      verbose = TRUE
    )
    
    res_df <- community_current |> 
      dplyr::mutate(
        response = purrr::map_chr(responses, ~ purrr::pluck(.x, "message", "content")),
        model = purrr::map_chr(responses, ~ purrr::pluck(.x, "model")),
        model_queried_at = purrr::map_chr(responses, ~ purrr::pluck(.x, "created_at")),
        model_total_duration = purrr::map_dbl(responses, ~ purrr::pluck(.x, "total_duration"))
      ) |>  
      dplyr::mutate(
        json_cleaned = clean_json_strings(response),
        parsed_json = lapply(json_cleaned, function(x) tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)),
        is_valid_json = sapply(json_cleaned, jsonlite::validate),
        label = sapply(parsed_json, function(x) if (is.null(x)) NA else x$label),
        description = sapply(parsed_json, function(x) if (is.null(x)) NA else x$description),
        lang = sapply(parsed_json, function(x) if (is.null(x)) NA else x$lang)
      ) |> 
      dplyr::select(-json_cleaned, -parsed_json)
    
    res_all[[retry + 1]] <- res_df
    
    if (all(res_df$is_valid_json) || retry >= retries) {
      break
    }
    
    retry <- retry + 1
    
    community_current <- dplyr::filter(res_df, !is_valid_json) |> 
      dplyr::left_join(community_sample, by = "community") |> 
      dplyr::mutate(text = stringr::str_trunc(text, retry_trunc))
  }
  
  res_df <- dplyr::bind_rows(res_all)
  
  groups_data$community_content <- res_df
  
  return(groups_data)
}
