
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





#' Generate Community Labels and Descriptions Using Local LLM
#'
#' This function uses a locally hosted language model (LLM) through Ollama to generate labels and descriptions for communities based on sampled community content. The function includes retry logic and can switch to an alternative model if the primary model fails to produce results.
#'
#' @param groups_data A list containing community data, including `community_sample` with sampled community content for labeling and summarization.
#' @param llm Character. The primary model name hosted in Ollama to use for generating community labels and descriptions.
#' @param llm2 Character. An alternative model name to use if the primary model fails after the specified number of retries.
#' @param instruction Character. The instruction text to prepend to each community's content to guide the LLM's output.
#' @param retries Numeric. The maximum number of retry attempts to fill missing labels and descriptions if the primary model fails. Defaults to 3.
#' @param trunc Numeric. The character limit for truncating content before passing it to the model. Defaults to 4000.
#'
#' @return A modified version of `groups_data` with an additional `labelled_communities` data frame. This data frame includes:
#' - `label_generated`: The generated label for each community.
#' - `description_generated`: A descriptive summary for each community.
#' - `message.content`: The model’s raw output content for each query.
#' - `sampled_content`: The sampled text content from each community.
#' - `label_failed`: A flag indicating whether the function was unable to generate a label after all attempts.
#'
#' @details
#' The function proceeds through the following steps:
#' 1. **Initial Query**: Checks if Ollama is running and submits content queries to the primary model (`llm`) for labeling and summarization.
#' 2. **Label and Description Extraction**: Parses the model’s output to extract labels and descriptions based on specified patterns.
#' 3. **Retry Logic**: For communities missing labels or descriptions, retries with the primary model up to `retries` times, and if still unsuccessful, switches to `llm2`.
#' 4. **Final Output**: Combines results back into `groups_data`, appending `labelled_communities` with all generated labels and descriptions.
#'
#' @seealso \code{\link[rollama]{ping_ollama}}, \code{\link[rollama]{query}} for handling queries with Ollama models.
#'
#' @export
label_communities <- function(groups_data, 
                              llm, 
                              llm2, 
                              instruction, 
                              retries = 3, 
                              trunc = 4000) {
  
  
  ### Check if ollama is running
  
  ping_res <- try(rollama::ping_ollama(), silent = TRUE)
  
  if(!ping_res[[1]]) stop("Ollama is not running.")
  
  
  # Define query function
  process_queries <- function(instruction, content_vector, community_vector, model) {
    # Generate queries
    queries <- paste0(instruction, content_vector, sep = " \n ")
    
    # Process each query and retain the community identifier
    res <- lapply(seq_along(queries), function(i) {
      cli::cli_inform("Labelling community no. {community_vector[i]} with model {model}")
      r <- rollama::query(q = queries[i], model = model)
      r <- as.data.frame(r)
      r$query_text <- queries[i]  # Add the query text for reference
      r$community <- community_vector[i]  # Add the community identifier
      return(r)
    })
    
    # Combine all results, filling any missing columns
    r_df <- data.table::rbindlist(res, use.names = TRUE, fill = TRUE)
    
    # Group by `community` and `query_text`, then summarize if there are multiple rows per query
    r_df <- r_df |>
      dplyr::group_by(community, query_text) |>
      dplyr::summarize(dplyr::across(dplyr::everything(), ~ paste(.x, collapse = "; "), .names = "collapsed_{.col}"), .groups = "drop")
    
    # Remove the `collapsed_` prefix for consistency
    colnames(r_df) <- sub("^collapsed_", "", colnames(r_df))
    
    return(r_df)
  }
  
  # Initial query
  cli::cli_inform("Querying {llm}.")
  q_df <- groups_data$community_sample
  
  ## Truncate content
  if(!is.null(trunc)) content_vector <- q_df$sampled_content |> stringr::str_trunc(width = trunc)
  
  # Process queries and ensure that `community` is preserved
  r_df <- process_queries(instruction = instruction, 
                          content_vector = content_vector, 
                          community_vector = q_df$community,
                          model = llm)
  
  # Merge results back into the original data frame using `community`
  q_df <- merge(q_df, r_df, by = "community", all.x = TRUE)
  
  extract_labels_descriptions <- function(df) {
    df |>
      dplyr::mutate(
        # Perform splitting based on the first occurrence of "[DES" (to capture descriptions starting with any variation of [DESCRIPTION])
        split_text = stringr::str_split_fixed(message.content, "\\[DES.*?\\]", 2),
        
        # Take the first part before the [DES] as the label
        label_generated = stringr::str_trim(split_text[, 1]),
        
        # Take the second part after the [DES] as the description, if available
        description_generated = ifelse(split_text[, 2] != "", stringr::str_trim(split_text[, 2]), NA),
        
        # Remove unwanted terms from the label, especially [name:] or NAME
        label_generated = ifelse(
          stringr::str_detect(label_generated, "(?i)\\[name:?\\]|NAME"),
          "", 
          label_generated
        ),
        
        # Clean up label and description fields (remove square brackets, LABEL, DESCRIPTION remnants)
        label_generated = stringr::str_replace_all(label_generated, "\\[|\\]|:|(?i)LABE\\w*", "") |> stringr::str_trim(),
        description_generated = stringr::str_replace_all(description_generated, "\\[|\\]|:|(?i)DESCR\\w*", "") |> stringr::str_trim()
      ) |>
      dplyr::select(-split_text)  # Remove the intermediate split column
  }
  
  # Extract and then check for empty labels or descriptions
  q_df <- extract_labels_descriptions(q_df)  
  missing_rows <- dplyr::filter(q_df, is.na(label_generated) | is.na(description_generated) | label_generated == "" | description_generated == "")
  
  # Initialize retry counter and label_failed column
  retry_count <- 0
  q_df$label_failed <- FALSE  # Add a flag to indicate failure after 4 attempts
  
  
  # Loop until all labels and descriptions are filled or retry limit is reached
  while (nrow(missing_rows) > 0 && retry_count < retries) {
    retry_count <- retry_count + 1
    cli::cli_inform("Re-querying {nrow(missing_rows)} missing entries with trunctation {trunc}. Attempt {retry_count} of {retries}.")
    
    ## Truncate content
    if(!is.null(trunc)) content_vector <- missing_rows$sampled_content |> stringr::str_trunc(width = trunc)
    
    # Re-run the query for missing entries only
    r_df_retry <- process_queries(
      instruction = instruction, 
      content_vector = content_vector, 
      community_vector = missing_rows$community
    )
    
    # Merge re-queried results
    q_df <- dplyr::rows_update(q_df, r_df_retry, by = "community")
    
    # Re-extract labels and descriptions
    q_df <- extract_labels_descriptions(q_df)
    
    # Update missing rows
    missing_rows <- dplyr::filter(q_df, is.na(label_generated) | is.na(description_generated) | label_generated == "" | description_generated == "")
    
    # If the retry limit is reached, mark as failed
    if (retry_count == retries) {
      q_df$label_failed[q_df$community %in% missing_rows$community] <- TRUE
    }
  }
  
  if (nrow(missing_rows) > 0) {
    cli::cli_warn("Some labels and descriptions failed after {retries} attempts. Trying with {llm2}.")
    
    ## Truncate content
    if(!is.null(trunc)) content_vector <- missing_rows$sampled_content |> stringr::str_trunc(width = trunc)
    
    # Try llm2 for the failed entries
    r_df_llm2 <- process_queries(
      instruction = instruction,
      content_vector = content_vector,
      community_vector = missing_rows$community,
      model = llm2
    )
    
    # Merge llm2 results back into the original data frame
    q_df <- dplyr::rows_update(q_df, r_df_llm2, by = "community")
    
    # Re-extract labels and descriptions for llm2 output
    q_df <- extract_labels_descriptions(q_df)
    
    # Update missing rows
    missing_rows <- dplyr::filter(q_df, is.na(label_generated) | is.na(description_generated) | label_generated == "" | description_generated == "")
  }
  
  # Check if any rows still failed after trying both models
  if (nrow(missing_rows) > 0) {
    cli::cli_warn("Some labels and descriptions could not be filled even after using {llm2}.")
  }
  
  ## Merge parameters to samped texts to prepare export as .csv
  
  
  q_df <- q_df |> dplyr::select(community, label_generated, description_generated, message.content, sampled_content, dplyr::everything()) 
  
  ### File to store
  store_list <- append(groups_data, list(labelled_communities = q_df))
  
  return(store_list)
}

