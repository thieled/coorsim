
### TO DO:
### Use huggingface models and conda-coorsim infrastructure instead of rollama. 


#' Prepare and Clean Account Texts
#'
#' This function processes account-related text data in a given `groups_data` object. It cleans post content, 
#' samples a specified number of posts per account, and prepares a consolidated text output containing account 
#' names, bios, and locations. This output is added to the `node_list` in `groups_data`.
#'
#' @param groups_data A list containing `post_data` and `node_list` data tables, where `post_data` includes posts associated with each account, and `node_list` includes account metadata.
#' @param sample_n Numeric. The number of posts to sample per account. Defaults to 5.
#' @param sep Character. A separator to use between sampled posts in the aggregated output. Defaults to `" + "`.
#' @param trunc_width Numeric. The maximum character width for truncating texts. Set to `NULL` for no truncation. Defaults to 100.
#' @param min_n_char Numeric. The minimum number of characters a post must have to be included in sampling. Defaults to 5.
#' @param verbose Logical. If `TRUE`, provides informational messages about each processing step. Defaults to `TRUE`.
#' @param seed Numeric. A seed for random sampling, ensuring reproducibility. Defaults to 42.
#'
#' @return A modified version of `groups_data`, with a new `account_text_prep` field in the `node_list`, which consolidates the cleaned name, location, bio, and sampled posts for each account.
#'
#' @details
#' The function proceeds through the following steps:
#' 1. Cleans the post content by removing URLs, mentions, and special characters, and truncates texts if `trunc_width` is specified.
#' 2. Filters posts by `min_n_char` and samples up to `sample_n` posts per account.
#' 3. Merges the sampled post content with `node_list` in `groups_data`.
#' 4. Cleans and truncates account metadata, such as names, descriptions, and locations.
#' 5. Creates a consolidated text output for each account in the `account_text_prep` field.
#'
#' @export
prepare_account_texts <- function(groups_data,
                                  sample_n = 5,
                                  sep = " + ",
                                  trunc_width = 100,
                                  min_n_char = 5,
                                  verbose = TRUE,
                                  seed = 42){
  
  ### Helper functions
  
  # Define a cleaning function
  clean_text <- function(text, 
                         trunc_width = NULL) {
    cleaned_text <- text |> 
      stringr::str_remove_all("(f|ht)(tp)(s?)://\\S+|www\\.\\S+")  |>         # Remove hyperlinks
      stringr::str_remove_all("@\\w+")  |>                 # Remove @mentions
      stringr::str_remove_all("[\\p{So}\\p{Cn}\\U0001F000-\\U0001FAFF]") |>  # Normalize unusual fonts and symbols 
      stringr::str_replace_all("[\r\n]+", " ")  |>         # Replace line breaks with a space
      stringr::str_squish()  # Conditionally truncate if trunc_width is provided
    
    # Conditionally truncate if trunc_width is provided
    if (!is.null(trunc_width)) {
      cleaned_text <- stringr::str_trunc(cleaned_text, width = trunc_width, ellipsis = ".")
    }
    
    return(cleaned_text)
  }
  
 
  
  data.table::setDT(groups_data$post_data)
  
  ### Step 1: Cleaning and sampling Tweets from accounts
  
  if (verbose) cli::cli_inform("Step 1: Cleaning and sampling Tweets from accounts...\n")
  
  # Cleaning
  groups_data$post_data$content_clean <- clean_text(groups_data$post_data$content, trunc_width = trunc_width)
  
  # Drop too short texts
  filtered_data <- groups_data$post_data[ , nchar := nchar(content_clean)][nchar >= min_n_char, ]
  # Create a sampled subset of post_data
  set.seed(seed)
  sampled_data <- filtered_data[, .SD[sample(.N, min(.N, sample_n))], by = "account_id"]
  
  
  # Aggregate the sampled subset to create the variables
  content_sample <- sampled_data[, .(
    sampled_content = paste(content_clean, collapse = sep)
  ), by = account_id]
  
  
  ### Step 2: Bind to nodes_list
  
  # Dropping existing sample
  if("sampled_content" %in% names(groups_data$node_list)) groups_data$node_list[, sampled_content := NULL]
  

  
  if (verbose) cli::cli_inform("Step 2: Merging {sample_n} sampled posts to 'groups_data$node_list' by 'account_id'...\n")
  groups_data$node_list <- merge(x = groups_data$node_list, y = content_sample, by = "account_id", all.x = T)
  
  ### Step 3: Prepare names and account bios
  
  if (verbose) cli::cli_inform("Step 3: Prepare names and bios...\n")
  groups_data$node_list$account_name_full_clean <- clean_text(groups_data$node_list$account_name_full, trunc_width = trunc_width)
  groups_data$node_list$account_description_clean <- clean_text(groups_data$node_list$account_description, trunc_width = trunc_width)
  groups_data$node_list$account_location_clean <- clean_text(groups_data$node_list$account_location, trunc_width = trunc_width)
  
  
  ### Step 4: Bind the text info:
  groups_data$node_list[, account_text_prep := paste(
    "[name:] ", account_name_full_clean,
    " [loc:] ", account_location_clean,
    " [bio:] ", account_description_clean,
    " [posts:] ", sampled_content, 
    sep = ""
  ) |> stringr::str_replace_all(" NA ", " ")]
  
  ### Return
  return(groups_data)
  
}






#' Prepare and Aggregate Community Texts
#'
#' This function prepares and aggregates text data for each community within a `groups_data` object. It first processes individual 
#' account data, then samples accounts within each community to create a summarized text field for the community, including 
#' sampled and all account identifiers.
#'
#' @param groups_data A list containing `post_data` and `node_list` data tables, where `post_data` includes posts associated with each account, and `node_list` includes account metadata.
#' @param sample_n Numeric. The number of posts to sample per account. Defaults to 5.
#' @param sep Character. A separator to use between sampled posts or account identifiers in the aggregated output. Defaults to `" + "`.
#' @param trunc_width Numeric. The maximum character width for truncating texts. Set to `NULL` for no truncation. Defaults to 100.
#' @param min_n_char Numeric. The minimum number of characters a post must have to be included in sampling. Defaults to 5.
#' @param sample_n_users Numeric. The number of accounts to sample per community. Defaults to 10.
#' @param verbose Logical. If `TRUE`, provides informational messages about each processing step. Defaults to `TRUE`.
#' @param seed Numeric. A seed for random sampling, ensuring reproducibility. Defaults to 42.
#'
#' @return A modified version of `groups_data` with an additional `community_sample` data table. This table includes:
#' - `sampled_content`: Aggregated text data from sampled accounts within each community.
#' - `sampled_accounts`: Account IDs of the sampled accounts within each community.
#' - `all_accounts`: All account IDs within each community.
#' - `community_size`: The total number of accounts within each community.
#'
#' @details
#' The function proceeds through the following steps:
#' 1. Calls `prepare_account_texts()` to clean and sample text data for individual accounts.
#' 2. Samples up to `sample_n_users` accounts per community to create a summarized text representation for the community.
#' 3. Aggregates sampled text data and stores all account IDs and the community size within each community.
#'
#' @seealso \code{\link{prepare_account_texts}} for individual account-level text preparation.
#'
#' @export

prepare_community_texts <- function(groups_data,
                                    sample_n = 5,
                                    sep = " + ",
                                    trunc_width = 100,
                                    min_n_char = 5,
                                    sample_n_users = 10,
                                    verbose = TRUE,
                                    seed = 42){
  
  
  prepared_text_data <- prepare_account_texts(groups_data = groups_data, 
                                              sample_n = sample_n, 
                                              sep = sep, 
                                              trunc_width = trunc_width, 
                                              min_n_char = min_n_char, 
                                              verbose = verbose, 
                                              seed = seed
  )
  
  groups_data <- prepared_text_data
  
  
  ### Sampling users 
  
  if (verbose) cli::cli_inform("Sampling users from communities...\n")
  
  
  # Create a sampled subset of user data
  set.seed(seed)
  sampled_data <- prepared_text_data$node_list[, .SD[sample(.N, min(.N, sample_n_users))], by = community]
  
  # Aggregate the sampled subset to create the variables
  content_sample <- sampled_data[, .(
    sampled_content = paste(account_text_prep, collapse = sep),
    sampled_accounts = paste(account_id, collapse = "; ")
  ), by = community]
  
  
  ### Store community size
  content_sample[ , all_accounts := groups_data$node_list[, paste(account_id, collapse = "; "), by=community][["V1"]]]
  content_sample[ , community_size := groups_data$node_list[, .N, by=community][["N"]]]
  
  # Store as
  groups_data$community_sample <- content_sample
  
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

