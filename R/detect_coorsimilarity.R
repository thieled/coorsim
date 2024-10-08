#' Detect Coordinated Social Media Manipulation
#'
#' This function detects highly similar social media content shared within a specified time window,
#' aiming to capture coordinated social media manipulation. It uses embeddings to calculate the 
#' cosine similarity between posts.
#'
#' @param data A data.frame or data.table containing social media data. Must include columns for post IDs, account IDs, timestamps, and content.
#' @param vector_matrix A matrix of embeddings where columns are embedding dimensions and rows are documents, with rownames() indicating the post IDs. Default is NULL.
#' @param time_window An integer specifying the time window (in seconds) within which to detect similar posts. Default is 60.
#' @param min_simil A numeric value specifying the minimum cosine similarity threshold to consider posts as similar. Default is 0.9.
#' @param min_participation An integer specifying the minimum number of participations required to consider an account as part of coordinated activity. Default is 3.
#' @param post_id Optional column name for post IDs if not named 'post_id' in `data`.
#' @param account_id Optional column name for account IDs if not named 'account_id' in `data`.
#' @param time Optional column name for timestamps if not named 'time' in `data`.
#' @param content Optional column name for content if not named 'content' in `data`.
#' @param verbose A logical value indicating whether to display progress messages. Default is TRUE.
#' @param method A character string specifying the similarity method to use. Default is "cosine".
#' @param remove_loops A logical value indicating whether to remove loops (self-similar posts from the same account). Default is TRUE.
#' @param parallel A logical value indicating whether tbb for parallel processing should be used. Also see set_num_threads. 
#'
#' @return A data.table containing pairs of similar posts along with their account IDs and similarity scores.
#'
#' @import data.table
#' 
#' @export
detect_cosimilarity <- function(
    data,
    vector_matrix = NULL,
    time_window = 60,
    min_simil = .9,
    min_participation = 3,
    post_id = NULL, 
    account_id = NULL, 
    time = NULL,
    content = NULL,
    verbose = TRUE,
    method = "cosine", 
    remove_loops = TRUE,
    parallel = TRUE
) {
  
  
  ### Step 1: Preprocess data
  
  if(verbose) cli::cli_progress_step("[1/4]: Preprocessing.")
  
  data <- coorsim_prepare_data(
                data = data,
                vector_matrix = vector_matrix,
                time_window = time_window,
                post_id = post_id, 
                account_id = account_id, 
                time = time,
                content = content,
                verbose = verbose
              )
              
  # End preprocess 
  if(verbose) cli::cli_process_done()
  
  
  
  ### Step 2: Match overlapping posts by time_window
  
  if(verbose)  cli::cli_progress_step("[2/4]: Matching posts published within {time_window}s.",
                           msg_done = "[2/4]: Matched posts published within {time_window}s.")
    
    id_list <- coorsim_match_overlaps(data = data)
    
  if(verbose) cli::cli_progress_done()
  
  
  ### Step 3:  Query the vector matrix and calculate pairwise similarities using Cpp

  if(verbose)   cli::cli_progress_step("[3/4]: Querying embeddings and calculate similarities using C++.",
                                         msg_done = "[3/4]: Queried embeddings, calculated similarities using C++.")
    
    if(parallel){
      pairwise_simil_list <- query_and_compute_similarities_tbb(m = vector_matrix, post_id_lists = id_list, threshold = min_simil)
    }else{
      pairwise_simil_list <- query_and_compute_similarities(m = vector_matrix, post_id_lists = id_list, threshold = min_simil)  
    }
    
    
    if(verbose)  cli::cli_progress_done()
    

  # Bind to simil_dt 
  simil_dt <- pairwise_simil_list |>
    data.table::rbindlist(use.names = T, fill = T) |>
    unique()
  
  # Subset
  simil_dt <- simil_dt[
    post_id != post_id_y &
    similarity > 0 &
    !is.na(similarity)
  ]
  

  
  ### Step 4: Merging account and time data
  
  if(verbose) cli::cli_progress_step("[4/4]: Filter accounts by min_participation={min_participation}",
                                     msg_done = "[4/4]: Filtered accounts by min_participation={min_participation}")
  
 
  ### Join account_ids
  
  # Set keys for fast joins
  data.table::setkey(data, post_id)
  data.table::setkey(simil_dt, post_id)
  
  
  # Join 'data' with 'simil_dt' on 'post_id', retaining 'account_id'
  simil_dt_x <- data[simil_dt, .(post_id, time, account_id, post_id_y, similarity), on = "post_id", nomatch = 0]
  
  # Set keys for the second join
  data.table::setkey(simil_dt_x, post_id_y)
  
  # Join data
  simil_dt <- simil_dt_x[data, .(post_id, time, account_id, post_id_y, similarity, account_id_y = i.account_id, time_y = i.time), 
                         on = c("post_id_y" = "post_id"), nomatch = 0]
  
  # Compute difftime variable
  simil_dt[, time_diff := difftime(time1 = time_y, time2 = time, units = "secs")]  
  
  
  ### Filter by n account occurences
  
  # Remove loops from identical account
  if(remove_loops) simil_dt <- simil_dt[account_id != account_id_y]
  
  # Count occurrences of each account_id and account_id_y
  account_counts <- data.table::rbindlist(list(
    simil_dt[, .(account_id)], 
    simil_dt[, .(account_id = account_id_y)]
  ))[, .N, by = account_id]
  
  # Filter accounts based on min_participation
  account_counts <- account_counts[N >= min_participation, account_id]
  
  # Filter simil_dt to keep only rows where account_id and account_id_y are in account_counts
  simil_dt <- simil_dt[account_id %in% account_counts & account_id_y %in% account_counts]
  
  if(verbose) cli::cli_progress_done()
  
  return(simil_dt)
}







#' Prepare Data for Correlation Similarity Analysis
#'
#' This function prepares data for correlation similarity analysis by performing various checks and transformations 
#' on the input data and vector matrix. It ensures that the necessary columns are present, converts data types, and 
#' subsets the data as required.
#'
#' @param data A data frame or data table containing the posts data.
#' @param vector_matrix A matrix with vector represenations for social media items. The row names should correspond to post IDs.
#' @param time_window An integer specifying the time window (in seconds) within which to detect similar posts. Default is 60.
#' @param post_id A character string representing the alternative name for the post ID column in the data (if not named "post_id").
#' @param account_id A character string representing the alternative name for the account ID column in the data (if not named "account_id").
#' @param time A character string representing the alternative name for the time column in the data (if not named "time").
#' @param content A character string representing the alternative name for the content column in the data (if not named "content").
#' @param verbose A logical value indicating whether to display progress messages. Default is TRUE.
#'
#' @return A data table with the prepared data, including necessary transformations and subsetting.
#' @export
coorsim_prepare_data <- function(
    data,
    vector_matrix = NULL,
    time_window = 60,
    post_id = NULL, 
    account_id = NULL, 
    time = NULL,
    content = NULL,
    verbose = TRUE
    ) {
  
  # Assert that data is data frame or table
  assertthat::assert_that(is.data.frame(data), msg =  
                            "Please provide 'data' in data.frame or data.table format.")
  
  # Assert that vector matrix is a matrix
  assertthat::assert_that(is.matrix(vector_matrix), msg = 
                            "Please provide a 'vector_matrix' in matrix format.")
  
  # Assert that vector matrix has rownames
  assertthat::assert_that(!is.null(rownames(vector_matrix)), msg = 
                            "Please provide the post_ids as rownames() of 'vector_matrix'.")
  
  # Convert to data.table if data.frame
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  ### Column names
  
  # Define the required columns and their alternative names
  required_columns <- c("post_id", 
                        "account_id", 
                        "time", 
                        "content")
  alternative_names <- list(post_id = post_id, 
                            account_id = account_id, 
                            time = time, 
                            content = content)
  
  # Create a logical vector to identify missing columns
  missing_columns <- !required_columns %in% names(data)
  
  # Function to check and rename columns if missing
  check_and_rename <- function(col) {
    alt_name <- alternative_names[[col]]
    assertthat::assert_that(!is.null(alt_name), 
                            msg = paste("'", col, "' not specified and not found in 'data'.", sep = ""))
    assertthat::assert_that(alt_name %in% names(data), 
                            msg = paste("Alternative name for '", col, "' provided but not found in 'data'.", sep = ""))
    data.table::setnames(data, old = alt_name, new = col)
  }
  
  # Apply the function to missing columns
  if (any(missing_columns)) {
    invisible(lapply(required_columns[missing_columns], check_and_rename))
  }
  
  # Subset the data table to these four columns
  data <- data[, required_columns, with = FALSE]
  
  
  ### Drop observations that are not represented in vector_matrix
  
  # Identify matched rows
  matched_indices <- data$post_id %in% rownames(vector_matrix)
  
  # Show message if rows are dropped and verbose is TRUE
  if (verbose) {
    dropped_count <- sum(!matched_indices)
    if (dropped_count > 0) {
      warning(paste0("Dropped n=", dropped_count, " posts without representation in 'vector_matrix'"))
    }
  }
  
  # Subset the data.table to keep only matched rows
  data <- data[matched_indices, ]
  
  
  ### Time variables
  
  # Check if 'time' column is already in POSIXct or POSIXlt format
  if (!lubridate::is.POSIXct(data$time) || !lubridate::is.POSIXlt(data$time)) {
    # Parse 'time' column to datetime
    parsed_time <- lubridate::parse_date_time(data$time, orders = c("ymd HMS", "ymd HM", "ymd H", "ymd", "dmy HMS", "dmy HM", "dmy H", "dmy"))
    
    # Filter out invalid datetime observations
    if(verbose && any(is.na(parsed_time))) {
      n_error <- nrow(data[is.na(parsed_time)])
      warning(paste("Dropping", n_error, "observations due to invalid time formats."))
    }
    data <- data[!is.na(parsed_time)]
    # Convert to POSIXct format and set to UTC for valid observations
    data[, time := as.POSIXct(lubridate::with_tz(parsed_time[!is.na(parsed_time)], tzone = "UTC"))]
    
  }
  
  
  # Join the data.table with itself based on the time window
  data[, end_time := time + time_window] # Define end time for the window
  
  # In y dt, created_time and end_time should be identical
  data[, time_dup := time]
  data.table::setkey(data, time, time_dup) # Set key for join
  
  # Split off content table - to economize memory
  content_dt <- data[, .(post_id, content)]
  data <- data[, !("content"), with = FALSE]
  
  return(data)
  
}
  
  






#' Match Overlapping Posts by Time Window
#'
#' This function matches overlapping posts within a specified time window, removes duplicated pairs,
#' and groups overlaps by `post_id` using data.table functions.
#'
#' @param data A data.table containing the columns `post_id`, `account_id`, `time`, `end_time`, and `time_dup`.
#' 
#' @return A named list where each name corresponds to a `post_id` and each element is a vector of post IDs that overlap within the specified time window.
#' @export
#' 
coorsim_match_overlaps <- function(data){
  
    # Match overlapping posts by time_window
  
    overlaps <- data.table::foverlaps(data, data,
                                      by.x = c("time", "end_time"),
                                      by.y = c("time", "time_dup"),
                                      mult = "all",
                                      type="any",
                                      nomatch=0L,
                                      verbose = F)
    
    # Rename and subset columns, remove "diagonal" pairs
    overlaps <- data.table::setnames(overlaps[, .(post_id, account_id, time, i.post_id, i.account_id, i.time)], 
                         old = c("i.post_id", "i.account_id", "i.time"), 
                         new = c("post_id_y", "account_id_y", "time_y"))[
                           post_id != post_id_y
                         ]

  # Function to remove duplicated pairs -- (benchmarked) 
  remove_dup_pairs <- function(dt) {
    dt[, c("post_id", "post_id_y") := .(pmin(post_id, post_id_y), # sorts both IDs 
                                        pmax(post_id, post_id_y))]
    unique(dt) # drops non-unique pairs
  }
  
  overlaps <- remove_dup_pairs(overlaps)
  
  
  # Split overlaps in groups by post_id - using data.table functions -- (benchmarked)
  id_list <- overlaps[, .(id_vector = list(unique(c(.BY[[1]], post_id_y)))), by = post_id]
  id_list <- stats::setNames(id_list$id_vector, id_list$post_id)
  
  return(id_list)
  
}



