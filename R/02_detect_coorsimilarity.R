#' Detect Coordinated Social Media Manipulation
#'
#' @description This function identifies highly similar social media posts shared within a specified time window,
#' aiming to detect coordinated social media manipulation. It uses either text-based sparse document-feature matrices
#' or numerical embeddings to compute pairwise similarity scores.
#'
#' @param data A `data.frame` or `data.table` containing social media data. Must include columns for post IDs, account IDs, timestamps, and content.
#' @param embeddings Optional. Embeddings for the posts, which can be provided in the following formats:
#'   \itemize{
#'     \item **A matrix**: Row names must correspond to `post_id` values.
#'     \item **A valid `.h5` file path**: The file must contain a `metadata/post_id` dataset.
#'     \item **NULL (default)**: Uses text-based similarity instead of embeddings.
#'   }
#' @param time_window Integer. Specifies the time window (in seconds) within which to detect similar posts. Default is `60`.
#' @param min_simil Numeric. The minimum similarity threshold to consider posts as similar. Default is `0.9`.
#' @param min_participation Integer. The minimum number of participations required to consider an account as part of coordinated activity. Default is `3`.
#' @param post_id Optional. Character string specifying the column name for post IDs if different from "post_id".
#' @param account_id Optional. Character string specifying the column name for account IDs if different from "account_id".
#' @param time Optional. Character string specifying the column name for timestamps if different from "time".
#' @param content Optional. Character string specifying the column name for post content if different from "content".
#' @param verbose Logical. If `TRUE`, displays progress messages. Default is `TRUE`.
#' @param method Character. Specifies the similarity method to use (e.g., "cosine"). Default is "cosine".
#' @param remove_loops Logical. If `TRUE`, removes self-similar posts from the same account. Default is `TRUE`.
#' @param parallel Logical. If `TRUE`, enables parallel computation for similarity calculations. Default is `TRUE`.
#' @param n_threads Optional. Integer specifying the number of threads for parallel computation. If `NULL`, defaults to available cores minus one.
#'
#' @details
#' The function follows four main steps:
#'   \enumerate{
#'     \item **Preprocess Data**: Standardizes column names, ensures valid timestamps, and prepares embeddings if provided.
#'     \item **Match Overlapping Posts**: Identifies posts published within the specified `time_window`.
#'     \item **Compute Similarities**: 
#'       \itemize{
#'         \item If `embeddings` are `NULL`, constructs a sparse document-feature matrix and calculates similarity scores.
#'         \item If `embeddings` are provided, queries and computes pairwise similarities using optimized C++ functions.
#'       }
#'     \item **Filter and Aggregate**: Joins account and timestamp data, filters accounts by `min_participation`, and removes redundant pairs.
#'   }
#'
#' @return A `data.table` containing pairs of similar posts with the following columns:
#'   \itemize{
#'     \item `post_id` - The ID of the first post in the pair.
#'     \item `post_id_y` - The ID of the second post in the pair.
#'     \item `similarity` - The computed similarity score.
#'     \item `account_id` - The account that posted `post_id`.
#'     \item `account_id_y` - The account that posted `post_id_y`.
#'     \item `time` - The timestamp of `post_id`.
#'     \item `time_y` - The timestamp of `post_id_y`.
#'     \item `time_diff` - The time difference between the two posts (in seconds).
#'   }
#'
#' @import data.table
#'
#' @export
detect_cosimilarity <- function(
    data,
    embeddings = NULL,
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
    parallel = TRUE,
    n_threads = NULL
) {
  
  
  ### Step 1: Preprocess data
  
  if(verbose) cli::cli_progress_step("[1/4]: Preprocessing.")
  
  data <- coorsim_prepare_data(
    data = data,
    embeddings = embeddings,
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
  
  
  ### Step 3: Calculate similarities
  
  # Check if embeddings are provided or not
  if(is.null(embeddings)){
    
    # Define function
    get_sparse_similarity <- function(ids, 
                                      data,
                                      method = "cosine",
                                      min_simil = .8,
                                      digits = 16L) {
      # Make a fresh subset of `data` (forcing a copy to avoid reference issues)
      data_subset <- data.table::copy(data[post_id %in% ids]) 
      
      # Construct the document-term matrix
      dtm <- text2map::dtm_builder(data = data_subset, 
                                   text = "content", doc_id = "post_id")
      
      # Compute pairwise similarity
      sim <- proxyC::simil(dtm,
                           margin = 1,  # Row-wise vectors of matrix
                           method = method,
                           use_nan = TRUE,
                           min_simil = min_simil,
                           drop0 = TRUE,
                           digits = digits)
      
      # Convert similarity matrix to triplet format
      triplet <- Matrix::mat2triplet(sim, uniqT = TRUE) |> data.table::as.data.table()
      
      # Assign column names efficiently
      data.table::set(triplet, j = "post_id", value = rownames(sim)[triplet$i])
      data.table::set(triplet, j = "i.post_id", value = colnames(sim)[triplet$j])
      
      # Subset in-place (avoiding unnecessary copies)
      triplet <- triplet[
        post_id != i.post_id & x > 0 & !is.na(x), 
        .(post_id, i.post_id, simil = x)  # Select relevant columns directly
      ]
      
      return(triplet)
    }
    
    
    if(parallel){ 
      
      if(is.null(n_threads)) n_threads = parallel::detectCores() - 1
      
      cli::cli_progress_step("[3/4]: Calculating similarities using sparse document-feature-matrixes and {n_threads} cores in parallel.",
                             msg_done = "[3/4]: Calculated similarities using sparse document-feature-matrixes in parallel.")
      
      # Set up parallel executionp
      future::plan(future::multisession, workers = n_threads)
      
      # Apply function in parallel
      pairwise_simil_list <- furrr::future_map(
        id_list, 
        ~ get_sparse_similarity(.x, data = data, method = method, min_simil = min_simil), 
        .progress = TRUE,
        .options = furrr::furrr_options(seed = TRUE)  # Ensures parallel-safe RNG
      )
      
      # Stop parallel processing when done
      future::plan(future::sequential)
      
    }else{
      
      cli::cli_progress_step("[3/4]: Calculating similarities using sparse document-feature-matrixes.",
                             msg_done = "[3/4]: Calculated similarities using sparse document-feature-matrixes.")
      
      
      # Apply function over `id_list`
      pairwise_simil_list <- pbapply::pblapply(id_list, function(ids) {
        get_sparse_similarity(ids, data = data, method = method, min_simil = min_simil)
      })
      
    }
    
    # Bind to simil_dt 
    simil_dt <- pairwise_simil_list |>
      data.table::rbindlist(use.names = T, fill = T) |>
      unique()
    
    # Harmonize variable names
    data.table::setnames(simil_dt, old = c("i.post_id", "simil"), new = c("post_id_y", "similarity"))
    
    
    if(verbose)  cli::cli_progress_done()
    
  }else{
    
    
    ### Step 3:  Query the vector matrix and calculate pairwise similarities using Cpp
    
    if(verbose)   cli::cli_progress_step("[3/4]: Querying embeddings and calculate similarities using C++.",
                                         msg_done = "[3/4]: Queried embeddings, calculated similarities using C++.")
    
    if(parallel){
      pairwise_simil_list <- query_and_compute_similarities_tbb(m = embeddings, post_id_lists = id_list, threshold = min_simil)
    }else{
      pairwise_simil_list <- query_and_compute_similarities(m = embeddings, post_id_lists = id_list, threshold = min_simil)  
    }
    
    # Bind to simil_dt 
    simil_dt <- pairwise_simil_list |>
      data.table::rbindlist(use.names = T, fill = T) |>
      unique()
    
    
    if(verbose)  cli::cli_progress_done()
    
  }
  
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
  simil_dt_x <- data[simil_dt, .(post_id, time, account_id, content, post_id_y, similarity), on = "post_id", nomatch = 0]
  
  # Set keys for the second join
  data.table::setkey(simil_dt_x, post_id_y)
  
  # Join data
  simil_dt <- simil_dt_x[data, .(post_id, time, account_id, content, post_id_y, similarity, account_id_y = i.account_id, time_y = i.time, content_y = i.content), 
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









#' Prepare Data for Co-Similarity Analysis
#'
#' @description This function processes and prepares social media post data for correlation similarity analysis.
#' It ensures that required columns are present, standardizes time formats, and optionally integrates
#' embeddings either from an in-memory matrix or an `.h5` file.
#'
#' @param data A `data.frame` or `data.table` containing post data. The table must include at least
#' `post_id`, `account_id`, `time`, and `content`, either directly or via specified alternative column names.
#' @param embeddings Embeddings for the posts, which can be provided in **three formats**:
#'   \itemize{
#'     \item **A matrix**: Row names must correspond to `post_id` values.
#'     \item **A valid `.h5` file path**: The file must contain a `metadata/post_id` dataset.
#'     \item **NULL (default)**: No embeddings are used.
#'   }
#' @param time_window Integer. The time window (in seconds) within which similar posts are detected. Default is `60`.
#' @param post_id Character. Alternative column name for `post_id`, if not present in `data`.
#' @param account_id Character. Alternative column name for `account_id`, if not present in `data`.
#' @param time Character. Alternative column name for `time`, if not present in `data`.
#' @param content Character. Alternative column name for `content`, if not present in `data`.
#' @param verbose Logical. If `TRUE`, displays progress messages. Default is `TRUE`.
#'
#' @details
#' This function performs the following preprocessing steps:
#'   \itemize{
#'     \item Ensures that `post_id`, `account_id`, `time`, and `content` exist in `data`,
#'           renaming alternative column names if provided.
#'     \item Drops duplicate posts based on `post_id`.
#'     \item Standardizes `time` to **Unix timestamps (UTC)** for cross-platform compatibility.
#'     \item Sorts posts in ascending order by `time`.
#'     \item If **embeddings are provided as a matrix**, retains only posts present in `rownames(embeddings)`.
#'     \item If **embeddings are provided as an `.h5` file**, verifies that `metadata/post_id` exists
#'           before extracting relevant post IDs.
#'   }
#'
#' The function also filters out invalid time formats and ensures efficient memory usage by removing the `content` column
#' from the final output.
#'
#' @return A processed `data.table` ready for correlation similarity analysis.
#'
#' @export
coorsim_prepare_data <- function(
    data,
    embeddings = NULL,
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
  
  
  # Drop duplicates in data
  if(any(duplicated(data, by = "post_id"))){
    warning("Duplicates in 'data' by 'post_id' detected and dropped.")
    data <- unique(data, by = "post_id")
  }
  
  
  ### Option 1: Embeddings provided as matrix 
  # Drop observations that are not represented in embeddings
  
  if(is.matrix(embeddings)) {
    
    if(verbose) cli::cli_inform("Embeddings provides as matrix fully loaded in memory.")
    
    # Assert that vector matrix has rownames
    assertthat::assert_that(!is.null(rownames(embeddings)), msg = 
                              "Please provide the post_ids as rownames() of 'embeddings'.")
    
    # Identify matched rows
    matched_indices <- data$post_id %in% rownames(embeddings)
    
    # Show message if rows are dropped and verbose is TRUE
    if (verbose) {
      dropped_count <- sum(!matched_indices)
      if (dropped_count > 0) {
        warning(paste0("Dropped n=", dropped_count, " posts without representation in 'embeddings'"))
      }
    }
    
    # Subset the data.table to keep only matched rows
    data <- data[matched_indices, ]
    
  }
  
  ### Option 2: Embeddings provided by .h5 file
  
  # Helper function to check if a valid .h5 file path is provided
  is_h5file <- function(v) {
    is.character(v) && file.exists(v) && grepl("\\.h5$", v, ignore.case = TRUE)
  }
  
  if (is_h5file(embeddings)) {
    
    if(verbose) cli::cli_inform("Embeddings provided by .h5 file.")
    
    # Check if 'metadata/post_id' exists in the .h5 file
    h5_contents <- rhdf5::h5ls(embeddings)
    
    # Ensure 'metadata/post_id' exists by checking both group and name
    metadata_exists <- any(h5_contents$group == "/metadata" & h5_contents$name == "post_id")
    
    if (!metadata_exists) {
      stop("Embeddings provided as .h5 file but 'metadata/post_id' was not found. ",
           "Please use 'coorsim::save_embeddings()' to retain a correct .h5 file.")
    }
    
    # Read post IDs
    ids <- rhdf5::h5read(embeddings, "metadata/post_id")
  }
  
  ### Option 3: No embeddings provided - do nothing
  if(is.null(embeddings)){
    if(verbose) cli::cli_inform("No embeddings provided.")
  }
  
  ### Time variables
  
  # Check if 'time' column is already in POSIXct or POSIXlt format
  if (!lubridate::is.POSIXct(data$time) && !lubridate::is.POSIXlt(data$time)) {
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
  
  # Convert to timestamp 
  data[, time := as.numeric(time)]
  
  # Sort by time
  data <- data[order(time)]
  
  # Filter out invalid datetime observations - also 
  if(verbose && any(is.na(data$time))) {
    n_error <- nrow(data[is.na(data$time)])
    warning(paste("Dropping", n_error, "observations due to invalid time formats."))
    data <- data[!is.na(data$time)]
  }
  
  
  # Join the data.table with itself based on the time window
  data[, end_time := time + time_window] # Define end time for the window
  
  # In y dt, created_time and end_time should be identical
  data[, time_dup := time]
  data.table::setkey(data, time, time_dup) # Set key for join
  
  # Split off content table - to economize memory
  #content_dt <- data[, .(post_id, content)]
  #data <- data[, !("content"), with = FALSE]
  
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



