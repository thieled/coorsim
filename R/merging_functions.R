#' Augment a Similarity Table with Post and User Data
#'
#' This function merges data from `post_data` and `user_data` into a similarity table (`sim_dt`). 
#' It adds columns for post content and user information while handling potential name collisions.
#'
#' @param post_data A `data.table` containing post data.
#' @param user_data A `data.table` containing user data.
#' @param sim_dt A `data.table` containing similarity data between posts.
#' @param post_id Optional. The column name in `post_data` to be renamed to 'post_id' for joining.
#' @param account_id Optional. The column name in `user_data` to be renamed to 'account_id' for joining.
#' @param account_name Optional. The column name in `user_data` to be renamed to 'account_name'.
#' @param content Optional. The column name in `post_data` containing the post content to be renamed to 'content'.
#' @param other_post_vars Optional. Additional columns to retain from `post_data`.
#' @param other_user_vars Optional. Additional columns to retain from `user_data`.
#' @param verbose Logical. If `TRUE`, provides verbose output.
#'
#' @return A `data.table` with `post_data` and `user_data` merged into `sim_dt`.
#' @export
augment_similarity_table <- function(
    post_data,
    user_data,
    sim_dt,
    post_id = NULL, 
    account_id = NULL, 
    account_name = NULL,
    content = NULL,
    other_post_vars = NULL,
    other_user_vars = NULL,
    verbose = FALSE
) {
  # Ensure inputs are data.tables if not already
  if (!data.table::is.data.table(sim_dt)) sim_dt <- data.table::as.data.table(sim_dt)
  if (!data.table::is.data.table(post_data)) post_data <- data.table::as.data.table(post_data)
  if (!data.table::is.data.table(user_data)) user_data <- data.table::as.data.table(user_data)
  
  # Check and rename post_data columns
  if (verbose) cat("Harmonizing 'post_data'...\n")
  
  # Rename columns if alternative names are given and match
  post_col_renames <- list(post_id = post_id, content = content)
  for (col in names(post_col_renames)) {
    alt_name <- post_col_renames[[col]]
    if (!is.null(alt_name) && alt_name %in% names(post_data)) {
      data.table::setnames(post_data, alt_name, col)
    }
  }
  
  # Select relevant columns from post_data
  post_cols_to_keep <- c("post_id", "content", other_post_vars)
  post_data <- post_data[, intersect(post_cols_to_keep, names(post_data)), with = FALSE]
  
  ### De-duplicating post_data ###
  if (any(duplicated(post_data$post_id))){
    if (verbose) cat("De-duplicating 'post_data'...\n")
    post_data <- post_data[!duplicated(post_data$post_id)]
  }
  
  
  # Check and rename user_data columns
  if (verbose) cat("Harmonizing 'user_data'...\n")
  
  user_col_renames <- list(account_id = account_id, account_name = account_name)
  for (col in names(user_col_renames)) {
    alt_name <- user_col_renames[[col]]
    if (!is.null(alt_name) && alt_name %in% names(user_data)) {
      data.table::setnames(user_data, alt_name, col)
    }
  }
  
  # Add prefix to user_data columns (except account_id)
  user_cols_to_keep <- c("account_id", "account_name", other_user_vars)
  user_data <- user_data[, intersect(user_cols_to_keep, names(user_data)), with = FALSE]
  user_cols <- setdiff(names(user_data), c("account_id", "account_name"))
  data.table::setnames(user_data, user_cols, paste0("account_", user_cols))
  
  ### De-duplicating user_data ###
  if (any(duplicated(user_data$user_id))){
    if (verbose) cat("De-duplicating 'user_data'...\n")
    user_data <- user_data[!duplicated(user_data$account_id)]
  } 
  ### Merging process using data.table's direct join syntax ###
  
  # Join 'post_data' to 'sim_dt' by "post_id" (left join)
  if (verbose) cat("Merging 'post_data' with 'sim_dt' by 'post_id'...\n")
  sim_dt <- post_data[sim_dt, on = "post_id", nomatch = 0]
  
  ### Copy post_data before second join to avoid i. prefix ###
  post_data_y <- data.table::copy(post_data)
  
  ### Add _y suffix to post_data columns before second join
  data.table::setnames(post_data_y, old = names(post_data_y), new = paste0(names(post_data_y), "_y"))
  
  # Join 'post_data_y' to 'sim_dt' by "post_id_y" (left join)
  if (verbose) cat("Merging 'post_data_y' with 'sim_dt' by 'post_id_y'...\n")
  sim_dt <- post_data_y[sim_dt, on = "post_id_y", nomatch = 0]
  
  # Join 'user_data_y' to 'sim_dt' by "account_id_y" (left join)
  if (verbose) cat("Merging 'user_data_y' with 'sim_dt' by 'account_id'...\n")
  sim_dt <- user_data[sim_dt, on = "account_id", nomatch = 0]
  
  ### Copy user_data before second join to avoid i. prefix ###
  user_data_y <- data.table::copy(user_data)
  
  ### Add _y suffix to user_data columns before second join
  data.table::setnames(user_data_y, old = names(user_data_y), new = paste0(names(user_data_y), "_y"))
  
  # Join 'user_data_y' to 'sim_dt' by "account_id_y" (left join)
  if (verbose) cat("Merging 'user_data_y' with 'sim_dt' by 'account_id_y'...\n")
  sim_dt <- user_data_y[sim_dt, on = "account_id_y", nomatch = 0]
  
  # Reorder columns in the desired order
  desired_order <- c("post_id", "post_id_y", "similarity", 
                     "content", "content_y",
                     "time", "time_y",
                     "account_id", "account_id_y",
                     "account_name", "account_name_y")
  
  # Combine the desired order with the remaining columns
  remaining_columns <- setdiff(names(sim_dt), desired_order)
  new_column_order <- c(desired_order, remaining_columns)
  
  # Reorder the data.table columns
  sim_dt <- sim_dt[, ..new_column_order]
  
  # Return the augmented similarity table
  return(sim_dt)
}





#' Augment Group Data with Post and User Information
#'
#' This function merges additional post and user data into a grouped data structure, `groups_data`.
#' It supports renaming columns for consistency, de-duplication, and optional content sampling.
#'
#' @param groups_data A list with at least one element `node_list` (a `data.table` containing node information).
#'                    Optionally, it may also contain `post_data` (a `data.table` of post information).
#' @param post_data A `data.table` containing additional post data to be merged.
#' @param user_data A `data.table` containing user data to be merged.
#' @param post_id Optional. Column name in `post_data` to be renamed to 'post_id' for joining.
#' @param account_id Optional. Column name in `user_data` and `groups_data` to be renamed to 'account_id' for joining.
#' @param content Optional. Column name in `post_data` containing post content, renamed to 'content'.
#' @param other_post_vars Optional. Additional columns to retain from `post_data`.
#' @param other_user_vars Optional. Additional columns to retain from `user_data`.
#' @param sample_content Logical. If `TRUE`, samples content from `post_data` by `account_id` and merges sampled text into `node_list`.
#' @param sample_n Integer. Number of posts to sample per `account_id` if `sample_content` is `TRUE`. Defaults to 10.
#' @param sep Character. Separator used to concatenate sampled content. Defaults to " +++ ".
#' @param seed Integer. Random seed for reproducibility of sampled content. Defaults to 42.
#' @param verbose Logical. If `TRUE`, provides detailed output about each processing step.
#'
#' @return A list, `groups_data`, with the following modifications:
#'   - `post_data` augmented with user information and additional post columns, if present in `groups_data`.
#'   - `node_list` augmented with user data and optionally, sampled content from `post_data`.
#'
#' @details
#' This function harmonizes column names and performs left joins to add relevant data. When `sample_content`
#' is `TRUE`, it samples `sample_n` posts from `post_data` per `account_id`, concatenates them with the specified separator `sep`,
#' and adds the resulting strings to `node_list`.
#'
#' @export
augment_groups_data <- function(
    groups_data,
    post_data,
    user_data,
    post_id = NULL,
    account_id = NULL,
    content = NULL,
    other_post_vars = NULL,
    other_user_vars = NULL,
    sample_content = FALSE,
    sample_n = 10,
    sep = " +++ ",
    seed = 42,
    verbose = FALSE
) {
  # Ensure inputs are data.tables if not already
  if (!data.table::is.data.table(groups_data$node_list)) groups_data$node_list <- data.table::as.data.table(groups_data$node_list)
  if ("post_data" %in% names(groups_data)) {
    if (!data.table::is.data.table(groups_data$post_data)) groups_data$post_data <- data.table::as.data.table(groups_data$post_data)
  }
  if (!data.table::is.data.table(post_data)) post_data <- data.table::as.data.table(post_data)
  if (!data.table::is.data.table(user_data)) user_data <- data.table::as.data.table(user_data)
  
  ## Copy data.tables
  posts <- data.table::copy(post_data)
  users <- data.table::copy(user_data)
  
  
  # Check and rename posts columns
  if (verbose) cli::cli_inform("Harmonizing 'posts'...\n")
  
  # Rename columns if alternative names are given and match
  post_col_renames <- list(post_id = post_id, content = content)
  for (col in names(post_col_renames)) {
    alt_name <- post_col_renames[[col]]
    if (!is.null(alt_name) && alt_name %in% names(posts)) {
      data.table::setnames(posts, alt_name, col)
    }
  }
  
  # Select relevant columns from posts
  post_cols_to_keep <- c("post_id", "content", other_post_vars)
  posts <- posts[, intersect(post_cols_to_keep, names(posts)), with = FALSE]
  
  ### De-duplicating posts ###
  if (any(duplicated(posts$post_id))){
    if (verbose) cat("De-duplicating 'posts'...\n")
    posts <- posts[!duplicated(posts$post_id)]
  }
  
  
  # Check and rename users columns
  if (verbose) cli::cli_inform("Harmonizing 'users'...\n")
  
  user_col_renames <- list(account_id = account_id)
  for (col in names(user_col_renames)) {
    alt_name <- user_col_renames[[col]]
    if (!is.null(alt_name) && alt_name %in% names(users)) {
      data.table::setnames(users, alt_name, col)
    }
  }
  
  # Add prefix to users columns (except account_id)
  user_cols_to_keep <- c("account_id", other_user_vars)
  users <- users[, intersect(user_cols_to_keep, names(users)), with = FALSE]
  user_cols <- setdiff(names(users), c("account_id"))
  data.table::setnames(users, user_cols, paste0("account_", user_cols))
  
  ### De-duplicating users ###
  if (any(duplicated(users$user_id))){
    if (verbose) cli::cli_inform("De-duplicating 'users'...\n")
    users <- users[!duplicated(users$account_id)]
  }
  
  ### Merging process using data.table's direct join syntax ###
  # Join 'posts' to 'posts' by "post_id" (left join)
  if ("post_data" %in% names(groups_data)) {
    if (verbose) cli::cli_inform("Merging 'post_data' with 'groups_data$post_data' by 'post_id'...\n")
    groups_data$post_data <- posts[groups_data$post_data, on = "post_id", nomatch = 0]
    
    # Join 'users' to 'post_data' by "account_id" (left join)
    if (verbose) cli::cli_inform("Merging 'users' with 'groups_data$post_data' by 'account_id'...\n")
    groups_data$post_data <- users[groups_data$post_data, on = "account_id", nomatch = 0]
    
    # Reorder columns in the desired order
    desired_order <- c("post_id",
                       "time",
                       "content",
                       "account_id",
                       "account_name",
                       "community",
                       "component",
                       "algorithm",
                       "parameters"
    )
    
    # Combine the desired order with the remaining columns
    remaining_columns <- setdiff(names(groups_data$post_data), desired_order)
    new_column_order <- c(desired_order, remaining_columns)
    
    # Reorder the data.table columns
    groups_data$post_data <- groups_data$post_data[, ..new_column_order]
    
    # Sample content
    if(sample_content){
      if (verbose) cli::cli_inform("Sampling {sample_n} posts from 'groups_data$post_data' per 'account_id'...\n")
      set.seed(seed)
      content_sample <- groups_data$post_data[, .(sampled_content = paste(sample(content, min(.N, sample_n)), collapse = sep)), by = account_id]
    }
    
  }
  
  # Join 'users' to 'node_list' by "account_id" (left join)
  if (verbose) cli::cli_inform("Merging 'users' with 'groups_data$node_list' by 'account_id'...\n")
  groups_data$node_list <- users[groups_data$node_list, on = "account_id", nomatch = 0, verbose = F]
  
  if(sample_content){
    if (verbose) cli::cli_inform("Merging {sample_n} sampled posts to 'groups_data$node_list' by 'account_id'...\n")
    groups_data$node_list <- content_sample[groups_data$node_list, on = "account_id", nomatch = 0]
  }
  
  # Reorder columns in the desired order
  desired_order <- c("account_id",
                     "account_name",
                     "community",
                     "component",
                     "algorithm",
                     "parameters"
  )
  
  # Combine the desired order with the remaining columns
  remaining_columns <- setdiff(names(groups_data$node_list), desired_order)
  new_column_order <- c(desired_order, remaining_columns)
  
  # Reorder the data.table columns
  groups_data$node_list <- groups_data$node_list[, ..new_column_order]
  
  # Return the augmented similarity table
  return(groups_data)
}
