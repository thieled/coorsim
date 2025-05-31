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
    verbose = FALSE) {
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
  if (any(duplicated(post_data$post_id))) {
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
  if (any(duplicated(user_data$user_id))) {
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
  desired_order <- c(
    "post_id", "post_id_y", "similarity",
    "content", "content_y",
    "time", "time_y",
    "account_id", "account_id_y",
    "account_name", "account_name_y"
  )

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
#' It supports renaming columns for consistency, de-duplication, and optional community annotation in the similarity table.
#'
#' @param groups_data A list with at least one element `node_list` (a `data.table` containing node information).
#'                    Optionally, it may also contain `post_data` and `sim_dt`, which will be augmented.
#' @param post_data A `data.table` containing additional post data to be merged.
#' @param user_data A `data.table` containing user data to be merged.
#' @param post_id Optional. Column name in `post_data` to be renamed to `'post_id'` for joining.
#' @param account_id Optional. Column name in `user_data` and `groups_data` to be renamed to `'account_id'` for joining.
#' @param other_post_vars Optional. Additional columns to retain from `post_data`.
#' @param other_user_vars Optional. Additional columns to retain from `user_data`.
#' @param sim_dt_community Logical. If `TRUE`, community identifiers from `node_list` will be joined onto `sim_dt`,
#'                         matching on `account_id` and `account_id_y`. The resulting `sim_dt` will be reordered
#'                         to follow the structure: x-side → similarity → y-side → parameters.
#' @param verbose Logical. If `TRUE`, provides detailed output about each processing step.
#'
#' @return A list, `groups_data`, with the following possible modifications:
#' \itemize{
#'   \item \code{post_data}: augmented with user information and additional post columns, if present.
#'   \item \code{node_list}: augmented with user data and optionally sampled content from post data.
#'   \item \code{sim_dt}: (if \code{sim_dt_community = TRUE}) augmented with \code{community} and \code{community_y}
#'         for \code{account_id} and \code{account_id_y}, and columns reordered accordingly.
#' }
#'
#' @details
#' This function standardizes column names, selects relevant fields, and performs efficient `data.table` joins.
#' User-level data is prefixed with \code{"account_"} before merging. If \code{sim_dt_community = TRUE}, community IDs
#' are merged into `sim_dt` and the column order is standardized to place the x-side account and post information first,
#' followed by similarity metrics, y-side information, and parameter columns.
#'
#' @export
augment_groups_data <- function(
    groups_data,
    post_data,
    user_data,
    post_id = NULL,
    account_id = NULL,
    other_post_vars = NULL,
    other_user_vars = NULL,
    sim_dt_community = TRUE,
    verbose = FALSE) {
  
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
  post_col_renames <- list(post_id = post_id) ### < Removed 'content'
  for (col in names(post_col_renames)) {
    alt_name <- post_col_renames[[col]]
    if (!is.null(alt_name) && alt_name %in% names(posts)) {
      data.table::setnames(posts, alt_name, col)
    }
  }
  
  # Select relevant columns from posts
  post_cols_to_keep <- c("post_id", other_post_vars)
  posts <- posts[, intersect(post_cols_to_keep, names(posts)), with = FALSE]
  
  ### De-duplicating posts
  if (any(duplicated(posts[["post_id"]]))) {
    if (verbose) cat("De-duplicating 'posts'...\n")
    posts <- posts[!duplicated(posts[["post_id"]])]
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
  if (any(duplicated(users[["account_id"]]))) {
    if (verbose) cli::cli_inform("De-duplicating 'users'...\n")
    users <- users[!duplicated(users[["account_id"]])]
  }
  
  
  ### Merging process using data.table's direct join syntax ###
  # Join 'posts' to 'posts' by "post_id" (left join)
  if ("post_data" %in% names(groups_data)) {
    if (verbose) cli::cli_inform("Merging 'post_data' with 'groups_data$post_data' by 'post_id'...\n")
    groups_data$post_data <- merge(x = groups_data$post_data, y = posts, by = "post_id", all.x = T)
    
    
    # Join 'users' to 'post_data' by "account_id" (left join)
    if (verbose) cli::cli_inform("Merging 'users' with 'groups_data$post_data' by 'account_id'...\n")
    groups_data$post_data <- merge(x = groups_data$post_data, y = users, by = "account_id", all.x = T)
    
    # Reorder columns in the desired order
    desired_order <- c(  ### <<< Removed 'parameters' and 'component'
      "post_id",
      "time",
      "content",
      "account_id",
      "account_name",
      "community",
      "algorithm"
    )
    
    # Combine the desired order with the remaining columns
    remaining_columns <- setdiff(names(groups_data$post_data), desired_order)
    new_column_order <- c(desired_order, remaining_columns)
    
    # Reorder the data.table columns
    groups_data$post_data <- groups_data$post_data[, ..new_column_order]
    
  }
  
  # Join 'users' to 'node_list' by "account_id" (left join)
  if (verbose) cli::cli_inform("Merging 'users' with 'groups_data$node_list' by 'account_id'...\n")
  groups_data$node_list <- merge(x = groups_data$node_list, y = users, by = "account_id", all.x = T)
  
  names(groups_data$node_list)
  
  # Reorder columns in the desired order
  desired_order <- c(
    "account_id",
    "account_name",
    "community",
    "algorithm"
  )
  
  # Combine the desired order with the remaining columns
  remaining_columns <- setdiff(names(groups_data$node_list), desired_order)
  new_column_order <- c(desired_order, remaining_columns)
  
  # Reorder the data.table columns
  groups_data$node_list <- groups_data$node_list[, ..new_column_order]
  
  
  ### Add community identifier to sim_dt
  
  if(sim_dt_community == TRUE && "sim_dt" %in% names(groups_data)){
    
    if(!"community" %in% names(groups_data$sim_dt)){
      
      if(verbose) cli::cli_inform("Adding community ID to sim_dt.")
      
      comm_dt_x <- data.table::copy(groups_data$node_list)[, .(account_id, community)]
      comm_dt_y <- data.table::copy(groups_data$node_list)[, .(account_id_y = account_id, community_y = community)]
      
      sim_dt <- data.table::copy(groups_data$sim_dt)
      sim_dt <- comm_dt_x[sim_dt, on = "account_id"]
      sim_dt <- comm_dt_y[sim_dt, on = "account_id_y"]
      
      data.table::setcolorder(
        sim_dt,
        c(
          "path_simdt",
          "account_id", "post_id", "time", "content", "community",
          "similarity",
          "time_diff",
          "account_id_y", "post_id_y", "time_y", "content_y", "community_y",
          grep("^param_", names(sim_dt), value = TRUE)
        )
      )
      
      # Replace
      groups_data$sim_dt <- sim_dt
      
    }
    
  }
  
  # Return the augmented similarity table
  return(groups_data)
}




#' Filter and reduce a groups_data object by edge, time, similarity, or node-based constraints
#'
#' This function filters the elements of a `groups_data` object (typically returned by `coorsim_detect_groups()`)
#' according to user-specified thresholds for edge weight, similarity, time window, community size, node participation,
#' and node degree. It conditionally re-runs the group detection if thresholds on similarity or time window are stricter
#' than originally set. The returned object includes only the valid nodes and edges across all contained components.
#'
#' @param groups_data A named list created by `coorsim::coorsim_detect_groups()` containing elements like `graph`,
#'   `communities`, `edge_list`, `node_list`, optionally `sim_dt` and `post_data`, and a `params` list.
#' @param edge_weight Numeric. Keep only edges with `weight >= edge_weight`.
#' @param time_window Integer (seconds). Re-runs group detection using only links with `time_diff <= time_window`.
#' @param min_simil Numeric. Re-runs group detection using only links with `similarity >= min_simil`.
#' @param min_comm_size Integer. Keep only communities of size at least this value.
#' @param min_participation Integer. Keep only nodes that appear in at least this number of edges (as `account_id`).
#' @param min_degree Integer. Keep only nodes with degree >= this value.
#' @param verbose Logical. If TRUE, progress messages are printed using `cli::cli_inform()`.
#'
#' @return A filtered `groups_data` list with updated elements based on the applied constraints.
#'
#' @import data.table
#' @export
filter_groups_data <- function(groups_data,
                               edge_weight = NULL,
                               time_window = NULL,
                               min_simil = NULL,
                               min_comm_size = NULL,
                               min_participation = NULL,
                               min_degree = NULL,
                               verbose = T) {
  # Check minimum requirements
  req_elements <- c(
    "graph",
    "communities",
    "edge_list",
    "node_list"
  )

  if (!all(req_elements %in% names(groups_data))) stop("Not all required elements provided in 'groups_data'.")

  ### Function to subset all elements after node-based filtering

  subset_after_node_filter <- function(groups_data,
                                       valid_accounts) {
    ## Extract elements
    g <- groups_data$graph
    communities <- groups_data$communities
    edge_list <- data.table::copy(groups_data$edge_list)
    node_list <- data.table::copy(groups_data$node_list)

    # 1. Filter the igraph graph object `g`
    g <- igraph::induced_subgraph(g, vids = igraph::V(g)[name %in% valid_accounts])

    # 2. Filter the igraph communities object `communities`
    # Extract membership only for valid nodes
    filtered_membership <- igraph::membership(communities)[names(igraph::membership(communities)) %in% valid_accounts]
    # Recreate the community structure
    communities <- igraph::make_clusters(g, filtered_membership)

    # 3. Filter the data.table `edge_list`
    edge_list <- edge_list[account_id %in% valid_accounts & account_id_y %in% valid_accounts]

    # 4. Filter the data.table `edge_list`
    node_list <- node_list[account_id %in% valid_accounts]

    # 5. Filter the data.table `post_data`
    if ("post_data" %in% names(groups_data)) {
      post_data <- data.table::copy(groups_data$post_data)
      post_data <- post_data[account_id %in% valid_accounts]
    } else {
      post_data <- NULL
    }

    # 6. Filter sim_dt
    if ("sim_dt" %in% names(groups_data)) {
      sim_dt <- data.table::copy(groups_data$sim_dt)
      sim_dt <- sim_dt[account_id %in% valid_accounts & account_id_y %in% valid_accounts]
    } else {
      sim_dt <- NULL
    }

    groups_data_filtered <- list(
      graph = g,
      communities = communities,
      edge_list = edge_list,
      sim_dt = sim_dt,
      node_list = node_list,
      post_data = post_data,
      params = groups_data$params
    )

    return(groups_data_filtered)
  }


  ### FILTERING ON EDGES

  # A) Filter by edge_weigth

  if (!is.null(edge_weight)) {
    edge_list <- data.table::copy(groups_data$edge_list)

    n_prev <- nrow(edge_list)

    edge_list_new <- edge_list[weight >= edge_weight]

    n_drop <- n_prev - nrow(edge_list_new)

    if (verbose) cli::cli_inform("Dropping n = {n_drop} edges by edge_weight >= {edge_weight}.")

    # Create a data.table of nodes
    node_list <- data.table::copy(groups_data$node_list)
    node_list_new <- data.table::data.table(account_id = unique(c(edge_list_new$account_id, edge_list_new$account_id_y)))
    node_list_new <- merge(node_list_new, node_list, by = "account_id", all.x = T)

    if ("N" %in% names(node_list_new)) node_list_new[, N := NULL]
    if ("params" %in% names(groups_data)) min_comm_size <- groups_data[["params"]][["min_comm_size"]]
    if (is.null(min_comm_size)) min_comm_size <- 2

    keep_accounts <- unique(node_list_new[
      , .N,
      by = community
    ][
      N >= min_comm_size
    ][
      node_list_new,
      on = "community", nomatch = 0
    ][, account_id])

    groups_data <- subset_after_node_filter(
      groups_data = groups_data,
      valid_accounts = keep_accounts
    )

    groups_data$edge_list <- edge_list_new
  }

  # B) Filter by time window or min_simil

  if (!is.null(time_window) || !is.null(min_simil)) {
    if (!"sim_dt" %in% names(groups_data)) stop("No 'sim_dt' found. Filtering by 'time_window' not possible.")

    sim_dt <- data.table::copy(groups_data$sim_dt)

    og_time_window <- sim_dt$param_time_window[[1]]
    og_min_simil <- sim_dt$param_min_simil[[1]]

    if ((is.null(time_window) && og_time_window > time_window) ||
      (is.null(min_simil) && og_min_simil > min_simil)) {
      if (!is.null(time_window)) sim_dt <- sim_dt[time_diff <= time_window]
      if (!is.null(min_simil)) sim_dt <- sim_dt[similarity >= min_simil]

      node_list <- data.table::copy(groups_data$node_list)
      user_data <- node_list[, .SD, .SDcols = grep("^account_", names(node_list), value = TRUE)]
      other_user_vars <- setdiff(names(user_data), c("account_id", "account_name"))

      return_post_dt <- ifelse("post_data" %in% names(groups_data), T, F)

      if (verbose) cli::cli_inform("Filtering by 'time_window' = {time_window} and 'min_simil' = {min_simil}. \n
                                Requires to re-run group detection.")

      if ("params" %in% names(groups_data)) {
        params <- groups_data$params
      } else {
        stop("Re-running group detection without 'params' in groups_data not possible. Please re-run manually.")
      }

      groups_data <- coorsim_detect_groups(
        simdt = sim_dt,
        user_data = user_data,
        account_id = "account_id",
        account_name = "account_name",
        other_user_vars = other_user_vars,
        edge_weight = edge_weight,
        cluster_method = params$cluster_method,
        resolution = params$resolution,
        theta = params$theta,
        return_post_dt = return_post_dt,
        verbose = verbose,
        min_comm_size = min_comm_size
      )
    } else {
      cli::cli_inform("Specified min_simil or time_window are not smaller than in the provided sim_dt.")
    }
  }

  ### Node-based filters

  # C) Filter by min_participation
  if (!is.null(min_participation)) {
    n_prev <- length(unique(groups_data$node_list$account_id))

    # Count occurrences of each account_id and account_id_y
    keep_accounts <- groups_data$node_list[
      , .N,
      by = account_id
    ][N >= min_participation, account_id]

    n_drop <- n_prev - length(keep_accounts)

    if (verbose) cli::cli_inform("Dropping n = {n_drop} nodes by min_participation >= {min_participation}.")
    groups_data <- subset_after_node_filter(
      groups_data = groups_data,
      valid_accounts = keep_accounts
    )
  }

  # D) Filter by min_comm_size
  if (!is.null(min_comm_size)) {
    n_prev <- length(unique(groups_data$node_list$account_id))

    node_list <- data.table::copy(groups_data$node_list)
    if ("N" %in% names(node_list)) node_list[, N := NULL]

    keep_accounts <- unique(node_list[
      , .N,
      by = community
    ][
      N >= min_comm_size
    ][
      node_list,
      on = "community", nomatch = 0
    ][, account_id])

    n_drop <- n_prev - length(keep_accounts)

    if (verbose) cli::cli_inform("Dropping n = {n_drop} nodes by min_comm_size >= {min_comm_size}.")
    groups_data <- subset_after_node_filter(
      groups_data = groups_data,
      valid_accounts = keep_accounts
    )
  }

  # E) Filter by degree
  if (!is.null(min_degree)) {
    n_prev <- length(unique(groups_data$node_list$account_id))

    edge_list <- data.table::copy(groups_data$edge_list)

    # Count degree per node
    keep_accounts <- unique(edge_list[
      , .(account_id = c(account_id, account_id_y))
    ][
      , .N,
      by = account_id
    ][
      N >= min_degree
    ][, account_id])

    n_drop <- n_prev - length(keep_accounts)

    if (verbose) cli::cli_inform("Dropping n = {n_drop} nodes by min_degree >= {min_degree}.")
    groups_data <- subset_after_node_filter(
      groups_data = groups_data,
      valid_accounts = keep_accounts
    )
  }

  return(groups_data)
}
