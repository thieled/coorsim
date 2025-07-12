
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
  
  # Add prefix to user columns (except account_id), but only if not already prefixed   <<<<< Changed 2025-07-03
  user_cols_to_keep <- c("account_id", other_user_vars)
  users <- users[, intersect(user_cols_to_keep, names(users)), with = FALSE]
  user_cols <- setdiff(names(users), "account_id")
  user_cols_renamed <- ifelse(startsWith(user_cols, "account_"), user_cols, paste0("account_", user_cols))
  data.table::setnames(users, user_cols, user_cols_renamed)
  
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
       #  "path_simdt",
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
  
  # Clean duplicated "account_" prefixes in node_list and post_data
  for (tbl_name in c("node_list", "post_data")) {
    if (!is.null(groups_data[[tbl_name]]) && data.table::is.data.table(groups_data[[tbl_name]])) {
      dt <- groups_data[[tbl_name]]
      cols <- names(dt)
      
      # Match any "account_" repeated two or more times
      dup_cols <- grep("^account_(account_)+", cols, value = TRUE)
      new_names <- sub("^account_(account_)+", "account_", dup_cols)
      
      if (length(dup_cols)) {
        data.table::setnames(dt, old = dup_cols, new = new_names)
      }
      
      # Reassign cleaned table
      groups_data[[tbl_name]] <- dt
    }
  }
  
  # Return the augmented similarity table
  return(groups_data)
}




#' Filter a detected group structure based on various criteria
#'
#' This function filters a detected group structure (`groups_data`) by a wide range of optional thresholds
#' and selection criteria, including account-level, post-level, and similarity-based filters. It supports
#' optional re-detection of groups after filtering similarity pairs (`sim_dt`), and returns a cleaned, 
#' consistent `groups_data` object.
#'
#' @param groups_data A list returned by `coorsim_detect_groups()` or `augment_groups_data()`.
#' @param by_col Column name in `post_data` to filter by (e.g. `"lang"`, `"country"`).
#' @param by_val Value(s) to keep in `by_col`.
#' @param edge_weight Minimum edge weight to retain in the similarity network.
#' @param time_window Maximum time difference allowed between posts (in seconds).
#' @param min_simil Minimum cosine similarity allowed between post embeddings.
#' @param min_comm_size Minimum number of accounts in a community.
#' @param min_comp_size Minimum number of nodes in a graph component.
#' @param min_degree Minimum node degree.
#' @param min_participation Minimum number of post-level similarity relations per account.
#' @param communities_index Vector of community indices to keep.
#' @param quantile_accountwise Drop communities below a given quantile of account size.
#' @param quantile_postwise Drop communities below a given quantile of post size.
#' @param top_n_accountwise Keep the top-n largest communities by account count.
#' @param top_n_postwise Keep the top-n largest communities by post count.
#' @param stringsimil_cutoff Optional. Additional filter: minimum geometric mean of cosine similarity and string similarity.
#' @param stringsimil_method String similarity method passed to `stringdist::stringsim()` (e.g. `"cosine"`, `"jaccard"`).
#' @param stringsimil_ngram Integer n-gram size used in string similarity.
#' @param nthread Number of threads used in `stringsim` filtering.
#' @param rerun_detect_groups Logical. Whether to re-run group detection after filtering similarity pairs.
#' @param verbose Logical. Whether to print informative messages.
#' @param ... Additional arguments passed to `coorsim_detect_groups()` if rerun is triggered.
#'
#' @return A filtered `groups_data` list with updated `$filter` slot for reproducibility.
#'
#' @seealso [coorsim_detect_groups()], [augment_groups_data()]
#'
#' @export
filter_groups_data <- function(groups_data,
                               by_col = NULL,
                               by_val = NULL,
                               edge_weight = NULL,
                               time_window = NULL,
                               min_simil = NULL,
                               min_comm_size = NULL,
                               min_comp_size = NULL,
                               min_degree = NULL,
                               min_participation = NULL,
                               communities_index = NULL,
                               quantile_accountwise = NULL,
                               quantile_postwise = NULL,
                               top_n_accountwise = NULL,
                               top_n_postwise = NULL,
                               stringsimil_cutoff = NULL,
                               stringsimil_method = "cosine",
                               stringsimil_ngram = 3,
                               nthread = 10,
                               rerun_detect_groups = FALSE,
                               verbose = TRUE,
                               ...) {
  # Check minimum requirements
  req_elements <- c("graph", "communities", "edge_list", "node_list")
  if (!all(req_elements %in% names(groups_data))) stop("Not all required elements provided in 'groups_data'.")
  
  # Internal function to subset filtered groups_data
  subset_after_node_filter <- function(groups_data, valid_accounts) {
    g <- igraph::induced_subgraph(groups_data$graph, vids = igraph::V(groups_data$graph)[name %in% valid_accounts])
    filtered_membership <- igraph::membership(groups_data$communities)[names(igraph::membership(groups_data$communities)) %in% valid_accounts]
    communities <- igraph::make_clusters(g, filtered_membership)
    edge_list <- groups_data$edge_list[account_id %in% valid_accounts & account_id_y %in% valid_accounts]
    node_list <- groups_data$node_list[account_id %in% valid_accounts]
    post_data <- if ("post_data" %in% names(groups_data)  && !is.null(groups_data$post_data)) groups_data$post_data[account_id %in% valid_accounts] else NULL
    sim_dt <- if ("sim_dt" %in% names(groups_data)  && !is.null(groups_data$sim_dt)) groups_data$sim_dt[account_id %in% valid_accounts & account_id_y %in% valid_accounts] else NULL
    user_labels <- if ("user_labels" %in% names(groups_data)  && !is.null(groups_data$user_labels)) groups_data$user_labels[account_id %in% valid_accounts] else NULL
    community_labels <- if ("community_labels" %in% names(groups_data)  && !is.null(groups_data$community_labels)) groups_data$community_labels[community %in% unique(node_list$community)] else NULL
    community_metrics <- if ("community_metrics" %in% names(groups_data) && !is.null(groups_data$community_metrics)) groups_data$community_metrics[community %in% unique(node_list$community)] else NULL
    filter <- if ("filter" %in% names(groups_data)) groups_data$filter else NULL
    params <- if ("params" %in% names(groups_data)) groups_data$params else NULL
    
    # Clean up duplicated "account_" prefixes in node_list
    node_cols <- names(node_list)
    dup_account_cols <- grep("^account_account_", node_cols, value = TRUE)
    cleaned_names <- sub("^account_account_", "account_", dup_account_cols)
    if (length(dup_account_cols)) {
      data.table::setnames(node_list, old = dup_account_cols, new = cleaned_names)
    }
    
    result <- list(
      graph = g,
      communities = communities,
      edge_list = edge_list,
      node_list = node_list,
      post_data = post_data,
      sim_dt = sim_dt,
      params = params,
      filter = filter,
      user_labels = user_labels,
      community_labels = community_labels,
      community_metrics = community_metrics
    )
    
    result <- Filter(Negate(is.null), result)
    return(result)
  }
  
  # Filter by post_data column
  if (!is.null(by_col)) {
    if (!"sim_dt" %in% names(groups_data)) stop("No 'sim_dt' found. Filtering 'by' not possible.")
    if (!"post_data" %in% names(groups_data)) stop("No 'sim_dt' found. Filtering 'by' not possible.")
    if (is.null("by_val")) stop("No 'by_val' specified. Please specify a value.")
    
    post_data <- data.table::copy(groups_data$post_data)
    sim_dt <- data.table::copy(groups_data$sim_dt)
    
    node_list <- data.table::copy(groups_data$node_list)
    user_data <- node_list[, .SD, .SDcols = grep("^account_", names(node_list), value = TRUE)]
    other_user_vars <- setdiff(names(user_data), c("account_id", "account_name"))
    return_post_dt <- "post_data" %in% names(groups_data)
    
    # Clean up duplicated "account_" prefixes in node_list
    node_cols <- names(node_list)
    dup_account_cols <- grep("^account_account_", node_cols, value = TRUE)
    cleaned_names <- sub("^account_account_", "account_", dup_account_cols)
    if (length(dup_account_cols)) {
      data.table::setnames(node_list, old = dup_account_cols, new = cleaned_names)
    }
    
    if (verbose) cli::cli_inform("Filtering by {by_col} = {by_val}.")
    
    # Filtering: Step 1: post_data
    if (!by_col %in% names(post_data)) {
      stop("Column '", by_col, "' not found in 'post_data'")
    }
    
    post_data <- post_data[get(by_col) %in% by_val] 
    
    # Step 2: sim_dt
    sim_dt <- sim_dt[
      post_id %in% post_data$post_id & post_id_y %in% post_data$post_id
    ]
    
    # The edge list connects 'account_id' with 'account_id_y' and counts the connections (as weight)
    edge_list <- sim_dt[
      , .(account_id = pmin(account_id, account_id_y),
          account_id_y = pmax(account_id, account_id_y))  # Normalize direction
    ][
      , .(weight = .N), by = .(account_id, account_id_y)  # Aggregate
    ]
    
    # Optionally: Filter the edge list based on the edge_weight parameter
    if (!is.null(edge_weight)) {
      if (verbose) cli::cli_inform("Filter by edge_weight >= {edge_weight}.")
      edge_list <- edge_list[weight >= edge_weight]
      
      # Filter simdt to only include post pairs that contributed to retained edges
      sim_dt <- sim_dt[, edge_key := paste0(pmin(account_id, account_id_y), "_", pmax(account_id, account_id_y))][
        , weight := .N, by = edge_key][weight >= edge_weight][, c("edge_key", "weight") := NULL]
    } 
    
    # Step 4: node_list
    nodes <- unique(c(sim_dt$account_id, sim_dt$account_id_y))
    node_list <- node_list[account_id %in% nodes]
    
    # Clean up duplicated "account_" prefixes in node_list
    node_cols <- names(node_list)
    dup_account_cols <- grep("^account_account_", node_cols, value = TRUE)
    cleaned_names <- sub("^account_account_", "account_", dup_account_cols)
    if (length(dup_account_cols)) {
      data.table::setnames(node_list, old = dup_account_cols, new = cleaned_names)
    }
    
    # Step 5: Create the igraph object
    g <- igraph::graph_from_data_frame(d = edge_list, 
                                       vertices = node_list, 
                                       directed = FALSE)
    
    # Step 6: Communities object
    communities <- groups_data$communities
    filtered_membership <- igraph::membership(communities)[names(igraph::membership(communities)) %in% nodes]
    # Recreate the community structure
    communities <- igraph::make_clusters(g, filtered_membership)
    
    # Subset remaining objects
    user_labels <- if ("user_labels" %in% names(groups_data)  && !is.null(groups_data$user_labels)) data.table::copy(groups_data$user_labels[account_id %in% nodes]) else NULL
    community_labels <- if ("community_labels" %in% names(groups_data)  && !is.null(groups_data$community_labels)) groups_data$community_labels[community %in% unique(node_list$community)] else NULL
    community_metrics <- if ("community_metrics" %in% names(groups_data) && !is.null(groups_data$community_metrics)) groups_data$community_metrics[community %in% unique(node_list$community)] else NULL
    filter <- if ("filter" %in% names(groups_data)) groups_data$filter else NULL
    params <- if ("params" %in% names(groups_data)) groups_data$params else NULL
    
    groups_data <- list(graph = g, 
                        communities = communities,
                        edge_list = edge_list,
                        sim_dt = sim_dt,
                        node_list = node_list,
                        post_data = post_data,
                        params = params,
                        filter = filter,
                        user_labels = user_labels,
                        community_labels = community_labels,
                        community_metrics = community_metrics
    )    
    
    
    groups_data <- Filter(Negate(is.null), groups_data)
  } 
  
  
  # Filter by edge weight
  if (!is.null(edge_weight)) {
    edge_list <- data.table::copy(groups_data$edge_list)
    n_prev <- nrow(edge_list)
    edge_list_new <- edge_list[weight >= edge_weight]
    n_drop <- n_prev - nrow(edge_list_new)
    if (verbose) cli::cli_inform("Dropping n = {n_drop} edges by edge_weight >= {edge_weight}.")
    
    node_list <- data.table::copy(groups_data$node_list)
    node_list_new <- data.table::data.table(account_id = unique(c(edge_list_new$account_id, edge_list_new$account_id_y)))
    node_list_new <- merge(node_list_new, node_list, by = "account_id", all.x = TRUE)
    if ("N" %in% names(node_list_new)) node_list_new[, N := NULL]
    
    if (is.null(min_comm_size)) min_comm_size <- groups_data$params$min_comm_size %||% 2
    
    keep_accounts <- unique(node_list_new[, .N, by = community][
      N >= min_comm_size][node_list_new, on = "community", nomatch = 0][, account_id])
    
    groups_data <- subset_after_node_filter(groups_data, keep_accounts)
    groups_data$edge_list <- edge_list_new
  }
  
  
  # Filter by stricter similarity or time window
  if (!is.null(time_window) || !is.null(min_simil) || !is.null(stringsimil_cutoff)) {
    if (!"sim_dt" %in% names(groups_data)) stop("No 'sim_dt' found. Filtering by 'time_window' or 'min_simil' not possible.")
    
    sim_dt <- data.table::copy(groups_data$sim_dt)
    og_time_window <- sim_dt$param_time_window[[1]]
    og_min_simil <- sim_dt$param_min_simil[[1]]
    
    trigger <- FALSE
    
    
    # Get additional filtering by stringdist::stringsimil()
    if(!is.null(stringsimil_cutoff)){
      
      if (!requireNamespace("stringdist", quietly = TRUE)) {
        stop("Package 'stringdist' is required for this function. Please install it.")
      }
      
      og_nrow <- nrow(sim_dt)
      
      # Deduplicate
      sim_dt <- sim_dt |>
        unique(by = c("post_id", "post_id_y"))
      
      # Prepare clean doc table from both sides
      if(verbose) cli::cli_progress_step(msg = "Additional string similarity filter: Cleaning ... \n",
                                         msg_done = "Additional string similarity filter: Cleaning finished. \n")
      
      
      content_dt <- sim_dt[, .(post_id, content)][
        !duplicated(post_id)
      ][
        , .(post_id, content_clean = replace_emoji_with_name(content) |> tolower())
      ]
      
      content_dt_y <- sim_dt[, .(post_id_y, content_y)][
        !duplicated(post_id_y)
      ][
        , .(post_id_y, content_clean_y = replace_emoji_with_name(content_y) |> tolower())
      ]
      
      
      # Merge cleaned text to sim_dt
      sim_dt <- merge(sim_dt, content_dt[,.(post_id, content_clean)], by = "post_id", all.x = T)
      sim_dt <- merge(sim_dt, content_dt_y[,.(post_id_y, content_clean_y)], by = "post_id_y", all.x = T)
      
      if(verbose) cli::cli_progress_done()
      
      # String simil
      if(verbose) cli::cli_progress_step(msg = "Additional string similarity filter: 
            {stringsimil_method} similarity, {stringsimil_ngram}-grams, cutoff: geom. mean (simil*string_simil) >= {stringsimil_cutoff}.",
                                         msg_done = "Filtered by string similarity:
             {stringsimil_method} similarity, {stringsimil_ngram}-grams, cutoff: geom. mean (simil*string_simil) >= {stringsimil_cutoff}.")
      
      
      sim_dt <- sim_dt[, string_similarity := stringdist::stringsim(content_clean, 
                                                                    content_clean_y, 
                                                                    method = stringsimil_method, 
                                                                    q = stringsimil_ngram,
                                                                    nthread = nthread)]
      
      sim_dt <- sim_dt[, geom_mean_simil := sqrt(similarity * string_similarity)
      ][geom_mean_simil >= stringsimil_cutoff][, c("content_clean", "content_clean_y") := NULL]
      
      new_nrow <- nrow(sim_dt)
      
      if(verbose) cli::cli_progress_done()
      if(verbose) cli::cli_inform("Dropped {og_nrow - new_nrow} pairs.")
      
      trigger <- TRUE
      
    }
    
    
    if ((!is.null(time_window) && time_window < og_time_window) ||
        (!is.null(min_simil) && min_simil > og_min_simil)) {
      
      if (!is.null(time_window)) sim_dt <- sim_dt[time_diff <= time_window]
      if (!is.null(min_simil)) sim_dt <- sim_dt[similarity >= min_simil]
      
      trigger <- TRUE
      
    }else if (verbose) {
      cli::cli_inform("Specified min_simil or time_window are not stricter than in the provided sim_dt.")
    }
    
    if(trigger){
      
      node_list <- data.table::copy(groups_data$node_list)
      
      # Clean up duplicated "account_" prefixes in node_list
      node_cols <- names(node_list)
      dup_account_cols <- grep("^account_account_", node_cols, value = TRUE)
      cleaned_names <- sub("^account_account_", "account_", dup_account_cols)
      if (length(dup_account_cols)) {
        data.table::setnames(node_list, old = dup_account_cols, new = cleaned_names)
      }
      
      user_data <- node_list[, .SD, .SDcols = grep("^account_", names(node_list), value = TRUE)]
      other_user_vars <- setdiff(names(user_data), c("account_id", "account_name"))
      return_post_dt <- "post_data" %in% names(groups_data)
      
      if (verbose) cli::cli_inform("Filtering by 'time_window' = {time_window} and 'min_simil' = {min_simil}. Re-running group detection.")
      
      # Pass on new arguments from ... 
      dot_args <- list(...)
      args <- c(
        dot_args,
        if (!is.null(time_window)) list(time_window = time_window),
        if (!is.null(min_simil)) list(min_simil = min_simil)
      )
      
      # otherwise take parameters from groups_data
      if (exists("args") && is.list(args) && length(args) > 0) {
        params <- utils::modifyList(groups_data$params, args)
      } else {
        params <- groups_data$params
      }
      
      # Check if sim_dt has community id
      sim_dt_community <- "community" %in% names(groups_data$sim_dt)
      
      # Re-apply groups detection
      groups_data_new <- coorsim_detect_groups(
        simdt = sim_dt[, (intersect(c("community", "community_y"), names(sim_dt))) := NULL],
        user_data = user_data,
        account_id = "account_id",
        account_name = "account_name",
        other_user_vars = NULL,
        edge_weight = edge_weight,
        cluster_method = params$cluster_method,
        resolution = params$resolution,
        theta = params$theta,
        return_post_dt = return_post_dt,
        verbose = verbose,
        min_comm_size = min_comm_size
      )
      
      # Chek if post_data has been augmented
      if(return_post_dt){
        post_data <- data.table::copy(groups_data$post_data)
        other_post_vars <- grep("^account_", 
                                setdiff(names(post_data), names(groups_data_new$post_data)),
                                value = T, invert = T)
      }else{
        post_data = NULL
        other_post_vars = NULL
      }
      
      groups_data_new <- augment_groups_data(groups_data = groups_data_new, 
                                                      post_data = post_data,
                                                      user_data = user_data,
                                                      other_user_vars = other_user_vars,
                                                      other_post_vars = other_post_vars, 
                                                      verbose =  verbose, 
                                                      sim_dt_community = sim_dt_community)
      
      # Subset remaining objects
      groups_data_new$user_labels <- if ("user_labels" %in% names(groups_data) && !is.null(groups_data$user_labels)) groups_data$user_labels[account_id %in% groups_data_new$node_list$account_id] else NULL
      groups_data_new$community_labels <- if ("community_labels" %in% names(groups_data) && !is.null(groups_data$community_labels)) groups_data$community_labels[community %in% unique(groups_data_new$node_list$community)] else NULL
      community_metrics <- if ("community_metrics" %in% names(groups_data) && !is.null(groups_data$community_metrics)) groups_data$community_metrics[community %in% unique(node_list$community)] else NULL
      filter <- if ("filter" %in% names(groups_data)) groups_data$filter else NULL
      params <- if ("params" %in% names(groups_data)) groups_data$params else NULL
      
      
      ## Overwrite groups_data object
      groups_data <- groups_data_new
      rm(groups_data_new)
      groups_data <- Filter(Negate(is.null), groups_data)
      
    } 
  }
  
  # Filter by community index
  if (!is.null(communities_index)) {
    node_list <- data.table::copy(groups_data$node_list)
    keep_accounts <- unique(node_list[community %in% communities_index, account_id])
    n_drop <- nrow(node_list) - length(keep_accounts)
    if (verbose) cli::cli_inform("Dropping n = {n_drop} nodes. Keeping communities of index {communities_index}")
    groups_data <- subset_after_node_filter(groups_data, keep_accounts)
  }
  
  # Filter by min_comm_size
  if (!is.null(min_comm_size)) {
    node_list <- data.table::copy(groups_data$node_list)
    if ("N" %in% names(node_list)) node_list[, N := NULL]
    keep_accounts <- unique(node_list[, .N, by = community][
      N >= min_comm_size][node_list, on = "community", nomatch = 0][, account_id])
    n_drop <- nrow(node_list) - length(keep_accounts)
    if (verbose) cli::cli_inform("Dropping n = {n_drop} nodes by min_comm_size >= {min_comm_size}.")
    groups_data <- subset_after_node_filter(groups_data, keep_accounts)
  }
  
  
  # Filter by min_comp_size
  if (!is.null(min_comp_size)) {
    g <- groups_data$graph
    g <- igraph::induced_subgraph(g, 
                                  igraph::V(g)[igraph::components(g)$membership %in% 
                                                 which(igraph::components(g)$csize >= min_comp_size)])
    keep_accounts <- igraph::V(g) |> names()
    n_drop <- nrow(groups_data$node_list) - length(keep_accounts)
    if (verbose) cli::cli_inform("Dropping n = {n_drop} nodes by min_comp_size >= {min_comp_size}.")
    groups_data <- subset_after_node_filter(groups_data, keep_accounts)
  }
  
  # Filter by min_degree
  if (!is.null(min_degree)) {
    g <- groups_data$graph
    deg <- igraph::degree(g, mode = "all")  # all = undirected
    keep_accounts <- names(deg[deg >= min_degree])
    n_drop <- nrow(groups_data$node_list) - length(keep_accounts)
    if (verbose) cli::cli_inform("Dropping n = {n_drop} nodes by min_degree >= {min_degree}.")
    groups_data <- subset_after_node_filter(groups_data, keep_accounts)
  }
  
  
  # Filter by min_participation
  if (!is.null(min_participation)) {
    if (!"sim_dt" %in% names(groups_data)) stop("No 'sim_dt' found. Cannot apply 'min_participation' filter.")
    sim_dt <- data.table::copy(groups_data$sim_dt)
    account_counts <- data.table::rbindlist(list(
      sim_dt[, .(account_id)],
      sim_dt[, .(account_id = account_id_y)]
    ))[, .N, by = account_id]
    keep_accounts <- account_counts[N >= min_participation, account_id]
    n_drop <- nrow(groups_data$node_list) - length(keep_accounts)
    if (verbose) cli::cli_inform("Dropping n = {n_drop} nodes by min_participation >= {min_participation}.")
    groups_data <- subset_after_node_filter(groups_data, keep_accounts)
  }
  
  # Quantile filter by account count
  if (!is.null(quantile_accountwise)) {
    node_list <- data.table::copy(groups_data$node_list)
    if ("N" %in% names(node_list)) node_list[, N := NULL]
    keep_accounts <- unique(filter_ntile(node_list[, .N, by = community], "N", quantile_accountwise)[
      node_list, on = "community", nomatch = 0][, account_id])
    n_drop <- nrow(node_list) - length(keep_accounts)
    if (verbose) cli::cli_inform("Dropping n = {n_drop} nodes. Dropping lower {quantile_accountwise} quantile of communities by account count.")
    groups_data <- subset_after_node_filter(groups_data, keep_accounts)
  }
  
  # Quantile filter by post count
  if (!is.null(quantile_postwise)) {
    node_list <- data.table::copy(groups_data$node_list)
    if ("post_data" %in% names(groups_data)) {
      post_data <- data.table::copy(groups_data$post_data)
      post_size <- post_data[, .N, by = community]
    } else if ("sim_dt" %in% names(groups_data)) {
      sim_dt <- data.table::copy(groups_data$sim_dt)
      post_size <- unique(sim_dt[, .(account_id = c(account_id, account_id_y), post_id = c(post_id, post_id_y))])[
        node_list[, .(account_id, community)], on = "account_id"][, .N, by = community]
    } else stop("Neither 'sim_dt' nor 'post_data' found. Filtering by post quantile not possible.")
    
    keep_accounts <- unique(filter_ntile(post_size, "N", quantile_postwise)[
      node_list, on = "community", nomatch = 0][, account_id])
    n_drop <- nrow(node_list) - length(keep_accounts)
    if (verbose) cli::cli_inform("Dropping n = {n_drop} nodes. Dropping lower {quantile_postwise} quantile of communities by post count.")
    groups_data <- subset_after_node_filter(groups_data, keep_accounts)
  }
  
  # Top-n posts per community
  if (!is.null(top_n_postwise)) {
    node_list <- data.table::copy(groups_data$node_list)
    if ("post_data" %in% names(groups_data)) {
      post_data <- data.table::copy(groups_data$post_data)
      post_size <- post_data[, .N, by = community][order(-N)][1:top_n_postwise]
    } else if ("sim_dt" %in% names(groups_data)) {
      sim_dt <- data.table::copy(groups_data$sim_dt)
      post_size <- unique(sim_dt[, .(account_id = c(account_id, account_id_y), post_id = c(post_id, post_id_y))])[
        node_list[, .(account_id, community)], on = "account_id"][, .N, by = community][order(-N)][1:top_n_postwise]
    } else stop("No post count data found.")
    
    keep_accounts <- unique(node_list[community %in% post_size$community, account_id])
    n_drop <- nrow(node_list) - length(keep_accounts)
    if (verbose) cli::cli_inform("Dropping n = {n_drop} nodes. Keeping top {top_n_postwise} communities by post count.")
    groups_data <- subset_after_node_filter(groups_data, keep_accounts)
  }
  
  # Top-n accounts per community
  if (!is.null(top_n_accountwise)) {
    node_list <- data.table::copy(groups_data$node_list)
    acc_size <- node_list[, .N, by = community][order(-N)][1:top_n_accountwise]
    keep_accounts <- unique(node_list[community %in% acc_size$community, account_id])
    n_drop <- nrow(node_list) - length(keep_accounts)
    if (verbose) cli::cli_inform("Dropping n = {n_drop} nodes. Keeping top {top_n_accountwise} communities by account count.")
    groups_data <- subset_after_node_filter(groups_data, keep_accounts)
  }
  
  
  # Re-run groups detection if wished
  if(rerun_detect_groups){
    
    node_list <- data.table::copy(groups_data$node_list)
    
    # Clean up duplicated "account_" prefixes in node_list
    node_cols <- names(node_list)
    dup_account_cols <- grep("^account_account_", node_cols, value = TRUE)
    cleaned_names <- sub("^account_account_", "account_", dup_account_cols)
    if (length(dup_account_cols)) {
      data.table::setnames(node_list, old = dup_account_cols, new = cleaned_names)
    }
    
    sim_dt <- data.table::copy(groups_data$sim_dt)
    
    user_data <- node_list[, .SD, .SDcols = grep("^account_", names(node_list), value = TRUE)]
    other_user_vars <- setdiff(names(user_data), c("account_id", "account_name"))
    return_post_dt <- "post_data" %in% names(groups_data)
    
    if (verbose) cli::cli_inform("Re-running group detection.")
    
    # Pass on new arguments from ... 
    dot_args <- list(...)
    args <- c(
      dot_args,
      if (!is.null(time_window)) list(time_window = time_window),
      if (!is.null(min_simil)) list(min_simil = min_simil)
    )
    
    # otherwise take parameters from groups_data
    if (exists("args") && is.list(args) && length(args) > 0) {
      params <- utils::modifyList(groups_data$params, args)
    } else {
      params <- groups_data$params
    }
    
    # Check if sim_dt has community id
    sim_dt_community <- "community" %in% names(groups_data$sim_dt)
    
    # Re-apply groups detection
    groups_data_new <- coorsim_detect_groups(
      simdt = sim_dt[, (intersect(c("community", "community_y"), names(sim_dt))) := NULL],
      user_data = user_data,
      account_id = "account_id",
      account_name = "account_name",
      other_user_vars = NULL,
      edge_weight = edge_weight,
      cluster_method = params$cluster_method,
      resolution = params$resolution,
      theta = params$theta,
      return_post_dt = return_post_dt,
      verbose = verbose,
      min_comm_size = min_comm_size
    )
    
    # Chek if post_data has been augmented
    if(return_post_dt){
      post_data <- data.table::copy(groups_data$post_data)
      other_post_vars <- grep("^account_", 
                              setdiff(names(post_data), names(groups_data_new$post_data)),
                              value = T, invert = T)
    }else{
      post_data = NULL
      other_post_vars = NULL
    }
    
    groups_data_new <- augment_groups_data(groups_data = groups_data_new, 
                                                    post_data = post_data,
                                                    user_data = user_data,
                                                    other_user_vars = other_user_vars,
                                                    other_post_vars = other_post_vars, 
                                                    verbose =  verbose, 
                                                    sim_dt_community = sim_dt_community)
    
    # Subset remaining objects
    groups_data_new$user_labels <- if ("user_labels" %in% names(groups_data) && !is.null(groups_data$user_labels)) groups_data$user_labels[account_id %in% groups_data_new$node_list$account_id] else NULL
    groups_data_new$community_labels <- if ("community_labels" %in% names(groups_data) && !is.null(groups_data$community_labels)) groups_data$community_labels[community %in% unique(groups_data_new$node_list$community)] else NULL
    community_metrics <- if ("community_metrics" %in% names(groups_data) && !is.null(groups_data$community_metrics)) groups_data$community_metrics[community %in% unique(node_list$community)] else NULL
    filter <- if ("filter" %in% names(groups_data)) groups_data$filter else NULL
    params <- if ("params" %in% names(groups_data)) groups_data$params else NULL
    
    
    ## Overwrite groups_data object
    groups_data <- groups_data_new
    rm(groups_data_new)
    groups_data <- Filter(Negate(is.null), groups_data)
    
    
  } 
  
  
  ## Fetch previous filter arguments
  if(!is.null(groups_data$filter))
    
    # Define %||% if not yet defined
    `%||%` <- function(x, y) if (!is.null(x)) x else y
  
  # Define new filter values
  new_filter <- list(
    by_col = by_col,
    by_val = by_val,
    edge_weight = edge_weight,
    time_window = time_window,
    min_simil = min_simil,
    min_comm_size = min_comm_size,
    min_comp_size = min_comp_size,
    min_degree = min_degree,
    min_participation = min_participation,
    communities_index = communities_index,
    quantile_accountwise = quantile_accountwise,
    quantile_postwise = quantile_postwise,
    top_n_accountwise = top_n_accountwise,
    top_n_postwise = top_n_postwise,
    stringsimil_cutoff = stringsimil_cutoff,
    stringsimil_method = if (!is.null(stringsimil_cutoff)) stringsimil_method else NULL,
    stringsimil_ngram = if (!is.null(stringsimil_cutoff)) stringsimil_ngram else NULL
  )
  
  # Get existing filter (if any)
  previous_filter <- if ("filter" %in% names(groups_data)) groups_data$filter else list()
  
  # Custom merge: keep old if new is NULL
  merged_filter <- mapply(
    FUN = function(new, old) if (!is.null(new)) new else old,
    new = new_filter,
    old = previous_filter[names(new_filter)],
    SIMPLIFY = FALSE
  )
  
  # Add previous entries not listed in new_filter
  extra_old <- previous_filter[setdiff(names(previous_filter), names(new_filter))]
  merged_filter <- c(merged_filter, extra_old)
  
  # Drop NULLs
  groups_data$filter <- Filter(Negate(is.null), merged_filter)
  
  # Clean duplicated "account_" prefixes in node_list and post_data
  for (tbl_name in c("node_list", "post_data")) {
    if (!is.null(groups_data[[tbl_name]]) && data.table::is.data.table(groups_data[[tbl_name]])) {
      dt <- groups_data[[tbl_name]]
      cols <- names(dt)
      
      # Match any "account_" repeated two or more times
      dup_cols <- grep("^account_(account_)+", cols, value = TRUE)
      new_names <- sub("^account_(account_)+", "account_", dup_cols)
      
      if (length(dup_cols)) {
        data.table::setnames(dt, old = dup_cols, new = new_names)
      }
      
      # Reassign cleaned table
      groups_data[[tbl_name]] <- dt
    }
  }
  
  return(groups_data)
}
