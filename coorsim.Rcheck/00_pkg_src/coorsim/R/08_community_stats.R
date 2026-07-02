
#' Compute and aggregate multi-domain community-level metrics
#'
#' This function computes a comprehensive set of community-level metrics based on the 
#' input `groups_data` object, which includes graph, similarity, and account data. It 
#' extracts and aggregates network structure metrics, similarity and temporal coordination 
#' statistics, and account-based indicators. Optionally, it also computes lexical metrics 
#' from post and account-level text content.
#'
#' @param groups_data A list containing the following elements:
#'   - `graph`: an igraph object representing the network.
#'   - `communities`: an igraph community object (e.g. from `cluster_louvain()`).
#'   - `node_list`: a data.table with account-level metadata.
#'   - `sim_dt`: a data.table with coordinated post pairs and similarity info.
#'   - `post_data` (optional): a data.table with post-level content and metadata.
#'
#' @param account_description Name of the column containing account descriptions (default `NULL`). 
#'   If not present under standard name, the function will attempt to use this.
#' @param account_creation_date Name of the column with account creation timestamps (default `NULL`).
#' @param follower Name of the follower count column (default `NULL`).
#' @param following Name of the following count column (default `NULL`).
#' @param content_stats Logical; if `TRUE`, compute and aggregate lexical content metrics (default `TRUE`).
#' @param zstd Logical; if `TRUE` will z-standardize all columns (default `FALSE`).
#' @param verbose Logical; if `TRUE`, shows progress messages using the `cli` package (default `TRUE`).
#'
#' @return A data.table where each row represents one community and columns contain
#'   metrics for:
#'   - Network topology (e.g., degree, centrality, density)
#'   - Similarity structure (e.g., average pairwise similarity within communities)
#'   - Temporal coordination (e.g., entropy of posting over time)
#'   - Account-level metadata (e.g., account age, follower ratio, post count)
#'   - Optionally, post and bio content elaborateness metrics (if `content_stats = TRUE`)
#'
#' @details
#' The function internally computes:
#' - Network statistics from subgraphs induced by communities
#' - Similarity and time-difference metrics from coordinated post pairs
#' - Posting temporal entropy and burstiness (via 3-hour bins)
#' - Optional text metrics from post and account descriptions (if `content_stats = TRUE`)
#'
#' It automatically standardizes column names for description, creation date,
#' follower/following if alternate column names are supplied.
#'
#' @seealso [coorsim::get_content_metrics()], [coorsim::z_standardize()]
#' 
#' @import data.table
#' 
#' @export
get_community_metrics <- function(groups_data,
                                 account_description = NULL,
                                 account_creation_date = NULL,
                                 follower = NULL,
                                 following = NULL,
                                 content_stats = TRUE,
                                 zstd = FALSE,
                                 verbose = T
){
  
  if (!requireNamespace("ineq", quietly = TRUE)) {
    stop("Package 'ineq' is required for this function. Please install it.")
  }
  
  
  g <- groups_data$graph
  communities <- groups_data$communities
  node_list <- data.table::copy(groups_data$node_list)
  sim_dt <- data.table::copy(groups_data$sim_dt)
  
  if("post_data" %in% names(groups_data)){
    post_data <- data.table::copy(groups_data$post_data)
  }else{
    post_data <- NULL
  }
  
  ### Block 1: Network metrics
  
  if(verbose)  cli::cli_progress_step("Compute network metrics.",
                                      msg_done = "Computed network metrics.")
  
  # Ensure igraph and data.table are available
  stopifnot(inherits(g, "igraph"))
  stopifnot(inherits(communities, "communities"))
  
  # Get membership vector
  memb <- igraph::membership(communities)
  
  # Network metrics
  network_metrics <- lapply(unique(memb), function(comm) {
    nodes <- names(memb)[memb == comm]
    subg <- igraph::induced_subgraph(g, vids = nodes)
    
    deg <- igraph::degree(subg, mode = "all")
    wt <- if ("weight" %in% igraph::edge_attr_names(subg)) {
      igraph::E(subg)$weight
    } else {
      rep(1, igraph::ecount(subg))
    }
    
    n_nodes <- igraph::vcount(subg)
    n_edges <- igraph::ecount(subg)
    
    data.table::data.table(
      community = comm,
      g_nodes_n = igraph::vcount(subg),
      g_edges_n = igraph::ecount(subg),
      g_degree_mean = mean(deg),
      g_degree_sd = stats::sd(deg),
      g_edge_weight_mean = mean(wt),
      g_edge_weight_sd = stats::sd(wt),
      g_edge_density = igraph::edge_density(subg, loops = FALSE),
      g_centr_degree = igraph::centr_degree(subg, normalized = TRUE)$centralization,
      g_centr_betw = igraph::centr_betw(subg, normalized = TRUE)$centralization,
      g_transitivity = mean(igraph::transitivity(subg, type = "local", isolates = "zero")),
      g_diameter = if (n_nodes > 1) igraph::diameter(subg, directed = FALSE, weights = NA) else NA_real_,
      g_mean_distance = if (n_nodes > 1) igraph::mean_distance(subg, directed = FALSE, unconnected = TRUE) else NA_real_
    )
  })
  
  network_metrics <- data.table::rbindlist(network_metrics, fill = T, use.names = T)
  
  if(verbose) cli::cli_progress_done()
  
  ### Block 2: Metrics from sim_dt
  
  if(verbose)  cli::cli_progress_step("Compute similarity and temporal metrics.",
                                      msg_done = "Computed similarity and temporal metrics.")
  
  
  # Add community id to sim_dt if not available
  if(!"community" %in% names(sim_dt)){
    if(verbose) cli::cli_inform("Adding community ID to sim_dt.")
    comm_dt_x <- data.table::copy(groups_data$node_list)[, .(account_id, community)]
    comm_dt_y <- data.table::copy(groups_data$node_list)[, .(account_id_y = account_id, community_y = community)]
    
    sim_dt <- data.table::copy(groups_data$sim_dt)
    sim_dt <- comm_dt_x[sim_dt, on = "account_id"]
    sim_dt <- comm_dt_y[sim_dt, on = "account_id_y"]
    
    data.table::setcolorder(
      sim_dt,
      c(
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
  
  # Similarity-based metrics
  sim_dt[, is_within := community == community_y]
  
  similarity_metrics <- sim_dt[, .(
    s_similarity_mean = mean(similarity[is_within]),
    s_similarity_sd   = sd(similarity[is_within]),
    t_time_diff_mean  = mean(as.numeric(time_diff[is_within]), na.rm = TRUE),
    t_time_diff_sd    = sd(as.numeric(time_diff[is_within]), na.rm = TRUE),
    s_within_comm_ratio = sum(is_within) / .N,
    s_duplication_ratio = sum(similarity >= .989) / .N
  ), by = community]
  
  # Get post_data from sim_dt (more robust - as 'post_data' is a optional output in coorsim)
  post_dt <- data.table::rbindlist(list(
    sim_dt[, .(account_id, post_id, time, content, community)],
    sim_dt[, .(account_id = account_id_y, post_id = post_id_y, time = time_y, content = content_y, community = community_y)]
  ), use.names = TRUE)
  
  # Remove duplicates by post_id
  post_dt <- unique(post_dt, by = "post_id")
  
  # Temporal metrics
  post_dt[, time := as.POSIXct(time, origin = "1970-01-01", tz = "UTC")]
  
  temporal_metrics <- post_dt[, {
    time_range <- range(time, na.rm = TRUE)
    half_life <- as.numeric(difftime(time_range[2], time_range[1], units = "hours"))
    
    # Posts per 3-hour interval
    hourly <- .SD[, .N, by = .(hour = as.POSIXct(cut(time, breaks = "3 hours")))]
    counts <- hourly$N
    
    # Entropy helper
    entropy <- function(p) {
      p <- p[p > 0]
      -sum(p * log2(p))
    }
    
    p_norm <- counts / sum(counts)
    
    coef_var <- if (mean(counts) > 0) sd(counts) / mean(counts) else NA_real_
    coef_var[is.na(coef_var)] <- 0  # Set NA to 0 explicitly
    
    list(
      t_half_life_h = half_life,
      t_coef_var_3h = coef_var,
      t_entropy_3h = entropy(p_norm)
    )
  }, by = community]
  
  if(verbose) cli::cli_progress_done()
  
  ### Block 3: Metrics from post_dt
  
  # Extract account stats for later
  acc_stats <- post_dt[, `:=` (account_n_post = .N, account_last_post = max(time, na.rm = T)),by = account_id
  ][, .(account_id, account_n_post, account_last_post)][!duplicated(account_id)]
  node_list <- merge(node_list, acc_stats, by = "account_id")
  
  if(content_stats == T){
    
    if(verbose)  cli::cli_progress_step("Computing content metrics for posts.",
                                        msg_done = "Computed content metrics for posts.")
    
    post_content_metrics <- get_content_metrics(post_dt, content_col = "content", prefix = "c_posts_")
    
    if(verbose) cli::cli_progress_done()
    
  }
  
  
  ### Block 4: Account level metrics 
  
  if(verbose)  cli::cli_progress_step("Computing account-level metrics.",
                                      msg_done = "Computed account-level metrics.")
  
  
  # Define expected variables and possible user-supplied alternatives
  vars <- list(
    account_description = account_description,
    account_creation_date = account_creation_date,
    follower = follower,
    following = following
  )
  
  
  for (std_name in names(vars)) {
    if (!std_name %in% names(node_list)) {
      user_name <- vars[[std_name]]
      
      if (!is.null(user_name) && user_name %in% names(node_list)) {
        data.table::setnames(node_list, old = user_name, new = std_name)
      } else {
        # If neither standard nor user-provided name is available, create NA column
        node_list[, (std_name) := NA]
      }
    }
  }
  
  # Ensure datetime format
  node_list[, account_creation_date := as.POSIXct(account_creation_date, origin = "1970-01-01", tz = "UTC")]
  
  # Compute metrics
  node_list[, a_age_h := as.numeric(difftime(account_last_post, account_creation_date, units = "hours"))]
  node_list[, a_ffr := ifelse(is.na(follower) | is.na(following), NA_real_,
                              (follower + 1e-4)  / (following + 1e-4))]
  node_list[, a_npost := account_n_post]
  node_list[, a_following := following]
  node_list[, a_follower := follower]
  
  metrics <- c("a_age_h", "a_ffr", "a_following", "a_follower", "a_npost")
  
  
  # Set individual NA values to 0 unless the whole column is NA
  for (m in metrics) {
    if (!all(is.na(node_list[[m]]))) {
      node_list[is.na(get(m)), (m) := 0]
    }
  }
  
  # Aggregate per community
  mean_dt <- node_list[, lapply(.SD, mean), by = community, .SDcols = metrics]
  sd_dt   <- node_list[, lapply(.SD, sd),   by = community, .SDcols = metrics]
  
  # Rename
  data.table::setnames(mean_dt, old = metrics, new = paste0(metrics, "_mean"))
  data.table::setnames(sd_dt,   old = metrics, new = paste0(metrics, "_sd"))
  
  # Merge and return
  account_metrics <- merge(mean_dt, sd_dt, by = "community", sort = FALSE)
  
  # additional summary metrics
  summary_metrics <- node_list[, .(
    a_followers_gini = ineq::ineq(follower, type = "Gini"),
    a_posts_n = sum(account_n_post)
  ), by = community]
  
  
  if(content_stats == T){
    if(verbose) cli::cli_progress_done()
    if(verbose)  cli::cli_progress_step("Computing content metrics for accounts.",
                                        msg_done = "Computed content metrics for accounts.")
    
    account_content_metrics <- get_content_metrics(node_list, 
                                                   id_col = "account_id", 
                                                   content_col = "account_description", prefix = "c_bio_")
    
    if(verbose) cli::cli_progress_done()
    
  }else{
    if(verbose) cli::cli_progress_done()
  }
  
  
  
  ### Merging the features
  
  if(verbose)  cli::cli_progress_step("Merging metrics.",
                                      msg_done = "Merged metrics.")
  
  # Required checks
  dt_list <- list(network_metrics, similarity_metrics, temporal_metrics, account_metrics)
  dt_names <- c("network_metrics", "similarity_metrics", "temporal_metrics", "account_metrics")
  for (i in seq_along(dt_list)) {
    dt <- dt_list[[i]]
    stopifnot(data.table::is.data.table(dt), "community" %in% names(dt))
  }
  
  # Start join chain
  metrics_dt <- data.table::copy(network_metrics)
  metrics_dt <- merge(metrics_dt, similarity_metrics,     by = "community", all.x = TRUE, sort = FALSE)
  metrics_dt <- merge(metrics_dt, temporal_metrics,       by = "community", all.x = TRUE, sort = FALSE)
  metrics_dt <- merge(metrics_dt, account_metrics,        by = "community", all.x = TRUE, sort = FALSE)
  metrics_dt <- merge(metrics_dt, summary_metrics,        by = "community", all.x = TRUE, sort = FALSE)
  
  if (content_stats) {
    if (!is.null(post_content_metrics) && data.table::is.data.table(post_content_metrics)) {
      stopifnot("community" %in% names(post_content_metrics))
      metrics_dt <- merge(metrics_dt, post_content_metrics, by = "community", all.x = TRUE, sort = FALSE)
    } else {
      warning("Skipping post_content_metrics (not provided or invalid)")
    }
    
    if (!is.null(account_content_metrics) && data.table::is.data.table(account_content_metrics)) {
      stopifnot("community" %in% names(account_content_metrics))
      metrics_dt <- merge(metrics_dt, account_content_metrics, by = "community", all.x = TRUE, sort = FALSE)
    } else {
      warning("Skipping account_content_metrics (not provided or invalid)")
    }
  }
  
  if (zstd==T) {
    num_cols <- names(metrics_dt)[
      vapply(metrics_dt, is.numeric, logical(1L)) &
        names(metrics_dt) != "community"
    ]
    metrics_dt[, (num_cols) := lapply(.SD, z_standardize), .SDcols = num_cols]
  }
  
  
  if(verbose) cli::cli_progress_done()
  
  
  
  groups_data$community_metrics <- metrics_dt
  
  
  return(groups_data)
  
  
}


#' Compute and aggregate lexical content metrics per community
#'
#' Computes content-level elaborateness metrics using quanteda, and
#' aggregates them per community as mean and sd. ID column is user-defined.
#'
#' @param dt A data.table with a content column, community column, and an ID column
#' @param content_col Name of the text column (default: "text")
#' @param id_col Name of the unique ID column (e.g., "post_id", "account_id")
#' @param prefix Prefix to add to all metric columns in output (default: "c_")
#'
#' @return data.table with one row per community and aggregated metrics
#' @import data.table
#' @export
get_content_metrics <- function(dt, content_col = "text", id_col = "post_id", prefix = "c_") {
  stopifnot(data.table::is.data.table(dt))
  stopifnot(all(c(id_col, "community") %in% names(dt)))
  stopifnot(content_col %in% names(dt))
  
  dt <- data.table::copy(dt)
  content <- dt[[content_col]]
  dt[, nchar := stringi::stri_length(content)]
  
  # Word tokens (main tokens object)
  toks_word <- quanteda::tokens(
    content,
    what = "word",
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_url = TRUE
  )
  
  ntoken <- quanteda::ntoken(toks_word)
  ntype <- vapply(toks_word, function(x) length(unique(x)), integer(1L))
  ttr <- ifelse(ntoken > 0, ntype / ntoken, NA_real_)
  
  token_lengths <- lapply(toks_word, stringi::stri_length)
  token_length <- vapply(token_lengths, function(x) if (length(x) > 0) mean(x) else NA_real_, numeric(1L))
  tokens_gt6_ratio <- vapply(token_lengths, function(x) if (length(x) > 0) mean(x > 6) else NA_real_, numeric(1L))
  
  # Character-level tokens for emoji and punctuation
  toks_char <- quanteda::tokens(content, what = "character", remove_url = TRUE)
  emoji_ratio <- vapply(toks_char, function(x) {
    if (length(x) == 0L) return(NA_real_)
    sum(grepl("\\p{Emoji}", x, perl = TRUE)) / length(x)
  }, numeric(1L))
  
  punct_counts <- vapply(toks_char, function(x) {
    sum(grepl("^[[:punct:]]$", x))
  }, integer(1L))
  
  punct_ratio <- ifelse(ntoken > 0, punct_counts / ntoken, NA_real_)
  
  
  # Assemble metrics
  metrics_dt <- data.table::data.table(
    community = dt$community,
    nchar = dt$nchar,
    ntoken = ntoken,
    ntype = ntype,
    ttr = ttr,
    token_length = token_length,
    tokens_gt6_ratio = tokens_gt6_ratio,
    punct_ratio = punct_ratio,
    emoji_ratio = emoji_ratio
  )
  
  # Replace NA with 0 where appropriate
  for (j in names(metrics_dt)) {
    if (is.numeric(metrics_dt[[j]]) && !all(is.na(metrics_dt[[j]]))) {
      metrics_dt[is.na(get(j)), (j) := 0]
    }
  }
  
  metric_vars <- setdiff(names(metrics_dt), "community")
  
  mean_dt <- metrics_dt[, lapply(.SD, mean), by = community, .SDcols = metric_vars]
  sd_dt   <- metrics_dt[, lapply(.SD, sd),   by = community, .SDcols = metric_vars]
  
  data.table::setnames(mean_dt, old = metric_vars, new = paste0(prefix, metric_vars, "_mean"))
  data.table::setnames(sd_dt,   old = metric_vars, new = paste0(prefix, metric_vars, "_sd"))
  
  return(merge(mean_dt, sd_dt, by = "community", sort = FALSE))
}
