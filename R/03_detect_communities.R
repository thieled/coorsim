#' Detect Communities of Coordinated Accounts
#' 
#' This function identifies communities of coordinated accounts in a co-similarity network. 
#' It supports two community detection algorithms—the classic "Louvain" algorithm, the "Leiden" algorithm, the "label_propagation" algorithm,
#' and the Focal Structures Algorithm "FSA_V" proposed by Weber & Neumann (2021), based on Sen et al. (2016). 
#' It can filter communities based on a minimum edge weight threshold and filter the network, and all other output by minimum community size.
#' 
#' @param simdt A `data.table` containing the network edges, with columns for account IDs 
#'   (`account_id`, `account_id_y`) and optional columns for `post_id` and `time`.
#' @param user_data A `data.table` of account-level data with at least one column for `account_id`. 
#'   Additional columns can be specified to be included in the output.
#' @param account_id Name of the column in `user_data` that identifies accounts. Defaults to "account_id".
#' @param account_name Name of the column in `user_data` for account names or labels. Defaults to "account_name".
#' @param other_user_vars Optional character vector of additional column names in `user_data` 
#'   to include in the output.
#' @param edge_weight Optional numeric threshold for filtering the edge list. 
#'   Only edges with a weight greater than or equal to this value will be included.
#' @param cluster_method Character, the algorithm to use for community detection. 
#'   Options are "louvain", "leiden", "label_prop" or "FSA_V". Defaults to "label_prop".
#' @param resolution Numeric resolution parameter for the Louvain algorithm. 
#'   Controls the size of detected communities. Higher values create smaller communities. Defaults to `1`.
#' @param theta Numeric, specifies the threshold for the FSA_V algorithm to control 
#'   the inclusion of edges. Higher values prioritize stronger edges in community construction. Defaults to `0.7`.
#' @param return_post_dt Logical, whether to return a `data.table` with post data merged with community labels. 
#'   Defaults to `TRUE`.
#' @param verbose Logical, whether to display progress messages. Defaults to `TRUE`.
#' @param seed Numeric, the random seed to ensure reproducibility. Defaults to `42`.
#' @param min_comm_size Integer. Minimum community size for filtering. Default is `NULL`.
#' @param ... Additional parameters passed to the clustering functions.
#' 
#' @return A named list containing:
#'   \describe{
#'     \item{\code{graph}}{An `igraph` object representing the network graph created from the filtered edge list.}
#'     \item{\code{communities}}{An `igraph::communities` object representing the detected communities.}
#'     \item{\code{edge_list}}{A filtered `data.table` of edges used for community detection.}
#'     \item{\code{node_list}}{A `data.table` with node membership information, including account ID, community assignment, and algorithm used.}
#'     \item{\code{post_data}}{A `data.table` containing post-level data with community information, only returned if \code{return_post_dt} is `TRUE`.}
#'   }
#' 
#' @export
coorsim_detect_groups <- function(simdt, 
                                  user_data,
                                  account_id = "account_id",
                                  account_name = "account_name",
                                  other_user_vars = NULL,
                                  edge_weight = NULL,
                                  cluster_method = "label_prop",
                                  resolution = 1,
                                  theta = .3,
                                  return_post_dt = TRUE,
                                  verbose = TRUE,
                                  seed = 42,
                                  min_comm_size = NULL,
                                  ...
) {
  
  # Step 1: Harmonizing user data
  
  # Check and rename user_data columns
  if(verbose)  cli::cli_progress_step("[1/5]: Harmonizing user data.",
                                      msg_done = "[1/5]: Harmonized user data.")
  
  user_col_renames <- list(account_id = account_id, account_name = account_name)
  for (col in names(user_col_renames)) {
    alt_name <- user_col_renames[[col]]
    if (!is.null(alt_name) && alt_name %in% names(user_data)) {
      data.table::setnames(user_data, alt_name, col)
    }
  }
  
  # Add prefix to user_data columns (except account_id)
  user_cols_to_keep <- c("account_id", "account_name", other_user_vars)
  user_data <- data.table::copy(user_data[, intersect(user_cols_to_keep, names(user_data)), with = FALSE])
  
  if(!is.null(other_user_vars)){
    user_cols <- setdiff(names(user_data), c("account_id", "account_name"))
    data.table::setnames(user_data, user_cols, paste0("account_", user_cols))
  }
  
  ### De-duplicating user_data ###
  if (verbose) cli::cli_inform("De-duplicating 'user_data'...\n")
  user_data <- user_data[!duplicated(user_data$account_id)]
  
  if(verbose) cli::cli_progress_done()
  
  
  
  # Step 2: Create the edge list
  
  if(verbose)  cli::cli_progress_step("[2/5]: Create edge list.",
                                      msg_done = "[2/5]: Created edge list.")
  
  # The edge list connects 'account_id' with 'account_id_y' and counts the connections (as weight)
  edge_list <- simdt[, .(weight = .N), by = .(account_id, account_id_y)]
  
  # Optionally: Filter the edge list based on the edge_weight parameter
  if (!is.null(edge_weight)) {
    if (verbose) cli::cli_inform("Filter by edge_weight >= {edge_weight}.")
    edge_list <- edge_list[weight >= edge_weight]
  } 
  
  # Calculate overall mean weight for all edges
  g_mean <- mean(edge_list$weight)
  
  if(verbose) cli::cli_progress_done()
  
  
  # Step 3: Create the node list
  
  if(verbose)  cli::cli_progress_step("[3/5]: Create node list and graph.",
                                      msg_done = "[3/5]: Created node list and graph.")
  
  # Create the vector of unique account_ids (nodes)
  nodes <- unique(c(simdt$account_id, simdt$account_id_y))
  # Create a data.table of nodes
  node_list <- data.table::data.table(account_id = nodes)
  # Left join on nodes_dt
  node_list <- user_data[node_list, on = .(account_id)]
  # Step 4: Create the igraph object
  g <- igraph::graph_from_data_frame(d = edge_list, 
                                     vertices = node_list, 
                                     directed = FALSE)
  
  if(verbose) cli::cli_progress_done()
  
  
  
  # Step 5: Apply clustering based on the selected algorithm
  
  if(verbose)  cli::cli_progress_step("[4/5]: Finding communities.",
                                      msg_done = "[4/5]: Finding communities.")
  
  if (cluster_method == "label_prop") {
    
    # Apply cluster_label_propagation
    communities <- igraph::cluster_label_prop(g, ...)
    
    # Extract membership igraph object
    membership <- igraph::membership(communities)
    
    # Create node membership datatable for export
    node_membership_dt <- merge(node_list[match(igraph::V(g)$name, node_list$account_id)],
                                data.table::data.table(
                                  account_id = as.character(names(membership)), 
                                  community = membership),
                                by = "account_id")
    
    node_membership_dt[, algorithm := cluster_method]
    
  } else if (cluster_method == "louvain") {
    
    set.seed(seed)
    
    # Apply louvan clustering algorithm
    communities <- igraph::cluster_louvain(g, 
                                           resolution = resolution,
                                           ...)
    
    # Extract membership igraph object
    membership <- igraph::membership(communities)
    
    # Create node membership datatable for export
    node_membership_dt <- merge(node_list[match(igraph::V(g)$name, node_list$account_id)],
                                data.table::data.table(
                                  account_id = as.character(names(membership)), 
                                  community = membership),
                                by = "account_id")
    
    node_membership_dt[, algorithm := cluster_method]
    
    
  } else if (cluster_method == "leiden") {
    
    set.seed(seed)
    
    # Apply louvan clustering algorithm
    communities <- igraph::cluster_leiden(g, 
                                          resolution = resolution,
                                          ...)
    
    # Extract membership igraph object
    membership <- igraph::membership(communities)
    
    # Create node membership datatable for export
    node_membership_dt <- merge(node_list[match(igraph::V(g)$name, node_list$account_id)],
                                data.table::data.table(
                                  account_id = as.character(names(membership)), 
                                  community = membership),
                                by = "account_id")
    
    node_membership_dt[, algorithm := cluster_method]
    
    
  } else if (cluster_method == "FSA_V") {
    
    set.seed(seed) 
    
    # Louvain for initial clustering
    communities <- igraph::cluster_louvain(g, 
                                           resolution = resolution,
                                           ...)
    
    membership <- igraph::membership(communities)
    community_edges <- data.table::as.data.table(igraph::as_data_frame(g, what = "edges"))
    community_edges <- merge(community_edges, 
                             data.table::data.table(
                               account_id = as.character(names(membership)), 
                               community = membership), 
                             by.x = "from", 
                             by.y = "account_id")
    
    # Step 2: Apply FSA_V algorithm
    fs_list <- community_edges[, {
      
      sorted_edges <- .SD[order(-weight)]
      if (is.na(sorted_edges[1, weight])) return(NULL)  # skip if no valid weight
      
      candidate_edges <- vector("list", .N)
      candidate_edges[[1]] <- sorted_edges[1, .(from, to, weight)]
      edge_weights <- sorted_edges[1, weight]
      edge_count <- 1
      still_growing <- TRUE
      
      for (i in 2:.N) {
        current_weight <- sorted_edges[i, weight]
        new_mean <- mean(c(edge_weights, current_weight), na.rm = TRUE)
        previous_mean <- mean(edge_weights, na.rm = TRUE)
        
        if (!is.na(current_weight) &&
            current_weight >= g_mean &&
            new_mean >= theta * previous_mean) {
          
          edge_weights <- c(edge_weights, current_weight)
          candidate_edges[[edge_count + 1]] <- sorted_edges[i, .(from, to, weight)]
          edge_count <- edge_count + 1
          
        } else {
          still_growing <- FALSE
          break
        }
      }
      
      data.table::rbindlist(candidate_edges[1:edge_count], fill = TRUE)
      
    }, by = community]
    
    
    # Step 3: Create a new graph based on the edges from FSA_V
    g_filtered <- igraph::graph_from_data_frame(fs_list[, .(from, to, weight)], directed = FALSE)
    
    # # Step 4: Re-run clustering by 'cluster_label_prop' on the filtered graph to assign final community labels
    communities <- igraph::cluster_label_prop(g_filtered)
    
    # Extract membership igraph object
    membership <- igraph::membership(communities)
    
    # Create node membership datatable for export
    node_membership_dt <- merge(node_list[match(igraph::V(g_filtered)$name, node_list$account_id)],
                                data.table::data.table(
                                  account_id = as.character(names(membership)), 
                                  community = membership),
                                by = "account_id")
    
    node_membership_dt[, algorithm := cluster_method]
    
    # # Merge algorithm info
    node_membership_dt[, algorithm := cluster_method]
    node_membership_dt[, parameters := paste0("theta = ", theta, "; resolution = ", resolution)]
    
    # Store graph, edge list and communities object
    # communities <- fsa_communities
    g <- g_filtered
    edge_list <- fs_list[, .(account_id = from, account_id_y = to, weight, community)]
    
  } else {
    stop("Invalid cluster_method. Choose 'louvain', 'leiden', 'label_prop', or 'FSA_V'.")
  }
  
  
  if(verbose) cli::cli_progress_done()
  
  # Create the communities dataframe with account_id, name, community, and algorithm used
  # Match the name in V(g)$name with node_list$name to get the correct account_id (node_id)
  
  
  if(verbose)  cli::cli_progress_step("[5/5]: Merge and prepare output data.",
                                      msg_done = "[5/5]: Prepared output data.")
  
  
  if(return_post_dt){
    # Create coordinated post data.table
    post_dt <- data.table::melt(simdt, 
                                measure.vars = list(post_id = c("post_id", "post_id_y"), 
                                                    account_id = c("account_id", "account_id_y"),
                                                    time = c("time", "time_y"),
                                                    content = c("content", "content_y")
                                ))
    
    # Remove duplicates to get unique post_id and account_id pairs
    post_dt <- unique(post_dt[, .(post_id, account_id, time, content)])
    
    # Join community info
    post_dt <- post_dt[node_membership_dt, on = .(account_id)]
  }else{
    post_dt <- NULL
  }
  
  ### Filter by community size
  
  if(!is.null(min_comm_size)){
    
    
    if(verbose) cli::cli_progress_done()
    
    
    if(verbose)  cli::cli_progress_step("Filtering by min_comm_size = {min_comm_size}).",
                                        msg_done = "Filtered by min_comm_size = {min_comm_size}).")
    
    node_membership_dt <- node_membership_dt[
      , .N, by = community][N > min_comm_size][
        node_membership_dt, on = "community", nomatch = 0]
    
    # Define the set of valid account IDs
    valid_accounts <- node_membership_dt$account_id
    
    # 1. Filter the igraph graph object `g`
    g <- igraph::induced_subgraph(g, vids = igraph::V(g)[name %in% valid_accounts])
    
    # 2. Filter the igraph communities object `communities`
    # Extract membership only for valid nodes
    filtered_membership <- igraph::membership(communities)[names(igraph::membership(communities)) %in% valid_accounts]
    # Recreate the community structure
    communities <- igraph::make_clusters(g, filtered_membership)
    
    # 3. Filter the data.table `post_data`
    if (!is.null(post_dt)) {
      post_dt <- post_dt[account_id %in% valid_accounts]
    }
    
    # 4. Filter the data.table `edge_list`
    edge_list <- edge_list[account_id %in% valid_accounts & account_id_y %in% valid_accounts]
    
    # 5. Filter simdt
    simdt <- simdt[account_id %in% valid_accounts & account_id_y %in% valid_accounts]
    
    
  }
  
  
  # Return the results as list
  result <- list(
    graph = g,
    communities = communities,
    edge_list = edge_list,
    sim_dt = simdt,
    node_list = node_membership_dt,
    post_data = post_dt
  )
  
  if(verbose) cli::cli_progress_done()
  
  return(result)
}
