#' Calculate Internal-External Connections and E-I Index for Communities
#'
#' This function calculates the number of internal, external, and total connections for each community, 
#' as well as the E-I index, which represents the surplus of internal connections over external connections
#' normalized by the total number of connections. The function treats connections symmetrically, accounting 
#' for both incoming and outgoing edges between communities.
#'
#' @param network_data A list containing the coordinated network data, including the node list with `account_id` and 
#'   `community` columns.
#' @param global_network_data A list containing the global interaction network data, including the edge list with `from` 
#'   and `to` columns and a node list with `name` (node names) and `account_id`.
#'
#' @return A data.table with the following columns:
#' \describe{
#'   \item{community}{The community ID.}
#'   \item{internal_connections}{The number of internal connections within the community.}
#'   \item{external_connections}{The number of external connections from the community to other communities.}
#'   \item{total_connections}{The total number of connections (internal + external) involving the community.}
#'   \item{ei_index}{The E-I index for the community, calculated as the surplus of internal connections over 
#'   external connections, normalized by the total number of connections.}
#' }
#'
#' @details
#' The function first validates that the necessary columns exist in the input data. It then assigns missing 
#' community memberships by assigning unmatched nodes to a new community, and merges community information 
#' with the edge list. It calculates the number of internal and external connections, treating edges 
#' symmetrically to account for both incoming and outgoing connections. Finally, it computes the aggregated 
#' statistics by community, including the E-I index.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   result <- community_ei(network_data, global_network_data)
#'   print(result)
#' }
#'
#' @export
community_ei <- function(network_data,
                         global_network_data){
  
  if(!"node_list" %in% names(network_data)){
    stop("No 'node_list' found in 'global_network_data'.")
  }
  
  if(!"community" %in% names(network_data$node_list)){
    stop("No 'community' column found in 'network_data$node_list'.")
  }
  
  if(!"node_list" %in% names(global_network_data)){
    stop("No 'node_list' found in 'network_data'.")
  }
  
  if(!data.table::is.data.table(global_network_data$node_list)) global_network_data$node_list <- data.table::as.data.table(global_network_data$node_list)
  if(!data.table::is.data.table(network_data$node_list)) network_data$node_list <- data.table::as.data.table(global_network_data$node_list)
  
  coor_node_list <- data.table::copy(network_data$node_list[ , .(account_id, community)]) |> unique()
  glob_node_list <- data.table::copy(global_network_data$node_list[ , .(name)]) |> unique()
  data.table::setnames(glob_node_list, "name", "account_id")
  
  # Merge group membership info with node list
  glob_node_list_merged <- merge(glob_node_list, coor_node_list, by = "account_id", all.x = TRUE)
  
  # Assign unmatched nodes to a new community
  max_community <- max(glob_node_list_merged$community, na.rm = TRUE) + 1
  glob_node_list_merged[, community := data.table::fifelse(is.na(community), max_community, community)]
  
  # Merge community information into edge list
  glob_edge_list <- data.table::copy(global_network_data$edge_list[ , .(from, to)]) |> unique()
  
  # Set keys on 'account_id' to optimize merging
  data.table::setkey(glob_node_list_merged, account_id)
  
  # Perform the merge for both 'from' and 'to' columns in a single step
  glob_edge_list_merged <- merge(glob_edge_list, 
                                 glob_node_list_merged[, .(account_id, community)], 
                                 by.x = "from", by.y = "account_id", 
                                 all.x = TRUE, suffixes = c("_from", "_to"))
  
  glob_edge_list_merged <- merge(glob_edge_list_merged, 
                                 glob_node_list_merged[, .(account_id, community)], 
                                 by.x = "to", by.y = "account_id", 
                                 all.x = TRUE, suffixes = c("_from", "_to"))
  
  # Creat variable that identifies internal and external connections
  glob_edge_list_merged[, `:=`(
    internal_connections = as.numeric(community_from == community_to),
    external_connections = as.numeric(community_from != community_to)
  )]
  
  # Stack the data to treat community_from and community_to symmetrically
  symmetric_edges <- rbind(
    glob_edge_list_merged[, .(community = community_from, 
                              internal_connections, 
                              external_connections)],
    glob_edge_list_merged[, .(community = community_to, 
                              internal_connections, 
                              external_connections)]
  )
  
  # Now calculate the aggregated statistics by community
  comm_stats <- symmetric_edges[, .(
    internal_connections = sum(internal_connections, na.rm = TRUE),  # Total internal connections (from + to)
    external_connections = sum(external_connections, na.rm = TRUE),  # Total external connections (from + to)
    total_connections = .N,                                           # Total connections (from + to)
    ei_index = ifelse(.N > 0, 
                      (sum(internal_connections, na.rm = TRUE) - sum(external_connections, na.rm = TRUE)) / .N, 
                      NA_real_)                                  # Surplus ratio (from + to)
  ), by = community]
  
  return(comm_stats)
  
}



