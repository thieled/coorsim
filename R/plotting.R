
#' Plot Coordinated Posts by Community and Time
#'
#' This function generates a plot of coordinated posts over time by community, with options to filter by the top `n` communities
#' based on community size or post frequency. It utilizes a stacked bar chart to show the volume of posts per community, 
#' aggregated by the specified time unit.
#'
#' @param network_data A list containing data.tables with network data, where `post_data` must be one of the elements.
#' @param unit Character string specifying the time unit for flooring dates, defaulting to `"hour"`. Accepts units recognized by `lubridate::floor_date`.
#' @param n_communities Integer specifying the number of top communities to plot. Defaults to `NULL`, in which case all communities are included.
#' @param quantile Numeric value between 0 and 1 specifying the quantile threshold for filtering communities by size. 
#'                 Communities below this quantile threshold are excluded.
#' @param param_info Optional parameter info string that appears in the plot caption, providing context for the plot, such as parameter settings.
#' @param verbose Logical indicating whether to print status updates during execution. Defaults to `TRUE`.
#' @param legend Logical value indicating whether to display a legend for the plot. Defaults to `TRUE`.
#' @param title_prefix Optional character string used as a prefix for the plot title.
#'
#' @return A `ggplot` object visualizing the number of coordinated posts per community over time. 
#' If `n_communities` is specified, the plot is limited to the top `n_communities` by size.
#'
#' @export
plot_coordinated_posts <- function(network_data,
                                   unit = "hour",
                                   n_communities = NULL,
                                   quantile = NULL,
                                   param_info = NULL,
                                   verbose = TRUE,
                                   legend = TRUE,
                                   title_prefix = NULL
) {
  
  if(!"post_data" %in% names(network_data)){
    stop("No 'post_data' provided in 'netword_data'. Unable to plot.")
  }
  
  # Create copy of dt
  dt <- data.table::copy(network_data$post_data)
  
  # Extract parameter info
  param_info <- paste(paste0("Parameters: ", param_info), 
                      paste0("COMM_ALG: ", network_data$node_list$algorithm[[1]]),
                      network_data$node_list$parameters[[1]], sep = "; "
  )
  filter_info <- NULL
  
  
  # Step 1: 
  if(!is.null(quantile)){
    
    # Calculate sizes of communities
    comm_size <- dt[, .(N = data.table::uniqueN(account_id)), by = community]
    
    keep_comm <- filter_ntile(comm_size, var_field = "N", probs = quantile)
    
    # How many are dropped
    orig_n <- comm_size[, .N]
    keep_n <- keep_comm[, .N]
    drop_n <- orig_n - keep_n
    
    # Subset
    dt <- dt[community %in% keep_comm$community, ]
    
    # Set n communities if null
    n_communities = keep_n
    
    pct <- paste0((1 - quantile)*100, "%")
    
    if(verbose)cli::cli_inform("Keeping top {pct}; n={keep_n} of lagrest communities (N members[{min(keep_comm$N)}, {max(keep_comm$N)}]).
                               Dropped n={drop_n} communities.")
    
    # Save filter info
    filter_info <- paste0("Largest ", pct, " of communities; n=", 
                          keep_n, 
                          "; Members: min=", min(keep_comm$N, na.rm = T),
                          ", max=", max(keep_comm$N, na.rm = T),
                          ", mean=", round(mean(keep_comm$N, na.rm = T),1),
                          "(", round(stats::sd(keep_comm$N, na.rm = T),1), ")."
    )
    
  }
  
  if(verbose)cli::cli_inform("Preparing post data.")
  
  # Step 1: Floor the 'time' column per hour and per day
  dt[, time_floored := lubridate::floor_date(time, unit = unit)]
  
  # Step 2: Calculate the number of observations per community and per floored time
  agg_data <- dt[, .N, by = .(community, time_floored)]
  
  # Step 3: Remove rows with NA in time_floored_hour or N (though likely .N won't produce NA)
  agg_data <- agg_data[!is.na(time_floored) & !is.na(N)]
  
  
  if(!is.null(n_communities)){
    
    # Step 1: Calculate the sum of N by community
    comm_size <- agg_data[, .(N = sum(N)), by = community]
    
    # Step 2: Get the top communities by total N
    keep_comm_i <- comm_size[order(-N)][1:n_communities, community]
    
    # Step 3: Filter agg_data to include only rows from the top communities
    keep_comm <- agg_data[community %in% keep_comm_i]
    
    
    # Store filter info
    if(is.null(quantile)){
      
      filter_info <- paste0("Members: min=", min(keep_comm$N, na.rm = T),
                            ", max=", max(keep_comm$N, na.rm = T),
                            ", mean=", round(mean(keep_comm$N, na.rm = T),1),
                            "(", round(stats::sd(keep_comm$N, na.rm = T),1), ")."
      )
    }
    
    
    # Step 6: Create a ggplot geom_bar plot, showing the number of observations per community per hour
    p <- keep_comm |> 
      ggplot2::ggplot(ggplot2::aes(x = time_floored, 
                                   y = N, 
                                   fill = factor(community))) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
      ggplot2::scale_x_datetime(breaks = "1 day", date_labels = "%b %d") +  # Adjust x-axis breaks and labels
      ggplot2::labs(title = paste0(paste(title_prefix, "Coordinated Posts of Largest n="), 
                                   n_communities, 
                                   " Communities."),
                    subtitle = filter_info,
                    caption = param_info,
                    y = "Number of Coordinated Posts",
                    x = "Time",
                    fill = "Community") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 1, hjust = 1))
    
    if(!legend) p <- p + ggplot2::theme(legend.position = "none")
    
  }else{
    
    member_info <- paste0("Members: min=", min(keep_comm$N, na.rm = T),
                          ", max=", max(keep_comm$N, na.rm = T),
                          ", mean=", round(mean(keep_comm$N, na.rm = T),1),
                          "(", round(stats::sd(keep_comm$N, na.rm = T),1), ")."
    )
    
    p <- agg_data |> 
      ggplot2::ggplot(ggplot2::aes(x = time_floored, 
                                   y = N)) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
      ggplot2::scale_x_datetime(breaks = "1 day", date_labels = "%b %d") +  # Adjust x-axis breaks and labels
      ggplot2::labs(title = paste0(paste(title_prefix, "Coordinated Posts Overall.")),
                    subtitle = member_info,
                    caption = param_info,
                    y = "Number of Coordinated Posts",
                    x = "Time") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 1, hjust = 1))
    
    
  }
  
  return(p)  
}






#' Plot a Community Graph with Enhanced Visualization
#'
#' This function takes a network object with a graph and node list and 
#' creates a community graph visualization. Nodes with degree <= 1 
#' are removed, and the graph is further filtered based on edge weights 
#' and component size. Community colors are assigned using the viridis palette.
#'
#' @param network_data A list containing an igraph graph, and a node_list as resulting from coorsim::detect_communities.
#' @param edge_weight_threshold Numeric, the minimum weight for edges to be included in the plot.
#' @param community_size_threshold Numeric, the minimum size of components to be included.
#' @param palette_option Character, the viridis palette option for coloring communities (default is "A").
#' @param end_color Numeric, the end value for color intensity in the viridis palette (default is 0.9).
#'
#' @return A ggplot2 object representing the community graph.
#' @export
#'
#' 
plot_communities <- function(network_data, 
                                 edge_weight_threshold = 5, 
                                 community_size_threshold = 5, 
                                 palette_option = "A", 
                                 end_color = 0.9) {
  
  # Extract the graph and communities from the network object
  g <- network_data$graph
  node_list <- network_data$node_list
  
  # Remove nodes with degree <= 1
  g <- igraph::delete_vertices(g, which(igraph::degree(g) <= 1))
  
  # Keep only components with size >= community_size_threshold
  g <- igraph::induced_subgraph(g, 
                                igraph::V(g)[igraph::components(g)$membership %in% 
                                               which(igraph::components(g)$csize >= community_size_threshold)])
  
  # Filter edges based on weight
  g <- igraph::subgraph.edges(g, igraph::E(g)[igraph::E(g)$weight > edge_weight_threshold])
  
  # Get unique communities and assign colors
  unique_communities <- unique(node_list[node_list$account_id %in% igraph::V(g)$name]$community)
  community_colors <- viridis::viridis(length(unique_communities), option = palette_option, end = end_color)
  igraph::V(g)$color <- community_colors[match(igraph::V(g)$name, node_list$account_id)]
  
  # Convert graph to tidygraph format and join community information
  tidy_g <- g |> 
    tidygraph::as_tbl_graph()  |> 
    tidygraph::activate(nodes) |> 
    dplyr::left_join(node_list, by = c("name" = "account_id"))
  
  # Plot the graph with enhanced visualization
  p <- ggraph::ggraph(tidy_g, layout = "stress") +
    ggraph::geom_edge_link(ggplot2::aes(alpha = weight), color = "grey50", show.legend = FALSE) + 
    ggraph::geom_node_point(ggplot2::aes(color = as.factor(community), size = igraph::degree(g, mode = "all")), show.legend = TRUE) + 
    ggplot2::scale_color_manual(values = community_colors) + 
    ggplot2::scale_alpha_continuous(range = c(0.2, 0.9)) + 
    ggplot2::theme_void() 
  
  return(p)
}
