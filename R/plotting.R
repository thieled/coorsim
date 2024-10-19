#' Plot Coordinated Posts of Communities
#'
#' This function generates a bar plot showing the coordinated posts of different communities over time. The plot can be configured to show the number of posts per community, with additional options to include labels or randomly sampled examples of peak coordinated activity for each community.
#'
#' @param network_data A list containing at least a `post_data` data.table and an `igraph` graph object. Optionally, the list may include `labelled_communities` for community labels and `param_list` for metadata.
#' @param by_communtiy Logical. If `TRUE`, the plot will display data grouped by community. If `FALSE`, the plot will display the overall activity.
#' @param unit A string specifying the time unit for floor date rounding, e.g., `"6 hours"`. Passed to `lubridate::floor_date()`.
#' @param component_size_threshold Integer. The minimum size of connected components to retain in the graph. Communities below this size are excluded from the plot.
#' @param palette_option A string specifying the color palette to use, passed to `viridis::viridis()`. Default is `"A"`.
#' @param start_color Numeric. The starting value of the color gradient for the palette, between 0 and 1.
#' @param end_color Numeric. The ending value of the color gradient for the palette, between 0 and 1.
#' @param title_prefix A string to prefix the plot title. Default is `NULL`.
#' @param include_examples Logical. If `TRUE`, the plot includes examples of peak coordinated activity. Default is `TRUE`.
#' @param add_labels Logical. If `TRUE`, the plot includes labels on the bars for each community's name and size. Default is `TRUE`.
#' @param legend Logical. If `TRUE`, the plot includes a legend for the communities. Default is `TRUE`.
#' @param n_random_accounts Integer. The number of random accounts per community to include in the examples, if `include_examples = TRUE`. Default is 3.
#' @param n_random_rows Integer. The number of random rows to include in the examples per account. Default is 2.
#' @param seed Integer. A seed for reproducibility of the random sampling of examples. Default is 42.
#' @param verbose Logical. If `TRUE`, progress and information messages are printed. Default is `TRUE`.
#'
#' @return A `ggplot2` object representing the coordinated posts of the communities.
#'
#' @export
plot_coordinated_posts <- function(network_data, 
                                   by_communtiy = T,
                                   unit = "6 hours",
                                   component_size_threshold = NULL,
                                   palette_option = "A", 
                                   start_color = 0,
                                   end_color = 0.9,
                                   title_prefix = NULL,
                                   include_examples= T,
                                   add_labels = F,
                                   legend = T,
                                   n_random_accounts = 3,
                                   n_random_rows = 2,
                                   seed = 42,
                                   verbose = T) {
  
  
  
  if(!"post_data" %in% names(network_data)){
    stop("No 'post_data' provided in 'netword_data'. Unable to plot.")
  }
  
  if(include_examples){
    if(!"content" %in% names(network_data$post_data)){
      warning("No 'content' in post_data. Please augment your network_data object. Proceeding without examples.")
      include_examples <- FALSE  
    }
  }
  
  if(add_labels){
    if(!"labelled_communities" %in% names(network_data)){
      warning("No 'labelled_communities' in network_data. Please generate labels. Proceeding without labels.")
      add_labels <- FALSE
    }
  }
  
  
  # Create copy of dt
  dt <- data.table::copy(network_data$post_data)
  
  ### Step 1: Filtering by component size
  
  # Extract the graph and communities from the network object
  g <- network_data$graph
  
  # Remove nodes with degree <= 1
  g <- igraph::delete_vertices(g, which(igraph::degree(g) <= 1))
  
  # Keep only components with size >= community_size_threshold
  if(!is.null(component_size_threshold)){
    g <- igraph::induced_subgraph(g, 
                                  igraph::V(g)[igraph::components(g)$membership %in% 
                                                 which(igraph::components(g)$csize >= component_size_threshold)])
  }
  
  # Filter dt 
  dt <- dt[account_id %in% igraph::V(g)$name, ]
  
  ### Step 2: Extract filter and param info to present in plot
  
  # Extract filter and parameter information
  if("param_list" %in% names(network_data)){
    # Drop NULL or NA elements
    param_list <- network_data$param_list[!sapply(network_data$param_list, function(x) is.null(x) || is.na(x))]
    # Bind the list into a string "name1: value1; name2: value2"
    param_string <- paste0("Parameters: ", paste0(names(param_list), ": ", unlist(param_list), collapse = "; "))
  }else if("post_data" %in% names(network_data) && "parameters" %in% names(network_data$post_data)){
    param_string <- paste0("Parameters: ", network_data$post_data$parameters[[1]])
  }else{
    param_string = paste0("Parameters: ", "NA")
  }
  
  if("filter_info" %in% names(network_data)){
    # Drop NULL or NA elements
    filter_info <- network_data$filter_info[!sapply(network_data$filter_info, function(x) is.null(x) || is.na(x))]
    
    # Rename "min_size" to "min_community_size"
    if ("min_size" %in% names(filter_info)) {
      names(filter_info)[names(filter_info) == "min_size"] <- "min_community_size"
    }
    
  }else{
    filter_info <- list()
  }
  
  # Add component size
  if (!is.null(component_size_threshold)) {
    if ("min_component_size" %in% names(filter_info)) {
      filter_info[names(filter_info) == "min_component_size"] <- component_size_threshold
    }
    if (!"min_component_size" %in% names(filter_info)) {
      filter_info <- append(filter_info, list(min_component_size = component_size_threshold))
    }
  }
  
  # Bind the list into a string "name1: value1; name2: value2"
  filter_string <- paste0("Filter (plot): ", paste0(names(filter_info), ": ", unlist(filter_info), collapse = "; "))
  
  
  
  
  ### Step 3: Prepare Data
  
  ### Prepare data
  if(verbose)cli::cli_inform("Preparing post data.")
  
  # Floor the 'time' column per hour and per day
  dt[, time_floored := lubridate::floor_date(time, unit = unit)]
  
  # Calculate the number of observations per community and per floored time
  agg_data <- dt[, .N, by = .(community, time_floored)]
  
  # Step 3: Remove rows with NA in time_floored_hour or N (though likely .N won't produce NA)
  agg_data <- agg_data[!is.na(time_floored) & !is.na(N)]
  
  
  
  
  ### Condition A: Plot by community
  
  if(by_communtiy){
    
    ## Community member size stats
    comm_stats_dt <- unique(dt, by = "account_id")[, .N, by = community]
    
    # Community post stats
    comm_post_stats_dt <- dt[, .(n_comm_posts = .N), by = community]
    
    # Merge community post stats
    agg_data <- dplyr::left_join(agg_data, comm_post_stats_dt)
    
    # Generate label
    agg_data <- agg_data |> dplyr::mutate(community_label = paste0(community, " \n [N=", n_comm_posts, "]")) 
    
    
    ### Use Labels if labels were generated
    if("labelled_communities" %in% names(network_data)) {
      
      if(verbose)cli::cli_inform("Using generated labels.")
      
      # Select community and label_generated columns from network_data
      comm_labels <- network_data$labelled_communities |> dplyr::select(community, label_generated)
      
      # Merge labels with the main agg_data
      agg_data <- dplyr::left_join(agg_data, comm_labels, by = "community")
      
      # Combine label_generated and community number for the legend
      agg_data <- agg_data |> 
        dplyr::mutate(community_label = ifelse(!is.na(label_generated), 
                                               paste0(community, " ",
                                                      label_generated, " [N=", n_comm_posts, "]")) 
        )
    }
    
    
    ### Order community_label factor
    # Define as factor
    agg_data$community_label <- factor(agg_data$community_label)
    
    # Reorder the levels based on the descendin number of posts 
    new_levels <- levels(agg_data$community_label)[order(-comm_post_stats_dt |> dplyr::arrange(as.character(community)) |> magrittr::extract2("n_comm_posts"))]  # Negative sign for descending order
    
    # Set the new levels 
    agg_data$community_label <- factor(agg_data$community_label, levels = new_levels)
    
    
    ### Find indicator for peaking moments per community
    
    # Find examples from peak coordinated activity
    agg_data_max <- agg_data[, .SD[which.max(N)], by = community]
    
    agg_data <- dplyr::left_join(agg_data, agg_data_max |> dplyr::mutate(local_max = 1) |>  dplyr::select(community, time_floored, local_max))
    
    
    if(include_examples){  
      
      
      if(verbose)cli::cli_inform("Sampling examples.")
      
      # Filter max week per community
      dt_local_max <- dt[agg_data_max, on = .(community, time_floored)]
      
      # Step 1: Find the n most frequent account_id in each community
      set.seed(seed)
      most_frequent_accounts <- dt_local_max[, .N, by = .(community, account_id)][
        order(-N), .SD[1:n_random_accounts], by = community]  # Get top n_random_accounts by frequency
      
      # Step 2: Subset filtered_dt by the most frequent account_ids for each community; and keep only content of > 10 characters
      filtered_dt_valid_content <- dt_local_max[most_frequent_accounts, on = .(community, account_id)][nchar(content) > 10]
      
      # Step 4: Randomly pick n_random_rows, ensuring each account is picked only once
      random_rows_per_community <- filtered_dt_valid_content[
        , .SD[sample(.N, 1)], by = .(community, account_id)][  # Ensure one row per account
          , .SD[sample(.N, min(.N, n_random_rows))], by = community]  # Randomly pick up to n_random_rows per community
      
      # Step 1: Truncate "content" to 90 characters
      random_rows_per_community[, content := stringr::str_trunc(content, 100, side = "right") |> stringr::str_squish() |> stringr::str_wrap(width = 60)]
      
      # Step 2: Create a new variable "acc_content" that combines "account_name", "time", and "content"
      random_rows_per_community[, acc_content := paste(paste0("[", account_name, " - ", time, ":]"), content, sep = "\n")]
      
      # Step 3: Aggregate by community, concatenate the 'n_random_rows' examples into a single string separated by newlines
      aggregated_examples <- random_rows_per_community[, .(examples = paste(acc_content, collapse = "\n")), by = community]
      
      # Save example times
      aggregated_examples <- dplyr::left_join(aggregated_examples, agg_data_max[, .(community, time_floored)])
      
      # Bind to agg_data
      agg_data <- dplyr::left_join(agg_data, aggregated_examples) 
      
    }
    
    
    ### Get community size stats as character
    filter_info <- paste0("Community members: min=", min(comm_stats_dt$N, na.rm = T),
                          ", max=", max(comm_stats_dt$N, na.rm = T),
                          ", mean=", round(mean(comm_stats_dt$N, na.rm = T),1),
                          "(", round(stats::sd(comm_stats_dt$N, na.rm = T),1), ").")
    
    # Get unique communities and assign colors
    unique_communities <- unique(dt[dt$account_id %in% igraph::V(g)$name, ]$community)
    community_colors <- viridis::viridis(length(unique_communities), option = palette_option, begin = start_color, end = end_color)
    
    ## Generate title
    n_communities <- comm_stats_dt[, .N]
    title <- paste0(ifelse(!is.null(title_prefix), paste(title_prefix, "Coordinated Posts of Largest n="), "Coordinated Posts of Largest n="), 
                    n_communities, " Communities.")
    
    
    if(verbose)cli::cli_inform("Plotting by community.")
    
    
    # Plot: Create a ggplot geom_bar plot, showing the number of observations per community per hour
    p <- agg_data |> 
      ggplot2::ggplot(ggplot2::aes(x = time_floored, 
                                   y = N, 
                                   fill = factor(community_label))) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
      ggplot2::scale_x_datetime(breaks = "1 day", date_labels = "%b %d") +  # Adjust x-axis breaks and labels
      ggplot2::labs(title = title,
                    subtitle = filter_info,
                    caption = param_string,
                    y = "Number of Coordinated Posts",
                    x = "Time",
                    fill = "Community") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 1, hjust = 1))+
      ggplot2::scale_fill_manual(values = community_colors)
    
    
    
    if("examples" %in% names(agg_data) && include_examples){
      
      
      if(verbose)cli::cli_inform("Adding examples.")
      
      # Add the text with ggrepel::geom_text_repel()
      p <-  p +
        ggrepel::geom_text_repel(data = agg_data |> dplyr::filter(!is.na(local_max)),
                                 ggplot2::aes(x = time_floored, 
                                              y = N, 
                                              label = paste(as.character(community_label),
                                                            examples,
                                                            sep = "\n"),
                                              color = community_label), 
                                 size = 2.5,  # Reduce text size
                                 box.padding = 4,  # Increase padding between text and box
                                 point.padding = 2,  # Add more padding between points and labels
                                 min.segment.length = 0,  # Always show the line
                                 segment.color = community_colors, 
                                 nudge_y = log(agg_data |> dplyr::filter(!is.na(local_max)) |> magrittr::extract2("N"))+10,  # Dynamically move the text higher based on N values
                                 nudge_x = 100,  # Adjust horizontal nudging to move labels away
                                 force = 40,  # Further increase separation force
                                 force_pull = 20,  # Pull labels more away from dense areas
                                 segment.curvature = -0.3,  # Add curvature to the lines
                                 segment.angle = 10,  # Angle of the curve
                                 segment.ncp = 5,  # More control points for a smoother curve
                                 max.overlaps = 20,  # Allow more overlaps for better text separation
                                 hjust = 1,  # Right-align the text at the end of the curve
                                 vjust = 0) +
        ggplot2::scale_color_manual(values = community_colors, guide = "none")# Center-align the text vertically
      
      
      
    }
    
    if(!"examples" %in% names(agg_data)){
      
      if(add_labels){
        
        
        if(verbose)cli::cli_inform("Adding labels.")
        
        p <- p +
          ggrepel::geom_text_repel(data = agg_data |> dplyr::filter(!is.na(local_max)),
                                   ggplot2::aes(x = time_floored, 
                                                y = N, 
                                                label = community_label,
                                                color = community_label
                                   ), 
                                   size = 4,  # Reduce text size
                                   box.padding = 1,  # Increase padding between text and box
                                   point.padding = .5,  # Add more padding between points and labels
                                   min.segment.length = 0,  # Always show the line
                                   # segment.color = "black",  
                                   nudge_y = log(agg_data |> dplyr::filter(!is.na(local_max)) |> magrittr::extract2("N"))+10,  # Dynamically move the text higher based on N values
                                   nudge_x = 10,  # Adjust horizontal nudging to move labels away
                                   force = 20,  # Further increase separation force
                                   force_pull = 10,  # Pull labels more away from dense areas
                                   segment.curvature = -0.3,  # Add curvature to the lines
                                   segment.angle = 10,  # Angle of the curve
                                   segment.ncp = 5,  # More control points for a smoother curve
                                   max.overlaps = 20,  # Allow more overlaps for better text separation
                                   hjust = 1,  # Right-align the text at the end of the curve
                                   vjust = .5, 
                                   ylim = c(max(agg_data$N), min(agg_data$N)),
                                   xlim = c(min(agg_data$time_floored), max(agg_data$time_floored))
          )+
          ggplot2::scale_color_manual(values = community_colors, guide = "none")
        
        
      }
      
    }
    
    
    if(!legend) p <- p + ggplot2::theme(legend.position = "none")
    
    
  }else{
    
    ## Community member size stats
    comm_stats_dt <- unique(dt, by = "account_id")[, .N, by = community]
    
    ### Get community size stats as character
    member_info <- paste0("Community members: min=", min(comm_stats_dt$N, na.rm = T),
                          ", max=", max(comm_stats_dt$N, na.rm = T),
                          ", mean=", round(mean(comm_stats_dt$N, na.rm = T),1),
                          "(", round(stats::sd(comm_stats_dt$N, na.rm = T),1), ").")
    
    
    p <- agg_data |> 
      ggplot2::ggplot(ggplot2::aes(x = time_floored, 
                                   y = N)) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
      ggplot2::scale_x_datetime(breaks = "1 day", date_labels = "%b %d") +  # Adjust x-axis breaks and labels
      ggplot2::labs(title = paste0(paste(title_prefix, "Coordinated Posts Overall.")),
                    subtitle = member_info,
                    caption = param_string,
                    y = "Number of Coordinated Posts",
                    x = "Time") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 1, hjust = 1))
    
  }
  
  return(p)
  
}




 
# Old version: 
#
# plot_coordinated_posts <- function(network_data,
#                                    unit = "hour",
#                                    n_communities = NULL,
#                                    quantile = NULL,
#                                    param_info = NULL,
#                                    verbose = TRUE,
#                                    legend = TRUE,
#                                    title_prefix = NULL
# ) {
#   
#   if(!"post_data" %in% names(network_data)){
#     stop("No 'post_data' provided in 'netword_data'. Unable to plot.")
#   }
#   
#   # Create copy of dt
#   dt <- data.table::copy(network_data$post_data)
#   
#   # Extract parameter info
#   param_info <- paste(paste0("Parameters: ", param_info), 
#                       paste0("COMM_ALG: ", network_data$node_list$algorithm[[1]]),
#                       network_data$node_list$parameters[[1]], sep = "; "
#   )
#   filter_info <- NULL
#   
#   
#   # Step 1: 
#   if(!is.null(quantile)){
#     
#     # Calculate sizes of communities
#     comm_size <- dt[, .(N = data.table::uniqueN(account_id)), by = community]
#     
#     keep_comm <- filter_ntile(comm_size, var_field = "N", probs = quantile)
#     
#     # How many are dropped
#     orig_n <- comm_size[, .N]
#     keep_n <- keep_comm[, .N]
#     drop_n <- orig_n - keep_n
#     
#     # Subset
#     dt <- dt[community %in% keep_comm$community, ]
#     
#     # Set n communities if null
#     n_communities = keep_n
#     
#     pct <- paste0((1 - quantile)*100, "%")
#     
#     if(verbose)cli::cli_inform("Keeping top {pct}; n={keep_n} of lagrest communities (N members[{min(keep_comm$N)}, {max(keep_comm$N)}]).
#                                Dropped n={drop_n} communities.")
#     
#     # Save filter info
#     filter_info <- paste0("Largest ", pct, " of communities; n=", 
#                           keep_n, 
#                           "; Members: min=", min(keep_comm$N, na.rm = T),
#                           ", max=", max(keep_comm$N, na.rm = T),
#                           ", mean=", round(mean(keep_comm$N, na.rm = T),1),
#                           "(", round(stats::sd(keep_comm$N, na.rm = T),1), ")."
#     )
#     
#   }
#   
#   if(verbose)cli::cli_inform("Preparing post data.")
#   
#   # Step 1: Floor the 'time' column per hour and per day
#   dt[, time_floored := lubridate::floor_date(time, unit = unit)]
#   
#   # Step 2: Calculate the number of observations per community and per floored time
#   agg_data <- dt[, .N, by = .(community, time_floored)]
#   
#   # Step 3: Remove rows with NA in time_floored_hour or N (though likely .N won't produce NA)
#   agg_data <- agg_data[!is.na(time_floored) & !is.na(N)]
#   
#   
#   if(!is.null(n_communities)){
#     
#     # Step 1: Calculate the sum of N by community
#     comm_size <- agg_data[, .(N = sum(N)), by = community]
#     
#     # Step 2: Get the top communities by total N
#     keep_comm_i <- comm_size[order(-N)][1:n_communities, community]
#     
#     # Step 3: Filter agg_data to include only rows from the top communities
#     keep_comm <- agg_data[community %in% keep_comm_i]
#     
#     
#     # Store filter info
#     if(is.null(quantile)){
#       
#       filter_info <- paste0("Members: min=", min(keep_comm$N, na.rm = T),
#                             ", max=", max(keep_comm$N, na.rm = T),
#                             ", mean=", round(mean(keep_comm$N, na.rm = T),1),
#                             "(", round(stats::sd(keep_comm$N, na.rm = T),1), ")."
#       )
#     }
#     
#     
#     # Step 6: Create a ggplot geom_bar plot, showing the number of observations per community per hour
#     p <- keep_comm |> 
#       ggplot2::ggplot(ggplot2::aes(x = time_floored, 
#                                    y = N, 
#                                    fill = factor(community))) +
#       ggplot2::geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
#       ggplot2::scale_x_datetime(breaks = "1 day", date_labels = "%b %d") +  # Adjust x-axis breaks and labels
#       ggplot2::labs(title = paste0(paste(title_prefix, "Coordinated Posts of Largest n="), 
#                                    n_communities, 
#                                    " Communities."),
#                     subtitle = filter_info,
#                     caption = param_info,
#                     y = "Number of Coordinated Posts",
#                     x = "Time",
#                     fill = "Community") +
#       ggplot2::theme_minimal() +
#       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 1, hjust = 1))
#     
#     if(!legend) p <- p + ggplot2::theme(legend.position = "none")
#     
#   }else{
#     
#     member_info <- paste0("Members: min=", min(keep_comm$N, na.rm = T),
#                           ", max=", max(keep_comm$N, na.rm = T),
#                           ", mean=", round(mean(keep_comm$N, na.rm = T),1),
#                           "(", round(stats::sd(keep_comm$N, na.rm = T),1), ")."
#     )
#     
#     p <- agg_data |> 
#       ggplot2::ggplot(ggplot2::aes(x = time_floored, 
#                                    y = N)) +
#       ggplot2::geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
#       ggplot2::scale_x_datetime(breaks = "1 day", date_labels = "%b %d") +  # Adjust x-axis breaks and labels
#       ggplot2::labs(title = paste0(paste(title_prefix, "Coordinated Posts Overall.")),
#                     subtitle = member_info,
#                     caption = param_info,
#                     y = "Number of Coordinated Posts",
#                     x = "Time") +
#       ggplot2::theme_minimal() +
#       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 1, hjust = 1))
#     
#     
#   }
#   
#   return(p)  
# }
# 
# 




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
