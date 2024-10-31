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
  # g <- igraph::delete_vertices(g, which(igraph::degree(g) <= 1))
  
  # Simplify graph
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  
  
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






#' Plot communities in a network graph
#'
#' This function generates a plot of communities in a network graph using a stress layout. The graph
#' is filtered based on component size and edge weight thresholds. Node colors and soft community boundaries 
#' are assigned based on community membership, and node labels for the top nodes by degree are displayed.
#'
#' @param network_data A list containing the graph object (`$graph`) and node list (`$node_list`). Optionally, it can include 
#' labelled communities (`$labelled_communities`), parameter list (`$param_list`), and filter information (`$filter_info`).
#' @param edge_weight_threshold Numeric. If provided, edges with a weight below this threshold will be removed. Defaults to `NULL`.
#' @param component_size_threshold Numeric. If provided, components smaller than this threshold will be removed. Defaults to `NULL`.
#' @param palette_option Character. Specifies the Viridis palette option to use. Defaults to "A".
#' @param end_color Numeric. The end point for the color gradient (between 0 and 1). Defaults to 1.
#' @param start_color Numeric. The start point for the color gradient (between 0 and 1). Defaults to 0.
#' @param title_prefix Character. A prefix to be added to the title of the plot. Defaults to `NULL`.
#' @param n_top_nodes Numeric. The number of top nodes by degree to display labels for within each community. Defaults to 10.
#' @param label.fontsize Numeric vector. First value provides font size of label, second for description.
#'
#' @return A `ggplot` object representing the plotted network graph with communities, node labels, and boundary highlights.
#' 
#' @export
plot_communities <- function(network_data, 
                             edge_weight_threshold = NULL, 
                             component_size_threshold = NULL, 
                             palette_option = "A", 
                             end_color = 1,
                             start_color = 0,
                             title_prefix = NULL,
                             n_top_nodes = 10,
                             label.fontsize = c(8,6)) {
  
  # Extract the graph and communities from the network object
  g <- network_data$graph
  node_list <- network_data$node_list
  
  # Remove nodes with degree <= 1
  # g <- igraph::delete_vertices(g, which(igraph::degree(g) <= 1))
  
  # Simplify
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  
  
  # Keep only components with size >= community_size_threshold
  if(!is.null(component_size_threshold)){
    g <- igraph::induced_subgraph(g, 
                                  igraph::V(g)[igraph::components(g)$membership %in% 
                                                 which(igraph::components(g)$csize >= component_size_threshold)])
  }
  
  # Filter edges based on weight
  if(!is.null(edge_weight_threshold)){
    g <- igraph::subgraph.edges(g, igraph::E(g)[igraph::E(g)$weight > edge_weight_threshold])
  }
  
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
  
  # Add edge weight filter
  if (!is.null(edge_weight_threshold)) {
    if ("edge_weight_threshold" %in% names(filter_info)) {
      filter_info[names(filter_info) == "edge_weight_plot"] <- edge_weight_threshold
    }
    if (!"edge_weight_threshold" %in% names(filter_info)) {
      filter_info <- append(filter_info, list(edge_weight_plot = edge_weight_threshold))
    }
  }
  
  # Bind the list into a string "name1: value1; name2: value2"
  filter_string <- paste0("Filter (plot): ", paste0(names(filter_info), ": ", unlist(filter_info), collapse = "; "))
  
  
  # Get unique communities and assign colors
  unique_communities <- unique(node_list[node_list$account_id %in% igraph::V(g)$name, ]$community)
  community_colors <- viridis::viridis(length(unique_communities), option = palette_option, end = end_color, begin = start_color)
  
  # Calculate size attribute
  igraph::V(g)$degree <- igraph::degree(g, mode = "all")
  
  
  ### Create labels
  
  ## Community member size stats
  comm_stats_dt <- node_list[node_list$account_id %in% igraph::V(g)$name, ][, .N, by = community]
  
  # Add community labels if available
  if ("labelled_communities" %in% names(network_data)) {
    comm_df <- network_data$labelled_communities |>
      dplyr::select(community, label = label_generated, desc = description_generated)
    
    comm_df <- dplyr::left_join(comm_stats_dt, comm_df) |> 
      dplyr::mutate(community_label = paste0(community, " ", label, " [N=", N, "]")) 
    
  }else{
    # Generate label & empty desc
    comm_df <- comm_stats_dt |> dplyr::mutate(community_label = paste0(community, " [N=", N, "]")) 
    comm_df$desc = " "
  }
  
  # Convert graph to tidygraph format and join community information
  tidy_g <- g |> 
    tidygraph::as_tbl_graph() |> 
    tidygraph::activate(nodes) |> 
    dplyr::left_join(node_list, by = c("name" = "account_id"))  # Ensure you have account_id matching with name
  
  # Create a data frame with account names, degrees, and community memberships
  node_degree_df <- tidy_g |> 
    tidygraph::as_tibble() |>  # Convert to tibble
    dplyr::select(account_name, degree, community)  # Use account_name instead of V(g)$name
  
  # Group by community, sort by degree in descending order, and select the top 10 nodes in each community
  top_nodes <- node_degree_df |> 
    dplyr::group_by(community) |> 
    dplyr::arrange(community, desc(degree)) |> 
    dplyr::slice_max(degree, n = n_top_nodes, with_ties = FALSE) |>  # Select top nodes by degree
    dplyr::ungroup()
  
  # Compute the layout with ggraph
  layout <- ggraph::create_layout(tidy_g, layout = "stress", bbox = 40)
  
  # Make sure the community column is present in layout
  layout <- layout |> dplyr::left_join(comm_df, by = "community")  # Join the labels for each community
  
  # Create title
  title <- paste0(ifelse(!is.null(title_prefix), paste(title_prefix, "Coordinated top communities of size >= "), "Coordinated top communities of size >= "), 
                  component_size_threshold)
  
  # Plot the graph with ggraph
  p <- ggraph::ggraph(layout) +
    ggraph::geom_edge_link(ggplot2::aes(edge_linewidth = weight), color = "grey", show.legend = FALSE) + 
    ggraph::geom_node_point(ggplot2::aes(color = as.factor(community), size = degree), show.legend = FALSE) +
    # Draw soft boundary around each community
    ggforce::geom_mark_hull(
      ggplot2::aes(x = x, 
                   y = y, 
                   group = as.factor(community), 
                   label = community_label, 
                   description = desc, 
                   fill = as.factor(community),
                   color = as.factor(community)),
      concavity = 2,
      alpha = 0.07,
      label.fontsize = label.fontsize) +
    # Add node labels for top 10 nodes per community
    ggraph::geom_node_text(
      data = layout |> dplyr::filter(account_name %in% top_nodes$account_name),  # Filter for top 10 nodes
      ggplot2::aes(label = account_name,  size = log(degree)*10),  # Use account_name as label  # Adjust size if needed
      repel = TRUE  # Avoid label overlap
    ) +
    ggplot2::scale_color_manual(values = community_colors) +
    ggplot2::scale_fill_manual(values = community_colors, guide = "none") +
    ggplot2::scale_alpha_continuous(range = c(0.1, 0.9)) +
    ggraph::scale_edge_width(range = c(0.5, 5)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = title,
                  subtitle = paste(param_string, filter_string),
                  color = "Community") 
  
  
  
  return(p)
}


