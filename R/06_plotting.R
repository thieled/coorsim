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
#' @import data.table
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








#' Plot Coordinated Communities in a Network
#'
#' Visualizes communities of coordinated accounts within a network graph, either as a single overview plot
#' or as a grid of community-level plots with metadata side panels. Nodes represent accounts, edges reflect
#' similarity or interaction, and layout and annotations emphasize coordination patterns.
#'
#' @param network_data A named list containing at least a simplified graph object (`network_data$graph`, as an `igraph`) 
#'   and a node table (`network_data$node_list`, coercible to a tibble). Optionally includes:
#'   - `params`: named list of parameters (e.g., cluster method, thresholds)
#'   - `filter`: optional filter metadata
#'   - `community_labels`: a data frame with `community`, `label`, `description`, `n_accs`, and `n_posts`
#'
#' @param palette_option Character. Viridis palette option (e.g., "A", "B", "C", "D"). Passed to `viridis::viridis()`.
#' @param end_color Numeric. End value for viridis color scale (between 0 and 1). Default is `1`.
#' @param start_color Numeric. Start value for viridis color scale (between 0 and 1). Default is `0`.
#' @param title_prefix Character. Optional prefix for the plot title. If `NULL`, a default title is used.
#' @param n_top_nodes Integer. Number of top-degree nodes per community to label. Default is `10`.
#' @param label_fontsize Integer. Base font size for text panels. If `NULL`, size is automatically scaled to `ncol`.
#' @param by_community Logical. If `TRUE`, create individual plots per community with metadata side panels. 
#'   If `FALSE`, show a single integrated graph. Default is `TRUE`.
#' @param ncol Integer. Number of columns in the grid layout when `by_community = TRUE`. Default is `2`.
#' @param expand_hairballs Numeric. Factor to visually expand densely connected communities 
#'   ("hairballs") to avoid overlap. Default is `NULL`.
#' @param use_palette Function. User-defined palette from a function, e.g. `ggsci::pal_aaas()`. Overrides the default viridis palette. Default is `NULL`.
#'
#' @return A `ggplot` or `patchwork` object showing the coordinated communities, ready for rendering or saving.
#'
#' @details 
#' Communities are plotted using a `stress` layout and optionally separated into grid plots with 
#' accompanying text summaries. Top-degree nodes within each community are labeled. If metadata 
#' (`community_labels`) is available, it will be shown alongside each subgraph.
#'
#' Dense subgraphs (high edge density and size) can be expanded for visual clarity using the 
#' `expand_hairballs` argument.
#' 
#' @export
plot_communities <- function(network_data, 
                             palette_option = "A", 
                             end_color = 1,
                             start_color = 0,
                             title_prefix = NULL,
                             n_top_nodes = 10,
                             label_fontsize = NULL,
                             by_community = TRUE,
                             ncol = 2,
                             expand_hairballs = NULL,
                             use_palette = NULL) {
  
  # Require packages
  required_pkgs <- c("ggraph", "patchwork", "ggplot2", "igraph", "ggforce", "tidygraph", "ggtext", "scales")
  
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required for this function. Please install it using install.packages(\"", pkg, "\")."), call. = FALSE)
    }
  }
  
  
  # Simplify graph and convert node list to tibble
  g <- igraph::simplify(network_data$graph, remove.multiple = TRUE, remove.loops = TRUE)
  node_list <- dplyr::as_tibble(network_data$node_list)
  
  ucfirst <- function(x) {
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  }
  # Extract parameters and filter info
  get_caption <- function(x, label) {
    if (!label %in% names(network_data)) return("")
    x <- network_data[[label]]
    x <- x[!sapply(x, function(v) is.null(v) || is.na(v))]
    formatted <- mapply(function(nm, val) {
      if (is.numeric(val)) {
        format(round(val, 2), trim = TRUE)
      } else {
        as.character(val)
      }
    }, nm = names(x), val = x, USE.NAMES = FALSE)
    paste0(ucfirst(label), ": ", paste(names(x), formatted, sep = ": ", collapse = "; "), ".")
  }
  
  param_string  <- get_caption(network_data, "params")
  filter_string <- get_caption(network_data, "filter")
  
  if (!is.null(network_data$params$cluster_method) && network_data$params$cluster_method != "FSA_V") network_data$params$theta <- NULL
  
  # Prepare color palette
  unique_communities <- unique(dplyr::filter(node_list, account_id %in% igraph::V(g)$name)$community)
  n_colors <- length(unique_communities)
  
  community_colors <- if (is.null(use_palette)) {
    viridis::viridis(n_colors, option = palette_option, begin = start_color, end = end_color)
  } else {
    if (!is.function(use_palette)) stop("'use_palette' must be a function (e.g., ggsci::pal_d3('category20'))")
    cols <- use_palette(n_colors)
    if (length(cols) < n_colors) stop(sprintf("Palette has only %d colors but %d are required.", length(cols), n_colors))
    cols
  }
  names(community_colors) <- unique_communities
  
  
  
  # Font sizes
  if (is.null(ncol)) ncol <- 1
  if (is.null(label_fontsize)) label_fontsize <- round(max(6, 12 - length(unique_communities) / 12 - (ncol - 1) * 0.5), 0)
  label.fontsize <- c(label_fontsize, label_fontsize - 2)
  
  # Title
  title <- if (!is.null(title_prefix)) paste(title_prefix, "Coordinated communities") else "Coordinated communities"
  
  # Add degree
  igraph::V(g)$degree <- igraph::degree(g, mode = "all")
  
  # Create tidygraph object and join community info
  tidy_g <- g |>
    tidygraph::as_tbl_graph() |>
    tidygraph::activate(nodes)
  
  tidy_g <- tidy_g |>
    dplyr::left_join(
      node_list |> dplyr::select(-dplyr::any_of(names(tidygraph::as_tibble(tidy_g)))),
      by = c("name" = "account_id")
    )
  
  # Get top nodes per community
  top_nodes <- tidygraph::as_tibble(tidy_g) |>
    dplyr::select(account_name, degree, community) |>
    dplyr::group_by(community) |>
    dplyr::arrange(dplyr::desc(degree)) |>
    dplyr::slice_head(n = n_top_nodes) |>
    dplyr::ungroup()
  
  
  # Community labels
  if ("community_labels" %in% names(network_data)) {
    
    comm_df <- network_data$community_labels |>
      dplyr::select(community, label, description, n_accs, n_posts) |>
      dplyr::mutate(community_label = paste0("[No.", community, "]: ", label))
  } else {
    
    node_list_s <- node_list |>
      dplyr::filter(account_id %in% igraph::V(g)$name) |> 
      dplyr::select(account_id, community)
    
    # Calculate n_posts from sim_dt
    post_count <- unique(data.table::rbindlist(list(
      sim_dt[, .(post_id, account_id)],
      sim_dt[, .(post_id = post_id_y, account_id = account_id_y)]
    )))[, .N, by = account_id][, `:=` (n_posts = N, N = NULL)]
    
    comm_df <- unique(merge(post_count, node_list_s, by = "account_id", all.x = TRUE))[
      , .(n_accs = uniqueN(account_id), n_posts = sum(n_posts)), by = community
    ][
      , `:=` (community_label = paste0(community, " [N=", n_accs, "]"), description = " ")
    ] |> as.data.frame()
    
  }
  
  # Layout with ggraph
  layout <- ggraph::create_layout(tidy_g, layout = "stress") |>
    dplyr::select(-dplyr::any_of("N")) |>
    dplyr::mutate(community = as.character(community)) |>
    dplyr::left_join(comm_df |> dplyr::mutate(community = as.character(community)), by = "community")
  
  # Expand hairball communities if enabled
  if (!is.null(expand_hairballs)) {
    hairball_comms <- tidygraph::as_tibble(tidy_g) |>
      dplyr::select(name, community) |>
      dplyr::inner_join(igraph::as_data_frame(g, what = "edges") |> 
                          dplyr::count(from, name = "edge_count"), by = c("name" = "from")) |>
      dplyr::group_by(community) |>
      dplyr::summarise(size = dplyr::n(), edges = sum(edge_count, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(density = edges / pmax(1, size * (size - 1))) |>
      dplyr::filter(density > 0.2, size > 20) |>
      dplyr::pull(community)
    
    layout <- layout |> dplyr::mutate(
      x = ifelse(community %in% hairball_comms, x * expand_hairballs, x),
      y = ifelse(community %in% hairball_comms, y * expand_hairballs, y)
    )
  }
  
  
  
  if(by_community == F){
    
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
                     description = paste0("[n_acc=", n_accs, " | n_post=", n_posts, "] \n", description),
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
      ggplot2::theme(legend.position = 'none',
                     plot.caption = ggplot2::element_text(hjust = 0)) +
      ggplot2::labs(title = title,
                    caption = paste0(stringr::str_wrap(param_string, width = 200, whitespace_only = T),
                                     "\n", stringr::str_wrap(filter_string, width = 200, whitespace_only = T)),
                    color = "Community")
    
    
  }else{
    
    plots <- list()
    
    for (c in unique_communities) {
      
      sub_layout <- layout |> dplyr::filter(community == c)
      
      # Extract edges that connect nodes within this community
      edges_in_community <- igraph::as_data_frame(g, what = "edges") |>
        dplyr::filter(from %in% sub_layout$name & to %in% sub_layout$name)
      
      g_plot <- ggraph::ggraph(
        graph = tidygraph::tbl_graph(nodes = sub_layout, edges = edges_in_community),
        layout = "manual", x = x, y = y, circular = FALSE  
      ) +
        ggraph::geom_edge_link(
          ggplot2::aes(edge_width = sqrt(weight) * 0.5),
          color = "grey", show.legend = TRUE
        ) +
        ggraph::geom_node_point(ggplot2::aes(color = as.factor(community), 
                                             size = sqrt(degree) * 0.5), show.legend = FALSE
        ) +
        ggforce::geom_mark_hull(ggplot2::aes(x = x, 
                                             y = y, 
                                             group = as.factor(community), 
                                             fill = as.factor(community), 
                                             color = as.factor(community)),
                                concavity = 1, 
                                alpha = 0.05, 
                                radius = grid::unit(2, "pt")) +
        ggraph::geom_node_text(
          data = sub_layout |> dplyr::filter(account_name %in% top_nodes$account_name),  # Filter for top 10 nodes
          ggplot2::aes(label = account_name,  
                       size = sqrt(degree) * 0.5), 
          check_overlap = T,  # Use account_name as label  # Adjust size if needed
          repel = TRUE, 
          show.legend = FALSE  # Avoid label overlap
        )  +
        ggplot2::scale_color_manual(values = community_colors) +
        ggplot2::scale_fill_manual(values = community_colors, guide = "none") +
        ggplot2::scale_alpha_continuous(range = c(0.1, 0.9)) +
        ggraph::scale_edge_width(range = c(0.5, 5)) +
        ggplot2::theme_void( )+
        ggplot2::theme(legend.position = 'none') + 
        ggplot2::coord_cartesian(expand = TRUE)
      
      
      
      meta <- comm_df |> dplyr::filter(community == c) ## <<< filter
      
      wrapped_text <- paste0(
        "**[No. ", meta$community, "]: ", meta$label, "**\n\n",
        meta$description, "\n\n",
        "*n_accs*: ", meta$n_accs, "   |   *n_posts*: ", meta$n_posts
      )
      
      text_plot <- ggplot2::ggplot() +
        ggtext::geom_textbox(
          data = data.frame(x = 0, y = 1, label = wrapped_text),
          ggplot2::aes(x = x, y = y, label = label),
          box.color = NA, fill = NA,
          hjust = 0, vjust = 1,
          size = label_fontsize / ggplot2::.pt,
          lineheight = 1.0,
          width = grid::unit(.95, "npc"),
          halign = 0
        ) +
        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
        ggplot2::theme_void()
      
      plots[[length(plots) + 1]] <- patchwork::wrap_plots(
        g_plot, text_plot,
        ncol = 2, widths = c(2, 1.5)
      )
      
    }
    
    p <- patchwork::wrap_plots(plots, ncol = ncol) +
      patchwork::plot_annotation(
        title = title,
        caption = paste0(stringr::str_wrap(param_string, width = 200, whitespace_only = T),
                         "\n", stringr::str_wrap(filter_string, width = 200, whitespace_only = T)),
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0),
          plot.caption = ggplot2::element_text(size = 9, hjust = 0)
        )
      )
    
  }
  
  return(p)
}



