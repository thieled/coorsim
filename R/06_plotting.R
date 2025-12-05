#' Plot Coordinated Posts Over Time
#'
#' Creates line plots showing coordinated posts over time. The function can
#' either plot all communities together (colored lines) or produce individual
#' community-level time series arranged in a grid.
#'
#' @param network_data A list containing:
#'   - post_data: data.table or data.frame with columns account_id, community, time
#'   - node_list: community membership table (account_id, community)
#'   - params, filter: optional lists for caption metadata
#'   - community_labels: optional data.frame with columns community, label
#' @param by_community Logical. If TRUE, plot one panel per community; if FALSE
#'   plot all communities together in one plot.
#' @param unit Character time unit passed to lubridate::floor_date().
#' @param palette_option Viridis palette option ("A","B","C","D").
#' @param start_color,end_color Numeric range for viridis palette.
#' @param use_palette Optional custom palette function(n).
#' @param title_prefix Optional prefix for plot title.
#' @param ncol Number of columns for per-community layout.
#' @param include_examples,n_examples,trunc Reserved for future use.
#' @param label_fontsize Base font size.
#' @param legend Logical. Show legend when by_community = FALSE.
#' @param seed Random seed (not used yet).
#' @param verbose Print progress messages.
#'
#' @return A ggplot or patchwork object.
#' @export
plot_posts <- function(network_data,
                       by_community = TRUE,
                       unit = "6 hours",
                       palette_option = "A",
                       start_color = 0,
                       end_color = 1,
                       use_palette = NULL,
                       title_prefix = NULL,
                       ncol = 2,
                       include_examples = FALSE,
                       n_examples = 2,
                       trunc = 100,
                       label_fontsize = NULL,
                       legend = TRUE,
                       seed = 42,
                       verbose = TRUE) {
  
  for (pkg in c("data.table","ggplot2","viridis","patchwork","lubridate","stringr","cli")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Package '", pkg, "' is required.")
  }
  
  dt <- data.table::as.data.table(network_data$post_data)
  node_list <- data.table::as.data.table(network_data$node_list)
  
  if (!all(c("community","time") %in% names(dt)))
    stop("post_data must contain 'community' and 'time'.")
  
  # Caption builder (same style as plot_communities)
  ucfirst <- function(x) paste0(toupper(substr(x,1,1)), substr(x,2,nchar(x)))
  get_cap <- function(label) {
    if (!label %in% names(network_data)) return("")
    x <- network_data[[label]]
    x <- x[!vapply(x, function(v) is.null(v) || all(is.na(v)), logical(1L))]
    if (length(x)==0L) return("")
    vals <- mapply(function(nm,v)
      paste(nm, if(is.numeric(v)) paste(round(v,2),collapse=", ") else paste(v,collapse=", "), sep=": "),
      nm = names(x), v = x)
    paste0(ucfirst(label), ": ", paste(vals, collapse="; "), ".")
  }
  
  caption <- paste(
    stringr::str_wrap(get_cap("params"), 200),
    stringr::str_wrap(get_cap("filter"), 200),
    sep = "\n"
  )
  
  title <- if (!is.null(title_prefix)) paste(title_prefix, "Coordinated posts over time") else "Coordinated posts over time"
  
  # Communities
  dt[, community := as.character(community)]
  unique_comms <- sort(unique(dt$community))
  n_colors <- length(unique_comms)
  
  # Palette
  if (is.null(use_palette)) {
    cols <- viridis::viridis(
      n_colors,
      option = palette_option,
      begin = start_color,
      end   = end_color
    )
  } else if (is.function(use_palette)) {
    # palette function, e.g., ggsci::pal_d3("category20")
    cols <- use_palette(n_colors)
    if (length(cols) < n_colors) stop("Custom palette too short.")
  } else if (is.character(use_palette)) {
    # vector of hex colors
    if (length(use_palette) < n_colors)
      stop("Color vector in 'use_palette' is shorter than number of communities.")
    cols <- use_palette[seq_len(n_colors)]
  } else {
    stop("'use_palette' must be NULL, a palette function, or a character vector of colors.")
  }
  
  # assign names following the sorted community order
  names(cols) <- unique_comms
  
  base_size <- if (is.null(label_fontsize)) 11 else label_fontsize
  
  # Time aggregation
  if (verbose) cli::cli_inform("Aggregating posts.")
  dt[, time_floored := lubridate::floor_date(lubridate::as_datetime(time), unit)]
  agg <- dt[, .N, by = .(community, time_floored)]
  
  # Community stats
  comm_post_stats <- dt[, .(n_comm_posts = .N), by = community]
  agg <- merge(agg, comm_post_stats, by="community", all.x=TRUE)
  
  # Add default labels
  agg[, community_label := paste0(community, " [n_posts=", n_comm_posts, "]")]
  
  # Insert generated labels
  if ("community_labels" %in% names(network_data)) {
    labs <- data.table::as.data.table(network_data$community_labels)
    labs[, community := as.character(community)]
    agg <- merge(agg, labs[,.(community,label)], by="community", all.x=TRUE)
    agg[!is.na(label), community_label := paste0("No. ", community, ": ", label, " [n_posts=", n_comm_posts, "]")]
  }
  
  # Order labels
  data.table::setorder(agg, -n_comm_posts, community)
  agg[, community_label := factor(community_label, levels = unique(community_label))]
  
  # Combined plot
  if (!by_community) {
    p <- ggplot2::ggplot(agg, ggplot2::aes(x = time_floored, y = N,
                                           color = community, group = community)) +
      ggplot2::geom_line(linewidth=0.8, alpha=0.9) +
      ggplot2::scale_color_manual(values = cols) +
      ggplot2::scale_x_datetime(breaks = "1 day", date_labels = "%b %d") +
      ggplot2::labs(title=title, caption=caption, x="Time", y="Number of coordinated posts") +
      ggplot2::theme_minimal(base_size=base_size) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle=30,hjust=1),
                     plot.caption = ggplot2::element_text(hjust=0))
    
    if (!legend) p <- p + ggplot2::theme(legend.position="none")
    
  }else{
    
    # Per-community panels
    if (verbose) cli::cli_inform("Plotting individual communities.")
    
    # Fill missing time bins
    all_times <- sort(unique(agg$time_floored))
    grid <- data.table::CJ(community = unique_comms, time_floored = all_times, unique=TRUE)
    agg2 <- merge(grid, agg, by=c("community","time_floored"), all.x=TRUE)
    agg2[is.na(N), N := 0L]
    
    plots <- lapply(unique_comms, function(comm){
      sub <- agg2[community == comm]
      ggplot2::ggplot(sub, ggplot2::aes(x=time_floored, y=N)) +
        ggplot2::geom_line(linewidth=0.9, color=cols[[comm]]) +
        ggplot2::scale_x_datetime(breaks="1 day", date_labels="%b %d") +
        ggplot2::labs(title=paste0("Community ", comm),
                      x="Time", y="Number of coordinated posts") +
        ggplot2::theme_minimal(base_size=base_size) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=30, hjust=1))
    })
    
    p <- patchwork::wrap_plots(plots, ncol=ncol) +
      patchwork::plot_annotation(
        title = title,
        caption = caption,
        theme = ggplot2::theme(
          plot.title   = ggplot2::element_text(size=14,face="bold",hjust=0),
          plot.caption = ggplot2::element_text(size=9,hjust=0)
        )
      )
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
  required_pkgs <- c("ggraph", "patchwork", "ggplot2", "igraph", "ggforce", "tidygraph", "ggtext", "scales", "viridis")
  
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
    
    # remove NULL or entirely NA entries
    x <- x[!sapply(x, function(v) is.null(v) || all(is.na(v)))]
    
    # format each value (handle vectors too)
    formatted <- mapply(function(nm, val) {
      if (is.numeric(val)) {
        paste(format(round(val, 2), trim = TRUE), collapse = ", ")
      } else {
        paste(as.character(val), collapse = ", ")
      }
    }, nm = names(x), val = x, USE.NAMES = FALSE)
    
    paste0(ucfirst(label), ": ", paste(names(x), formatted, sep = ": ", collapse = "; "), ".")
  }
  
  param_string  <- get_caption(network_data, "params")
  filter_string <- get_caption(network_data, "filter")
  
  if (!is.null(network_data$params$cluster_method) && network_data$params$cluster_method != "FSA_V") network_data$params$theta <- NULL
  
  # Prepare color palette
  unique_communities <- sort(unique(dplyr::filter(node_list, account_id %in% igraph::V(g)$name)$community))
  n_colors <- length(unique_communities)
  
  if (is.null(use_palette)) {
    
    # default viridis palette
    community_colors <- viridis::viridis(
      n_colors,
      option = palette_option,
      begin = start_color,
      end   = end_color
    )
    
  } else if (is.function(use_palette)) {
    
    # palette function, e.g. ggsci::pal_d3("category20")
    community_colors <- use_palette(n_colors)
    if (length(community_colors) < n_colors)
      stop("Palette function returned too few colors.")
    
  } else if (is.character(use_palette)) {
    
    # user-supplied hex color vector
    if (length(use_palette) < n_colors)
      stop("'use_palette' is a color vector but shorter than number of communities.")
    community_colors <- use_palette[seq_len(n_colors)]
    
  } else {
    stop("'use_palette' must be NULL, a function, or a character vector of colors.")
  }
  
  
  
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
    sim_dt <- data.table::copy(network_data$sim_dt)
    post_count <- unique(data.table::rbindlist(list(
      sim_dt[, .(post_id, account_id)],
      sim_dt[, .(post_id = post_id_y, account_id = account_id_y)]
    )))[, .N, by = account_id][, `:=` (n_posts = N, N = NULL)]
    
    comm_df <- unique(merge(post_count, node_list_s, by = "account_id", all.x = TRUE))[
      , .(n_accs = data.table::uniqueN(account_id), n_posts = sum(n_posts)), by = community
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



