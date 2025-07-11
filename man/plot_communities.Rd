% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/06_plotting.R
\name{plot_communities}
\alias{plot_communities}
\title{Plot Coordinated Communities in a Network}
\usage{
plot_communities(
  network_data,
  palette_option = "A",
  end_color = 1,
  start_color = 0,
  title_prefix = NULL,
  n_top_nodes = 10,
  label_fontsize = NULL,
  by_community = TRUE,
  ncol = 2,
  expand_hairballs = NULL,
  use_palette = NULL
)
}
\arguments{
\item{network_data}{A named list containing at least a simplified graph object (`network_data$graph`, as an `igraph`) 
and a node table (`network_data$node_list`, coercible to a tibble). Optionally includes:
- `params`: named list of parameters (e.g., cluster method, thresholds)
- `filter`: optional filter metadata
- `community_labels`: a data frame with `community`, `label`, `description`, `n_accs`, and `n_posts`}

\item{palette_option}{Character. Viridis palette option (e.g., "A", "B", "C", "D"). Passed to `viridis::viridis()`.}

\item{end_color}{Numeric. End value for viridis color scale (between 0 and 1). Default is `1`.}

\item{start_color}{Numeric. Start value for viridis color scale (between 0 and 1). Default is `0`.}

\item{title_prefix}{Character. Optional prefix for the plot title. If `NULL`, a default title is used.}

\item{n_top_nodes}{Integer. Number of top-degree nodes per community to label. Default is `10`.}

\item{label_fontsize}{Integer. Base font size for text panels. If `NULL`, size is automatically scaled to `ncol`.}

\item{by_community}{Logical. If `TRUE`, create individual plots per community with metadata side panels. 
If `FALSE`, show a single integrated graph. Default is `TRUE`.}

\item{ncol}{Integer. Number of columns in the grid layout when `by_community = TRUE`. Default is `2`.}

\item{expand_hairballs}{Numeric. Factor to visually expand densely connected communities 
("hairballs") to avoid overlap. Default is `NULL`.}

\item{use_palette}{Function. User-defined palette from a function, e.g. `ggsci::pal_aaas()`. Overrides the default viridis palette. Default is `NULL`.}
}
\value{
A `ggplot` or `patchwork` object showing the coordinated communities, ready for rendering or saving.
}
\description{
Visualizes communities of coordinated accounts within a network graph, either as a single overview plot
or as a grid of community-level plots with metadata side panels. Nodes represent accounts, edges reflect
similarity or interaction, and layout and annotations emphasize coordination patterns.
}
\details{
Communities are plotted using a `stress` layout and optionally separated into grid plots with 
accompanying text summaries. Top-degree nodes within each community are labeled. If metadata 
(`community_labels`) is available, it will be shown alongside each subgraph.

Dense subgraphs (high edge density and size) can be expanded for visual clarity using the 
`expand_hairballs` argument.
}
