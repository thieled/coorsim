% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/06_plotting.R
\name{plot_coordinated_posts}
\alias{plot_coordinated_posts}
\title{Plot Coordinated Posts of Communities}
\usage{
plot_coordinated_posts(
  network_data,
  by_communtiy = T,
  unit = "6 hours",
  component_size_threshold = NULL,
  palette_option = "A",
  start_color = 0,
  end_color = 0.9,
  title_prefix = NULL,
  include_examples = T,
  add_labels = F,
  legend = T,
  n_random_accounts = 3,
  n_random_rows = 2,
  seed = 42,
  verbose = T
)
}
\arguments{
\item{network_data}{A list containing at least a `post_data` data.table and an `igraph` graph object. Optionally, the list may include `labelled_communities` for community labels and `param_list` for metadata.}

\item{by_communtiy}{Logical. If `TRUE`, the plot will display data grouped by community. If `FALSE`, the plot will display the overall activity.}

\item{unit}{A string specifying the time unit for floor date rounding, e.g., `"6 hours"`. Passed to `lubridate::floor_date()`.}

\item{component_size_threshold}{Integer. The minimum size of connected components to retain in the graph. Communities below this size are excluded from the plot.}

\item{palette_option}{A string specifying the color palette to use, passed to `viridis::viridis()`. Default is `"A"`.}

\item{start_color}{Numeric. The starting value of the color gradient for the palette, between 0 and 1.}

\item{end_color}{Numeric. The ending value of the color gradient for the palette, between 0 and 1.}

\item{title_prefix}{A string to prefix the plot title. Default is `NULL`.}

\item{include_examples}{Logical. If `TRUE`, the plot includes examples of peak coordinated activity. Default is `TRUE`.}

\item{add_labels}{Logical. If `TRUE`, the plot includes labels on the bars for each community's name and size. Default is `TRUE`.}

\item{legend}{Logical. If `TRUE`, the plot includes a legend for the communities. Default is `TRUE`.}

\item{n_random_accounts}{Integer. The number of random accounts per community to include in the examples, if `include_examples = TRUE`. Default is 3.}

\item{n_random_rows}{Integer. The number of random rows to include in the examples per account. Default is 2.}

\item{seed}{Integer. A seed for reproducibility of the random sampling of examples. Default is 42.}

\item{verbose}{Logical. If `TRUE`, progress and information messages are printed. Default is `TRUE`.}
}
\value{
A `ggplot2` object representing the coordinated posts of the communities.
}
\description{
This function generates a bar plot showing the coordinated posts of different communities over time. The plot can be configured to show the number of posts per community, with additional options to include labels or randomly sampled examples of peak coordinated activity for each community.
}
