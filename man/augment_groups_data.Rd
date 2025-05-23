% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_merging_filtering_functions.R
\name{augment_groups_data}
\alias{augment_groups_data}
\title{Augment Group Data with Post and User Information}
\usage{
augment_groups_data(
  groups_data,
  post_data,
  user_data,
  post_id = NULL,
  account_id = NULL,
  content = NULL,
  other_post_vars = NULL,
  other_user_vars = NULL,
  sample_content = FALSE,
  sample_n = 10,
  sep = " +++ ",
  seed = 42,
  verbose = FALSE
)
}
\arguments{
\item{groups_data}{A list with at least one element `node_list` (a `data.table` containing node information).
Optionally, it may also contain `post_data` (a `data.table` of post information).}

\item{post_data}{A `data.table` containing additional post data to be merged.}

\item{user_data}{A `data.table` containing user data to be merged.}

\item{post_id}{Optional. Column name in `post_data` to be renamed to 'post_id' for joining.}

\item{account_id}{Optional. Column name in `user_data` and `groups_data` to be renamed to 'account_id' for joining.}

\item{content}{Optional. Column name in `post_data` containing post content, renamed to 'content'.}

\item{other_post_vars}{Optional. Additional columns to retain from `post_data`.}

\item{other_user_vars}{Optional. Additional columns to retain from `user_data`.}

\item{sample_content}{Logical. If `TRUE`, samples content from `post_data` by `account_id` and merges sampled text into `node_list`.}

\item{sample_n}{Integer. Number of posts to sample per `account_id` if `sample_content` is `TRUE`. Defaults to 10.}

\item{sep}{Character. Separator used to concatenate sampled content. Defaults to " +++ ".}

\item{seed}{Integer. Random seed for reproducibility of sampled content. Defaults to 42.}

\item{verbose}{Logical. If `TRUE`, provides detailed output about each processing step.}
}
\value{
A list, `groups_data`, with the following modifications:
  - `post_data` augmented with user information and additional post columns, if present in `groups_data`.
  - `node_list` augmented with user data and optionally, sampled content from `post_data`.
}
\description{
This function merges additional post and user data into a grouped data structure, `groups_data`.
It supports renaming columns for consistency, de-duplication, and optional content sampling.
}
\details{
This function harmonizes column names and performs left joins to add relevant data. When `sample_content`
is `TRUE`, it samples `sample_n` posts from `post_data` per `account_id`, concatenates them with the specified separator `sep`,
and adds the resulting strings to `node_list`.
}
