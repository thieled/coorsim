% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_merging_filtering_functions.R
\name{augment_similarity_table}
\alias{augment_similarity_table}
\title{Augment a Similarity Table with Post and User Data}
\usage{
augment_similarity_table(
  post_data,
  user_data,
  sim_dt,
  post_id = NULL,
  account_id = NULL,
  account_name = NULL,
  content = NULL,
  other_post_vars = NULL,
  other_user_vars = NULL,
  verbose = FALSE
)
}
\arguments{
\item{post_data}{A `data.table` containing post data.}

\item{user_data}{A `data.table` containing user data.}

\item{sim_dt}{A `data.table` containing similarity data between posts.}

\item{post_id}{Optional. The column name in `post_data` to be renamed to 'post_id' for joining.}

\item{account_id}{Optional. The column name in `user_data` to be renamed to 'account_id' for joining.}

\item{account_name}{Optional. The column name in `user_data` to be renamed to 'account_name'.}

\item{content}{Optional. The column name in `post_data` containing the post content to be renamed to 'content'.}

\item{other_post_vars}{Optional. Additional columns to retain from `post_data`.}

\item{other_user_vars}{Optional. Additional columns to retain from `user_data`.}

\item{verbose}{Logical. If `TRUE`, provides verbose output.}
}
\value{
A `data.table` with `post_data` and `user_data` merged into `sim_dt`.
}
\description{
This function merges data from `post_data` and `user_data` into a similarity table (`sim_dt`).
It adds columns for post content and user information while handling potential name collisions.
}
