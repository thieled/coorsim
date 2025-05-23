% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_detect_coorsimilarity.R
\name{detect_cosimilarity}
\alias{detect_cosimilarity}
\title{Detect Co-Similarities}
\usage{
detect_cosimilarity(
  data,
  embeddings = NULL,
  time_window = 60,
  min_simil = 0.9,
  min_participation = 3,
  post_id = NULL,
  account_id = NULL,
  time = NULL,
  content = NULL,
  verbose = TRUE,
  method = "cosine",
  remove_loops = TRUE,
  parallel = TRUE,
  n_threads = NULL,
  subset_emb = FALSE,
  embeddings_name = NULL
)
}
\arguments{
\item{data}{A `data.frame` or `data.table` containing social media data. Must include columns for post IDs, account IDs, timestamps, and content.}

\item{embeddings}{Optional. Embeddings for the posts, which can be provided in the following formats:
\itemize{
  \item **A matrix**: Row names must correspond to `post_id` values.
  \item **A valid `.h5` file path**: The file must contain a `metadata/post_id` dataset.
  \item **NULL (default)**: Uses text-based similarity instead of embeddings.
}}

\item{time_window}{Integer. Specifies the time window (in seconds) within which to detect similar posts. Default is `60`.}

\item{min_simil}{Numeric. The minimum similarity threshold to consider posts as similar. Default is `0.9`.}

\item{min_participation}{Integer. The minimum number of participations required to consider an account as part of coordinated activity. Default is `3`.}

\item{post_id}{Optional. Character string specifying the column name for post IDs if different from "post_id".}

\item{account_id}{Optional. Character string specifying the column name for account IDs if different from "account_id".}

\item{time}{Optional. Character string specifying the column name for timestamps if different from "time".}

\item{content}{Optional. Character string specifying the column name for post content if different from "content".}

\item{verbose}{Logical. If `TRUE`, displays progress messages. Default is `TRUE`.}

\item{method}{Character. Specifies the similarity method to use (e.g., "cosine"). Default is "cosine".}

\item{remove_loops}{Logical. If `TRUE`, removes self-similar posts from the same account. Default is `TRUE`.}

\item{parallel}{Logical. If `TRUE`, enables parallel computation for similarity calculations. Default is `TRUE`.}

\item{n_threads}{Optional. Integer specifying the number of threads for parallel computation. If `NULL`, defaults to available cores minus one.}

\item{subset_emb}{Logical. Should the embedding matrix be subsetted to observations in data? Slower but more memory-friendly. Default is `FALSE`.}

\item{embeddings_name}{Character. The name of the embedding model used to be stored in 'sim_dt'. Default is `NULL`.}
}
\value{
A `data.table` containing pairs of similar posts with the following columns:
  \itemize{
    \item `post_id` - The ID of the first post in the pair.
    \item `post_id_y` - The ID of the second post in the pair.
    \item `similarity` - The computed similarity score.
    \item `account_id` - The account that posted `post_id`.
    \item `account_id_y` - The account that posted `post_id_y`.
    \item `time` - The timestamp of `post_id`.
    \item `time_y` - The timestamp of `post_id_y`.
    \item `time_diff` - The time difference between the two posts (in seconds).
  }
}
\description{
This function identifies highly similar social media posts shared within a specified time window,
aiming to detect coordinated social media manipulation. It uses either text-based sparse document-feature matrices
or numerical embeddings to compute pairwise similarity scores.
}
\details{
The function follows four main steps:
  \enumerate{
    \item **Preprocess Data**: Standardizes column names, ensures valid timestamps, and prepares embeddings if provided.
    \item **Match Overlapping Posts**: Identifies posts published within the specified `time_window`.
    \item **Compute Similarities**: 
      \itemize{
        \item If `embeddings` are `NULL`, constructs a sparse document-feature matrix and calculates similarity scores.
        \item If `embeddings` are provided, queries and computes pairwise similarities using optimized C++ functions.
      }
    \item **Filter and Aggregate**: Joins account and timestamp data, filters accounts by `min_participation`, and removes redundant pairs.
  }
}
