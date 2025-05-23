% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05_generate_community_labels.R
\name{prepare_community_texts}
\alias{prepare_community_texts}
\title{Prepare and Aggregate Community Texts}
\usage{
prepare_community_texts(
  groups_data,
  sample_n = 5,
  sep = " + ",
  trunc_width = 100,
  min_n_char = 5,
  sample_n_users = 10,
  verbose = TRUE,
  seed = 42
)
}
\arguments{
\item{groups_data}{A list containing `post_data` and `node_list` data tables, where `post_data` includes posts associated with each account, and `node_list` includes account metadata.}

\item{sample_n}{Numeric. The number of posts to sample per account. Defaults to 5.}

\item{sep}{Character. A separator to use between sampled posts or account identifiers in the aggregated output. Defaults to `" + "`.}

\item{trunc_width}{Numeric. The maximum character width for truncating texts. Set to `NULL` for no truncation. Defaults to 100.}

\item{min_n_char}{Numeric. The minimum number of characters a post must have to be included in sampling. Defaults to 5.}

\item{sample_n_users}{Numeric. The number of accounts to sample per community. Defaults to 10.}

\item{verbose}{Logical. If `TRUE`, provides informational messages about each processing step. Defaults to `TRUE`.}

\item{seed}{Numeric. A seed for random sampling, ensuring reproducibility. Defaults to 42.}
}
\value{
A modified version of `groups_data` with an additional `community_sample` data table. This table includes:
- `sampled_content`: Aggregated text data from sampled accounts within each community.
- `sampled_accounts`: Account IDs of the sampled accounts within each community.
- `all_accounts`: All account IDs within each community.
- `community_size`: The total number of accounts within each community.
}
\description{
This function prepares and aggregates text data for each community within a `groups_data` object. It first processes individual 
account data, then samples accounts within each community to create a summarized text field for the community, including 
sampled and all account identifiers.
}
\details{
The function proceeds through the following steps:
1. Calls `prepare_account_texts()` to clean and sample text data for individual accounts.
2. Samples up to `sample_n_users` accounts per community to create a summarized text representation for the community.
3. Aggregates sampled text data and stores all account IDs and the community size within each community.
}
\seealso{
\code{\link{prepare_account_texts}} for individual account-level text preparation.
}
