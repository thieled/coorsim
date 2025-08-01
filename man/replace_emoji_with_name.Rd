% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{replace_emoji_with_name}
\alias{replace_emoji_with_name}
\title{Replace Emojis with Descriptive Names}
\usage{
replace_emoji_with_name(text_vec)
}
\arguments{
\item{text_vec}{A character vector containing text with emojis to be replaced.}
}
\value{
A character vector of the same length as `text_vec`, where all detected
emojis are replaced with their descriptive names in `:colon:` format.
}
\description{
Replaces emoji characters in a character vector with their corresponding
descriptive names in `:colon_syntax:` (e.g., "🔥" becomes ":fire:").
Only emojis present in the input are processed to maximize performance.
}
\details{
This function uses the `emoji::emoji_name` dataset for emoji-to-name mapping
and `stringi` for high-performance detection and substitution.
Only emojis that are actually present in the input are processed, making
the function suitable for large text corpora.
}
