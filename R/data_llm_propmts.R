#' Prompt and example data for LLM community annotation
#'
#' These objects bundle prompt templates and few-shot examples used for guiding large language models (LLMs) in the `coorsim` package.
#' All examples are anonymized and rewritten using GPT-4o to standardize tone and protect privacy.
#'
#' @section Prompts (`prompts`):
#' \describe{
#'   \item{prompt_user}{Instruction for generating a user description and language tag in JSON.}
#'   \item{prompt_comm}{Instruction for generating a community label and description in JSON.}
#'   \item{system_user}{System message instructing how to interpret individual user text.}
#'   \item{system_comm}{System message for generating a label and summary from community-level descriptions.}
#'   \item{system_comm_agg}{System message for summarizing community slices by share.}
#' }
#'
#' @section Examples (`examples`):
#' \describe{
#'   \item{example_user_text}{Simulated user posts in English and German.}
#'   \item{example_user_answer}{Expected output: user description and language.}
#'   \item{example_comm_text}{Concatenated user descriptions per community.}
#'   \item{example_comm_answer}{Expected output: community label and description.}
#'   \item{example_comm_slices_text}{Pre-labeled community slices with share info.}
#'   \item{example_comm_slices_answer}{Expected output for slice aggregation.}
#' }
#'
#' @format Lists of character vectors.
#' @encoding UTF-8
#' @keywords datasets
#' @docType data
#' @name prompt_data
"prompts"

#' @rdname prompt_data
"examples"
