% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05_generate_community_labels.R
\name{label_users}
\alias{label_users}
\title{Label Users Using a Local LLM}
\usage{
label_users(
  groups_data,
  model = "llama3.2:3b",
  prompt = NULL,
  system = NULL,
  example = NULL,
  answer = NULL,
  retries = 3,
  retry_trunc = 4000,
  seed = 42,
  temp = 0,
  verbose = TRUE
)
}
\arguments{
\item{groups_data}{A list that includes `user_labels`, typically produced by `sample_user_text()`.}

\item{model}{Character. Name of the local LLM model (must be available via Ollama). Default is `"llama3.2:3b"`.}

\item{prompt}{Optional character string. Prompt text to instruct the model (default: internal `prompts$prompt_user`).}

\item{system}{Optional character string. System message to guide the model’s behavior (default: internal `prompts$system_user`).}

\item{example}{Optional character string. Example input text (default: internal `examples$example_user_text`).}

\item{answer}{Optional character string. Expected model output for the example input (default: internal `examples$example_user_answer`).}

\item{retries}{Integer. Maximum number of retry attempts for users whose responses fail JSON validation. Default is 3.}

\item{retry_trunc}{Integer. Character limit used to truncate user text in retry attempts. Default is 4000.}

\item{seed}{Integer. Random seed used for model generation (increased by 1 with each retry). Default is 42.}

\item{temp}{Numeric. Sampling temperature passed to the LLM. Default is 0.0 (deterministic output).}

\item{verbose}{Logical. Whether to display progress messages during querying and retries. Default is `TRUE`.}
}
\value{
The same `groups_data` list with `user_labels` updated. New columns include:
\itemize{
  \item \code{label} – Community label returned by the model (if available).
  \item \code{description} – Description of the community.
  \item \code{lang} – Language code guessed by the model.
  \item \code{is_valid_json} – Logical indicating whether the model's response could be parsed as JSON.
  \item \code{response} – Raw model response.
  \item \code{model}, \code{model_queried_at}, \code{model_total_duration} – Metadata for audit and reproducibility.
}
}
\description{
This function uses a locally hosted large language model (LLM) via the `rollama` interface 
to assign labels, descriptions, and language tags to sampled user communities. It prompts the 
LLM with user-generated text and example annotations, expecting JSON-formatted responses. 
If a response cannot be parsed as valid JSON, the function retries a specified number of times, 
optionally truncating the input text during each retry.
}
\details{
This function relies on `rollama` to interface with an Ollama server running locally. Ensure that Ollama is installed, 
the specified model is available, and the server is running. You can check connectivity using `rollama::ping_ollama()`.

If model responses cannot be parsed into valid JSON after the maximum number of retries, only the latest (possibly invalid) 
response is retained.
}
\seealso{
[sample_user_text()], [rollama::query()], [jsonlite::fromJSON()]
}
