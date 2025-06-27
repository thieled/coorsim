
#' Sample and format user posts for classification or labeling tasks
#'
#' This function samples posts from social media users based on customizable constraints 
#' and formats them into a markdown-style text block suitable for input to classification 
#' or labeling models. It supports inclusion of user-level and post-level metadata, 
#' optional name cleaning, and stratified or top-down user sampling per community.
#'
#' @param groups_data A named list containing `post_data` and `node_list`, both of which must be `data.table` objects.
#' @param user_vars Character vector of user-level variables to include (in addition to `account_name`).
#' @param user_vars_short Optional character vector with short labels corresponding to `user_vars` for display.
#' @param post_vars Character vector of post-level variables to include (in addition to `content`).
#' @param post_vars_short Optional character vector with short labels corresponding to `post_vars` for display.
#' @param min_chars Minimum number of characters required for post content (after cleaning). Default is 20.
#' @param max_chars Maximum number of characters for cleaned content or metadata fields. Default is 250.
#' @param clean_name Logical. If `TRUE`, clean `account_name` to remove noise and truncate length. Default is `TRUE`.
#' @param min_n_posts Minimum number of posts per user to be included. Default is 2.
#' @param max_n_posts Maximum number of posts per user to be included. Default is 10.
#' @param sampling_ratio_posts Proportion of posts to sample per user. Default is 0.25.
#' @param min_n_users Minimum number of users to sample per community. Default is 2.
#' @param max_n_users Maximum number of users to sample per community. Default is 10.
#' @param sampling_ratio_users Proportion of users to sample per community. Default is 0.25.
#' @param strat_sample Logical. If `TRUE`, performs stratified sampling of users by post volume within community. 
#'   If `FALSE`, selects most active users. Default is `TRUE`.
#' @param seed Integer seed for reproducible sampling. Default is 42.
#'
#' @return A list identical to `groups_data` with an added `user_labels` element. 
#'   This element is a `data.table` containing:
#'   \itemize{
#'     \item \code{community}: Community identifier.
#'     \item \code{account_name_clean}: Cleaned display name.
#'     \item \code{text}: A markdown-style string combining the user's posts and metadata.
#'     \item \code{sampled_post_ids}: List column of sampled post IDs.
#'     \item \code{approx_tokens}: Approximate token count (assuming 1.3x whitespace word count).
#'     \item \code{user_share_comm}: Relative posting activity of the user in their community.
#'   }
#'
#' @export
sample_user_text <- function(groups_data,
                             user_vars = NULL,
                             user_vars_short = NULL,
                             post_vars = NULL,
                             post_vars_short = NULL,
                             min_chars = 1,
                             max_chars = 1000,
                             clean_name = TRUE,
                             min_n_posts = 1,
                             max_n_posts = 30,
                             sampling_ratio_posts = 0.5,
                             min_n_users = 1,
                             max_n_users = 30,
                             sampling_ratio_users = 0.5,
                             strat_sample = TRUE,
                             seed = 42) {
  
  if (!"node_list" %in% names(groups_data) || !"post_data" %in% names(groups_data)) {
    stop("'groups_data' must contain 'node_list' and 'post_data'.")
  }
  
  post_data <- data.table::copy(groups_data$post_data)
  stopifnot("data.table" %in% class(post_data))
  set.seed(seed)
  
  required <- c("post_id", "account_id", "account_name", "content", "community")
  all_vars <- unique(c(required, user_vars, post_vars))
  if (!all(all_vars %in% names(post_data))) {
    stop("Missing required variables in post_data: ", paste(setdiff(all_vars, names(post_data)), collapse = ", "))
  }
  
  clean_text <- function(text, min_chars) {
    text |>
      stringr::str_replace_all("([[:punct:]])\\1{1,}", "\\1") |>
      stringr::str_remove_all("\\b[a-fA-F0-9]{32,}\\b") |>
      stringr::str_replace_all("[\\r\\n\\t]+", " ") |>
      stringr::str_squish() |>
      stringr::str_trunc(width = max_chars, ellipsis = "..")
  }
  
  post_data[, content_clean := clean_text(content, max_chars)]
  
  if (!is.null(user_vars)) {
    for (var in user_vars) {
      data.table::set(post_data, j = paste0(var, "_clean"), value = clean_text(post_data[[var]], max_chars))
    }
  }
  
  if (!is.null(post_vars)) {
    for (var in post_vars) {
      data.table::set(post_data, j = paste0(var, "_clean"), value = clean_text(post_data[[var]], max_chars))
    }
  }
  
  if ("account_name_clean" %in% names(post_data)) {
    post_data[, "account_name_clean" := NULL]
  }
  data.table::set(
    post_data,
    j = "account_name_clean",
    value = if (clean_name) clean_text(post_data$account_name, max_chars) else post_data$account_name
  )
  
  # Calculate user shares
  user_shares <- post_data[, .N, by = .(community, account_id)][
    , user_share_comm := N / sum(N), by = community
  ][, .(community, account_id, user_share_comm)]
  
  post_data <- post_data[nchar(content_clean) >= min_chars]
  
  sampled_dt <- post_data[
    , {
      n_total <- .N
      n_sample <- min(max(min_n_posts, ceiling(n_total * sampling_ratio_posts)), max_n_posts)
      .SD[sample(.N, min(.N, n_sample))]
    },
    by = account_id
  ]
  
  keep_users <- sampled_dt[, .N, by = account_id][N >= min_n_posts]
  sampled_dt <- sampled_dt[account_id %in% keep_users$account_id]
  
  user_comm_map <- unique(sampled_dt[, .(account_id, community)])
  eligible_communities <- user_comm_map[
    , .(n_users = data.table::uniqueN(account_id)), by = community
  ][n_users >= min_n_users, community]
  
  sampled_dt <- sampled_dt[community %in% eligible_communities]
  
  user_stats <- sampled_dt[, .N, by = .(account_id, community)]
  selected_users <- user_stats[
    , {
      n_eligible <- .N
      n_sample <- min(max(min_n_users, ceiling(n_eligible * sampling_ratio_users)), max_n_users)
      if (strat_sample) {
        probs <- N / sum(N)
        .SD[sample(.N, min(.N, n_sample), prob = probs)]
      } else {
        .SD[order(-N)][1:min(.N, n_sample)]
      }
    },
    by = community
  ]
  
  sampled_dt <- sampled_dt[account_id %in% selected_users$account_id]
  
  out <- sampled_dt[
    , {
      user_meta <- unique(.SD[, c("account_name_clean", "community", paste0(user_vars, "_clean")), with = FALSE])[1L]
      user_info <- if (!is.null(user_vars)) {
        short_names <- if (!is.null(user_vars_short)) user_vars_short else user_vars
        values <- unlist(user_meta[, paste0(user_vars, "_clean"), with = FALSE])
        non_empty <- !is.na(values) & nzchar(values)
        if (any(non_empty)) paste0("[", paste(paste0(short_names[non_empty], ": ", values[non_empty]), collapse = ", "), "]") else ""
      } else ""
      
      post_lines <- apply(
        .SD[, c("content_clean", paste0(post_vars, "_clean")), with = FALSE],
        1,
        function(row) {
          if (length(post_vars) > 0) {
            short <- if (!is.null(post_vars_short)) post_vars_short else post_vars
            vals <- row[-1]
            non_empty <- !is.na(vals) & nzchar(vals)
            meta <- if (any(non_empty)) paste0(" [", paste(paste0(short[non_empty], ": ", vals[non_empty]), collapse = ", "), "]") else ""
          } else meta <- ""
          paste0(row[[1]], meta)
        }
      )
      
      md <- paste0(user_meta$account_name_clean,
                   if (nchar(user_info) > 0) paste0(" ", user_info),
                   ": ", paste(post_lines, collapse = " | "))
      
      list(
        community = user_meta$community,
        account_name_clean = user_meta$account_name_clean,
        text = md,
        sampled_post_ids = list(.SD$post_id),
        approx_tokens = as.integer(round(length(strsplit(md, "\\s+")[[1]]) * 1.3))
      )
    },
    by = account_id
  ]
  
  out <- merge(out, user_shares[, community := NULL], by = "account_id", all.x = T)
  
  data.table::setorder(out, community, -user_share_comm)
  
  groups_data$user_labels <- out
  return(groups_data)
}




#' Clean and repair JSON-like strings generated by LLMs
#'
#' This function attempts to sanitize JSON-like character strings returned by large language models (LLMs),
#' preparing them for parsing via `jsonlite::fromJSON()`. It extracts the first substring that resembles
#' a JSON object, replaces single quotes with double quotes (when safe), escapes backslashes,
#' and ensures the string is properly enclosed in curly braces.
#'
#' @param json_strings A character vector of JSON-like strings (often malformed) generated by an LLM.
#'
#' @return A character vector of cleaned strings, formatted to increase the likelihood of successful parsing
#'   with `jsonlite::fromJSON()`.
#'
#' @examples
#' # Example 1: Fix single quotes
#' raw_json <- "{'label':'Test','description':'A test case','lang':'en'}"
#' clean_json_strings(raw_json)
#'
#' # Example 2: Handle backslashes in paths
#' messy_json <- "{'label':'Path','description':'Folder path is C:\\\\Users\\\\','lang':'en'}"
#' clean_json_strings(messy_json)
#'
#' # Example 3: Add missing braces
#' no_braces <- "'label':'Oops','lang':'en'"
#' clean_json_strings(no_braces)
#'
#' @details
#' This function is not guaranteed to fix all malformed JSON but is useful for cleaning common
#' LLM formatting issues. It does not validate the final JSON structure—only prepares it for parsing.
#'
#' @seealso [jsonlite::fromJSON()]
#'
#' @export
clean_json_strings <- function(json_strings) {
  vapply(json_strings, function(x) {
    # Extract JSON-like block
    x <- sub(".*?(\\{.*\\}).*", "\\1", x)
    
    # Trim whitespace
    x <- trimws(x)
    
    # Ensure braces
    if (!startsWith(x, "{")) x <- paste0("{", x)
    if (!endsWith(x, "}")) x <- paste0(x, "}")
    
    # Replace single quotes with double quotes around keys and values
    x <- gsub("(?<=[:{,\\s])'(.*?)'", "\"\\1\"", x, perl = TRUE)
    x <- gsub("^'(.*?)'$", "\"\\1\"", x, perl = TRUE)
    
    # Escape unescaped backslashes (each \ → \\)
    x <- gsub("\\\\", "\\\\\\\\", x)
    
    # Remove newlines
    x <- gsub("\\n", " ", x)
    
    x
  }, character(1))
}




#' Label Users Using a Local LLM
#'
#' This function uses a locally hosted large language model (LLM) via the `rollama` interface 
#' to assign labels, descriptions, and language tags to sampled user communities. It prompts the 
#' LLM with user-generated text and example annotations, expecting JSON-formatted responses. 
#' If a response cannot be parsed as valid JSON, the function retries a specified number of times, 
#' optionally truncating the input text during each retry.
#'
#' @param groups_data A list that includes `user_labels`, typically produced by `sample_user_text()`.
#' @param model Character. Name of the local LLM model (must be available via Ollama). Default is `"llama3.2:3b"`.
#' @param prompt Optional character string. Prompt text to instruct the model (default: internal `prompts$prompt_user`).
#' @param system Optional character string. System message to guide the model’s behavior (default: internal `prompts$system_user`).
#' @param example Optional character string. Example input text (default: internal `examples$example_user_text`).
#' @param answer Optional character string. Expected model output for the example input (default: internal `examples$example_user_answer`).
#' @param retries Integer. Maximum number of retry attempts for users whose responses fail JSON validation. Default is 3.
#' @param retry_trunc Integer. Character limit used to truncate user text in retry attempts. Default is 4000.
#' @param seed Integer. Random seed used for model generation (increased by 1 with each retry). Default is 42.
#' @param temp Numeric. Sampling temperature passed to the LLM. Default is 0.0 (deterministic output).
#' @param verbose Logical. Whether to display progress messages during querying and retries. Default is `TRUE`.
#'
#' @return The same `groups_data` list with `user_labels` updated. New columns include:
#' \itemize{
#'   \item \code{label} – Community label returned by the model (if available).
#'   \item \code{description} – Description of the community.
#'   \item \code{lang} – Language code guessed by the model.
#'   \item \code{is_valid_json} – Logical indicating whether the model's response could be parsed as JSON.
#'   \item \code{response} – Raw model response.
#'   \item \code{model}, \code{model_queried_at}, \code{model_total_duration} – Metadata for audit and reproducibility.
#' }
#'
#' @details
#' This function relies on `rollama` to interface with an Ollama server running locally. Ensure that Ollama is installed, 
#' the specified model is available, and the server is running. You can check connectivity using `rollama::ping_ollama()`.
#'
#' If model responses cannot be parsed into valid JSON after the maximum number of retries, only the latest (possibly invalid) 
#' response is retained.
#'
#' @seealso [sample_user_text()], [rollama::query()], [jsonlite::fromJSON()]
#'
#' @export
label_users <- function(groups_data, 
                        model = "llama3.2:3b",
                        prompt = NULL,
                        system = NULL,
                        example = NULL,
                        answer = NULL,
                        retries = 3, 
                        retry_trunc = 4000, 
                        seed = 42,
                        temp = 0.0,
                        verbose = TRUE) {
  
  if (!"user_labels" %in% names(groups_data)) {
    stop("No 'user_labels' found. Please first sample content by 'sample_user_text'.")
  }
  
  # Set prompts and examples
  if(is.null(prompt)){
    prompt_user <- prompts$prompt_user
  }else{
    prompt_user <- prompt
  }
  
  if(is.null(system)){
    system_user <- prompts$system_user
  }else{
    system_user <- system
  }
  
  if(is.null(example)){
    example_user_text <- examples$example_user_text
  }else{
    example_user_text <- example
  }
  
  if(is.null(answer)){
    example_user_answer <- examples$example_user_answer
  }else{
    example_user_answer <- answer
  }
  
  
  
  user_labels <- data.table::copy(groups_data$user_labels)
  
  if (!requireNamespace("rollama", quietly = TRUE)) {
    stop("Package 'rollama' is required for this function. Please install it.")
  }
  
  ping_res <- try(rollama::ping_ollama(), silent = TRUE)
  if (!ping_res[[1]]) stop("Ollama is not running.")
  
  example_resp <- tibble::tibble(
    text = example_user_text, 
    answer = example_user_answer
  )
  
  retry <- 0
  res_all <- list()
  user_current <- user_labels
  
  for (retry in 0:retries) {
    
    if (verbose) cli::cli_inform("Retry round {retry}. Querying {nrow(user_current)} users...")
    
    queries <- rollama::make_query(
      text = user_current$text,
      template = "{prefix}\n{text}\n{prompt}",
      prompt = prompt_user,
      prefix = "Annotate:",
      system = system_user,
      example = example_resp
    )
    
    responses <- rollama::query(
      queries,
      model = model,
      screen = TRUE,
      model_params = list(seed = seed + retry, temperature = temp),
      verbose = TRUE
    )
    
    res_df <- user_current |>
      dplyr::mutate(
        response = purrr::map_chr(responses, ~ purrr::pluck(.x, "message", "content")),
        model = purrr::map_chr(responses, ~ purrr::pluck(.x, "model")),
        model_queried_at = purrr::map_chr(responses, ~ purrr::pluck(.x, "created_at")),
        model_total_duration = purrr::map_dbl(responses, ~ purrr::pluck(.x, "total_duration"))
      ) |>
      dplyr::mutate(
        json_cleaned = clean_json_strings(response),
        parsed_json = lapply(json_cleaned, function(x) tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)),
        is_valid_json = sapply(json_cleaned, jsonlite::validate),
        description = sapply(parsed_json, function(x) if (is.null(x)) NA else x$description),
        lang = sapply(parsed_json, function(x) if (is.null(x)) NA else x$lang)
      ) |> 
      dplyr::select(-json_cleaned, -parsed_json)
    
    res_all[[retry + 1]] <- res_df
    
    if (all(res_df$is_valid_json)) {
      if (verbose) cli::cli_inform("All answers parsed successfully.")
      break
    }
    
    # Prepare failed users for retry
    failed_ids <- dplyr::filter(res_df, !is_valid_json)$account_id
    if (length(failed_ids) == 0 || retry == retries) {
      if (verbose) cli::cli_inform("Stopping after retry {retry}.")
      break
    }
    
    user_current <- user_labels |>
      dplyr::filter(account_id %in% failed_ids) |>
      dplyr::mutate(text = stringr::str_trunc(text, retry_trunc))
  }
  
  
  res_df <- dplyr::bind_rows(res_all)
  
  # De-duplicate (for parsing errors)
  res_df <- res_df |>
    dplyr::group_by(account_id) |>
    dplyr::slice_max(order_by = is_valid_json, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |> 
    dplyr::arrange(community, -user_share_comm)
  
  res_dt <- data.table::as.data.table(res_df)
  
  groups_data$user_labels <- res_dt
  
  return(groups_data)
}



#' Slice Community-Level User Descriptions for LLM Annotation
#'
#' This function prepares community-level text slices for annotation by aggregating user descriptions 
#' sampled via `sample_user_text()`. It cleans and truncates descriptions, filters out short or low-coverage 
#' entries, and creates slice-level summaries by grouping top users in each community.
#'
#' @param groups_data A list that must include `user_labels`, typically created by `sample_user_text()`.
#' @param max_n_per_slice Integer. Maximum number of user descriptions to include per text slice. Default is 10.
#' @param min_chars Integer. Minimum number of characters required for a user description. Default is 20.
#' @param max_chars Integer. Maximum number of characters to retain per user description (after cleaning). Default is 500.
#' @param min_share Optional numeric. Minimum total `user_share_comm` per slice to retain the entry. Default is `NULL` (no filtering).
#' @param verbose Logical. If `TRUE`, prints how many community slices will be returned. Default is `TRUE`.
#' @param seed Integer. Random seed for reproducibility. Default is 42.
#'
#' @return A `data.table` with one row per community-slice, containing:
#' \itemize{
#'   \item \code{community} – Community ID.
#'   \item \code{slice} – Slice index within the community.
#'   \item \code{n} – Number of user descriptions included in the slice.
#'   \item \code{sum_user_share_comm} – Cumulative posting share of included users.
#'   \item \code{text_slice} – Concatenated and cleaned descriptions for LLM input.
#' }
#'
#' @details
#' This function is intended to generate concise, representative community-level summaries 
#' from user-level metadata before labeling with LLMs. Descriptions are truncated and sanitized to ensure compatibility.
#'
#' @seealso [sample_user_text()], [label_communities()], [stringr::str_trunc()]
#'
#' @export
slice_community_text <- function(groups_data,
                                 max_n_per_slice = 10,
                                 min_chars = 20,
                                 max_chars = 500,
                                 min_share = NULL,
                                 verbose = TRUE,
                                 seed = 42) {
  
  if (!"user_labels" %in% names(groups_data)) {
    stop("No 'user_labels' found. Call 'sample_user_text'!")
  }
  
  set.seed(seed)
  
  # Ensure a clean, modifiable copy
  user_labels <- data.table::copy(groups_data$user_labels)
  data.table::setDT(user_labels)  # Coerce to data.table just in case
  
  # Clean text function
  clean_text <- function(text, max_chars) {
    text |>
      stringr::str_replace_all("([[:punct:]])\\1{1,}", "\\1") |>
      stringr::str_replace_all("[\r\n\t]+", " ") |>
      stringr::str_squish() |>
      stringr::str_trunc(width = max_chars, ellipsis = "..")
  }
  
  # Clean the 'description' column safely
  user_labels[, description := clean_text(description, max_chars)]
  user_labels <- user_labels[nchar(description) >= min_chars]
  
  # Order and slice
  data.table::setorder(user_labels, community, -user_share_comm)
  user_labels[, slice := ceiling(seq_len(.N) / max_n_per_slice), by = community]
  
  comm_labels <- user_labels[, .(
    n = .N,
    sum_user_share_comm = sum(user_share_comm, na.rm = TRUE),
    text_slice = paste(description, collapse = " | ")
  ), by = .(community, slice)]
  
  if(!is.null(min_share)){
    comm_labels <- comm_labels[sum_user_share_comm >= min_share]
  }
  
  if (verbose) {
    cli::cli_inform("Returning {nrow(comm_labels)} community texts to annotate.")
  }
  
  return(comm_labels)
}




#' Label Communities Using a Local LLM
#'
#' This function generates descriptive labels for communities of users based on aggregated user-level metadata
#' and sample texts. It uses a local large language model (LLM) via the `rollama` interface to extract structured
#' labels and descriptions for each community. If the user count is high, the community is sliced into subsets, labeled
#' individually, and then re-aggregated. Malformed or incomplete model responses are retried up to a specified number of times.
#'
#' @param groups_data A list containing a `user_labels` element, as returned by `sample_user_text()`.
#' @param max_n_per_slice Integer. Maximum number of users to include per slice when breaking up large communities. Default is 10.
#' @param min_chars Integer. Minimum character length for valid user-generated descriptions. Default is 1.
#' @param max_chars Integer. Maximum character length for any cleaned user input. Default is 1000.
#' @param min_share Optional numeric. Minimum cumulative posting share (`user_share_comm`) per slice. Default is `NULL` (no filter).
#' @param model Character. Name of the LLM model available via Ollama (e.g., `"llama3.2:3b"`). Default is `"llama3.2:3b"`.
#' @param retries Integer. Number of retry attempts for invalid or unparseable JSON responses. Default is 3.
#' @param retry_trunc Integer. Character limit for truncating input when retrying. Default is 4000.
#' @param seed Integer. Random seed for reproducibility. Default is 42.
#' @param temp Numeric. Sampling temperature for the LLM (0 = deterministic). Default is 0.0.
#' @param prompt Character. Prompt string to guide labeling (overrides default in `prompts$prompt_comm`). Optional.
#' @param system Character. System prompt string for guiding general LLM behavior (overrides `prompts$system_comm`). Optional.
#' @param system_slices Character. Optional override for `prompts$system_comm_agg` when aggregating across slices.
#' @param example Character. Input example text for single-slice labeling. Optional.
#' @param example_slices Character. Input example for aggregated slice-level labeling. Optional.
#' @param answer Character. Expected answer format for single-slice labeling example. Optional.
#' @param answer_slices Character. Expected answer format for slice-aggregated labeling. Optional.
#' @param verbose Logical. Whether to print progress and retry information. Default is `TRUE`.
#'
#' @return The input `groups_data` list, updated with a new `community_labels` element. This is a `data.table` that includes:
#' \itemize{
#'   \item \code{community} – Community ID.
#'   \item \code{label} – LLM-generated label for the community.
#'   \item \code{description} – LLM-generated textual summary of the community.
#'   \item \code{text} – Final text input used for labeling.
#'   \item \code{is_valid_json} – Logical indicator for successful parsing of model output.
#'   \item \code{model}, \code{model_queried_at}, \code{model_total_duration} – Metadata for reproducibility.
#' }
#'
#' @details
#' If a community contains many users, the function first slices the community using `slice_community_text()`,
#' applies the LLM on each slice, and aggregates the results in a second LLM round. The function assumes the LLM
#' returns JSON with at least `label` and `description` fields. Invalid responses are filtered or retried up to `retries` times.
#'
#' Requires Ollama to be installed and running, with the selected model available. See `rollama::ping_ollama()` for setup checks.
#'
#' @seealso [sample_user_text()], [slice_community_text()], [rollama::make_query()], [clean_json_strings()]
#'
#' @export
label_communities <- function(groups_data,
                              max_n_per_slice = 10,
                              min_chars = 1,
                              max_chars = 1000,
                              min_share = NULL,
                              model = "llama3.2:3b",
                              retries = 3, 
                              retry_trunc = 4000, 
                              seed = 42,
                              temp = 0.0,
                              prompt = NULL,
                              system = NULL,
                              system_slices = NULL,
                              example = NULL,
                              example_slices = NULL,
                              answer = NULL,
                              answer_slices = NULL,
                              verbose = TRUE) {
  
  if (!"user_labels" %in% names(groups_data)) {
    stop("No 'user_labels' found. Please first sample content by 'sample_user_text'.")
  }
  
  # Set prompts and examples
  if(is.null(prompt)){
    prompt_comm <- prompts$prompt_comm
  }else{
    prompt_comm <- prompt
  }
  
  if(is.null(system)){
    system_comm <- prompts$system_comm
  }else{
    system_comm <- system
  }
  
  
  if(is.null(system_slices)){
    system_comm_agg <- prompts$system_comm_agg
  }else{
    system_comm_agg <- system_slices
  }
  
  if(is.null(example)){
    example_comm_text <- examples$example_comm_text
  }else{
    example_comm_text <- example
  }
  
  if(is.null(example_slices)){
    example_comm_slices_text <- examples$example_comm_slices_text
  }else{
    example_comm_slices_text <- example_slices
  }
  
  if(is.null(answer)){
    example_comm_answer <- examples$example_comm_answer
  }else{
    example_comm_answer <- answer
  }
  
  if(is.null(answer_slices)){
    example_comm_slices_answer <- examples$example_comm_slices_answer
  }else{
    example_comm_slices_answer <- answer_slices
  }
  
  
  
  # Bind user texts to community texts
  comm_texts <-  slice_community_text(
    groups_data = groups_data,
    max_n_per_slice = max_n_per_slice,
    min_chars = min_chars,
    max_chars = max_chars,
    min_share = min_share,
    verbose = verbose,
    seed = seed)
  
  
  # Ensure a clean, modifiable copy
  comm_labels <- data.table::copy(comm_texts)
  data.table::setDT(comm_labels)  # Coerce to data.table just in case
  
  # Create slice id
  comm_labels <- comm_labels |> dplyr::mutate(id = paste0(community, "-", slice))
  
  if (!requireNamespace("rollama", quietly = TRUE)) {
    stop("Package 'rollama' is required for this function. Please install it.")
  }
  
  ping_res <- try(rollama::ping_ollama(), silent = TRUE)
  if (!ping_res[[1]]) stop("Ollama is not running.")
  
  example_resp <- tibble::tibble(
    text = example_comm_text, 
    answer = example_comm_answer
  )
  
  retry <- 0
  res_all <- list()
  comm_current <- comm_labels
  
  for (retry in 0:retries) {
    
    if (verbose) cli::cli_inform("Retry round {retry}. Querying {nrow(comm_current)} communities...")
    
    queries <- rollama::make_query(
      text = comm_current$text_slice,
      template = "{prefix}\n{text}\n{prompt}",
      prompt = prompt_comm,
      prefix = "Annotate:",
      system = system_comm,
      example = example_resp
    )
    
    responses <- rollama::query(
      queries,
      model = model,
      screen = TRUE,
      model_params = list(seed = seed + retry, 
                          temperature = temp),
      verbose = TRUE
    )
    
    res_df <- comm_current |>
      dplyr::mutate(
        response = purrr::map_chr(responses, ~ purrr::pluck(.x, "message", "content")),
        model = purrr::map_chr(responses, ~ purrr::pluck(.x, "model")),
        model_queried_at = purrr::map_chr(responses, ~ purrr::pluck(.x, "created_at")),
        model_total_duration = purrr::map_dbl(responses, ~ purrr::pluck(.x, "total_duration"))
      ) |>
      dplyr::mutate(
        json_cleaned = clean_json_strings(response),
        parsed_json = lapply(json_cleaned, function(x) tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)),
        is_valid_json = sapply(json_cleaned, jsonlite::validate),
        label = sapply(parsed_json, function(x) if (is.null(x)) NA else x$label),
        description = sapply(parsed_json, function(x) if (is.null(x)) NA else x$description)
      ) |> 
      dplyr::select(-json_cleaned, -parsed_json)
    
    res_df <- res_df |> dplyr::mutate(l_d = sapply(description, length),
                                      l_l = sapply(label, length),
                                      is_valid_json = ifelse(l_d + l_l != 2, FALSE, is_valid_json)) |> 
      dplyr::select(-l_d, -l_l)
    
    res_all[[retry + 1]] <- res_df
    
    if (all(res_df$is_valid_json)) {
      if (verbose) cli::cli_inform("All answers parsed successfully.")
      break
    }
    
    # Prepare failed users for retry
    failed_ids <- dplyr::filter(res_df, !is_valid_json)$id
    if (length(failed_ids) == 0 || retry == retries) {
      if (verbose) cli::cli_inform("Stopping after retry {retry}.")
      break
    }
    
    comm_current <- comm_labels |>
      dplyr::filter(id %in% failed_ids) |>
      dplyr::mutate(text_slice = stringr::str_trunc(text_slice, retry_trunc))
  }
  
  
  res_df <- dplyr::bind_rows(res_all)
  
  # De-duplicate (for parsing errors)
  res_df <- res_df |>
    dplyr::group_by(id) |>
    dplyr::slice_max(order_by = is_valid_json, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |> 
    dplyr::arrange(community)
  
  
  res_dt <- data.table::as.data.table(res_df)
  
  # Split sliced and unsliced communities
  sliced_dt <- res_dt[res_dt[, .N, by = community][N > 1], on = "community"]
  unsliced_dt <- res_dt[res_dt[, .N, by = community][N == 1], on = "community"]
  
  # Process sliced data
  if(nrow(sliced_dt) > 0){
    
    if(verbose) cli::cli_inform("Aggregating {nrow(sliced_dt)} slices.")
    
    # Clean text function
    clean_text <- function(text, max_chars) {
      text |>
        stringr::str_replace_all("([[:punct:]])\\1{1,}", "\\1") |>
        stringr::str_replace_all("[\r\n\t]+", " ") |>
        stringr::str_squish() |>
        stringr::str_trunc(width = max_chars, ellipsis = "..")
    }
    
    # Clean the 'description' column safely
    sliced_dt[, description := clean_text(description, max_chars)]
    sliced_dt[, label := clean_text(label, max_chars)]
    sliced_dt <- sliced_dt[nchar(description) >= min_chars]
    
    # Aggregate
    sliced_dt_agg <- sliced_dt[
      , .(
        sum_user_share_comm_total = sum(sum_user_share_comm),
        text = paste0(
          "[slice ", slice, 
          "; share=", round(sum_user_share_comm * 100), "%; ",
          "label='", label, "'; ",
          "description='", description, "']"
        ) |> paste(collapse = " | ")
      ),
      by = community
    ]
    
    
    example_slices_resp <- tibble::tibble(
      text = example_comm_slices_text, 
      answer = example_comm_slices_answer
    )
    
    retry <- 0
    res_all <- list()
    comm_current <- sliced_dt_agg
    
    for (retry in 0:retries) {
      
      if (verbose) cli::cli_inform("Retry round {retry}. Aggregating labels for {nrow(comm_current)} communities...")
      
      queries <- rollama::make_query(
        text = comm_current$text,
        template = "{prefix}\n{text}\n{prompt}",
        prompt = prompt_comm,
        prefix = "Annotate:",
        system = system_comm_agg,
        example = example_slices_resp
      )
      
      responses <- rollama::query(
        queries,
        model = model,
        screen = TRUE,
        model_params = list(seed = seed + retry, 
                            temperature = temp),
        verbose = TRUE
      )
      
      res_df <- comm_current |>
        dplyr::mutate(
          response = purrr::map_chr(responses, ~ purrr::pluck(.x, "message", "content")),
          model = purrr::map_chr(responses, ~ purrr::pluck(.x, "model")),
          model_queried_at = purrr::map_chr(responses, ~ purrr::pluck(.x, "created_at")),
          model_total_duration = purrr::map_dbl(responses, ~ purrr::pluck(.x, "total_duration"))
        ) |>
        dplyr::mutate(
          json_cleaned = clean_json_strings(response),
          parsed_json = lapply(json_cleaned, function(x) tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)),
          is_valid_json = sapply(json_cleaned, jsonlite::validate),
          label = sapply(parsed_json, function(x) if (is.null(x)) NA else x$label),
          description = sapply(parsed_json, function(x) if (is.null(x)) NA else x$description)
        ) |> 
        dplyr::select(-json_cleaned, -parsed_json)
      
      res_df <- res_df |> dplyr::mutate(l_d = sapply(description, length),
                                        l_l = sapply(label, length),
                                        is_valid_json = ifelse(l_d + l_l != 2, FALSE, is_valid_json)) |> 
        dplyr::select(-l_d, -l_l)
      
      res_all[[retry + 1]] <- res_df
      
      if (all(res_df$is_valid_json)) {
        if (verbose) cli::cli_inform("All answers parsed successfully.")
        break
      }
      
      # Prepare failed users for retry
      failed_ids <- dplyr::filter(res_df, !is_valid_json)$community
      if (length(failed_ids) == 0 || retry == retries) {
        if (verbose) cli::cli_inform("Stopping after retry {retry}.")
        break
      }
      
      comm_current <- sliced_dt_agg |>
        dplyr::filter(community %in% failed_ids) |>
        dplyr::mutate(text = stringr::str_trunc(text, retry_trunc))
    }
    
    
    res_df <- dplyr::bind_rows(res_all)
    
    # De-duplicate (for parsing errors)
    res_df <- res_df |>
      dplyr::group_by(community) |>
      dplyr::slice_max(order_by = is_valid_json, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |> 
      dplyr::arrange(community)
    
    sliced_dt <- data.table::as.data.table(res_df)
    
    
    sliced_dt |> dplyr::glimpse()
    unsliced_dt |> dplyr::glimpse()
    
    # Harmonize variables
    sliced_dt[, sum_user_share_comm  := sum_user_share_comm_total][, c("sum_user_share_comm_total") := NULL]
    unsliced_dt[, text := text_slice][, c("text_slice", "id", "slice", "n", "N") := NULL]
    
    ## Bind together
    res_dt <- data.table::rbindlist(list(), use.names = T, fill = T, ignore.attr = T)
    
  }else{
    res_dt <- unsliced_dt[, text := text_slice][, c("text_slice", "id", "slice", "n", "N") := NULL]
  }
  
  groups_data$community_labels <- res_dt
  
  return(groups_data)
}


