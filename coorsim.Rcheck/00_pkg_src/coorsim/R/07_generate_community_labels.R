#' Label Communities Using a Local LLM
#'
#' This function uses a locally hosted large language model (LLM) via the \pkg{rollama} interface
#' to generate structured labels and descriptive summaries for user communities. It first labels
#' each community (or, if large, its individual slices) based on user-level annotations and text,
#' then optionally aggregates slice-level results in a second LLM pass to produce one coherent
#' community-level label and description.
#'
#' If malformed or incomplete JSON responses are returned, the function retries the query up to
#' a user-specified number of attempts. It supports both single-slice and multi-slice processing,
#' automatically detecting and aggregating communities that have been split during preprocessing.
#'
#' @param groups_data A list containing at least a \code{user_labels} element,
#' typically produced by [sample_user_text()]. This element must include community membership
#' and sampled post text for each user.
#' @param max_n_per_slice Integer. Maximum number of users per slice when splitting large
#' communities. Default is \code{10}.
#' @param min_chars Integer. Minimum character length required for a valid slice input.
#' Default is \code{1}.
#' @param max_chars Integer. Maximum allowed character length per slice (longer input will be truncated).
#' Default is \code{Inf}.
#' @param min_share Optional numeric. Minimum cumulative posting share (\code{user_share_comm})
#' per slice. Default is \code{NULL} (no filter).
#' @param model Character string. The local LLM model to query via Ollama
#' (e.g., \code{"gemma3:12b"}). Default is the task-evaluated \code{"gemma3:12b"}.
#' @param retries Integer. Maximum number of re-query attempts for invalid or unparseable
#' JSON responses. Default is \code{3}.
#' @param retry_trunc Integer. Character limit for truncating text inputs in retry rounds.
#' Default is \code{4000}.
#' @param seed Integer. Random seed for reproducibility. Default is \code{42}.
#' @param temp Numeric. Sampling temperature for model generation
#' (\code{0} = deterministic). Default is \code{0.0}.
#' @param prompt Character. User-level prompt text used to instruct the model when labeling
#' communities (overrides internal \code{prompts$prompt_comm}). Optional.
#' @param system Character. System-level instruction guiding the model's behavior
#' (overrides internal \code{prompts$system_comm}). Optional.
#' @param system_slices Character. Optional override for \code{prompts$system_comm_slices},
#' used for the aggregation step that combines slice-level labels into one community summary.
#' @param example Character vector containing few-shot example inputs for community-level labeling.
#' Defaults to \code{examples$example_comm_text}.
#' @param example_slices Character vector containing few-shot example inputs for aggregated
#' slice-level labeling. Defaults to \code{examples$example_comm_slices_text}.
#' @param answer Character vector of corresponding example model outputs for community-level labeling.
#' Defaults to \code{examples$example_comm_answer}.
#' @param answer_slices Character vector of example outputs for aggregated slice-level labeling.
#' Defaults to \code{examples$example_comm_slices_answer}.
#' @param schema Optional JSON-like list defining the expected structured output format.
#' Defaults to \code{schemata$schema_comm}.
#' @param keep_description_json Logical. If \code{FALSE} (default), extracts and concatenates
#' sentences from structured description objects into clean text. If \code{TRUE}, preserves JSON
#' structure as text. Only relevant when description is returned as a JSON object with
#' sentence_1 through sentence_5 fields.
#' @param verbose Logical. Whether to print progress messages, retry information,
#' and aggregation updates. Default is \code{TRUE}.
#'
#' @return
#' The same \code{groups_data} list, updated with a new \code{community_labels} element.
#' This is a \pkg{data.table} with one row per labeled community containing:
#' \describe{
#'   \item{\code{community}}{Community identifier.}
#'   \item{\code{label}}{LLM-generated label for the community.}
#'   \item{\code{description}}{Concise LLM-generated summary of the community's tone, style, and main topics.}
#'   \item{\code{lang}}{Predominant language inferred by the model.}
#'   \item{\code{topic_1–topic_5}}{Up to five thematic categories assigned by the model.}
#'   \item{\code{named_entity_1–named_entity_5}, \code{sentiment_1–sentiment_5}}{Named entities and associated sentiments.}
#'   \item{\code{pattern_1–pattern_3}}{Recurring emojis, slogans, or stylistic markers.}
#'   \item{\code{incivility}}{Whether the majority of users use uncivil or offensive language ("yes"/"no").}
#'   \item{\code{elaborate}}{Whether the majority of users post elaborate content ("yes"/"no").}

#'   \item{\code{confidence}}{Model-estimated confidence score between 0 and 1.}
#'   \item{\code{text}}{Input text provided to the model (either slice or aggregated JSON).}
#'   \item{\code{is_valid_json}}{Logical flag indicating successful JSON validation.}
#'   \item{\code{model}, \code{model_queried_at}, \code{model_total_duration}}{Metadata about the model run.}
#' }
#'
#' @details
#' The function executes in two main stages:
#' \enumerate{
#'   \item \strong{Slice-level labeling:} If a community exceeds \code{max_n_per_slice} users, it is divided
#'   into smaller slices using [slice_community_text()]. Each slice is labeled independently by the LLM.
#'   \item \strong{Community-level aggregation:} For multi-slice communities, the slice-level labels and
#'   descriptions are combined into a JSON array and passed to the model again with
#'   \code{system_comm_slices} to generate a single coherent label and summary.
#' }
#'
#' JSON responses are validated using \pkg{jsonlite}; malformed responses trigger retries with truncated text
#' up to \code{retries} times. The function expects a locally running Ollama instance with the specified
#' model available. Verify setup using [rollama::ping_ollama()].
#'
#' @seealso
#' [sample_user_text()], [slice_community_text()], [rollama::query()], [rollama::make_query()], [jsonlite::fromJSON()]
#'
#' @export
label_communities <- function(groups_data,
                              max_n_per_slice = 10,
                              min_chars = 1,
                              max_chars = Inf,
                              min_share = NULL,
                              model = "gemma3:12b",
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
                              schema = NULL,
                              keep_description_json = FALSE,
                              verbose = TRUE) {

  if (!"user_labels" %in% names(groups_data)) {
    stop("No 'user_labels' found. Please first sample content by 'sample_user_text'.")
  }

  # Set prompts and examples
  if (is.null(prompt)) {
    prompt_comm <- prompts$prompt_comm
  } else {
    prompt_comm <- prompt
  }

  if (is.null(system)) {
    system_comm <- prompts$system_comm
  } else {
    system_comm <- system
  }

  if (is.null(system_slices)) {
    system_comm_slices <- prompts$system_comm_slices
  } else {
    system_comm_slices <- system_slices
  }

  if (is.null(example)) {
    example_comm_text <- examples$example_comm_text
  } else {
    example_comm_text <- example
  }

  if (is.null(example_slices)) {
    example_comm_slices_text <- examples$example_comm_slices_text
  } else {
    example_comm_slices_text <- example_slices
  }

  if (is.null(answer)) {
    example_comm_answer <- examples$example_comm_answer
  } else {
    example_comm_answer <- answer
  }

  if (is.null(answer_slices)) {
    example_comm_slices_answer <- examples$example_comm_slices_answer
  } else {
    example_comm_slices_answer <- answer_slices
  }

  if (is.null(schema)) {
    schema_comm <- schemata$schema_comm
  } else {
    schema_comm <- schema
  }

  # Helper: parse one LLM response batch into a data.frame of extracted fields
  parse_comm_responses <- function(df, responses, keep_json) {
    df |>
      dplyr::mutate(
        response = purrr::map_chr(responses, ~ purrr::pluck(.x, "message", "content")),
        model = purrr::map_chr(responses, ~ purrr::pluck(.x, "model")),
        model_queried_at = purrr::map_chr(responses, ~ purrr::pluck(.x, "created_at")),
        model_total_duration = purrr::map_dbl(responses, ~ purrr::pluck(.x, "total_duration"))
      ) |>
      dplyr::mutate(
        parsed_json = purrr::map(
          response,
          ~ tryCatch(jsonlite::fromJSON(.x), error = function(e) NULL)
        ),
        is_valid_json = purrr::map_lgl(response, jsonlite::validate),

        # --- Scalar fields ---
        label       = purrr::map_chr(parsed_json, ~ .x$label %||% NA_character_),
        description = purrr::map_chr(
          parsed_json,
          ~ .reconstruct_description_v2(.x, keep_json = keep_json)
        ),
        lang        = purrr::map_chr(parsed_json, ~ .x$lang %||% NA_character_),
        incivility  = purrr::map_chr(parsed_json, ~ .x$incivility %||% NA_character_),
        elaborate   = purrr::map_chr(parsed_json, ~ .x$elaborate %||% NA_character_),
        confidence  = purrr::map_dbl(parsed_json, ~ .x$confidence %||% NA_real_)
      ) |>
      dplyr::mutate(
        # --- Topics ---
        topic   = purrr::map(parsed_json, ~ base::unlist(.x$topic) %||% character()),
        topic_1 = purrr::map_chr(topic, ~ .x[1] %||% NA_character_),
        topic_2 = purrr::map_chr(topic, ~ .x[2] %||% NA_character_),
        topic_3 = purrr::map_chr(topic, ~ .x[3] %||% NA_character_),
        topic_4 = purrr::map_chr(topic, ~ .x[4] %||% NA_character_),
        topic_5 = purrr::map_chr(topic, ~ .x[5] %||% NA_character_),

        # --- Repetitive patterns ---
        repetitive_patterns = purrr::map(parsed_json, ~ base::unlist(.x$repetitive_patterns) %||% character()),
        pattern_1 = purrr::map_chr(repetitive_patterns, ~ .x[1] %||% NA_character_),
        pattern_2 = purrr::map_chr(repetitive_patterns, ~ .x[2] %||% NA_character_),
        pattern_3 = purrr::map_chr(repetitive_patterns, ~ .x[3] %||% NA_character_),

        # --- Named entities ---
        named_entities  = purrr::map(parsed_json, ~ base::as.data.frame(.x$named_entities) %||% base::data.frame()),
        named_entity_1  = purrr::map_chr(named_entities, ~ .x$entity[1] %||% NA_character_),
        sentiment_1     = purrr::map_chr(named_entities, ~ .x$sentiment[1] %||% NA_character_),
        named_entity_2  = purrr::map_chr(named_entities, ~ .x$entity[2] %||% NA_character_),
        sentiment_2     = purrr::map_chr(named_entities, ~ .x$sentiment[2] %||% NA_character_),
        named_entity_3  = purrr::map_chr(named_entities, ~ .x$entity[3] %||% NA_character_),
        sentiment_3     = purrr::map_chr(named_entities, ~ .x$sentiment[3] %||% NA_character_),
        named_entity_4  = purrr::map_chr(named_entities, ~ .x$entity[4] %||% NA_character_),
        sentiment_4     = purrr::map_chr(named_entities, ~ .x$sentiment[4] %||% NA_character_),
        named_entity_5  = purrr::map_chr(named_entities, ~ .x$entity[5] %||% NA_character_),
        sentiment_5     = purrr::map_chr(named_entities, ~ .x$sentiment[5] %||% NA_character_)
      ) |>
      dplyr::select(
        -parsed_json,
        -topic,
        -repetitive_patterns,
        -named_entities
      ) |>
      # Invalidate rows where label or description are missing/empty
      dplyr::mutate(
        is_valid_json = dplyr::if_else(
          is.na(label) | is.na(description) | label == "" | description == "",
          FALSE,
          is_valid_json
        )
      )
  }

  # Calculate comm stats
  post_data  <- data.table::copy(groups_data$post_data)
  comm_stats <- post_data[, .(
    n_posts = data.table::uniqueN(post_id),
    n_accs  = data.table::uniqueN(account_id)
  ), by = community]

  user_labels <- data.table::copy(groups_data$user_labels)
  top_lang <- user_labels[, .N, by = .(community, lang)][
    order(-N), .SD[1], by = community
  ][, .(community, lang)]

  comm_stats <- top_lang[comm_stats, on = "community"]

  # Build community text slices
  comm_texts <- slice_community_text(
    groups_data    = groups_data,
    max_n_per_slice = max_n_per_slice,
    min_chars      = min_chars,
    max_chars      = max_chars,
    min_share      = min_share,
    verbose        = verbose,
    seed           = seed
  )

  comm_labels <- data.table::copy(comm_texts)
  data.table::setDT(comm_labels)
  comm_labels <- comm_labels |> dplyr::mutate(id = paste0(community, "-", slice))

  if (!requireNamespace("rollama", quietly = TRUE)) {
    stop("Package 'rollama' is required for this function. Please install it.")
  }

  ping_res <- try(rollama::ping_ollama(), silent = TRUE)
  if (!ping_res[[1]]) stop("Ollama is not running.")

  example_resp <- tibble::tibble(
    text   = example_comm_text,
    answer = example_comm_answer
  )

  # -------------------------------------------------------------------------
  # Stage 1: Label each slice
  # -------------------------------------------------------------------------
  res_all      <- list()
  comm_current <- comm_labels

  for (retry in 0:retries) {

    if (verbose) cli::cli_inform("Retry round {retry}. Querying {nrow(comm_current)} communities...")

   queries <- rollama::make_query(
      text = paste0("\n### EXAMPLES END ###\n\n", ### Clarify example boundaries to prevent context contamination
                    "### NEW INPUT ###\n",
                    "Annotate: \n",
                    comm_current$text_slice),
      template = "\n{text}\n{prompt}",
      prompt = prompt_comm,
      system = paste0(system_comm, 
                      "\n### EXAMPLES START ###\n"),
      example = example_resp
    )

    responses <- rollama::query(
      queries,
      model        = model,
      stream       = FALSE,
      model_params = list(seed = seed + retry, temperature = temp + retry * (2e-2)),
      format       = schema_comm,
      verbose      = TRUE
    )

    res_df <- parse_comm_responses(comm_current, responses, keep_description_json)

    res_all[[retry + 1]] <- res_df

    if (all(res_df$is_valid_json)) {
      if (verbose) cli::cli_inform("All answers parsed successfully.")
      break
    }

    failed_ids <- dplyr::filter(res_df, !is_valid_json)$id
    if (length(failed_ids) == 0 || retry == retries) {
      if (verbose) cli::cli_inform("Stopping after retry {retry}.")
      break
    }

    comm_current <- comm_labels |>
      dplyr::filter(id %in% failed_ids) |>
      dplyr::mutate(text_slice = stringr::str_trunc(text_slice, retry_trunc))
  }

  res_df <- dplyr::bind_rows(res_all) |>
    dplyr::group_by(id) |>
    dplyr::slice_max(order_by = is_valid_json, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::arrange(community)

  res_dt <- data.table::as.data.table(res_df)

  # Split sliced vs. unsliced communities
  sliced_dt   <- res_dt[res_dt[, .N, by = community][N > 1],  on = "community"]
  unsliced_dt <- res_dt[res_dt[, .N, by = community][N == 1], on = "community"]

  # -------------------------------------------------------------------------
  # Stage 2: Aggregate sliced communities
  # -------------------------------------------------------------------------
  if (nrow(sliced_dt) > 0) {

    if (verbose) cli::cli_inform("Aggregating {nrow(sliced_dt)} slices.")

    sliced_dt <- sliced_dt[nchar(text_slice) >= min_chars]

    sliced_dt_agg <- sliced_dt[
      , .(
        sum_user_share_comm_total = sum(sum_user_share_comm, na.rm = TRUE),
        text = jsonlite::toJSON(
          lapply(seq_len(.N), function(i) {
            parsed <- tryCatch(jsonlite::fromJSON(response[i]), error = function(e) list())
            parsed$confidence <- NULL
            c(
              list(
                slice = slice[i],
                share = round(sum_user_share_comm[i] * 100, 1)
              ),
              parsed
            )
          }),
          auto_unbox = TRUE,
          pretty     = FALSE
        )
      ),
      by = community
    ]

    example_slices_resp <- tibble::tibble(
      text   = example_comm_slices_text,
      answer = example_comm_slices_answer
    )

    res_all_slices  <- list()
    comm_current    <- sliced_dt_agg

    for (retry in 0:retries) {

      if (verbose) cli::cli_inform("Retry round {retry}. Aggregating labels for {nrow(comm_current)} communities...")
      
      queries <- rollama::make_query(
        text = paste0("\n### EXAMPLES END ###\n\n", ### Clarify example boundaries to prevent context contamination
                      "### NEW INPUT ###\n",
                      "Annotate: \n",
                      comm_current$text),
        template = "\n{text}\n{prompt}",
        prompt = prompt_comm,
        system = paste0(system_comm_slices, 
                        "\n### EXAMPLES START ###\n"),
        example = example_slices_resp
      )

      responses <- rollama::query(
        queries,
        model        = model,
        stream       = FALSE,
        model_params = list(seed = seed + retry, temperature = temp),
        format       = schema_comm,
        verbose      = TRUE
      )

      res_df <- parse_comm_responses(comm_current, responses, keep_description_json)

      res_all_slices[[retry + 1]] <- res_df

      if (all(res_df$is_valid_json)) {
        if (verbose) cli::cli_inform("All answers parsed successfully.")
        break
      }

      failed_ids <- dplyr::filter(res_df, !is_valid_json)$community
      if (length(failed_ids) == 0 || retry == retries) {
        if (verbose) cli::cli_inform("Stopping after retry {retry}.")
        break
      }

      comm_current <- sliced_dt_agg |>
        dplyr::filter(community %in% failed_ids) |>
        dplyr::mutate(text = stringr::str_trunc(text, retry_trunc))
    }

    res_df <- dplyr::bind_rows(res_all_slices) |>
      dplyr::group_by(community) |>
      dplyr::slice_max(order_by = is_valid_json, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::arrange(community)

    sliced_dt <- data.table::as.data.table(res_df)

    # Harmonize variable names before binding
    sliced_dt[, sum_user_share_comm := sum_user_share_comm_total][
      , c("sum_user_share_comm_total") := NULL
    ]
    unsliced_dt[, text := text_slice][
      , c("text_slice", "id", "slice", "n", "N") := NULL
    ]

    res_dt <- data.table::rbindlist(
      list(sliced_dt, unsliced_dt),
      use.names = TRUE, fill = TRUE, ignore.attr = TRUE
    )

  } else {
    res_dt <- unsliced_dt[, text := text_slice][
      , c("text_slice", "id", "slice", "n", "N") := NULL
    ]
  }

  res_dt <- merge(res_dt, comm_stats, by = "community", all.x = TRUE)

  # Define column order
  main_cols <- c(
    "community", "n_accs", "n_posts", "sum_user_share_comm",
    "label", "description"
  )
  end_cols <- c(
    "is_valid_json", "response", "text",
    "model", "model_queried_at", "model_total_duration"
  )
  other_cols      <- setdiff(names(res_dt), c(main_cols, end_cols))
  final_col_order <- c(main_cols, other_cols, end_cols)
  res_dt          <- res_dt[, ..final_col_order]

  groups_data$community_labels <- res_dt

  return(groups_data)
}