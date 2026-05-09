test_that("label_users handles mid-batch rollama streaming failure gracefully", {

  mock_user_labels <- data.table::data.table(
    account_id         = c("user_1", "user_2", "user_3"),
    community          = c(1L, 1L, 1L),
    account_name_clean = c("alice", "bob", "carol"),
    user_share_comm    = c(0.5, 0.3, 0.2),
    text               = c("some text about climate", "another post about politics", "third user post")
  )
  mock_groups_data <- list(user_labels = mock_user_labels)

  valid_response <- '{"description":"Test user.","lang":"en","topic":["politics"],"named_entities":[],"repetitive_patterns":[],"incivility":"no","elaborate":"yes","confidence":0.9}'

  call_count <- 0
  mock_query_per_user <- function(queries, ...) {
    call_count <<- call_count + 1
    if (call_count == 2) stop("simulated stream error at index 2")
    list(list(
      message        = list(content = valid_response),
      model          = "llama3.1:8b",
      created_at     = as.character(Sys.time()),
      total_duration = 1e9
    ))
  }

  testthat::with_mocked_bindings(
    ping_ollama = function(...) list(TRUE),
    query       = mock_query_per_user,
    .package    = "rollama",
    code = {
      result <- label_users(mock_groups_data, retries = 0, verbose = FALSE)
      ul <- result$user_labels

      expect_equal(nrow(ul), 3L)
      expect_false(ul[ul$account_id == "user_2", ]$is_valid_json)
      expect_false(is.na(ul[ul$account_id == "user_2", ]$error_msg))
      expect_true(is.na(ul[ul$account_id == "user_2", ]$description))
      expect_true(is.na(ul[ul$account_id == "user_2", ]$lang))
      expect_false(is.na(ul[ul$account_id == "user_1", ]$description))
      expect_false(is.na(ul[ul$account_id == "user_3", ]$description))
      expect_true(ul[ul$account_id == "user_1", ]$is_valid_json)
      expect_true(ul[ul$account_id == "user_3", ]$is_valid_json)
    }
  )
})

test_that("label_users treats empty JSON object response as invalid", {

  mock_user_labels <- data.table::data.table(
    account_id         = c("user_1"),
    community          = c(1L),
    account_name_clean = c("alice"),
    user_share_comm    = c(1.0),
    text               = c("some text")
  )
  mock_groups_data <- list(user_labels = mock_user_labels)

  mock_query_empty <- function(...) {
    list(list(
      message        = list(content = "{}"),
      model          = "llama3.1:8b",
      created_at     = as.character(Sys.time()),
      total_duration = NA_real_
    ))
  }

  testthat::with_mocked_bindings(
    ping_ollama = function(...) list(TRUE),
    query       = mock_query_empty,
    .package    = "rollama",
    code = {
      result <- label_users(mock_groups_data, retries = 0, verbose = FALSE)
      ul <- result$user_labels

      expect_false(ul$is_valid_json)
      expect_false(is.na(ul$error_msg))
      expect_true(is.na(ul$description))
    }
  )
})