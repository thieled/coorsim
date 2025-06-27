test_that("coorsim_prepare_data standardizes and filters correctly", {
  df <- data.frame(
    pid = c("a", "b"),
    accid = c("x", "y"),
    t = c("2023-01-01", "2023-01-02"),
    txt = c("text one", "text two")
  )
  
  out <- coorsim_prepare_data(
    data = df,
    post_id = "pid",
    account_id = "accid",
    time = "t",
    content = "txt",
    verbose = FALSE
  )
  
  expect_true("post_id" %in% names(out))
  expect_true(is.numeric(out$time))
})

test_that("coorsim_match_overlaps returns non-empty list with correct structure", {
  df <- data.table::data.table(
    post_id = c("p1", "p2", "p3"),
    account_id = c("a1", "a2", "a3"),
    time = as.numeric(as.POSIXct(c("2023-01-01 00:00:00", "2023-01-01 00:00:30", "2023-01-01 00:01:00"))),
    content = c("x", "y", "z")
  )
  df[, end_time := time + 60]
  df[, time_dup := time]
  data.table::setkey(df, time, end_time)
  
  out <- coorsim_match_overlaps(df)
  
  expect_type(out, "list")
  expect_true(all(names(out) %in% df$post_id))
  expect_true(all(vapply(out, is.character, TRUE)))
})

test_that("detect_cosimilarity returns data.table with similarity values", {
  df <- data.frame(
    post_id = c("p1", "p2", "p3"),
    account_id = c("a1", "a2", "a3"),
    time = c("2023-01-01 00:00:00", "2023-01-01 00:00:30", "2023-01-01 00:01:00"),
    content = c("hi there", "hi there", "different text")
  )
  
  out <- detect_cosimilarity(
    data = df,
    embeddings = NULL,
    time_window = 60,
    min_simil = 0.1,
    min_participation = 1,
    verbose = FALSE,
    parallel = FALSE
  )
  
  expect_true("similarity" %in% names(out))
  expect_true(nrow(out) > 0)
})
