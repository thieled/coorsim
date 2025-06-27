test_that("coorsim_detect_groups returns expected structure for small toy data", {
  dt <- data.table::data.table(
    post_id = c("p1", "p2", "p3"),
    post_id_y = c("p2", "p3", "p1"),
    account_id = c("a1", "a2", "a3"),
    account_id_y = c("a2", "a3", "a1"),
    time = as.POSIXct(c("2023-01-01", "2023-01-01", "2023-01-01")),
    time_y = as.POSIXct(c("2023-01-01", "2023-01-01", "2023-01-01")),
    similarity = c(0.9, 0.95, 0.92),
    content = c("x", "y", "z"),
    content_y = c("y", "z", "x")
  )
  
  user_dt <- data.table::data.table(
    account_id = c("a1", "a2", "a3"),
    account_name = c("User1", "User2", "User3")
  )
  
  res <- coorsim_detect_groups(
    simdt = dt,
    user_data = user_dt,
    cluster_method = "label_prop",
    verbose = FALSE,
    return_post_dt = TRUE
  )
  
  expect_named(res, c("graph", "communities", "edge_list", "sim_dt", "node_list", "post_data", "params"))
  expect_s3_class(res$graph, "igraph")
  expect_true("community" %in% names(res$node_list))
  expect_true("post_id" %in% names(res$post_data))
})
