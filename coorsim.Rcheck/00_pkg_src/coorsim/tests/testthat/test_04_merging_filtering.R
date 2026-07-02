test_that("augment_groups_data merges info into node_list and post_data", {
  
  dt <- data.table::data.table(
    post_id = c("p1", "p2", "p3"),
    post_id_y = c("p2", "p3", "p1"),
    account_id = c("a1", "a2", "a3"),
    account_id_y = c("a2", "a3", "a1"),
    time = as.POSIXct(c("2023-01-01", "2023-01-01", "2023-01-01")),
    time_y = as.POSIXct(c("2023-01-01", "2023-01-01", "2023-01-01")),
    time_diff = c(1,2,3),
    similarity = c(0.9, 0.95, 0.92),
    content = c("x", "y", "z"),
    content_y = c("y", "z", "x")
  )
  
  user_dt <- data.table::data.table(
    account_id = c("a1", "a2", "a3"),
    account_name = c("User1", "User2", "User3")
  )
  
  groups <- coorsim_detect_groups(
    simdt = dt,
    user_data = user_dt,
    cluster_method = "label_prop",
    verbose = FALSE,
    return_post_dt = TRUE
  )
  
  post_data <- data.table::data.table(
    pid = c("p1", "p2"),
    accid = c("a1", "a2"),
    txt = c("hello", "world")
  )
  
  user_data <- data.table::data.table(
    uid = c("a1", "a2"),
    uname = c("User1", "User2"),
    dummy = c("x", "y")
  )
  
  res <- augment_groups_data(
    groups_data = groups,
    post_data = post_data,
    user_data = user_data,
    post_id = "pid",
    account_id = "uid",
    other_post_vars = "txt",
    other_user_vars = "dummy",
    sim_dt_community = TRUE,
    verbose = FALSE
  )
  
  expect_true(all(names(res) %in% c("graph", "communities", "edge_list", "sim_dt", "node_list", "post_data", "params")))
  expect_true("account_name" %in% names(res$node_list))
  expect_true("txt" %in% names(res$post_data))
  expect_true("community" %in% names(res$sim_dt))
})

test_that("filter_groups_data applies all filters", {
  dt <- data.table::data.table(
    post_id = c("p1", "p2", "p3", "p4", "p5", "p6"),
    post_id_y = c("p2", "p3", "p4", "p5", "p6", "p1"),
    account_id = c("a1", "a2", "a3", "a4", "a5", "a6"),
    account_id_y = c("a2", "a3", "a4", "a5", "a6", "a1"),
    time = as.POSIXct(rep("2023-01-01", 6)),
    time_y = as.POSIXct(rep("2023-01-01", 6)),
    time_diff = c(1,2,3,4,5,6),
    similarity = c(0.9, 0.91, 0.92, 0.93, 0.94, 0.95),
    content = letters[1:6],
    content_y = letters[2:7],
    param_embeddings = "none",
    param_time_window = 6, 
    param_min_simil = .9,
    param_min_participation = 1
  )
  
  user_dt <- data.table::data.table(
    account_id = paste0("a", 1:6),
    account_name = paste0("User", 1:6)
  )
  
  groups <- coorsim_detect_groups(
    simdt = dt,
    user_data = user_dt,
    cluster_method = "label_prop",
    verbose = FALSE,
    return_post_dt = TRUE
  )
  
  expect_equal(nrow(filter_groups_data(groups, by_col = "community", by_val = 1)$node_list) >= 1, TRUE)
  expect_equal(nrow(filter_groups_data(groups, edge_weight = 10)$edge_list), 0)
  expect_equal(nrow(filter_groups_data(groups, min_comm_size = 4)$node_list) >= 0, TRUE)
  expect_equal(nrow(filter_groups_data(groups, min_comp_size = 4)$node_list) >= 0, TRUE)
  expect_equal(nrow(filter_groups_data(groups, min_degree = 2)$node_list) >= 0, TRUE)
  expect_equal(nrow(filter_groups_data(groups, min_participation = 2)$node_list) >= 0, TRUE)
  expect_equal(nrow(filter_groups_data(groups, communities_index = 1)$node_list) >= 0, TRUE)
  expect_equal(nrow(filter_groups_data(groups, quantile_accountwise = 1)$node_list) >= 0, TRUE)
  expect_equal(nrow(filter_groups_data(groups, quantile_postwise = 1)$post_data) >= 0, TRUE)
  expect_equal(nrow(filter_groups_data(groups, top_n_accountwise = 1)$node_list) >= 0, TRUE)
  expect_equal(nrow(filter_groups_data(groups, top_n_postwise = 1)$post_data) >= 0, TRUE)
})
