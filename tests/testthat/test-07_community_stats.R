testthat::test_that("get_community_metrics computes core network metrics correctly", {
  # --- Minimal reproducible groups_data setup ---
  edge_list <- data.table::data.table(
    account_id = c("a", "a", "b", "c"),
    account_id_y = c("b", "c", "c", "d"),
    weight = c(1, 2, 3, 1)
  )
  
  node_list <- data.table::data.table(
    account_id = letters[1:4],
    account_name = paste0("acc_", letters[1:4]),
    community = c(1, 1, 1, 2),
    follower = c(100, 1000, 50000, 5),
    following = c(100, 200, 300, 10),
    account_creation_date = as.POSIXct("2024-01-01", tz = "UTC"),
    account_description = c("Hello world", "News updates", "Politics", "Private user")
  )
  
  g <- igraph::graph_from_data_frame(
    d = edge_list,
    vertices = node_list[, .(name = account_id)],
    directed = FALSE
  )
  comm <- igraph::cluster_louvain(g)
  
  sim_dt <- data.table::data.table(
    account_id = c("a", "a", "b", "c"),
    account_id_y = c("b", "c", "c", "d"),
    similarity = c(0.95, 0.9, 0.8, 0.7),
    time_diff = c(10, 20, 30, 40),
    post_id = paste0("p", 1:4),
    post_id_y = paste0("p", 2:5),
    time = as.POSIXct("2025-01-01", tz = "UTC") + 1:4,
    time_y = as.POSIXct("2025-01-01", tz = "UTC") + 5:8,
    content = paste("post", 1:4),
    content_y = paste("post", 2:5),
    param_time_window = list(200),
    param_min_simil = list(0.5)
  )
  
  groups <- list(
    graph = g,
    communities = comm,
    node_list = node_list,
    sim_dt = sim_dt
  )
  
  # --- Run ---
  res <- get_community_metrics(groups, content_stats = FALSE, verbose = FALSE)
  
  metrics <- res$community_metrics
  
  # --- Assertions ---
  expect_s3_class(metrics, "data.table")
  expect_true("community" %in% names(metrics))
  expect_true(all(c("g_nodes_n", "g_edges_n", "g_degree_mean") %in% names(metrics)))
  expect_true(anyDuplicated(metrics$community) == 0)
  expect_true(all(metrics$g_edge_density >= 0 & metrics$g_edge_density <= 1))
  expect_setequal(metrics$community, sort(unique(igraph::membership(comm))))
})



testthat::test_that("get_community_metrics computes account-level inequality and robustness metrics", {
  edge_list <- data.table::data.table(
    account_id = c("a", "a", "b"),
    account_id_y = c("b", "c", "c"),
    weight = c(1, 2, 3)
  )
  
  node_list <- data.table::data.table(
    account_id = c("a", "b", "c"),
    community = c(1, 1, 1),
    follower = c(10, 1000, 100000),
    following = c(1, 100, 100),
    account_description = c("a", "b", "c"),
    account_creation_date = as.POSIXct("2024-01-01", tz = "UTC")
  )
  
  g <- igraph::graph_from_data_frame(edge_list, vertices = node_list[, .(name = account_id)], directed = FALSE)
  comm <- igraph::cluster_louvain(g)
  
  sim_dt <- data.table::data.table(
    account_id = c("a", "a", "b"),
    account_id_y = c("b", "c", "c"),
    similarity = c(0.9, 0.8, 0.7),
    time_diff = c(10, 20, 30),
    post_id = paste0("p", 1:3),
    post_id_y = paste0("p", 2:4),
    time = as.POSIXct("2025-01-01", tz = "UTC") + 1:3,
    time_y = as.POSIXct("2025-01-01", tz = "UTC") + 4:6,
    content = c("post1", "post2", "post3"),
    content_y = c("post4", "post5", "post6"),
    param_time_window = list(200),
    param_min_simil = list(0.5)
  )
  
  groups <- list(
    graph = g,
    communities = comm,
    node_list = node_list,
    sim_dt = sim_dt
  )
  
  res <- get_community_metrics(groups, content_stats = FALSE, verbose = FALSE)
  metrics <- res$community_metrics
  
  expect_true("a_followers_gini" %in% names(metrics))
  expect_true(all(metrics$a_followers_gini > 0 & metrics$a_followers_gini < 1))
  expect_true(any(grepl("a_ffr_mean", names(metrics))))
  expect_true(metrics$a_ffr_mean[1] > 0)
})



testthat::test_that("get_community_metrics computes temporal metrics from post times", {
  edge_list <- data.table::data.table(
    account_id = c("a", "b"),
    account_id_y = c("b", "c"),
    weight = c(1, 2)
  )
  
  node_list <- data.table::data.table(
    account_id = c("a", "b", "c"),
    community = c(1, 1, 2),
    follower = c(10, 20, 30),
    following = c(2, 2, 2),
    account_creation_date = as.POSIXct("2024-01-01", tz = "UTC")
  )
  
  g <- igraph::graph_from_data_frame(edge_list, vertices = node_list[, .(name = account_id)], directed = FALSE)
  comm <- igraph::cluster_louvain(g)
  
  sim_dt <- data.table::data.table(
    account_id = c("a", "b"),
    account_id_y = c("b", "c"),
    similarity = c(0.9, 0.8),
    time_diff = c(10, 20),
    post_id = c("p1", "p2"),
    post_id_y = c("p3", "p4"),
    time = as.POSIXct("2025-01-01", tz = "UTC") + c(1, 2),
    time_y = as.POSIXct("2025-01-01", tz = "UTC") + c(5, 6),
    content = c("post1", "post2"),
    content_y = c("post3", "post4"),
    param_time_window = list(200),
    param_min_simil = list(0.5)
  )
  
  groups <- list(
    graph = g,
    communities = comm,
    node_list = node_list,
    sim_dt = sim_dt
  )
  
  res <- get_community_metrics(groups, content_stats = FALSE, verbose = FALSE)
  metrics <- res$community_metrics
  
  expect_true(all(c("t_half_life_h", "t_coef_var_3h", "t_entropy_3h") %in% names(metrics)))
  expect_true(all(sapply(metrics[, .(t_half_life_h, t_coef_var_3h, t_entropy_3h)], is.numeric)))
  expect_true(all(metrics$t_entropy_3h >= 0))
})



testthat::test_that("get_community_metrics merges all metric domains correctly", {
  edge_list <- data.table::data.table(
    account_id = c("a", "b"),
    account_id_y = c("b", "c"),
    weight = c(1, 1)
  )
  
  node_list <- data.table::data.table(
    account_id = c("a", "b", "c"),
    community = c(1, 1, 2),
    follower = c(1, 10, 100),
    following = c(1, 1, 1),
    account_creation_date = as.POSIXct("2024-01-01", tz = "UTC")
  )
  
  g <- igraph::graph_from_data_frame(edge_list, vertices = node_list[, .(name = account_id)], directed = FALSE)
  comm <- igraph::cluster_louvain(g)
  
  sim_dt <- data.table::data.table(
    account_id = c("a", "b"),
    account_id_y = c("b", "c"),
    similarity = c(0.9, 0.8),
    time_diff = c(10, 20),
    post_id = c("p1", "p2"),
    post_id_y = c("p3", "p4"),
    time = as.POSIXct("2025-01-01", tz = "UTC") + c(1, 2),
    time_y = as.POSIXct("2025-01-01", tz = "UTC") + c(5, 6),
    content = c("post1", "post2"),
    content_y = c("post3", "post4"),
    param_time_window = list(200),
    param_min_simil = list(0.5)
  )
  
  groups <- list(
    graph = g,
    communities = comm,
    node_list = node_list,
    sim_dt = sim_dt
  )
  
  res <- get_community_metrics(groups, content_stats = FALSE, verbose = FALSE)
  metrics <- res$community_metrics
  
  expect_true(all(c("g_nodes_n", "s_similarity_mean", "t_half_life_h", "a_followers_gini") %in% names(metrics)))
})
