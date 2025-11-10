test_that("filter_groups_data correctly filters by edge_weight", {
  
  # Build a small reproducible test graph
  edge_dt <- data.table::data.table(
    account_id =   c("a", "a", "b", "c", "d"),
    account_id_y = c("b", "c", "c", "d", "e"),
    weight = c(1, 2, 3, 1, 5)
  )
  
  node_dt <- data.table::data.table(
    account_id = letters[1:5],
    account_name = paste0("acc_", letters[1:5]),
    community = c(1, 1, 1, 2, 2)
  )
  
  g <- igraph::graph_from_data_frame(d = edge_dt, vertices = node_dt, directed = FALSE)
  
  comm <- igraph::cluster_louvain(g)
  
  groups_data <- list(
    graph = g,
    communities = comm,
    edge_list = edge_dt,
    node_list = node_dt,
    params = list(min_comm_size = 1)
  )
  
  # Apply edge weight filter
  res <- coorsim::filter_groups_data(groups_data, edge_weight = 3, verbose = FALSE)
  
  # 1. All remaining edges have weight >= 3
  expect_true(all(res$edge_list$weight >= 3))
  
  # 2. Graph edges match edge_list pairs (unordered)
  g_edges <- igraph::as_data_frame(res$graph, what = "edges")[, c("from", "to")]
  g_edges <- apply(g_edges, 1, function(x) paste(sort(x), collapse = "_"))
  edge_pairs <- apply(res$edge_list[, .(account_id, account_id_y)], 1, function(x) paste(sort(x), collapse = "_"))
  expect_setequal(edge_pairs, g_edges)
  
  # 3. Nodes in node_list correspond exactly to graph vertices
  expect_setequal(res$node_list$account_id, igraph::V(res$graph)$name)
  
  # 4. Filter removed expected accounts (those only connected by edges with weight < 3)
  expect_false("a" %in% res$node_list$account_id)
  expect_true("d" %in% res$node_list$account_id)
  expect_true("e" %in% res$node_list$account_id)
})


test_that("filter_groups_data correctly filters by_col/by_val/by_condition", {
  # --- Setup test data ---
  post_data <- data.table::data.table(
    post_id = paste0("p", 1:10),
    account_id = rep(letters[1:5], each = 2),
    lang = rep(c("en", "de"), 5),
    score = 1:10
  )
  
  sim_dt <- data.table::data.table(
    post_id = rep(paste0("p", 1:9), each = 1),
    post_id_y = paste0("p", 2:10),
    account_id = rep(letters[1:9], each = 1),
    account_id_y = letters[2:10],
    similarity = runif(9)
  )
  
  edge_list <- sim_dt[
    , .(account_id = pmin(account_id, account_id_y),
        account_id_y = pmax(account_id, account_id_y))
  ][, .(weight = .N), by = .(account_id, account_id_y)]
  
  node_list <- data.table::data.table(
    account_id = letters[1:10],
    account_name = paste0("acc_", letters[1:10]),
    community = rep(1:2, each = 5)
  )
  
  g <- igraph::graph_from_data_frame(edge_list, vertices = node_list, directed = FALSE)
  comm <- igraph::cluster_louvain(g)
  
  groups_data <- list(
    graph = g,
    communities = comm,
    edge_list = edge_list,
    node_list = node_list,
    sim_dt = sim_dt,
    post_data = post_data,
    params = list(min_comm_size = 1)
  )
  
  # Test 1: by_condition = "equal" 
  res_equal <- filter_groups_data(
    groups_data = groups_data,
    by_col = "lang",
    by_val = "en",
    by_condition = "equal",
    verbose = FALSE
  )
  
  expect_true(all(res_equal$post_data$lang == "en"))
  expect_true(all(res_equal$sim_dt$post_id %in% res_equal$post_data$post_id))
  expect_true(nrow(res_equal$edge_list) <= nrow(groups_data$edge_list))
  
  # Test 2: by_condition = "greater" 
  res_greater <- filter_groups_data(
    groups_data = groups_data,
    by_col = "score",
    by_val = 5,
    by_condition = "greater",
    verbose = FALSE
  )
  
  expect_true(all(res_greater$post_data$score > 5))
  expect_true(all(res_greater$sim_dt$post_id %in% res_greater$post_data$post_id))
  expect_true(nrow(res_greater$edge_list) <= nrow(groups_data$edge_list))
  
  # Test 3: by_condition = "smaller"
  res_smaller <- filter_groups_data(
    groups_data = groups_data,
    by_col = "score",
    by_val = 5,
    by_condition = "smaller",
    verbose = FALSE
  )
  
  expect_true(all(res_smaller$post_data$score < 5))
  expect_true(all(res_smaller$sim_dt$post_id %in% res_smaller$post_data$post_id))
  expect_true(nrow(res_smaller$edge_list) <= nrow(groups_data$edge_list))
  
  # Consistency checks for all outputs 
  for (res in list(res_equal, res_greater, res_smaller)) {
    expect_s3_class(res$graph, "igraph")
    expect_true(all(res$node_list$account_id %in% igraph::V(res$graph)$name))
    expect_true(all(res$edge_list$account_id %in% res$node_list$account_id))
  }
})


test_that("filter_groups_data filters by time_window (no rerun) and stays consistent", {
  post_data <- data.table::data.table(
    post_id = paste0("p", 1:7),
    account_id = letters[1:7],
    time = as.POSIXct("2025-01-01", tz = "UTC") + 1:7,
    content = paste("txt", 1:7),
    community = rep(1:2, length.out = 7),
    algorithm = "test"
  )
  
  # Create pairs p1-p2, p2-p3, ..., p6-p7 with varying time_diff
  sim_dt <- data.table::data.table(
    post_id = paste0("p", 1:6),
    post_id_y = paste0("p", 2:7),
    account_id = letters[1:6],
    account_id_y = letters[2:7],
    time_diff = c(10, 30, 60, 90, 120, 150),
    similarity = c(0.9, 0.85, 0.8, 0.75, 0.7, 0.65),
    time = post_data$time[1:6],
    time_y = post_data$time[2:7],
    content = post_data$content[1:6],
    content_y = post_data$content[2:7],
    param_time_window = list(200),
    param_min_simil = list(0.5)
  )
  
  edge_list <- sim_dt[
    , .(account_id = pmin(account_id, account_id_y),
        account_id_y = pmax(account_id, account_id_y))
  ][, .(weight = .N), by = .(account_id, account_id_y)]
  
  node_list <- data.table::data.table(
    account_id = letters[1:7],
    account_name = paste0("acc_", letters[1:7]),
    community = rep(1:2, length.out = 7),
    algorithm = "test"
  )
  
  g <- igraph::graph_from_data_frame(edge_list, vertices = node_list, directed = FALSE)
  comm <- igraph::cluster_louvain(g)
  
  groups_data <- list(
    graph = g,
    communities = comm,
    edge_list = edge_list,
    node_list = node_list,
    sim_dt = sim_dt,
    post_data = post_data,
    params = list(
      min_comm_size = 1,
      cluster_method = "louvain",
      resolution = 1,
      theta = 0.5
    )
  )
  
  res <- filter_groups_data(
    groups_data = groups_data,
    time_window = 60,      # stricter than 200
    verbose = FALSE,
    rerun_detect_groups = T
  )
  
  # 1) All remaining pairs honor the window
  testthat::expect_true(all(res$sim_dt$time_diff <= 60))
  
  # 2) Pairs decreased
  testthat::expect_true(nrow(res$sim_dt) < nrow(groups_data$sim_dt))
  
  # 3) edge_list comes only from remaining sim_dt
  valid_pairs <- unique(data.table::data.table(
    account_id = pmin(res$sim_dt$account_id, res$sim_dt$account_id_y),
    account_id_y = pmax(res$sim_dt$account_id, res$sim_dt$account_id_y)
  ))
  testthat::expect_true(all(
    paste(res$edge_list$account_id, res$edge_list$account_id_y) %in%
      paste(valid_pairs$account_id, valid_pairs$account_id_y)
  ))
  
  # 4) node_list matches remaining sim_dt nodes
  remaining_nodes <- unique(c(res$sim_dt$account_id, res$sim_dt$account_id_y))
  testthat::expect_setequal(res$node_list$account_id, remaining_nodes)
  
  # 5) Graph vertices match node_list
  testthat::expect_setequal(igraph::V(res$graph)$name, res$node_list$account_id)
  
  # 6) Communities reference existing vertices
  testthat::expect_true(all(names(igraph::membership(res$communities)) %in% res$node_list$account_id))
})

