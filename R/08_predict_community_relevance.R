#' Load the default XGBoost model included in the package
#'
#' This function loads a pre-trained `xgboost::xgb.Booster` model stored in `inst/extdata/final_model.xgb`.
#'
#' @return An `xgb.Booster` object.
#' @export
#'
load_xgb_model <- function() {
  
  required_pkgs <- c("xgboost")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required for this function. Please install it using install.packages(\"", pkg, "\")."), call. = FALSE)
    }
  }
  
  path <- system.file("extdata", "xgb_pairw_rank_twitter.xgb", package = "coorsim")
  if (path == "") stop("Model file not found in package.")
  xgboost::xgb.load(path)
}


#' Preprocess Community Metrics for Inference
#'
#' Prepares the output of `coorsim::get_community_metrics()` for inference using a trained XGBoost ranking model.
#' This includes selection and ordering of training features, log1p transformation of skewed variables,
#' one-hot encoding of categorical variables, NA handling, and feature padding to match training layout.
#'
#' @param groups_data A named list returned by `coorsim::get_community_metrics()`, including elements `community_metrics` and `params`.
#' @param train_cols A character vector of feature names used during training. Defaults to the complete set of known training features.
#' @param categorical_cols A character vector of categorical variable base names that were one-hot encoded in training.
#' @param log_cols A character vector of numeric variables that should be log1p-transformed (if present).
#' @param verbose Logical. If `TRUE`, prints messages during preprocessing. Default is `TRUE`.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{`X`}{A numeric matrix of predictors, matching training features and column order.}
#'   \item{`community_ids`}{A numeric vector of community identifiers preserved from the input.}
#' }
#'
#' @details
#' - Missing variables are padded as `NA_real_` or `0` for categorical dummy variables.
#' - Log1p transformations are only applied to variables listed in `log_cols` that are present in the data.
#' - Categorical variables are encoded using `mltools::one_hot()` and dot characters in resulting column names are replaced with underscores to ensure SHAP compatibility.
#'
#' @export
preprocess_for_inference <- function(groups_data,
                                     train_cols = c(
                                       "g_nodes_n", "g_degree_mean", "g_edge_weight_mean", "g_transitivity", "g_diameter", "g_mean_distance",
                                       "t_time_diff_mean", "s_duplication_ratio", "a_npost_mean", "a_npost_sd", "c_posts_nchar_mean",
                                       "c_posts_ntoken_mean", "c_posts_ntype_mean", "c_posts_token_length_mean", "c_posts_tokens_gt6_ratio_mean",
                                       "c_posts_punct_ratio_mean", "c_posts_emoji_ratio_mean", "c_posts_nchar_sd", "c_posts_ntoken_sd",
                                       "c_posts_ntype_sd", "c_posts_token_length_sd", "c_posts_tokens_gt6_ratio_sd", "c_posts_punct_ratio_sd",
                                       "c_posts_emoji_ratio_sd", "c_bio_nchar_mean", "c_bio_ntoken_mean", "c_bio_ntype_mean", "c_bio_ttr_mean",
                                       "c_bio_token_length_mean", "c_bio_tokens_gt6_ratio_mean", "c_bio_punct_ratio_mean", "c_bio_nchar_sd",
                                       "c_bio_ntoken_sd", "c_bio_ntype_sd", "c_bio_ttr_sd", "c_bio_token_length_sd", "c_bio_tokens_gt6_ratio_sd",
                                       "c_bio_punct_ratio_sd", "cluster_method_FSA_V", "cluster_method_label_prop", "cluster_method_louvain",
                                       "resolution_0_25", "resolution_0_7", "resolution_1"
                                     ),
                                     categorical_cols = c("cluster_method", "resolution"),
                                     log_cols = c(
                                       "g_nodes_n", "g_degree_mean", "g_edge_weight_mean", "g_transitivity", "g_diameter", "g_mean_distance",
                                       "t_time_diff_mean", "a_npost_mean", "a_npost_sd", "c_posts_ntoken_mean", "c_posts_token_length_mean",
                                       "c_posts_nchar_sd", "c_posts_ntoken_sd", "c_posts_ntype_sd", "c_posts_token_length_sd",
                                       "c_bio_token_length_mean", "c_bio_nchar_sd", "c_bio_ntoken_sd", "c_bio_token_length_sd"
                                     ),
                                     verbose = T) {
  
  if(!"community_metrics" %in% names(groups_data)) stop("Please first run 'coorsim::get_community_metrics()'.")
  
  # Require packages
  required_pkgs <- c("mltools")
  
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required for this function. Please install it using install.packages(\"", pkg, "\")."), call. = FALSE)
    }
  }
  
  
  dt <- data.table::copy(groups_data$community_metrics)
  
  # Preserve community ID
  community_ids <- dt$community
  
  # Filter train cols to keep cols (converting one hot encoded cat. cols)
  filter_keep_cols <- function(keep_cols, categorical_cols) {
    # Build regex pattern: ^(cat1|cat2|...)
    pattern <- paste0("^(", paste0(categorical_cols, collapse = "|"), ")")
    keep_cols <- keep_cols[!grepl(pattern, keep_cols)]
    
    keep_cols <- c(keep_cols, categorical_cols)
    return(keep_cols)
  }
  
  keep_cols <-  filter_keep_cols(train_cols, categorical_cols)
  
  # Drop ID-like or unwanted columns
  dt <- dt[, intersect(keep_cols, names(dt)), with = FALSE]
  
  
  # Clean list of log cols
  log_cols <- intersect(log_cols, keep_cols)
  
  # Replace NAs in _sd variables by 0
  replace_sd_na_with_zero <- function(dt) {
    sd_cols <- grep("_sd$", names(dt), value = TRUE)
    for (col in sd_cols) {
      data.table::set(dt, which(is.na(dt[[col]])), col, 0)
    }
    invisible(dt)
  }
  dt <- replace_sd_na_with_zero(dt)
  
  # Get resolution and cluster_method - and impute NAs like in training
  dt[, resolution := groups_data$params$resolution][, resolution := ifelse(is.na(resolution), 1, as.numeric(resolution))]
  dt[, cluster_method := groups_data$params$cluster_method]
  
  # Calculate the N communities detected per run
  dt[, n_comm := dt[, .N]]
  
  # Log1p() the same cols as for training
  if(!is.null(log_cols)){
    
    if (verbose && length(log_cols) > 0) {
      message("Applying log1p() to columns: ", paste(log_cols, collapse = ", "))
    }
    
    for (col in log_cols) {
      dt[, (col) := log1p(get(col))]
    }
    
  }
  # One-hot encode categorical variables
  if (!is.null(categorical_cols)) {
    for (col in categorical_cols) {
      if (col %in% names(dt)) {
        dt[[col]] <- as.factor(dt[[col]])
      }
    }
    dt <- mltools::one_hot(dt, cols = categorical_cols)
    
    # Replace dots in new colnames with underscores
    data.table::setnames(dt, old = names(dt), new = gsub("\\.", "_", names(dt)))
  }
  
  # Create columns that were in train data but not in unseen data
  missing_cols <- setdiff(train_cols, names(dt))
  
  for (col in missing_cols) {
    # If column starts with any categorical base name → one-hot encoded dummy
    is_categorical <- any(startsWith(col, categorical_cols))
    dt[[col]] <- if (is_categorical) 0 else NA_real_
  }
  
  # Reorder columns
  dt <- dt[, train_cols, with = FALSE]  # if using data.table
  
  X <- as.matrix(dt)
  
  l <- list(X = X,
            community_ids = community_ids)
  
  return(l)
}




#' Predict ranking scores for communities using a trained XGBoost model
#'
#' @param groups_data A named list returned by `coorsim::get_community_metrics()`, including elements `community_metrics` and `params`.
#' @param model Trained `xgboost::xgb.Booster` object (optional). If `NULL`, loads default model from package.
#' @param verbose Logical. If `TRUE`, prints messages during preprocessing. Default is `TRUE`.
#' 
#' @return A `groups_data` list, with `pred_relevance` -- a indicator ranging from 0 = irrelevant, to 6 = most relevatnt, `pred_relevance_score`, 
#' `pred_relevance_rank` added to `community_metrics` and `post_data`.
#' @export
predict_community_relevance <- function(groups_data, 
                                     model = NULL, 
                                     verbose = TRUE) {
  
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Please install 'xgboost' to use this function.")
  }
  
  # Load model from package if not provided
  if (is.null(model)) {
    model <- load_xgb_model()
    if (verbose) message("Loaded default XGBoost model from package.")
  }
  
  # Preprocess data
  inf_data <- preprocess_for_inference(
    groups_data = groups_data,  
    verbose = verbose
  )
  
  dtest <- xgboost::xgb.DMatrix(data = inf_data$X)
  scores <- stats::predict(model, dtest)
  
  # Try quantile bins (mirroring training)
  probs <- c(0, 0.6, 0.7, 0.8, 0.9, 0.95, 1)
  q <- stats::quantile(scores, probs = probs, na.rm = TRUE)
  breaks_unique <- length(unique(q)) == length(q)
  
  labels <- if (breaks_unique) {
    as.integer(cut(scores, breaks = q, include.lowest = TRUE, labels = FALSE)) - 1L
  } else {
    # fallback: linear scaling to 0–6
    if (stats::sd(scores, na.rm = TRUE) > 0) {
      scaled <- (scores - min(scores, na.rm = TRUE)) / (max(scores, na.rm = TRUE) - min(scores, na.rm = TRUE))
      round(6 * scaled)
    } else {
      rep(0L, length(scores))
    }
  }
  
  result <- data.table::data.table(
    community = inf_data$community_ids,
    pred_relevance_score = scores,
    pred_relevance_rank = data.table::frank(-scores, ties.method = "average"),
    pred_relevance = labels
  )
  
  # Merge to community_metrics and post_data
  groups_data$community_metrics <- merge(groups_data$community_metrics, result, by = "community", all.x = T)
  groups_data$post_data <- merge(groups_data$post_data, result, by = "community", all.x = T)
  
  
  return(groups_data)
}


