#' Filter Data by Quantile Threshold
#'
#' This function filters a data frame or data.table by retaining rows where the specified variable is 
#' greater than or equal to a given quantile threshold. It supports filtering by the variable's name 
#' or by its index position.
#'
#' @param df A data frame or data.table to filter.
#' @param var_field Character or integer indicating the column to filter by. If character, it should match the column name; 
#' if integer, it should match the column index.
#' @param probs Numeric value between 0 and 1 indicating the quantile threshold for filtering. Rows with values 
#' in `var_field` greater than or equal to this quantile are retained.
#'
#' @return The filtered data frame or data.table containing rows where the specified variable is at or above the specified quantile.
#'
#' @export
filter_ntile <- function(df, var_field, probs){
  # Find index of variable
  var_index <- 0
  if (is.character(var_field)) {
    var_index <- match(var_field, names(df))
  } else {
    var_index <- match(var_field, seq(length(df)))
  }
  if (is.na(var_index))
    stop("var_field not found")
  if (!is.numeric(df[[var_index]]))
    stop("var_field must refer to a numeric mode column")
  
  # Filter df - base R method to allow index matching
  df <- df[which(df[[var_index]] >= stats::quantile(df[[var_index]],
                                                    probs = probs)), ,
           drop = FALSE]
  return(df)
  
}




#' Subset Network Data by Community Size
#'
#' This function subsets the network data by community size using various criteria, 
#' such as top `n` communities, quantile threshold, minimum size, or a specific 
#' set of community indices. It updates the `node_list`, `graph`, `communities`, 
#' `edge_list`, and `post_data` elements in `network_data` accordingly.
#'
#' @param network_data A list containing network data, including `node_list`, 
#'   `graph`, `communities`, `edge_list`, and optionally `post_data`. The 
#'   `node_list` element is expected to be a data.table with a `community` 
#'   column for filtering.
#' @param n_communities Integer, optional. Specifies the number of largest 
#'   communities to retain. If provided, it overrides other size filters.
#' @param quantile Numeric, optional. Specifies the quantile threshold for 
#'   filtering communities by size. For example, `0.75` retains the top 25% 
#'   of communities by size.
#' @param min_size Integer, optional. Retains communities with a minimum 
#'   number of members specified by `min_size`.
#' @param communities_index Integer vector, optional. A vector of community 
#'   indices to retain. If provided, only communities in this vector are kept.
#' @param subset_all_elements Logical, default `TRUE`. If `TRUE`, applies the 
#'   subsetting to all elements in `network_data`, including `graph`, 
#'   `communities`, `edge_list`, and `post_data`.
#' @param verbose Logical, default `TRUE`. If `TRUE`, prints progress 
#'   messages during execution.
#'
#' @return The function returns the modified `network_data` list with updated 
#'   `node_list`, `graph`, `communities`, `edge_list`, and `post_data` 
#'   elements (if applicable).
#' @export
subset_by_community_size <- function(network_data,
                                     n_communities = NULL,
                                     quantile = NULL,
                                     min_size = NULL,
                                     communities_index = NULL,
                                     subset_all_elements = TRUE,
                                     verbose = TRUE) {
  
  # Create copy of node list dt
  nodes_dt <- data.table::copy(network_data$node_list)
  
  
  # Subsettin by index
  if(!is.null(communities_index)){
    
    # Calculate sizes of communities
    comm_size <- nodes_dt[, .N, by = community]
    
    # How many are dropped
    orig_n <- comm_size[, .N]
    keep_n <- length(unique(communities_index))
    drop_n <- orig_n - keep_n
    
    # Filter 
    nodes_dt <- nodes_dt[community %in% communities_index, ]
    comm_size <- nodes_dt[, .N, by = community]
    
    if(verbose)cli::cli_inform("Subsetting communities by vector; n={keep_n} remaining (N members[{min(comm_size$N)}, {max(comm_size$N)}]).
                               Dropped n={drop_n} communities.")
  }else{
    
    # Narrowing by quantile: 
    if(!is.null(quantile)){
      
      # Calculate sizes of communities
      comm_size <- nodes_dt[, .N, by = community]
      
      keep_comm <- filter_ntile(comm_size, var_field = "N", probs = quantile)
      
      # How many are dropped
      orig_n <- comm_size[, .N]
      keep_n <- keep_comm[, .N]
      drop_n <- orig_n - keep_n
      
      # Subset
      nodes_dt <- nodes_dt[community %in% keep_comm$community, ]
      
      pct <- paste0((1 - quantile)*100, "%")
      
      if(verbose)cli::cli_inform("Keeping top {pct}; n={keep_n} of lagrest communities (N members[{min(keep_comm$N)}, {max(keep_comm$N)}]).
                               Dropped n={drop_n} communities.")
      
    }
    
    # Subsetting by top n
    if(!is.null(n_communities)){
      
      # Calculate sizes of communities
      comm_size <- nodes_dt[, .N, by = community]
      
      # Step 2: Get the top communities by total N
      keep_comm <- comm_size[order(-N)][1:n_communities, ]
      
      # How many are dropped
      orig_n <- comm_size[, .N]
      keep_n <- keep_comm[, .N]
      drop_n <- orig_n - keep_n
      
      # Step 3: Filter agg_data to include only rows from the top communities
      nodes_dt <- nodes_dt[community %in% keep_comm$community, ]
      
      if(verbose)cli::cli_inform("Keeping top n={keep_n} of lagrest communities (N members[{min(keep_comm$N)}, {max(keep_comm$N)}]).
                               Dropped n={drop_n} communities.")
      
    }
    
    # Subsettin by min_size
    if(!is.null(min_size)){
      
      # Calculate sizes of communities
      comm_size <- nodes_dt[, .N, by = community]
      
      keep_comm <- comm_size[N > min_size]
      
      # How many are dropped
      orig_n <- comm_size[, .N]
      keep_n <- keep_comm[, .N]
      drop_n <- orig_n - keep_n
      
      # Filter 
      nodes_dt <- nodes_dt[community %in% keep_comm$community, ]
      
      if(verbose)cli::cli_inform("Keeping communities larger than {min_size} (N members[{min(keep_comm$N)}, {max(keep_comm$N)}]).
                               Dropped n={drop_n} communities.")
    }
    
  }
  
  ### Store
  network_data$node_list <- nodes_dt
  
  
  if(subset_all_elements){
    
    ### Subset graph object
    if(verbose)cli::cli_inform("Subsetting 'graph' object to reduced node list.")
    g <- network_data$graph
    g_sub <- igraph::induced_subgraph(g, vids = igraph::V(g)[name %in% nodes_dt$account_id]) 
    
    ### Subset communities object
    if(verbose)cli::cli_inform("Subsetting 'communities' object to reduced node list.")
    communities <- network_data$communities
    
    # Create the full membership vector with NAs for non-included nodes
    full_membership <- rep(NA, igraph::vcount(g))
    
    # Fill in community memberships for nodes that are included in node_dt$account_id
    incl_nodes_i <- igraph::V(g)$name %in% nodes_dt$account_id
    full_membership[incl_nodes_i] <- igraph::membership(communities)[incl_nodes_i]
    
    # Create the new communities object for the subgraph
    communities_sub <- igraph::make_clusters(g_sub, 
                                             membership = full_membership[incl_nodes_i],
                                             algorithm = communities$algorithm)
    
    ### Store
    network_data$graph <- g_sub
    network_data$communities <- communities_sub
    
    ### Subset edge list
    if(verbose)cli::cli_inform("Subsetting 'edge_list' to reduced node list.")
    network_data$edge_list <- network_data$edge_list[account_id %in% nodes_dt$account_id, ]
    network_data$edge_list <- network_data$edge_list[account_id_y %in% nodes_dt$account_id, ]
    
    ### Subset post data
    if("post_data" %in% names(network_data)){
      
      if(verbose)cli::cli_inform("Subsetting 'post_data' to reduced node list.")
      network_data$post_data <- network_data$post_data[account_id %in% nodes_dt$account_id, ]
    }
    
    ### Store filter_info
    filter_info <- list(n_communities = n_communities,
                        quantile = quantile,
                        min_size = min_size,
                        communities_index = communities_index)
    
    network_data$filter_info <- filter_info
    
    return(network_data)
  }
  
}


#' Z-score Standardization
#'
#' This function performs Z-score standardization (also called normalization) on a numeric vector. 
#' It scales the input data so that it has a mean of 0 and a standard deviation of 1.
#'
#' @param x A numeric vector to be standardized. Missing values (NA) will be ignored in the calculation.
#'
#' @return A numeric vector of the same length as `x`, where each value is standardized to have a mean of 0 
#' and a standard deviation of 1. Any missing values (NA) in `x` are preserved.
#'
#' @details The function calculates the mean and standard deviation of the input vector, ignoring any 
#' missing values (`NA`). It then subtracts the mean from each value and divides the result by the standard deviation.
#' 
#' @examples
#' # Example usage
#' x <- c(1, 2, 3, 4, 5)
#' z_standardize(x)
#' 
#' @export
z_standardize <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sigma <- stats::sd(x, na.rm = TRUE)
  return((x - mu) / sigma)
}





#' Replace Emojis with Descriptive Names
#'
#' Replaces emoji characters in a character vector with their corresponding
#' descriptive names in `:colon_syntax:` (e.g., "🔥" becomes ":fire:").
#' Only emojis present in the input are processed to maximize performance.
#'
#' @param text_vec A character vector containing text with emojis to be replaced.
#'
#' @return A character vector of the same length as `text_vec`, where all detected
#' emojis are replaced with their descriptive names in `:colon:` format.
#'
#' @details
#' This function uses the `emoji::emoji_name` dataset for emoji-to-name mapping
#' and `stringi` for high-performance detection and substitution.
#' Only emojis that are actually present in the input are processed, making
#' the function suitable for large text corpora.
#' 
#' @export
replace_emoji_with_name <- function(text_vec) {
  
  if (!requireNamespace("emoji", quietly = TRUE)) {
    stop("Package 'emoji' is required for this function. Please install it.")
  }
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is required for this function. Please install it.")
  }
  
  emoji_vec <- emoji::emoji_name
  
  # emoji → :name: mapping
  replacement_map <- setNames(paste0(":", names(emoji_vec), ":"), unname(emoji_vec))
  emoji_chars <- names(replacement_map)
  
  # Only keep emojis that appear in the input
  present <- vapply(emoji_chars, function(e) any(stringi::stri_detect_fixed(text_vec, e)), logical(1))
  
  for (emoji in emoji_chars[present]) {
    text_vec <- stringi::stri_replace_all_fixed(
      str = text_vec,
      pattern = emoji,
      replacement = replacement_map[[emoji]],
      vectorize_all = FALSE
    )
  }
  
  text_vec
}
