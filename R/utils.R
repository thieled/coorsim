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
