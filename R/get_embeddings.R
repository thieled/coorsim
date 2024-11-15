#' Retrieve Text Embeddings Using Hugging Face Model via Python Script
#'
#' @param text_data A data.frame with 'id' and 'text' columns.
#' @param model_name The Hugging Face model to use for embeddings (default is "twitter/twhin-bert-base").
#' @param python_path Optional. The path to the Python executable to be used by reticulate.
#' @return A data.frame with 'id' and the computed embeddings as additional columns.
#' @export
get_embeddings <- function(text_data, model_name = "twitter/twhin-bert-base", python_path = NULL) {
  # Set Python path if specified
  if (!is.null(python_path)) {
    reticulate::use_python(python = python_path, required = TRUE)
  }
  
  #### NOTE: Improve handling of venv
  
  # Check Python environment availability
  if (!reticulate::py_available()) {
    stop("Python environment not available. Ensure Python is installed and configured correctly.")
  }
  
  # Source py script
  reticulate::source_python(system.file("python",
                                        "get_transformer_embeddings.py",
                                        package = "coorsim",
                                        mustWork = TRUE
  ))
  
  
  # Convert the text column to a list
  texts <- text_data$text
  
  # Call the Python function with model_name
  embeddings <- get_text_embeddings(texts, model_name = model_name)
  
  # Convert numpy array to R data frame
  embeddings_df <- as.data.frame(reticulate::py_to_r(embeddings))
  
  # Combine embeddings with IDs into a single data frame
  embeddings_df <- cbind(id = text_data$id, embeddings_df)
  
  return(embeddings_df)
}

