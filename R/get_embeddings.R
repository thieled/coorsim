#' Retrieve Text Embeddings Using Hugging Face Model via Python Script
#'
#' @param text_data A data.frame with 'id' and 'text' columns.
#' @param model_name The Hugging Face model to use for embeddings (default is "twitter/twhin-bert-base").
#' @return A data.frame with 'id' and the computed embeddings as additional columns.
#' @export
get_embeddings <- function(text_data, model_name = "twitter/twhin-bert-base") {
  
  
  # Check if transformers is initialized
  if(!options("transformers_initialized")$transformers_initialized){
    init_transformers()
  }
  
  if(!options("transformers_initialized")$transformers_initialized){
    stop("Initializing transformers failed. Try 'install_transformers()'.")
  }
  
  
  # Source py script
  reticulate::source_python(system.file("python",
                                        "get_transformer_embeddings.py",
                                        package = "coorsim",
                                        mustWork = TRUE
  ))
  
  
  embeddings <- get_text_embeddings(texts = text_data$text, model_name = model_name)
  
  embeddings_m <- Matrix::as.matrix(embeddings)
  
  rownames(embeddings_m) <- text_data$id
  
  return(embeddings_m)
}


