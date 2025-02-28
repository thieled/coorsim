#' Retrieve Text Embeddings Using Hugging Face Model via Python Script
#'
#' @param text_data A data.frame with 'post_id' and 'content' columns.
#' @param model_name The Hugging Face model to use for embeddings (default is "twitter/twhin-bert-base").
#' @param batch_size Integer. Batch size for processing texts (default is 32L).
#' @param max_length Integer. Maximum sequence length for tokenization (default is 512L).
#' @param use_fp16 Logical, whether to use fp16 precision on GPU if available (default is FALSE).
#' @param python_version Character. The Python version to install in the Conda environment.
#' Defaults to `"3.13"`.
#' @param conda_path Character. The path where Miniconda should be installed.
#' If `NULL`, Miniconda is installed in the default location.
#' @param conda_env_path Character. The path where the Conda environment should be created.
#' If `NULL`, the environment is created in the default Conda environments directory.
#' @param conda_env_name Character. The name of the Conda environment to create or use.
#' Defaults to `"conda-coorsim"`.
#' @param ask Logical. If `TRUE`, prompts the user for confirmation before installing Miniconda.
#' Defaults to `TRUE`.
#' @param force Logical. If `TRUE`, forces reinstallation of dependencies even if they are already installed.
#' Defaults to `TRUE`.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @return A matrix with rownames corresponding to the 'id' column and computed embeddings as columns.
#' @export
get_embeddings <- function(text_data, 
                           model_name = "twitter/twhin-bert-base", 
                           batch_size = 32L, 
                           max_length = 512L, 
                           use_fp16 = TRUE,
                           
                           python_version = "3.13",
                           conda_path = NULL,
                           conda_env_path = NULL,
                           conda_env_name = "conda-coorsim",
                           ask = TRUE,
                           force = FALSE,
                           verbose = TRUE
                           
                           ) {
  
  # Check if conda-coorsim is initialized
  if (is.null(options("conda_coorsim_initialized")$conda_coorsim_initialized)){
    
    initialize_conda_coorsim(python_version =  python_version,
                            conda_path = conda_path,
                            conda_env_path = conda_env_path,
                            conda_env_name = conda_env_name,
                            ask = ask,
                            force = force,
                            verbose = verbose)
    
  }
  
  
  # Call the Python function with additional parameters for batch_size, max_length, and use_fp16
  embeddings <- reticulate::py$get_text_embeddings(
    texts = text_data$content,
    model_name = model_name,
    batch_size = batch_size,
    max_length = max_length,
    use_fp16 = use_fp16
  )
  
  embeddings_m <- Matrix::as.matrix(embeddings)
  rownames(embeddings_m) <- text_data$post_id
  
  return(embeddings_m)
}
