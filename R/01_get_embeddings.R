#' Retrieve Text Embeddings Using Hugging Face Model via Python Script
#'
#' @param data A `data.frame` or `data.table` containing the input text data.
#' @param post_id Character. Column name in `data` containing unique post identifiers. If `NULL`, 
#' the function attempts to detect it automatically.
#' @param time Character. Column name in `data` containing timestamps. If `NULL`, the function 
#' attempts to detect it automatically.
#' @param content Character. Column name in `data` containing text content to embed. If `NULL`, 
#' the function attempts to detect it automatically.
#' @param model_name The Hugging Face model to use for embeddings (default is the CPU-friendly, multilingual model "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2").
#' Other useful, more complex models to consider are, e.g.:
#' \itemize{
#'   \item Social-media specific, multilingual: "Twitter/twhin-bert-base"
#'   \item Multilingual, lightweight model, trained on wikipedia: "distilbert/distilbert-base-multilingual-cased"
#'   }
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
get_embeddings <- function(data,
                           post_id = NULL, 
                           time = NULL,
                           content = NULL, 
                           model_name = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2", 
                           batch_size = 16L, 
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
  
  # Assert that data is data frame or table
  assertthat::assert_that(is.data.frame(data), msg =  
                            "Please provide 'data' in data.frame or data.table format.")
  
  # Convert to data.table if data.frame
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Define the required columns and their alternative names
  required_columns <- c("post_id", 
                        "time", 
                        "content")
  alternative_names <- list(post_id = post_id, 
                            time = time, 
                            content = content)
  
  # Create a logical vector to identify missing columns
  missing_columns <- !required_columns %in% names(data)
  
  # Function to check and rename columns if missing
  check_and_rename <- function(col) {
    alt_name <- alternative_names[[col]]
    assertthat::assert_that(!is.null(alt_name), 
                            msg = paste("'", col, "' not specified and not found in 'data'.", sep = ""))
    assertthat::assert_that(alt_name %in% names(data), 
                            msg = paste("Alternative name for '", col, "' provided but not found in 'data'.", sep = ""))
    data.table::setnames(data, old = alt_name, new = col)
  }
  
  # Apply the function to missing columns
  if (any(missing_columns)) {
    invisible(lapply(required_columns[missing_columns], check_and_rename))
  }
  
  # Subset the data table to these four columns
  data <- data[, required_columns, with = FALSE]
  
  # Drop cuplicates of data
  if(any(duplicated(data, by = "post_id"))){
    warning("Duplicates in 'data' by 'post_id' detected and dropped.")
    data <- unique(data, by = "post_id")
  }
  
  # Converting the time variable to UNIX Timestamp
  data[, time := as.numeric(as.POSIXct(time, tz = "UTC"))]
  
  # Sort by time
  data <- data[order(time)]
  
  
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
    texts = data$content,
    model_name = model_name,
    batch_size = batch_size,
    max_length = max_length,
    use_fp16 = use_fp16
  )
  
  embeddings_m <- Matrix::as.matrix(embeddings)
  rownames(embeddings_m) <- data$post_id
  
  return(embeddings_m)
}





#' @title Save Text Embeddings to an HDF5 File
#'
#' @description This function retrieves text embeddings using a transformer model, processes the 
#' input data, and stores the resulting embeddings in an HDF5 file along with metadata for efficient 
#' retrieval and analysis.
#'
#' @param data A `data.frame` or `data.table` containing the input text data.
#' @param post_id Character. Column name in `data` containing unique post identifiers. If `NULL`, 
#' the function attempts to detect it automatically.
#' @param time Character. Column name in `data` containing timestamps. If `NULL`, the function 
#' attempts to detect it automatically.
#' @param content Character. Column name in `data` containing text content to embed. If `NULL`, 
#' the function attempts to detect it automatically.
#' @param model_name Character. Name of the transformer model to use for generating embeddings. 
#' Defaults to `"sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"`.
#' @param batch_size Integer. Number of texts processed per batch when generating embeddings. 
#' Defaults to `32L`.
#' @param max_length Integer. Maximum token length for transformer-based embedding generation. 
#' Defaults to `512L`.
#' @param use_fp16 Logical. Whether to use half-precision floating point (`fp16`) for faster 
#' computation on GPUs. Defaults to `TRUE`.
#' @param chunk_size Integer. The number of observations per chunk when storing embeddings in the 
#' HDF5 file. Defaults to `512L`.
#' @param h5_fileprefix Character. Prefix for the generated HDF5 filename. Defaults to `"embeddings_"`.
#' @param save_dir Character. Directory where the HDF5 file will be saved. If `NULL`, defaults to the 
#' working directory.
#' @param overwrite Logical. Whether to overwrite an existing HDF5 file.
#' @param python_version Character. The Python version to use when retrieving embeddings. Defaults to `"3.13"`.
#' @param conda_path Character. The path to the Conda installation to use. If `NULL`, the function 
#' attempts to detect an existing installation.
#' @param conda_env_path Character. The path where the Conda environment should be created or used. 
#' If `NULL`, the default Conda environment location is used.
#' @param conda_env_name Character. The name of the Conda environment where embeddings will be 
#' generated. Defaults to `"conda-coorsim"`.
#' @param ask Logical. Whether to prompt the user for confirmation before installing Conda or dependencies.
#' Defaults to `TRUE`.
#' @param force Logical. If `TRUE`, forces reinstallation of dependencies even if they are already installed.
#' Defaults to `FALSE`.
#' @param verbose Logical. If `TRUE`, prints progress messages. Defaults to `TRUE`.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Ensures `data` contains the required columns (`post_id`, `time`, `content`), renaming them if necessary.
#'   \item Removes duplicate entries based on `post_id`.
#'   \item Converts `time` to a UNIX timestamp format for interoperability.
#'   \item Sorts the dataset by `time` to enable fast querying.
#'   \item Generates text embeddings using the specified transformer model.
#'   \item Stores embeddings and metadata (post IDs and timestamps) in an HDF5 file using chunked storage.
#' }
#'
#' The HDF5 file structure includes:
#' \itemize{
#'   \item `"metadata/time"`: Vector of timestamps (UNIX format).
#'   \item `"metadata/post_id"`: Vector of post IDs.
#'   \item `"embeddings"`: Matrix of text embeddings stored in chunks.
#' }
#'
#' The chunked storage format enables efficient retrieval of embeddings by time range.
#'
#' @return Invisibly returns `NULL`. The function is executed for its side effects of generating 
#' and storing text embeddings.
#'
#' @export
save_embeddings <- function(data,
                            post_id = NULL, 
                            time = NULL,
                            content = NULL,
                            model_name = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2",
                            batch_size = 32L,
                            max_length = 512L, 
                            use_fp16 = TRUE,
                            
                            chunk_size = 512L,
                            
                            h5_fileprefix = "embeddings_",
                            save_dir = NULL,
                            overwrite = TRUE,
                            
                            python_version = "3.13",
                            conda_path = NULL,
                            conda_env_path = NULL,
                            conda_env_name = "conda-coorsim",
                            ask = TRUE,
                            force = FALSE,
                            verbose = TRUE){
  
  # Assert that data is data frame or table
  assertthat::assert_that(is.data.frame(data), msg =  
                            "Please provide 'data' in data.frame or data.table format.")
  
  # Convert to data.table if data.frame
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Define the required columns and their alternative names
  required_columns <- c("post_id", 
                        "time", 
                        "content")
  alternative_names <- list(post_id = post_id, 
                            time = time, 
                            content = content)
  
  # Create a logical vector to identify missing columns
  missing_columns <- !required_columns %in% names(data)
  
  # Function to check and rename columns if missing
  check_and_rename <- function(col) {
    alt_name <- alternative_names[[col]]
    assertthat::assert_that(!is.null(alt_name), 
                            msg = paste("'", col, "' not specified and not found in 'data'.", sep = ""))
    assertthat::assert_that(alt_name %in% names(data), 
                            msg = paste("Alternative name for '", col, "' provided but not found in 'data'.", sep = ""))
    data.table::setnames(data, old = alt_name, new = col)
  }
  
  # Apply the function to missing columns
  if (any(missing_columns)) {
    invisible(lapply(required_columns[missing_columns], check_and_rename))
  }
  
  # Subset the data table to these four columns
  data <- data[, required_columns, with = FALSE]
  
  # Drop cuplicates of data
  if(any(duplicated(data, by = "post_id"))){
    cli::cli_alert("Duplicates in 'data' by 'post_id' detected and dropped.")
    data <- unique(data, by = "post_id")
  }
  
  # Converting the time variable to UNIX Timestamp
  data[, time := as.numeric(as.POSIXct(time, tz = "UTC"))]
  
  # Sort by time
  data <- data[order(time)]
  
  # Step 1: Retrieving embeddings
  emb_matrix <- coorsim::get_embeddings(data = data,
                                        post_id = post_id, 
                                        time = time,
                                        content = content, 
                                        model_name = model_name, 
                                        batch_size = batch_size, 
                                        max_length = max_length, 
                                        use_fp16 = use_fp16,
                                        python_version = python_version,
                                        conda_path = conda_path,
                                        conda_env_path = conda_env_path,
                                        conda_env_name = conda_env_name,
                                        ask = ask,
                                        force = force,
                                        verbose = verbose)
  
  # Step 2: Store embeddings as .h5 file
  post_id <- as.character(data$post_id)
  time <- data$time
  
  if(is.null(save_dir)) save_dir <- getwd()
  if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  h5_filename <- paste0(h5_fileprefix,
                        stringr::str_replace_all(string = basename(model_name), "[:punct:]", "-"),
                        ".h5")
  h5_file <- file.path(save_dir, h5_filename)
  
  # Handle overwrite condition
  if (file.exists(h5_file) && !overwrite) {
    stop("HDF5 file already exists and overwrite is set to FALSE.")
  }
  
  # Create HDF5 file using hdf5r
  h5 <- hdf5r::H5File$new(h5_file, mode = "w")
  
  # Create metadata group
  meta_group <- h5$create_group("metadata")
  meta_group$create_dataset("time", robj = time)
  meta_group$create_dataset("post_id", robj = post_id)
  
  # Create dataset for embeddings with chunking
  h5$create_dataset(
    name = "embeddings",
    robj = emb_matrix,
    chunk_dims = c(min(chunk_size, nrow(emb_matrix)), ncol(emb_matrix)),
    dtype = hdf5r::h5types$H5T_NATIVE_DOUBLE
  )
  
  # Close the HDF5 file
  h5$close_all()
}




#' Load Embeddings from an HDF5 (.h5) File
#'
#' @description This function loads precomputed embeddings stored in an `.h5` file and retrieves only the relevant embeddings matching post IDs present in a dataset.
#'
#' @param path Character. Path to the `.h5` file containing embeddings.
#' @param ids_subset Character. Vector of IDs to subset the embedding matrix. Default is `NULL`.
#' @param verbose Logical. If `TRUE`, displays progress messages. Default is `TRUE`.
#'
#' @details 
#' The function checks whether the `.h5` file contains a `metadata/post_id` dataset and loads the corresponding embeddings for matching post IDs.
#'
#' @return A numeric matrix where rows correspond to post IDs and columns represent embedding dimensions. If no matching post IDs are found, returns `NULL`.
#'
#' @export
load_h5_embeddings <- function(path, 
                               ids_subset = NULL,
                               verbose = TRUE) {
  
  # Helper function to check if a valid .h5 file path is provided
  is_h5file <- function(v) {
    is.character(v) && file.exists(v) && grepl("\\.h5$", v, ignore.case = TRUE)
  }
  
  if (!is_h5file(path)) {
    stop("Invalid HDF5 file path provided.")
  }
  
  if (verbose) cli::cli_inform("Loading embeddings from the .h5 file.")
  
  # Open the HDF5 file
  h5 <- hdf5r::H5File$new(path, mode = "r")
  
  # Check if metadata group exists
  if (!"metadata" %in% names(h5)) {
    h5$close_all()
    stop("Embeddings provided as .h5 file but 'metadata' group was not found. ",
         "Please use 'coorsim::save_embeddings()' to retain a correct .h5 file.")
  }
  
  metadata_group <- h5$open("metadata")
  
  # Check if post_id dataset exists
  if (!"post_id" %in% names(metadata_group)) {
    h5$close_all()
    stop("Embeddings provided as .h5 file but 'metadata/post_id' was not found. ",
         "Please use 'coorsim::save_embeddings()' to retain a correct .h5 file.")
  }
  
  # Load post IDs
  ids <- metadata_group$open("post_id")$read()
  
  if (!is.null(ids_subset)) {
    # Find indices matching the ids
    indices <- which(ids %in% ids_subset)
    
    if (length(indices) == 0) {
      h5$close_all()
      return(NULL)  # No matching data
    }
    
    # Load corresponding embeddings
    m <- h5$open("embeddings")$read(args = list(indices, NULL))
    rownames(m) <- ids[indices]
  } else {
    m <- h5$open("embeddings")$read()
    rownames(m) <- ids
  }
  
  h5$close_all()
  
  return(Matrix::as.matrix(m))
}