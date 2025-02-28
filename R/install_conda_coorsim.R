#' @title Install PyTorch in a Conda Environment
#'
#' @description Installs PyTorch and related libraries (`torch`, `torchvision`, and `torchaudio`)
#' in a specified Conda environment. The function automatically detects CUDA availability
#' and installs the appropriate GPU or CPU version of PyTorch.
#'
#' @param python_path Character. The path to the Python executable in the Conda environment.
#' If `NULL`, the function automatically detects the Python executable for the active Conda environment.
#' @param conda_env Character. The name of the Conda environment where PyTorch should be installed.
#' If `NULL`, the function attempts to detect the currently active Conda environment.
#' @param verbose Logical. If `TRUE`, the function prints progress messages.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Detects the active Conda environment if `conda_env` is not specified.
#'   \item Checks for GPU and CUDA availability.
#'   \item Selects the correct PyTorch installation URL based on CUDA version or CPU fallback.
#'   \item Installs PyTorch via `reticulate::py_install()`.
#'   \item Verifies installation by checking the versions of PyTorch and CUDA.
#' }
#'
#' If CUDA is available, the function installs the corresponding GPU-accelerated PyTorch build.
#' Otherwise, it falls back to a CPU-only installation.
#'
#' @return Invisibly returns `NULL`. The function is called for its side effects of installing
#' and verifying PyTorch.
#'
#' @export
install_torch <- function(python_path = NULL, conda_env = NULL, verbose = TRUE) {
  
  # Helper function for printing messages only when verbose = TRUE
  vmessage <- function(...) {
    if (verbose) message(...)
  }
  
  # Detect the currently active Conda environment
  detect_active_conda_env <- function() {
    envs <- reticulate::conda_list()
    active_python <- normalizePath(reticulate::py_exe(), winslash = "\\", mustWork = FALSE)
    envs$python <- normalizePath(envs$python, winslash = "\\", mustWork = FALSE)
    
    active_env <- envs$name[envs$python == active_python]
    
    if (length(active_env) == 0) {
      return(NULL)
    } else {
      return(active_env)
    }
  }
  
  # Detect GPU availability
  check_gpu <- function() {
    if (.Platform$OS.type == "windows") {
      gpu_info <- try(system("wmic path win32_VideoController get name", intern = TRUE), silent = TRUE)
      return(any(grepl("NVIDIA", gpu_info, ignore.case = TRUE)))
    } else {
      gpu_info <- try(system("nvidia-smi", intern = TRUE), silent = TRUE)
      return(!inherits(gpu_info, "try-error") && length(gpu_info) > 0)
    }
  }
  
  # Detect CUDA availability
  check_cuda <- function() {
    cuda_check <- try(system("nvcc --version", intern = TRUE), silent = TRUE)
    return(!inherits(cuda_check, "try-error") && any(grepl("release", cuda_check, ignore.case = TRUE)))
  }
  
  # Get CUDA version
  get_cuda_version <- function() {
    nvcc_output <- tryCatch(system("nvcc --version", intern = TRUE), error = function(e) NULL)
    if (!is.null(nvcc_output)) {
      version_line <- nvcc_output[stringr::str_detect(nvcc_output, "release")]
      return(stringr::str_extract(version_line, "\\d+\\.\\d+"))
    }
    return(NULL)
  }
  
  # Construct PyTorch installation URL
  construct_pytorch_install_url <- function(cuda_version) {
    base_url <- "https://download.pytorch.org/whl/"
    if (is.null(cuda_version)) {
      return(paste0(base_url, "cpu"))
    } else {
      return(paste0(base_url, "cu", gsub("\\.", "", cuda_version)))
    }
  }
  
  # Validate URL by checking HTTP response
  validate_url <- function(url) {
    if (!requireNamespace("httr", quietly = TRUE)) stop("Package 'httr' is required for URL validation.")
    
    response <- tryCatch(httr::HEAD(url), error = function(e) NULL)
    if (is.null(response) || httr::status_code(response) != 200) {
      vmessage("Warning: The PyTorch installation URL is invalid or unavailable.")
      return(FALSE)
    }
    return(TRUE)
  }
  
  # Set up Conda environment and Python path
  if (is.null(conda_env)) {
    conda_env <- detect_active_conda_env()
    if (is.null(conda_env)) {
      stop("No active Conda environment detected. Please specify `conda_env` explicitly.")
    }
    vmessage("Using detected active Conda environment: ", conda_env)
  } else {
    vmessage("Using specified Conda environment: ", conda_env)
  }
  
  # Detect the appropriate Python executable
  if (is.null(python_path)) {
    python_path <- reticulate::conda_python(envname = conda_env)
  }
  
  if (!file.exists(python_path)) {
    stop("The specified Python path does not exist: ", python_path)
  }
  
  vmessage("Using Python: ", python_path)
  
  # Detect CUDA & select appropriate PyTorch build
  gpu_available <- check_gpu()
  cuda_version <- if (gpu_available) get_cuda_version() else NULL
  index_url <- construct_pytorch_install_url(cuda_version)
  
  # Validate the generated PyTorch URL
  if (!validate_url(index_url)) {
    vmessage("Invalid URL detected. Falling back to CPU-only installation.")
    index_url <- "https://download.pytorch.org/whl/cpu"
    cuda_version <- NULL  # Force CPU-only install
  }
  
  # Select installation mode
  if (!is.null(cuda_version)) {
    vmessage("Detected CUDA version: ", cuda_version, ". Installing GPU-supported PyTorch...")
  } else {
    vmessage("No GPU detected or CUDA not installed. Installing CPU-only PyTorch...")
  }
  
  # Install PyTorch with reticulate::py_install()
  tryCatch({
    reticulate::py_install(
      packages = c("torch", "torchvision", "torchaudio"),
      pip = TRUE,
      envname = conda_env,
      python_version = NULL,  # Keep Conda Python version fixed
      pip_options = paste("--index-url", index_url)
    )
    vmessage("Torch installation completed successfully.")
  }, error = function(e) {
    stop("Torch installation failed: ", e$message)
  })
  
  # Verify installation
  tryCatch({
    torch <- reticulate::import("torch")
    vmessage("Torch successfully installed!")
    vmessage("Torch version: ", torch$`__version__`)
    vmessage("CUDA available: ", torch$cuda$is_available())
    if (!is.null(cuda_version)) {
      vmessage("CUDA version: ", torch$version$cuda)
    }
  }, error = function(e) {
    stop("Verification failed: Torch is not properly installed.")
  })
}



#' @title Install and Set Up a Conda Environment
#'
#' @description Installs Miniconda if no suitable Conda installation is found and 
#' creates a specified Conda environment with the given Python version. 
#' Optionally allows specifying the Miniconda installation path and the Conda environment location.
#'
#' @param python_version Character. The Python version to install in the Conda environment.
#' Defaults to `"3.13:latest"`.
#' @param conda_path Character. The path where Miniconda should be installed.
#' If `NULL`, Miniconda is installed in the default location.
#' @param conda_env_path Character. The path where the Conda environment should be created.
#' If `NULL`, the environment is created in the default Conda environments directory.
#' @param conda_env_name Character. The name of the Conda environment to create or use.
#' Defaults to `"conda-coorsim"`.
#' @param ask Logical. If `TRUE`, prompts the user for confirmation before installing Miniconda.
#' Defaults to `TRUE`.
#' @param force Logical. If `TRUE`, forces reinstallation even if an environment exists.
#' Defaults to `FALSE`.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Checks if a valid Conda installation exists.
#'   \item Prompts the user to install Miniconda if no Conda installation is found.
#'   \item Creates a Conda environment at the specified location.
#'   \item Activates the created Conda environment.
#' }
#'
#' If `conda_path` is specified, Miniconda is installed at that location. If 
#' `conda_env_path` is provided, the Conda environment is created there. 
#' The function ensures that Conda is properly set up before proceeding.
#'
#' @return Invisibly returns `NULL`. The function is executed for its side effects
#' of installing and configuring Conda.
#'
#' @export
install_conda <- function(python_version = "3.13",
                          conda_path = NULL,
                          conda_env_path = NULL,
                          conda_env_name = "conda-coorsim",
                          ask = TRUE,
                          force = FALSE,
                          verbose = TRUE) {
  
  # Helper function for verbose messages
  vmessage <- function(...) {
    if (verbose) message(...)
  }
  
  # Check if valid conda can be used
  t <- try({
    if (!is.null(conda_path)) {
      # Use conda from conda_path if specified
      reticulate::conda_list(conda = file.path(conda_path, "bin", "conda"))
    } else {
      reticulate::conda_list()
    }
  }, silent = TRUE)
  
  
  if (inherits(t, "try-error")) {
    
    permission <- TRUE
    
    if (ask) {
      # Function to prompt user for input using cli
      ask_permission <- function(question) {
        cli::cli_alert_warning(question)
        repeat {
          response <- tolower(trimws(readline(" (y/n): ")))
          if (response %in% c("y", "yes")) return(TRUE)
          if (response %in% c("n", "no")) return(FALSE)
          cli::cli_alert_danger("Invalid input. Please enter 'y' or 'n'.")
        }
      }
      
      # Construct question based on conda_path availability
      question <- if (!is.null(conda_path)) {
        sprintf("No suitable conda installation was found in: \n  {cli::col_green('%s')}\nDo you want to install it?", conda_path)
      } else {
        "No suitable conda installation was found. Do you want to install it?"
      }
      
      # Ask user for confirmation
      permission <- ask_permission(question)
    }
    
    
    if (permission) {
      
      if (!is.null(conda_path)) {
        vmessage("Installing Miniconda at: ", conda_path)
        python <- reticulate::install_miniconda(path = conda_path, force = force)
      }else{
        vmessage("Installing Miniconda at default location.")
        python <- reticulate::install_miniconda(force = force)
      }
      
    } else {
      stop("Aborted by user")
    }
    
  }
  
  ## check if conda_env_name exists
  if (!is.null(conda_path)) {
    # use specified conda
    env_exists <- reticulate::condaenv_exists(envname = conda_env_name, conda = file.path(conda_path, "bin", "conda"))
  }else{
    # use default conda
    env_exists <- reticulate::condaenv_exists(envname = conda_env_name)
  }
  
  if(env_exists == F){
    
    # Check if conda_env_path specified
    if(!is.null(conda_env_path)){
      # Set the CONDA_ENVS_PATH variable before creating the environment
      vmessage("Setting the CONDA_ENVS_PATH to: ", conda_env_path)
      Sys.setenv(CONDA_ENVS_PATH = conda_env_path)
    }
    
    # Create a Conda environment at the specified location
    vmessage("Creating conda environment: ", conda_env_name, " in: ", conda_env_path)
    if (!is.null(conda_path)) {
      
      # use specified conda
      reticulate::conda_create(envname = conda_env_name, 
                               python_version = python_version, 
                               conda = file.path(conda_path, "bin", "conda"))
    }else{
      # use default conda
      reticulate::conda_create(envname = conda_env_name, 
                               python_version = python_version)
    }
    
  }
  
  # Activate Conda environment
  vmessage("Activating Conda environment: ", conda_env_name)
  
  if (!is.null(conda_path)) {
    # use specified conda
    reticulate::use_condaenv(condaenv = conda_env_name, 
                             required = TRUE, 
                             conda = file.path(conda_path, "bin", "conda"))
  }else{
    # use default conda
    reticulate::use_condaenv(condaenv = conda_env_name)
  }
  
}


#' @title Wrapper for Installing Conda, PyTorch, and Transformers
#'
#' @description Installs Miniconda if necessary, creates a Conda environment, 
#' and installs PyTorch and Transformers within the specified environment.
#' This function serves as a high-level wrapper that ensures all dependencies 
#' are correctly set up.
#'
#' @param python_version Character. The Python version to install in the Conda environment.
#' Defaults to `"3.13:latest"`.
#' @param conda_path Character. The path where Miniconda should be installed.
#' If `NULL`, Miniconda is installed in the default location.
#' @param conda_env_path Character. The path where the Conda environment should be created.
#' If `NULL`, the environment is created in the default Conda environments directory.
#' @param conda_env_name Character. The name of the Conda environment to create or use.
#' Defaults to `"conda-coorsim"`.
#' @param ask Logical. If `TRUE`, prompts the user for confirmation before installing Miniconda.
#' Defaults to `TRUE`.
#' @param force Logical. If `TRUE`, forces reinstallation of dependencies even if they are already installed.
#' Defaults to `FALSE`.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Ensures that Miniconda is installed at the specified location (or the default).
#'   \item Creates a Conda environment at the given `conda_env_path`, if specified.
#'   \item Installs PyTorch in the Conda environment, choosing the appropriate CPU or GPU build.
#'   \item Installs the `transformers` package if it is not already installed.
#' }
#'
#' If `conda_path` is specified, Miniconda is installed at that location. If 
#' `conda_env_path` is provided, the Conda environment is created there. 
#' This wrapper ensures a fully configured environment for use with Coorsim.
#'
#' @return Invisibly returns `NULL`. The function is executed for its side effects
#' of installing and configuring Conda and dependencies.
#'
#' @export
install_conda_coorsim <- function(python_version = "3.13",
                                  conda_path = NULL,
                                  conda_env_path = NULL,
                                  conda_env_name = "conda-coorsim",
                                  ask = TRUE,
                                  force = FALSE,
                                  verbose = TRUE) {
  
  # Helper function for verbose messages
  vmessage <- function(...) {
    if (verbose) message(...)
  }
  
  ## Install or activate conda
  install_conda(python_version =  python_version,
                conda_path = conda_path,
                conda_env_path = conda_env_path,
                conda_env_name = conda_env_name,
                ask = ask,
                force = force,
                verbose = verbose)
  
  # Install PyTorch using the updated install_torch function
  if ("torch" %in% reticulate::py_list_packages(envname = conda_env_name)$package && !force){
    vmessage("Skipping installation of torch. Use `force = TRUE` to reinstall.")
  }else{
    vmessage("Installing PyTorch...")
    install_torch(conda_env = conda_env_name, verbose = verbose)
  }
  
  # Install transformers
  if ("transformers" %in% reticulate::py_list_packages(envname = conda_env_name)$package && !force){
    vmessage("Skipping installation of torch. Use `force = TRUE` to reinstall.")
  }else{
    vmessage("Installing transformers...")
    reticulate::py_install("transformers", pip = TRUE, envname = conda_env_name)
  }
  
  # Confirm installation
  message("Installation of PyTorch and Transformers confirmed in conda-env: ", conda_env_name)
  
  invisible(NULL)
}



#' @title Initialize the Coorsim Conda Environment
#'
#' @description This function ensures that the required Conda environment for Coorsim is set up,
#' activated, and properly configured. If the environment is not found, it installs Conda,
#' sets up the environment, and verifies the installation of required dependencies.
#'
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
#' Defaults to `FALSE`.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Checks if the Coorsim Conda environment is already initialized.
#'   \item Installs and activates Conda if necessary.
#'   \item Detects and verifies the active Conda environment.
#'   \item Confirms the installation of PyTorch and Transformers.
#'   \item Checks CUDA availability and version.
#'   \item Sources the required Python script from the package.
#'   \item Sets a global option indicating that the Coorsim Conda environment has been initialized.
#' }
#'
#' If the Conda environment is already initialized, the function does nothing.
#' Otherwise, it installs and sets up all required dependencies before making them available.
#'
#' @return Invisibly returns `NULL`. The function is executed for its side effects
#' of verifying and setting up the Conda environment and dependencies.
#'
#' @export
initialize_conda_coorsim <- function(python_version = "3.13",
                                     conda_path = NULL,
                                     conda_env_path = NULL,
                                     conda_env_name = "conda-coorsim",
                                     ask = TRUE,
                                     force = FALSE,
                                     verbose = TRUE){
  
  # Helper function for verbose messages
  vmessage <- function(...) {
    if (verbose) message(...)
  }
  
  if (!is.null(options("conda_coorsim_initialized")$conda_coorsim_initialized)){
    
    vmessage("Coorsim conda environment is already initialized.")
    
  }else{
    
    # Install or activate conda
    install_conda_coorsim(python_version =  python_version,
                          conda_path = conda_path,
                          conda_env_path = conda_env_path,
                          conda_env_name = conda_env_name,
                          ask = ask,
                          force = force,
                          verbose = verbose)
  }
  
  
  # Detect the currently active Conda environment
  detect_active_conda_env <- function(info = c("name", "path")) {
    envs <- reticulate::conda_list()
    active_python <- normalizePath(reticulate::py_exe(), winslash = "\\", mustWork = FALSE)
    envs$python <- normalizePath(envs$python, winslash = "\\", mustWork = FALSE)
    
    active_env <- envs$name[envs$python == active_python]
    active_path <- envs$python[envs$python == active_python]
    
    if (length(active_env) == 0) {
      return(NULL)
    } else {
      if(all(c("name", "path") %in% info)) return(list(env = active_env, path = active_path))
      if(info == "name") return(active_env)
      if(info == "path") return(active_path)
    }
  }
  
  # Function to verify installation
  verify_installation <- function(verbose = TRUE) {
    
    tryCatch({
      
      # Check Conda environment location
      conda_envs <- detect_active_conda_env()
      vmessage("Conda environment '", conda_envs$env, "' active.") 
      vmessage("Environment path: ", conda_envs$path) 
      
      # Import PyTorch
      torch <- reticulate::import("torch", delay_load = TRUE)
      vmessage("Pytorch version: ", torch$`__version__`)
      
      # Check CUDA availability
      cuda_available <- torch$cuda$is_available()
      vmessage(if (cuda_available) "CUDA is available." else "CUDA is NOT available.")
      
      # Check CUDA version
      if (cuda_available) {
        vmessage("CUDA version: ", torch$version$cuda)
      }
      
      # Import Transformers
      transformers <- reticulate::import("transformers", delay_load = TRUE)
      vmessage("Transformers version: ", transformers$`__version__`)
      
    }, error = function(e) {
      stop("Verification failed: ", e$message)
    })
  }
  
  # Print details about installation
  if(verbose) verify_installation(verbose = verbose)
  
  # Source the Python script from the package
  reticulate::source_python(
    system.file("python",
                "get_transformer_embeddings.py",
                package = "coorsim",
                mustWork = TRUE)
  )
  
  if(!"get_text_embeddings" %in% reticulate::py_list_attributes(reticulate::py)){
    stop("'get_text_embeddings' function could not be sourced from python.")
  }
  
  # Set global option that coorsim is initialized
  options("conda_coorsim_initialized" = TRUE)
  
}


