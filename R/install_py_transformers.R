
#' Install PyTorch with GPU or CPU Support
#'
#' Automatically installs PyTorch, TorchVision, and TorchAudio with the appropriate configuration
#' for GPU (CUDA) or CPU-only support, based on the system's capabilities.
#'
#' @param pytorch_cuda_url Character string. The URL for the PyTorch CUDA wheel index. 
#'   Defaults to \code{"https://download.pytorch.org/whl/cu124"}.
#'
#' @details
#' The function checks for CUDA availability on the system and installs the corresponding
#' PyTorch version. If CUDA is not available, the CPU-only version is installed. The function
#' uses \code{reticulate} to manage Python virtual environments and ensure proper installation.
#'
#' @export
install_torch <- function(pytorch_cuda_url = "https://download.pytorch.org/whl/cu124") {
  
  check_gpu <- function() {
    if (.Platform$OS.type == "windows") {
      # For Windows systems
      gpu_info <- try(system("wmic path win32_VideoController get name", intern = TRUE), silent = TRUE)
      gpu_detected <- any(grepl("NVIDIA", gpu_info, ignore.case = TRUE))
    } else {
      # For Unix-like systems (Linux/macOS) with nvidia-smi available
      gpu_info <- try(system("nvidia-smi", intern = TRUE), silent = TRUE)
      gpu_detected <- !inherits(gpu_info, "try-error") && length(gpu_info) > 0
    }
    
    if (gpu_detected) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  gpu_available <- check_gpu()
  
  # Helper function to check for CUDA
  check_cuda <- function() {
    if (.Platform$OS.type == "windows") {
      cuda_check <- try(system("where nvcc", intern = TRUE), silent = TRUE)
      cuda_available <- !inherits(cuda_check, "try-error") && any(grepl("CUDA", cuda_check, ignore.case = T) == TRUE)
    } else {
      cuda_check <- try(system("nvcc --version", intern = TRUE), silent = TRUE)
      cuda_available <- !inherits(cuda_check, "try-error") && any(grepl("release", cuda_check, ignore.case = TRUE))
    }
    return(cuda_available)
  }
  
  
  # Check CUDA availability
  cuda_available <- check_cuda() & check_gpu()
  
  
  # Determine package source based on CUDA availability
  if (cuda_available) {
    install_packages <- c(
      "torch", "torchvision", "torchaudio"
    )
    index_url <- pytorch_cuda_url
    message("Installing CUDA-supported torch version...")
  } else {
    install_packages <- c("torch", "torchvision", "torchaudio")
    index_url <- "https://download.pytorch.org/whl/cpu"
    message("Installing CPU-only torch version...")
  }
  
  # Install the packages using reticulate
  tryCatch({
    reticulate::py_install(
      packages = install_packages,
      pip = TRUE,
      envname = Sys.getenv("COORSIM_PYTHON", unset = "r-coorsim"),
      pip_options = paste("--index-url", index_url)
    )
    message("Torch installation completed successfully.")
  }, error = function(e) {
    stop("Torch installation failed: ", e$message)
  })
  
  # Verify installation
  tryCatch({
    torch <- reticulate::import("torch")
    message("Torch successfully installed!")
    message("Torch version: ", torch$`__version__`)
    message("CUDA available: ", torch$cuda$is_available())
  }, error = function(e) {
    stop("Verification failed: Torch is not properly installed.")
  })
}



#' Install Python, PyTorch, and Transformers
#'
#' Installs a Python virtual environment, PyTorch, and the Transformers library. This function
#' handles Python installation if not already available and ensures that the necessary Python
#' packages are installed in the virtual environment.
#'
#' @param py_version Character string. The version of Python to install, specified in 
#'   the format \code{"<major>.<minor>:<latest>"}. Defaults to \code{"3.11:latest"}.
#' @param ask Logical. Whether to prompt the user before installing Python if not found.
#'   Defaults to \code{interactive()}.
#' @param force Logical. If \code{TRUE}, forces the reinstallation of PyTorch and Transformers
#'   even if they are already installed. Defaults to \code{FALSE}.
#' @param pytorch_cuda_url Character string. The URL for the PyTorch CUDA wheel index.
#'   Defaults to \code{"https://download.pytorch.org/whl/cu124"}.
#' @param ... Additional arguments passed to \code{reticulate::install_python()}.
#'
#' @details
#' The function checks for the existence of a virtual environment specified by the 
#' \code{COORSIM_PYTHON} environment variable. If not found, it creates the environment and installs
#' Python. Subsequently, it installs PyTorch and Transformers, using the appropriate version
#' based on CUDA availability. This ensures a seamless setup for using these libraries in R.
#'
#' @export
transformers_install <- function(py_version = "3.11:latest",
                                 ask = interactive(),
                                 force = FALSE,
                                 pytorch_cuda_url = "https://download.pytorch.org/whl/cu124",
                                 ...) {
  
  ### Code partly copied from spacyr
  
  if (nchar(Sys.getenv("RETICULATE_PYTHON")) > 0) {
    message("You provided a custom RETICULATE_PYTHON, so we assume you know what you ",
            "are doing managing your virtual environments. Good luck!")
  } else if (!reticulate::virtualenv_exists(Sys.getenv("COORSIM_PYTHON", unset = "r-coorsim"))) {
    # this has turned out to be the easiest way to test if a suitable Python 
    # version is present. All other methods load Python, which creates
    # some headache.
    t <- try(reticulate::virtualenv_create(Sys.getenv("COORSIM_PYTHON", unset = "r-coorsim")), silent = TRUE)
    if (methods::is(t, "try-error")) {
      permission <- TRUE
      if (ask) {
        permission <- utils::askYesNo(paste0(
          "No suitable Python installation was found on your system. ",
          "Do you want to run `reticulate::install_python()` to install it?"
        ))
      }
      
      if (permission) {
        if (utils::packageVersion("reticulate") < "1.19") 
          stop("Your version or reticulate is too old for this action. Please update")
        python <- reticulate::install_python(version = py_version)
        reticulate::virtualenv_create(Sys.getenv("COORSIM_PYTHON", unset = "r-coorsim"),
                                      python = python)
      } else {
        stop("Aborted by user")
      }
    }
    reticulate::use_virtualenv(Sys.getenv("COORSIM_PYTHON", unset = "r-coorsim"))
  } else {
    reticulate::use_virtualenv(Sys.getenv("COORSIM_PYTHON", unset = "r-coorsim"))
  }
  
  
  if ("torch" %in% reticulate::py_list_packages()$package && !force) {
    warning("Skipping installation of torch. Use `force` to force installation or update. ")
    return(invisible(NULL))
  }
  
  
  # Run the function to install the appropriate torch version
  install_torch(pytorch_cuda_url = pytorch_cuda_url)
  
  
  reticulate::py_install("torch.utils", pip = TRUE)
  
  
  if ("transformers" %in% reticulate::py_list_packages()$package && !force) {
    warning("Skipping installation of transformers library. Use `force` to force installation or update. ")
    return(invisible(NULL))
  }
  
  reticulate::py_install("transformers", pip = TRUE)
  
  
  py_check_version <- function(package, ...) {
    packages <- reticulate::py_list_packages(...)
    packages$version[packages$package == package]
  }
  
  
  message("Successful installed torch ", 
          py_check_version("torch", envname = Sys.getenv("COORSIM_PYTHON", unset = "r-coorsim")),
          " and transformers ",
          py_check_version("torch", envname = Sys.getenv("COORSIM_PYTHON", unset = "r-coorsim")),
          ".")
  
  invisible(NULL)
}