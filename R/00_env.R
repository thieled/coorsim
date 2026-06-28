#' @title Initialize the coorsim Python environment
#'
#' @description
#' Declares all required Python packages via \code{\link[reticulate]{py_require}},
#' which provisions them using uv into an ephemeral virtual environment.
#' PyTorch is installed with \code{UV_TORCH_BACKEND=auto}, which lets uv
#' auto-detect NVIDIA (CUDA), AMD (ROCm), and Intel GPUs and pick the correct
#' wheels — no manual driver querying needed. Pass \code{gpu = FALSE} to force
#' CPU-only onnxruntime.
#'
#' The function is called automatically at the start of each coorsim function
#' and is a no-op after the first successful call within a session.
#'
#' @details
#' Non-torch packages are declared with \code{py_require()} so that reticulate /
#' uv can resolve them together. PyTorch is handled by setting
#' \code{UV_TORCH_BACKEND=auto}, which instructs uv to query for CUDA, ROCm,
#' and Intel GPU drivers and select the matching PyTorch index automatically.
#'
#' \code{onnxruntime-gpu} only supports CUDA, so the \code{gpu} parameter
#' reflects NVIDIA presence only; AMD and Intel users get CPU onnxruntime while
#' still getting GPU-accelerated PyTorch.
#'
#' @param gpu Logical. Whether to install \code{onnxruntime-gpu} (CUDA only).
#'   Defaults to auto-detection via \code{check_gpu()}.
#' @param uv_cache_dir Character (optional). Directory used by uv to install python libraries.
#' @param models_dir Character (optional).  Directory used to cache huggingface models.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
initialize_coorsim <- function(gpu = check_gpu(),
                               uv_cache_dir = NULL,        # Optional: custom cache directory
                               models_dir = NULL        # Optional: custom models directory

) {
  if (isTRUE(.env$initialized_coorsim)) {
    return(invisible(TRUE))
  }

   # Set custom paths if provided
  if (!is.null(uv_cache_dir)) {
    Sys.setenv(UV_CACHE_DIR = uv_cache_dir)
  }
  
  if (!is.null(models_dir)) {
    Sys.setenv(TORCH_HOME = models_dir)
    Sys.setenv(HF_HOME = models_dir)
    Sys.setenv(TRANSFORMERS_CACHE = file.path(models_dir, "transformers"))
  }

  # Set torch backend FIRST, before any Python/torch initialization
  Sys.setenv(UV_TORCH_BACKEND = "auto")
  if (isTRUE(gpu)) {
    Sys.setenv(UV_TORCH_DEVICE = "cuda")
  }

  # Non-torch NLP stack — resolved together by uv via py_require
  reticulate::py_require(
    c(
      "transformers",
      "numpy"
    )
  )

  # Torch stack with environment variables already set
  reticulate::py_require(c("torch", "torchvision", "torchaudio"))

  if (isTRUE(gpu)) {
    reticulate::py_require("onnxruntime-gpu")
  } else {
    reticulate::py_require("onnxruntime")
  }

  # Source the Python script from the package
  reticulate::source_python(
    system.file("python",
                "get_transformer_embeddings.py",
                package = "coorsim",
                mustWork = TRUE)
  )

  .env$initialized_coorsim <- TRUE
  invisible(TRUE)
}


#' Check GPU Availability
#'
#' Detects whether an NVIDIA GPU is available on the system.
#'
#' @return Logical. \code{TRUE} if a GPU is detected, \code{FALSE} otherwise.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   check_gpu()
#' }
#'
check_gpu <- function() {
  if (!is.null(.env$gpu_info)) {
    return(.env$gpu_info$n > 0)
  }
  has_nvidia <- nzchar(Sys.which("nvidia-smi")) || file.exists("/dev/nvidiactl")
  .env$gpu_info <- list(n = as.integer(has_nvidia))
  has_nvidia
}


#' Check coorsim Backend Configuration
#'
#' Displays diagnostic information about the coorsim Python backend setup.
#'
#' @return Invisibly returns a list with backend configuration details.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   coorsim::check_backend()
#' }
check_backend <- function() {
  gpu_available <- check_gpu()
  
  if (!reticulate::py_available()) {
    cli::cli_alert_warning("Python not initialized. Call initialize_coorsim() first.")
    return(invisible(NULL))
  }
  
  torch <- reticulate::import("torch")
  torch_version <- torch$`__version__`
  
  home <- path.expand("~")
  uv_cache <- file.path(Sys.getenv("LOCALAPPDATA"), "uv")
  hf_cache <- file.path(home, ".cache", "huggingface")
  
  cli::cli_h2("coorsim Backend")
  cli::cli_ul(c(
    "GPU detected: {if(gpu_available) 'YES' else 'NO'}",
    "PyTorch: {torch_version}",
    "UV cache: {uv_cache}",
    "Models cache: {hf_cache}"
  ))
  
  invisible(list(gpu = gpu_available, torch_version = torch_version))
}