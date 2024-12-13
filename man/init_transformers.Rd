% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install_py_transformers.R
\name{init_transformers}
\alias{init_transformers}
\title{Initialize Transformers and Torch in the Python Environment}
\usage{
init_transformers()
}
\description{
Initializes the `transformers` and `torch` libraries in the specified Python virtual environment. 
This function checks for the required environment and packages, sources the necessary Python script, 
and verifies that the libraries are correctly initialized.
}
\details{
The function checks if the `transformers` and `torch` libraries have already been initialized.
If they are, it reports the versions of both libraries and exits. If not, it ensures that a
Python virtual environment specified by the \code{COORSIM_PYTHON} environment variable exists,
contains the required libraries, and is properly configured. If any of these prerequisites are
missing, the function raises an error.

The function also sources a Python script, \code{get_transformer_embeddings.py}, from the 
\code{coorsim} package to enable further use of the initialized libraries.

To set up the environment and install the necessary libraries, use \code{install_transformers()}.
}
\note{
The function sets the \code{"transformers_initialized"} option to \code{TRUE} upon successful
initialization.
}
\seealso{
\code{\link{install_transformers}}
}
