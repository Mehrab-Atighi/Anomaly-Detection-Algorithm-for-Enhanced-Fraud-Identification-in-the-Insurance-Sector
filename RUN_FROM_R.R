# OPTIONAL R launcher for the Python implementation; not an R reimplementation.
# Revised model results were produced with Python; final plots use R/ggplot2.
# This optional launcher invokes the combined workflow; it is not the model engine.
# Run from the package root: Rscript RUN_FROM_R.R --rebuild-only
args <- commandArgs(trailingOnly = TRUE)
python <- Sys.getenv("SAD_PYTHON", unset = "")
if (!nzchar(python)) python <- Sys.which("python3")
if (!nzchar(python)) python <- Sys.which("python")
if (!nzchar(python)) stop("Install Python 3.12 or set SAD_PYTHON to its executable.")
if (!file.exists("RUN_ALL.py")) stop("Set the working directory to the extracted package root.")
if (!length(args)) args <- "--rebuild-only"
Sys.setenv(OPENBLAS_NUM_THREADS = "1", OMP_NUM_THREADS = "1")
status <- system2(python, args = c(shQuote("RUN_ALL.py"), vapply(args, shQuote, character(1))))
if (status != 0L) stop("Python workflow returned exit status ", status)
