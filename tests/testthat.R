library(testthat)

`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "tests/testthat.R"
root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE)
if (!dir.exists(file.path(root, "R"))) {
  root <- normalizePath(getwd(), mustWork = FALSE)
}

r_files <- file.path(root, "R", c(
  "aes3.R", "theme3d.R", "scene3d.R", "layers.R", "build.R", "export.R", "demo_data.R"
))
invisible(lapply(r_files, source))

test_dir(file.path(root, "tests", "testthat"))
