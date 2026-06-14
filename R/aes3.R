aes3 <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1]
  if (length(exprs) == 0) {
    out <- list()
    class(out) <- c("ggplot3scene_aes", "list")
    return(out)
  }

  aes_names <- names(exprs)
  if (is.null(aes_names)) {
    aes_names <- rep("", length(exprs))
  }

  positional <- c("x", "y", "z")
  out <- list()
  positional_i <- 1L

  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    aes_name <- aes_names[[i]]
    expr_name <- deparse(expr, width.cutoff = 500L)

    if (!nzchar(aes_name)) {
      if (positional_i <= length(positional)) {
        aes_name <- positional[[positional_i]]
        positional_i <- positional_i + 1L
      } else {
        aes_name <- expr_name
      }
    }

    out[[aes_name]] <- expr_name
  }

  class(out) <- c("ggplot3scene_aes", "list")
  out
}

as_mapping <- function(mapping) {
  if (is.null(mapping)) {
    return(list())
  }
  if (!inherits(mapping, "ggplot3scene_aes")) {
    stop("mapping must be created with aes3().", call. = FALSE)
  }
  unclass(mapping)
}
