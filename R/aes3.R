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
  unclass(normalize_aes3_mapping(mapping))
}

normalize_aes3_mapping <- function(mapping) {
  if (is.null(mapping)) {
    return(NULL)
  }
  if (inherits(mapping, "ggplot3scene_aes")) {
    return(mapping)
  }
  if (!is.list(mapping)) {
    stop("mapping must be created with aes3() or be a ggplot2-like aes mapping.", call. = FALSE)
  }

  aes_names <- names(mapping)
  if (is.null(aes_names) || any(!nzchar(aes_names))) {
    stop("ggplot2-like aes mappings must have named aesthetics.", call. = FALSE)
  }
  out <- lapply(mapping, mapping_expr_name)
  class(out) <- c("ggplot3scene_aes", "list")
  out
}

mapping_expr_name <- function(expr) {
  if (is.character(expr) && length(expr) == 1L) {
    return(expr)
  }
  if (inherits(expr, "quosure") && length(expr) >= 2L) {
    expr <- expr[[2L]]
  } else if (inherits(expr, "formula") && length(expr) >= 2L) {
    expr <- expr[[2L]]
  }
  if (is.name(expr)) {
    return(as.character(expr))
  }
  if (is.call(expr) && identical(expr[[1L]], as.name("I")) && length(expr) == 2L && is.name(expr[[2L]])) {
    return(as.character(expr[[2L]]))
  }
  deparse(expr, width.cutoff = 500L)
}
