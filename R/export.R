write_scene_json <- function(scene, file) {
  scene <- as_scene3d(scene)
  jsonlite::write_json(scene, path = file, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(normalizePath(file, mustWork = FALSE))
}

export_html <- function(scene, file, standalone = TRUE) {
  if (!isTRUE(standalone)) {
    stop("Only standalone = TRUE is implemented in this prototype.", call. = FALSE)
  }

  scene <- as_scene3d(scene)
  root <- find_dev_package_root()
  template <- file.path(root, "inst", "html", "scene3d-template.html")
  if (!file.exists(template)) {
    template <- system.file("html", "scene3d-template.html", package = "ggplot3scene")
  }
  if (!file.exists(template)) {
    stop("Cannot find scene3d-template.html.", call. = FALSE)
  }

  html <- paste(readLines(template, warn = FALSE), collapse = "\n")
  scene_json <- jsonlite::toJSON(scene, auto_unbox = TRUE, pretty = TRUE, null = "null")
  scene_json <- gsub("</script", "<\\/script", scene_json, fixed = TRUE)
  replacement <- paste0(
    '<script id="scene3d-data" type="application/json">\n',
    scene_json,
    "\n</script>"
  )
  html <- sub(
    '<script id="scene3d-data" type="application/json">[\\s\\S]*?</script>',
    replacement,
    html,
    perl = TRUE
  )

  writeLines(html, con = file, useBytes = TRUE)
  invisible(normalizePath(file, mustWork = FALSE))
}

find_dev_package_root <- function(path = getwd()) {
  path <- normalizePath(path, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "inst"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      return(getwd())
    }
    path <- parent
  }
}
