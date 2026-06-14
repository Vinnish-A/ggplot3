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
  vendor <- file.path(dirname(template), "three.module.js")
  if (file.exists(vendor)) {
    vendor_name <- "ggplot3scene-three.module.js"
    vendor_target <- file.path(dirname(normalizePath(file, mustWork = FALSE)), vendor_name)
    dir.create(dirname(vendor_target), recursive = TRUE, showWarnings = FALSE)
    file.copy(vendor, vendor_target, overwrite = TRUE)
    html <- sub(
      'import \\* as THREE from "https://cdn\\.jsdelivr\\.net/npm/three@0\\.160\\.1/build/three\\.module\\.js";',
      paste0('import * as THREE from "./', vendor_name, '";'),
      html
    )
  }
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

ggsave3 <- function(filename, plot, width = 6.72, height = 4.8,
                    units = c("in", "cm", "mm", "px"),
                    dpi = 100,
                    view = NULL,
                    vector_mode = c("hybrid", "simple"),
                    ...) {
  units <- match.arg(units)
  vector_mode <- match.arg(vector_mode)
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% c("png", "svg")) {
    stop("ggsave3() currently supports .png and .svg files.", call. = FALSE)
  }
  scene <- as_scene3d(plot)
  scene$render <- strip_classes(render_spec(width = width, height = height, units = units, dpi = dpi, export = TRUE))
  if (!is.null(view)) {
    scene <- apply_view3d(scene, view)
  }
  if (identical(ext, "png")) {
    return(export_png3d(scene, filename))
  }
  export_svg3d(scene, filename, vector_mode = vector_mode)
}

export_png3d <- function(scene, file, chromium = NULL) {
  scene <- as_scene3d(scene)
  render <- scene$render %||% strip_classes(render_spec(export = TRUE))
  scene$render <- render
  out_dir <- dirname(normalizePath(file, mustWork = FALSE))
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  html_file <- file.path(out_dir, paste0(".ggplot3scene-export-", Sys.getpid(), "-", as.integer(stats::runif(1, 1, 1e8)), ".html"))
  on.exit(unlink(html_file), add = TRUE)
  export_html(scene, html_file)
  chromium <- chromium %||% find_chromium()
  if (is.null(chromium)) {
    stop("Cannot find Chromium. Install chromium, chromium-browser, google-chrome, or google-chrome-stable to use ggsave3()/export_png3d().", call. = FALSE)
  }
  url <- paste0("file://", normalizePath(html_file, mustWork = TRUE))
  args <- c(
    "--headless",
    "--disable-gpu",
    "--no-sandbox",
    "--hide-scrollbars",
    "--allow-file-access-from-files",
    "--use-gl=swiftshader",
    "--enable-unsafe-swiftshader",
    "--ignore-gpu-blocklist",
    paste0("--force-device-scale-factor=", render$devicePixelRatio %||% 1),
    paste0("--window-size=", render$cssWidth, ",", render$cssHeight),
    "--run-all-compositor-stages-before-draw",
    "--virtual-time-budget=5000",
    paste0("--screenshot=", normalizePath(file, mustWork = FALSE)),
    url
  )
  status <- suppressWarnings(system2(chromium, args = args, stdout = TRUE, stderr = TRUE))
  exit_status <- attr(status, "status") %||% 0
  if (!identical(exit_status, 0)) {
    stop("Chromium screenshot failed:\n", paste(status, collapse = "\n"), call. = FALSE)
  }
  if (!file.exists(file)) {
    stop("Chromium did not create the requested PNG file.", call. = FALSE)
  }
  invisible(normalizePath(file, mustWork = FALSE))
}

export_svg3d <- function(scene, file, vector_mode = "hybrid") {
  scene <- as_scene3d(scene)
  render <- scene$render %||% strip_classes(render_spec(export = TRUE))
  scene$render <- render
  out_dir <- dirname(normalizePath(file, mustWork = FALSE))
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  png_file <- file.path(out_dir, paste0(".ggplot3scene-svg-", Sys.getpid(), "-", as.integer(stats::runif(1, 1, 1e8)), ".png"))
  on.exit(unlink(png_file), add = TRUE)
  raster_scene <- scene
  raster_scene$render$svgRasterMode <- TRUE
  export_png3d(raster_scene, png_file)
  png_data <- readBin(png_file, what = "raw", n = file.info(png_file)$size)
  encoded <- jsonlite::base64_enc(png_data)
  width <- render$pixelWidth
  height <- render$pixelHeight
  svg_width <- svg_length(render$width, render$units)
  svg_height <- svg_length(render$height, render$units)
  overlay <- svg_vector_overlay(scene, width, height)
  svg <- paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="', svg_width, '" height="', svg_height,
    '" viewBox="0 0 ', width, ' ', height, '">\n',
    '<image href="data:image/png;base64,', encoded, '" x="0" y="0" width="', width, '" height="', height, '"/>\n',
    overlay,
    "\n</svg>\n"
  )
  writeLines(svg, file, useBytes = TRUE)
  invisible(normalizePath(file, mustWork = FALSE))
}

svg_length <- function(value, units) {
  if (identical(units, "px")) {
    return(paste0(value, "px"))
  }
  paste0(value, units)
}

svg_vector_overlay <- function(scene, width, height) {
  labels <- scene$labels %||% list()
  theme <- scene$theme %||% list()
  title <- labels$title
  subtitle <- labels$subtitle
  caption <- labels$caption
  pieces <- character()
  title_style <- theme$plot$title %||% list()
  subtitle_style <- theme$plot$subtitle %||% list()
  caption_style <- theme$plot$caption %||% list()
  if (!is.null(title)) {
    pieces <- c(pieces, svg_text(title, 12, 24, title_style, "start"))
  }
  if (!is.null(subtitle)) {
    pieces <- c(pieces, svg_text(subtitle, 12, 44, subtitle_style, "start"))
  }
  if (!is.null(caption)) {
    pieces <- c(pieces, svg_text(caption, width - 12, height - 10, caption_style, "end"))
  }
  if (length(scene$guides %||% list()) > 0L) {
    x <- width - 132
    y <- 110
    for (guide in scene$guides) {
      pieces <- c(pieces, svg_text(guide$title %||% guide$aesthetic, x, y, theme$legend$title %||% list(), "start"))
      y <- y + 18
      for (entry in guide$entries %||% list()) {
        colour <- entry$glyph$colour %||% entry$value %||% "#3366CC"
        pieces <- c(pieces, paste0('<circle cx="', x + 6, '" cy="', y - 4, '" r="3" fill="', htmltools::htmlEscape(colour), '"/>'))
        pieces <- c(pieces, svg_text(entry$label %||% "", x + 18, y, theme$legend$text %||% list(), "start"))
        y <- y + 18
      }
      y <- y + 8
    }
  }
  paste(pieces, collapse = "\n")
}

svg_text <- function(text, x, y, style, anchor) {
  size <- style$size %||% 11
  colour <- style$colour %||% "#222222"
  weight <- if (identical(style$face, "bold")) " font-weight=\"700\"" else ""
  paste0(
    '<text x="', x, '" y="', y, '" fill="', htmltools::htmlEscape(colour),
    '" font-family="Arial, sans-serif" font-size="', size,
    '" text-anchor="', anchor, '"', weight, ">",
    htmltools::htmlEscape(text),
    "</text>"
  )
}

apply_view3d <- function(scene, view) {
  if (is.character(view) && length(view) == 1L) {
    view <- jsonlite::read_json(view, simplifyVector = FALSE)
  }
  if (!is.list(view) || is.null(view$camera)) {
    stop("view must be a view.json path or a list with a camera entry.", call. = FALSE)
  }
  scene$view <- list(camera = view$camera)
  scene$camera <- merge_list_simple(scene$camera, view$camera)
  scene
}

find_chromium <- function() {
  candidates <- c("chromium", "chromium-browser", "google-chrome", "google-chrome-stable")
  for (candidate in candidates) {
    path <- Sys.which(candidate)
    if (nzchar(path)) {
      return(unname(path))
    }
  }
  NULL
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
