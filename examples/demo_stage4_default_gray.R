`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "examples/demo_stage4_default_gray.R"
root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE)
if (!dir.exists(file.path(root, "R"))) {
  root <- normalizePath(getwd(), mustWork = FALSE)
}

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(root, quiet = TRUE)
} else {
  r_files <- file.path(root, "R", c(
    "aes3.R", "grid3d.R", "layout.R", "theme3d.R", "camera.R", "scene3d.R", "ggplot_adapter.R", "layers.R", "abs3d.R", "surface_stats.R", "surface_geoms.R", "face_projection.R", "build.R", "export.R", "demo_data.R"
  ))
  invisible(lapply(r_files, source))
}

if (requireNamespace("ggplot2", quietly = TRUE)) {
  df <- as.data.frame(ggplot2::mpg)
} else {
  set.seed(42)
  classes <- c("2seater", "compact", "midsize", "minivan", "pickup", "subcompact", "suv")
  df <- data.frame(
    displ = runif(180, 1.5, 7),
    hwy = runif(180, 12, 45),
    cty = runif(180, 9, 30),
    class = sample(classes, 180, replace = TRUE)
  )
}

p <- ggplot3(df, aes3(displ, hwy, z = cty, colour = class)) +
  geom_point3d(size = 2.2, projection = "faces") +
  labs3d(x = "displ", y = "hwy", z = "cty", colour = "class") +
  view_default3d() +
  theme_3d_gray()

scene <- as_scene3d(p)
html_file <- file.path(root, "demo_stage4_default_gray.html")
json_file <- file.path(root, "demo_stage4_default_gray.scene.json")
png_file <- file.path(root, "demo-default.png")

export_html(scene, html_file)
write_scene_json(scene, json_file)
ggsave3(png_file, p, width = 6.72, height = 4.8, units = "in", dpi = 100)

cat("Wrote HTML: ", normalizePath(html_file, mustWork = FALSE), "\n", sep = "")
cat("Wrote Scene JSON: ", normalizePath(json_file, mustWork = FALSE), "\n", sep = "")
cat("Wrote PNG: ", normalizePath(png_file, mustWork = FALSE), "\n", sep = "")
