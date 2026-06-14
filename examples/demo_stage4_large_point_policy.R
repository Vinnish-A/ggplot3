`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "examples/demo_stage4_large_point_policy.R"
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

set.seed(20260615)
n <- 100000
groups <- sprintf("class-%02d", seq_len(8))
df <- data.frame(
  x = c(rnorm(n * 0.7, -1.2, 0.45), rnorm(n * 0.3, 1.1, 0.55))[seq_len(n)],
  y = c(rnorm(n * 0.55, 0.7, 0.5), rnorm(n * 0.45, -0.8, 0.6))[seq_len(n)],
  z = rnorm(n, 0, 0.65),
  group = sample(groups, n, replace = TRUE)
)

p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
  geom_point3d(
    size = 1.5,
    alpha = 0.7,
    projection = projection_face3d(alpha = 0.22, size_scale = 0.75),
    max_points = 50000,
    sampling = "stratified"
  ) +
  labs3d(
    title = "100k point policy demo",
    subtitle = "Compiled with stratified sampling for JSON export",
    x = "x",
    y = "y",
    z = "z",
    colour = "group"
  ) +
  view_default3d(zoom = 1.15) +
  theme_3d_gray()

scene <- suppressWarnings(as_scene3d(p))

html_file <- file.path(root, "demo_stage4_large_point_policy.html")
json_file <- file.path(root, "demo_stage4_large_point_policy.scene.json")

export_html(scene, html_file)
write_scene_json(scene, json_file)

cat("Wrote HTML: ", normalizePath(html_file, mustWork = FALSE), "\n", sep = "")
cat("Wrote Scene JSON: ", normalizePath(json_file, mustWork = FALSE), "\n", sep = "")
cat("Original points: ", scene$layers[[1]]$metadata$performance$originalPointCount, "\n", sep = "")
cat("Emitted points: ", scene$layers[[1]]$metadata$performance$emittedPointCount, "\n", sep = "")
