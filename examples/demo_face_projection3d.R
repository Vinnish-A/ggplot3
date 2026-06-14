`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "examples/demo_face_projection3d.R"
root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE)
if (!dir.exists(file.path(root, "R"))) {
  root <- normalizePath(getwd(), mustWork = FALSE)
}

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(root, quiet = TRUE)
} else {
  r_files <- file.path(root, "R", c(
    "aes3.R", "grid3d.R", "theme3d.R", "scene3d.R", "ggplot_adapter.R", "layers.R", "abs3d.R", "surface_stats.R", "surface_geoms.R", "face_projection.R", "build.R", "export.R", "demo_data.R"
  ))
  invisible(lapply(r_files, source))
}

set.seed(2718)
n <- 280L
df <- data.frame(
  x = c(stats::rnorm(n / 2, -1.2, 0.55), stats::rnorm(n / 2, 1.15, 0.58)),
  y = c(stats::rnorm(n / 2, 0.95, 0.42), stats::rnorm(n / 2, -0.65, 0.46))
)
df$z <- 0.65 * exp(-0.35 * (df$x^2 + df$y^2)) + stats::rnorm(n, sd = 0.08)
df$group <- ifelse(df$x < 0, "A", "B")

p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
  geom_face_density3d(
    aes3(x, y),
    plane = "zmin",
    axes = c("x", "y"),
    offset = -0.05,
    grid_size = c(70, 70),
    bandwidth = c(0.42, 0.42),
    alpha = "combined_fade",
    fill = "#7AA6DC",
    opacity = 0.62,
    name = "floor density projection"
  ) +
  geom_point3d(size = 4, alpha = 0.9) +
  coord_3d(
    projection = "orthographic",
    position = c(2.2, -2.4, 1.7),
    origin_mode = "data_min",
    grid = grid_3d(planes = "xy", domain = "positive")
  ) +
  theme_3d_scientific()

scene <- as_scene3d(p)

html_file <- file.path(root, "demo_face_projection3d.html")
json_file <- file.path(root, "demo_face_projection3d.scene.json")

export_html(scene, html_file)
write_scene_json(scene, json_file)

cat("Wrote HTML: ", normalizePath(html_file, mustWork = FALSE), "\n", sep = "")
cat("Wrote Scene JSON: ", normalizePath(json_file, mustWork = FALSE), "\n", sep = "")
