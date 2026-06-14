`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "examples/demo_surface_stats3d.R"
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

set.seed(314)
n <- 360L
df <- data.frame(
  x = c(stats::rnorm(n / 2, -1.1, 0.48), stats::rnorm(n / 2, 1.2, 0.58)),
  y = c(stats::rnorm(n / 2, -0.6, 0.5), stats::rnorm(n / 2, 0.85, 0.52))
)
df$z <- 0
df$group <- ifelse(df$x < 0, "left mode", "right mode")

surface_fun <- function(x, y) {
  0.18 * sin(x * 1.4) * cos(y * 1.2) - 0.18
}

p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
  stat_function_surface3d(
    fun = surface_fun,
    xlim = c(-3.2, 3.4),
    ylim = c(-2.6, 2.8),
    grid_size = c(48, 48),
    fill = "#E5E7EB",
    alpha = 0.38,
    surface_alpha = "edge_fade",
    tessellation = "right2",
    name = "reference function"
  ) +
  stat_density_surface3d(
    aes3(x, y),
    grid_size = c(72, 72),
    bandwidth = c(0.42, 0.42),
    bounds = list(x = c(-3.2, 3.4), y = c(-2.6, 2.8)),
    alpha = "combined_fade",
    fill = "#4477AA",
    opacity = 0.72,
    height = 1.15,
    tessellation = "right1",
    name = "R-computed density"
  ) +
  geom_point3d(size = 3, alpha = 0.82) +
  coord_3d(
    projection = "orthographic",
    origin_mode = "data_min",
    grid = grid_3d(planes = "xy", domain = "positive")
  ) +
  theme_3d_scientific()

scene <- as_scene3d(p)

html_file <- file.path(root, "demo_surface_stats3d.html")
json_file <- file.path(root, "demo_surface_stats3d.scene.json")

export_html(scene, html_file)
write_scene_json(scene, json_file)

cat("Wrote HTML: ", normalizePath(html_file, mustWork = FALSE), "\n", sep = "")
cat("Wrote Scene JSON: ", normalizePath(json_file, mustWork = FALSE), "\n", sep = "")
