`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "examples/demo_abs_label3d.R"
root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE)
if (!dir.exists(file.path(root, "R"))) {
  root <- normalizePath(getwd(), mustWork = FALSE)
}

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(root, quiet = TRUE)
} else {
  r_files <- file.path(root, "R", c(
    "aes3.R", "grid3d.R", "theme3d.R", "scene3d.R", "layers.R", "abs3d.R", "build.R", "export.R", "demo_data.R"
  ))
  invisible(lapply(r_files, source))
}

set.seed(321)
n <- 420L
df <- data.frame(
  x = c(stats::rnorm(n * 0.65, 0.0, 0.72), stats::rnorm(n * 0.35, 1.15, 0.48)),
  y = c(stats::rnorm(n * 0.65, 0.0, 0.68), stats::rnorm(n * 0.35, 0.75, 0.42)),
  z = c(stats::rnorm(n * 0.65, 0.0, 0.62), stats::rnorm(n * 0.35, 0.55, 0.35))
)
df$group <- ifelse(df$x + df$y + df$z > 1.2, "foreground", "background")

anchor_i <- which.min(abs(df$x) + abs(df$y) + abs(df$z))
label_df <- data.frame(
  x = df$x[[anchor_i]],
  y = df$y[[anchor_i]],
  z = df$z[[anchor_i]],
  label = "anchored cluster core"
)

p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
  geom_point3d(size = 5, alpha = 0.82) +
  geom_abs_label3d(
    data = label_df,
    mapping = aes3(x, y, z = z, label = label),
    route = abs_route(up = 72, right = 150),
    label_offset = c(14, 0),
    point_size = 6,
    line_width = 2,
    occlusion = "depth-test"
  ) +
  coord_3d(
    projection = "orthographic",
    position = c(2.2, -2.6, 1.7),
    zoom = 1.05,
    origin_mode = "data_min",
    grid = grid_3d(
      planes = "xy",
      domain = "positive",
      axis_length_fraction = 0.58,
      axis_arrows = TRUE
    )
  ) +
  theme_3d_scientific()

scene <- as_scene3d(p)

html_file <- file.path(root, "demo_abs_label3d.html")
json_file <- file.path(root, "demo_abs_label3d.scene.json")

export_html(scene, html_file)
write_scene_json(scene, json_file)

cat("Wrote HTML: ", normalizePath(html_file, mustWork = FALSE), "\n", sep = "")
cat("Wrote Scene JSON: ", normalizePath(json_file, mustWork = FALSE), "\n", sep = "")
