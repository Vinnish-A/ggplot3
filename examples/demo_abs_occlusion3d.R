`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "examples/demo_abs_occlusion3d.R"
root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE)
if (!dir.exists(file.path(root, "R"))) {
  root <- normalizePath(getwd(), mustWork = FALSE)
}

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(root, quiet = TRUE)
} else {
  r_files <- file.path(root, "R", c(
    "aes3.R", "grid3d.R", "theme3d.R", "scene3d.R", "ggplot_adapter.R", "layers.R", "abs3d.R", "surface_stats.R", "face_projection.R", "build.R", "export.R", "demo_data.R"
  ))
  invisible(lapply(r_files, source))
}

set.seed(440)
background <- data.frame(
  x = stats::rnorm(220, 0, 0.75),
  y = stats::rnorm(220, 0, 0.65),
  z = stats::rnorm(220, 0, 0.55),
  group = "background"
)
foreground <- data.frame(
  x = stats::rnorm(80, 0.45, 0.18),
  y = stats::rnorm(80, 0.35, 0.18),
  z = stats::rnorm(80, 0.95, 0.12),
  group = "foreground"
)
df <- rbind(background, foreground)
label_df <- data.frame(x = -0.15, y = -0.05, z = 0.05, label = "depth-tested leader")

p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
  geom_point3d(size = 7, alpha = 0.9) +
  geom_abs_label3d(
    data = label_df,
    mapping = aes3(x, y, z = z, label = label),
    route = abs_route(abs_anchor(), abs_up(88), abs_right(180)),
    line_width = 7,
    point_size = 7,
    leader_occlusion = "depth-test",
    anchor_occlusion = "depth-test",
    label_occlusion = "none"
  ) +
  coord_3d(projection = "orthographic", position = c(1.1, -2.8, 1.5), zoom = 1.1) +
  theme_3d_scientific() +
  theme_3d(abs.line = element_abs_line(color = "#111827", width = 7, opacity = 1))

scene <- as_scene3d(p)

html_file <- file.path(root, "demo_abs_occlusion3d.html")
json_file <- file.path(root, "demo_abs_occlusion3d.scene.json")

export_html(scene, html_file)
write_scene_json(scene, json_file)

cat("Wrote HTML: ", normalizePath(html_file, mustWork = FALSE), "\n", sep = "")
cat("Wrote Scene JSON: ", normalizePath(json_file, mustWork = FALSE), "\n", sep = "")
