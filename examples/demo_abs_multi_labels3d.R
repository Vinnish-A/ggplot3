`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "examples/demo_abs_multi_labels3d.R"
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

set.seed(441)
centers <- data.frame(
  x = c(-1.1, 0.85, 0.05),
  y = c(-0.55, 0.15, 1.0),
  z = c(0.0, 0.45, -0.15),
  group = c("alpha", "beta", "gamma")
)
df <- do.call(rbind, lapply(seq_len(nrow(centers)), function(i) {
  data.frame(
    x = stats::rnorm(120, centers$x[[i]], 0.28),
    y = stats::rnorm(120, centers$y[[i]], 0.28),
    z = stats::rnorm(120, centers$z[[i]], 0.22),
    group = centers$group[[i]]
  )
}))
label_df <- transform(centers, label = paste("cluster", group))
alpha_label <- label_df[label_df$group == "alpha", ]
beta_label <- label_df[label_df$group == "beta", ]
gamma_label <- label_df[label_df$group == "gamma", ]

p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
  geom_point3d(size = 5, alpha = 0.86) +
  geom_abs_label3d(
    data = alpha_label,
    mapping = aes3(x, y, z = z, label = label),
    route = abs_route(abs_anchor(), abs_elbow("up-left", first = 58, second = 120)),
    line_width = 3,
    leader_occlusion = "depth-test",
    anchor_occlusion = "depth-test",
    label_occlusion = "none"
  ) +
  geom_abs_label3d(
    data = beta_label,
    mapping = aes3(x, y, z = z, label = label),
    route = abs_route(abs_anchor(), abs_elbow("up-right", first = 58, second = 120)),
    line_width = 3,
    leader_occlusion = "depth-test",
    anchor_occlusion = "depth-test",
    label_occlusion = "none"
  ) +
  geom_abs_label3d(
    data = gamma_label,
    mapping = aes3(x, y, z = z, label = label),
    route = abs_route(abs_anchor(), abs_elbow("down-right", first = 58, second = 120)),
    line_width = 3,
    leader_occlusion = "depth-test",
    anchor_occlusion = "depth-test",
    label_occlusion = "none"
  ) +
  coord_3d(projection = "orthographic", position = c(2.1, -2.5, 1.6), zoom = 1.08) +
  theme_3d_scientific()

scene <- as_scene3d(p)

html_file <- file.path(root, "demo_abs_multi_labels3d.html")
json_file <- file.path(root, "demo_abs_multi_labels3d.scene.json")

export_html(scene, html_file)
write_scene_json(scene, json_file)

cat("Wrote HTML: ", normalizePath(html_file, mustWork = FALSE), "\n", sep = "")
cat("Wrote Scene JSON: ", normalizePath(json_file, mustWork = FALSE), "\n", sep = "")
