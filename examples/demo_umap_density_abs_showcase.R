`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "examples/demo_umap_density_abs_showcase.R"
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

set.seed(2026)
centers <- data.frame(
  UMAP1 = c(-4.0, -0.45, 3.25, 1.6),
  UMAP2 = c(-2.25, 2.45, -1.15, 3.15),
  cluster = c("Naive T", "B cell", "Myeloid", "Stromal")
)
n_per_cluster <- c(150, 130, 125, 90)

umap_df <- do.call(rbind, lapply(seq_len(nrow(centers)), function(i) {
  n <- n_per_cluster[[i]]
  data.frame(
    UMAP1 = stats::rnorm(n, centers$UMAP1[[i]], sd = 0.48 + i * 0.035),
    UMAP2 = stats::rnorm(n, centers$UMAP2[[i]], sd = 0.42 + i * 0.03),
    z = stats::rnorm(n, mean = 0.05 * i, sd = 0.045),
    cluster = centers$cluster[[i]]
  )
}))

label_df <- data.frame(
  UMAP1 = centers$UMAP1,
  UMAP2 = centers$UMAP2,
  z = c(0.18, 0.2, 0.16, 0.24),
  label = centers$cluster
)
cluster_levels <- sort(unique(as.character(umap_df$cluster)))
cluster_colors <- stats::setNames(grDevices::hcl.colors(max(3L, length(cluster_levels)), "Dark 3")[seq_along(cluster_levels)], cluster_levels)

routes <- list(
  abs_route(abs_anchor(), abs_up(76), abs_left(150)),
  abs_route(abs_anchor(), abs_up(78), abs_right(155)),
  abs_route(abs_anchor(), abs_down(70), abs_right(145)),
  abs_route(abs_anchor(), abs_up(92), abs_right(140))
)

p <- ggplot3(umap_df, aes3(UMAP1, UMAP2, z = z, colour = cluster)) +
  geom_face_density3d(
    aes3(UMAP1, UMAP2),
    plane = "zmin",
    axes = c("x", "y"),
    offset = -0.08,
    grid_size = c(84, 84),
    bandwidth = c(0.5, 0.5),
    alpha = "combined_fade",
    fill = "#BFD7F0",
    opacity = 0.55,
    name = "floor density projection"
  ) +
  stat_density_surface3d(
    aes3(UMAP1, UMAP2),
    grid_size = c(88, 88),
    bandwidth = c(0.5, 0.5),
    alpha = "combined_fade",
    fill = "#4477AA",
    opacity = 0.68,
    height = 0.85,
    tessellation = "right1",
    name = "R density surface"
  ) +
  geom_point3d(size = 3, alpha = 0.86) +
  geom_abs_label3d(
    data = label_df[1, ],
    mapping = aes3(UMAP1, UMAP2, z = z, label = label),
    route = routes[[1]],
    line_width = 3,
    leader_occlusion = "depth-test",
    anchor_occlusion = "depth-test",
    label_occlusion = "none"
  ) +
  geom_abs_label3d(
    data = label_df[2, ],
    mapping = aes3(UMAP1, UMAP2, z = z, label = label),
    route = routes[[2]],
    line_width = 3,
    leader_occlusion = "depth-test",
    anchor_occlusion = "depth-test",
    label_occlusion = "none"
  ) +
  geom_abs_label3d(
    data = label_df[3, ],
    mapping = aes3(UMAP1, UMAP2, z = z, label = label),
    route = routes[[3]],
    line_width = 3,
    leader_occlusion = "depth-test",
    anchor_occlusion = "depth-test",
    label_occlusion = "none"
  ) +
  geom_abs_label3d(
    data = label_df[4, ],
    mapping = aes3(UMAP1, UMAP2, z = z, label = label),
    route = routes[[4]],
    line_width = 3,
    leader_occlusion = "depth-test",
    anchor_occlusion = "depth-test",
    label_occlusion = "none"
  ) +
  coord_umap3d(
    origin_mode = "data_min",
    positive_grid = TRUE,
    projection = "orthographic",
    axis = axis_3d(length_fraction = 0.62, arrows = TRUE)
  ) +
  theme_3d_umap() +
  guide_legend_scene3d(
    aesthetic = "colour",
    title = "Cluster",
    labels = centers$cluster,
    values = unname(cluster_colors[centers$cluster])
  ) +
  guide_colorbar_scene3d(
    aesthetic = "density",
    title = "Density",
    domain = c(0, 1),
    palette = c("#FFFFFF", "#BFD7F0", "#4477AA")
  )

scene <- as_scene3d(p)

html_file <- file.path(root, "demo_umap_density_abs_showcase.html")
json_file <- file.path(root, "demo_umap_density_abs_showcase.scene.json")

export_html(scene, html_file)
write_scene_json(scene, json_file)

cat("Wrote HTML: ", normalizePath(html_file, mustWork = FALSE), "\n", sep = "")
cat("Wrote Scene JSON: ", normalizePath(json_file, mustWork = FALSE), "\n", sep = "")
