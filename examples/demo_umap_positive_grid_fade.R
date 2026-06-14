`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "examples/demo_umap_positive_grid_fade.R"
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

set.seed(123)
centers <- data.frame(
  UMAP1 = c(-4.2, -0.2, 3.4, 2.0),
  UMAP2 = c(-2.6, 2.5, -1.5, 3.1),
  cluster = c("T cell", "B cell", "Myeloid", "Stromal")
)
n_per_cluster <- c(120, 110, 115, 85)

umap_df <- do.call(rbind, lapply(seq_len(nrow(centers)), function(i) {
  n <- n_per_cluster[[i]]
  data.frame(
    UMAP1 = stats::rnorm(n, centers$UMAP1[[i]], sd = 0.48 + i * 0.04),
    UMAP2 = stats::rnorm(n, centers$UMAP2[[i]], sd = 0.42 + i * 0.035),
    z = 0,
    cluster = centers$cluster[[i]]
  )
}))

xgrid <- seq(min(umap_df$UMAP1) - 0.65, max(umap_df$UMAP1) + 0.65, length.out = 80)
ygrid <- seq(min(umap_df$UMAP2) - 0.65, max(umap_df$UMAP2) + 0.65, length.out = 80)

gaussian_2d <- function(x, y, cx, cy, sx, sy, weight = 1) {
  weight * exp(-0.5 * (((x - cx) / sx)^2 + ((y - cy) / sy)^2))
}

zmat <- outer(xgrid, ygrid, Vectorize(function(x, y) {
  sum(mapply(
    gaussian_2d,
    cx = centers$UMAP1,
    cy = centers$UMAP2,
    sx = c(0.95, 0.9, 1.05, 0.85),
    sy = c(0.78, 0.88, 0.72, 0.8),
    weight = c(0.9, 1.0, 0.86, 0.72),
    MoreArgs = list(x = x, y = y)
  ))
}))
zmat <- zmat / max(zmat) * 1.25

density_grid <- grid2d(
  xgrid,
  ygrid,
  zmat,
  alpha = alpha_combined_fade(zmat, edge_width = 0.16, cutoff = 0.04, softness = 0.2, max_alpha = 0.78),
  name = "synthetic density",
  metadata = list(method = "synthetic-gaussian-mixture", computedBy = "R")
)

p <- ggplot3(umap_df, aes3(UMAP1, UMAP2, z = z, colour = cluster)) +
  geom_surface_grid3d(
    grid = density_grid,
    fill = "#4477AA"
  ) +
  geom_point3d(size = 3, alpha = 0.85) +
  coord_umap3d(origin_mode = "data_min", positive_grid = TRUE) +
  theme_3d_umap()

scene <- as_scene3d(p)

html_file <- file.path(root, "demo_umap_positive_grid_fade.html")
json_file <- file.path(root, "demo_umap_positive_grid_fade.scene.json")

export_html(scene, html_file)
write_scene_json(scene, json_file)

cat("Wrote HTML: ", normalizePath(html_file, mustWork = FALSE), "\n", sep = "")
cat("Wrote Scene JSON: ", normalizePath(json_file, mustWork = FALSE), "\n", sep = "")
