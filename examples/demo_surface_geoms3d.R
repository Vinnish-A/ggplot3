`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "examples/demo_surface_geoms3d.R"
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

x <- seq(-2.5, 2.5, length.out = 26)
y <- seq(-2.5, 2.5, length.out = 26)
z <- outer(x, y, function(px, py) 0.55 * sin(px) * cos(py))

vertices <- as.matrix(expand.grid(x = x, y = y))
vertices <- cbind(vertices[, 1], vertices[, 2], as.vector(z))
faces <- do.call(rbind, lapply(seq_len(length(x) - 1L), function(i) {
  do.call(rbind, lapply(seq_len(length(y) - 1L), function(j) {
    a <- (j - 1L) * length(x) + i
    b <- (j - 1L) * length(x) + i + 1L
    c <- j * length(x) + i + 1L
    d <- j * length(x) + i
    rbind(c(a, b, d), c(b, c, d))
  }))
}))
mesh <- surface_mesh(
  vertices,
  faces,
  name = "mesh surface",
  metadata = list(source = "R mesh construction")
)

contours <- contour_stack(
  lapply(c(-0.35, 0, 0.35), function(level) {
    t <- seq(0, 2 * pi, length.out = 120)
    radius <- 1.15 + abs(level)
    data.frame(
      x = radius * cos(t),
      y = radius * sin(t),
      z = rep(level, length(t))
    )
  }),
  levels = c(-0.35, 0, 0.35),
  name = "reference contours"
)

ridges <- ridgeline_stack(
  lapply(seq(-2, 2, length.out = 5), function(y0) {
    data.frame(
      x = x,
      y = rep(y0, length(x)),
      z = 0.65 + 0.22 * exp(-x^2) + 0.06 * sin(x * 2 + y0)
    )
  }),
  name = "lifted profiles"
)

p <- ggplot3() +
  geom_surface_mesh3d(mesh, fill = "#8DB3D3", alpha = 0.42) +
  geom_contour_stack3d(contours, colour = "#1F2937", line_width = 1.5) +
  geom_ridgeline3d(ridges, colour = "#B45309", line_width = 1.5) +
  coord_3d(
    projection = "orthographic",
    position = c(2.1, -2.4, 1.5),
    origin_mode = "data_min",
    grid = grid_3d(planes = "xy", domain = "positive"),
    axis = axis_3d(length_fraction = 0.7, arrows = TRUE)
  ) +
  theme_3d_scientific()

scene <- as_scene3d(p)

html_file <- file.path(root, "demo_surface_geoms3d.html")
json_file <- file.path(root, "demo_surface_geoms3d.scene.json")

export_html(scene, html_file)
write_scene_json(scene, json_file)

cat("Wrote HTML: ", normalizePath(html_file, mustWork = FALSE), "\n", sep = "")
cat("Wrote Scene JSON: ", normalizePath(json_file, mustWork = FALSE), "\n", sep = "")
