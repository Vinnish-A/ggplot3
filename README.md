# ggplot3scene

`ggplot3scene` is a prototype R-first compiler for small 3D scientific figures.
R builds a language-neutral Scene3D JSON document; the browser renderer consumes
only that JSON and renders points, surface grids, camera state, theme defaults,
and lights.

This is a Phase 0 vertical slice, not a full ggplot2 extension and not a full 3D
engine.

## Current MVP

- `ggplot3()` plot object with `+` composition.
- `aes3()` mapping without quosures, rlang, ggplot2 internals, grid, or grobs.
- ggplot2-like aes input normalization and a narrow `ggplot3_from_ggplot()` adapter.
- `geom_point3d()` point cloud layer.
- `geom_surface_grid3d()` gridded surface layer, preferably via `grid2d()`.
- `geom_surface_mesh3d()`, `geom_contour_stack3d()`, and `geom_ridgeline3d()` for non-grid surface objects.
- `grid2d()` reusable surface grid objects with optional alpha/mask payloads.
- R-side surface-producing stats: `stat_density_surface3d()`, `stat_function_surface3d()`, and `stat_smooth_surface3d()`.
- Scene3D-native face projections with `geom_face_density3d()` and `position_on_plane3d()`.
- `alpha_edge_fade()`, `alpha_density_fade()`, and `alpha_combined_fade()` for soft surface alpha.
- `coord_3d()` camera and projection settings.
- `grid_3d()`, `axis_3d()`, and `coord_umap3d()` for coordinate/grid/axis display protocol.
- `guide_legend_scene3d()` and `guide_colorbar_scene3d()` for first-class guide JSON.
- `theme_3d()` and `theme_3d_scientific()` JSON-compatible theme defaults.
- `theme_3d_umap()` visual defaults for UMAP-style scenes.
- `element_material_3d()` and `element_light_3d()` for material/light theme entries.
- ABS annotations with `geom_abs_label3d()` and composable `abs_route()` commands.
- `as_scene3d()` compiler boundary.
- `write_scene_json()` for Scene3D JSON.
- `export_html()` for a standalone HTML file with embedded scene data.
- Minimal three.js renderer loaded from CDN, with built-in drag-to-rotate and wheel zoom controls.

## Install Dependencies

From the package root:

```sh
Rscript -e 'install.packages(c("jsonlite", "htmltools", "testthat"), repos="https://cloud.r-project.org")'
```

## Run Demo

```sh
Rscript examples/demo_point_surface.R
```

This writes:

- `demo.html`
- `demo.scene.json`

Open `demo.html` in a browser. The scene data is embedded in the HTML. The first
prototype loads three.js from a CDN, so first render needs network access until
a local vendor bundle is added. Camera interaction is built into the exported
HTML: drag to rotate and use the mouse wheel or trackpad scroll to zoom.

## API Example

```r
df <- data.frame(
  x = runif(200, -3, 3),
  y = runif(200, -3, 3)
)
df$z <- sin(df$x) * cos(df$y) + rnorm(200, sd = 0.12)
df$group <- ifelse(df$x + df$y > 0, "ridge", "basin")

xgrid <- seq(-3, 3, length.out = 45)
ygrid <- seq(-3, 3, length.out = 45)
zmat <- outer(xgrid, ygrid, function(x, y) sin(x) * cos(y))

p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
  geom_surface_grid3d(x = xgrid, y = ygrid, z = zmat, alpha = 0.55) +
  geom_point3d(size = 5, alpha = 0.9) +
  coord_3d(projection = "orthographic") +
  theme_3d_scientific() +
  theme_3d(
    scene.background = "#FFFFFF",
    material.surface = element_material_3d(fill = "#4477AA", opacity = 0.55),
    light.key = element_light_3d(color = "#FFFFFF", intensity = 0.85, position = c(3, -4, 5))
  )

scene <- as_scene3d(p)
write_scene_json(scene, "demo.scene.json")
export_html(scene, "demo.html")
```

## UMAP-style coordinates and positive grid

UMAP coordinates are assumed to be precomputed. `ggplot3scene` does not run
UMAP and does not infer embedding parameters in the browser. The R side compiles
already-computed coordinates into Scene3D JSON, and the renderer only displays
that protocol.

Use `coord_umap3d()` to choose visual coordinate conventions for UMAP-style
figures:

```r
p <- ggplot3(umap_df, aes3(UMAP1, UMAP2, z = z, colour = cluster)) +
  geom_surface_grid3d(
    grid = grid2d(xgrid, ygrid, zmat, alpha = alpha_combined_fade(zmat)),
    fill = "#4477AA"
  ) +
  geom_point3d(size = 3, alpha = 0.85) +
  coord_umap3d(origin_mode = "data_min", positive_grid = TRUE) +
  theme_3d_umap()
```

`positive_grid = TRUE` affects grid display only. It does not mutate source
coordinates, does not remove negative values from layer data, and does not
belong to the theme system. The compiled scene stores this under
`axes.grid.domainMode`.

`grid2d()` is the preferred input for gridded surfaces. It carries `x`, `y`,
`z`, optional per-grid alpha, optional masks, shape metadata, and protocol
metadata as plain JSON-compatible data. Surface alpha helpers can make
KDE-like or density-like surfaces fade softly near edges or low-density regions.

Run the UMAP-style demo:

```sh
Rscript examples/demo_umap_positive_grid_fade.R
```

This writes:

- `demo_umap_positive_grid_fade.html`
- `demo_umap_positive_grid_fade.scene.json`

## Surface stats

Stage 3 separates surface-producing stats from surface-rendering geoms. Surface
stats run in R, produce `grid2d()` objects, and compile to ordinary
`surface_grid` layers in Scene3D. The browser does not recompute density,
smooths, or function surfaces.

```r
p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
  stat_density_surface3d(
    aes3(x, y),
    grid_size = c(72, 72),
    bandwidth = c(0.42, 0.42),
    alpha = "combined_fade",
    tessellation = "right1"
  ) +
  geom_point3d()
```

The compiled surface stores stat metadata as JSON-compatible data:

```json
"stat": {
  "type": "density_surface",
  "method": "gaussian_kde_product_kernel",
  "gridSize": [72, 72],
  "computedBy": "R"
}
```

`grid2d()` also carries a `tessellation` protocol field. The renderer uses it
only to triangulate an already-computed surface:

```r
grid2d(xgrid, ygrid, zmat, tessellation = "right2")
```

Run the surface stat demo:

```sh
Rscript examples/demo_surface_stats3d.R
```

This writes:

- `demo_surface_stats3d.html`
- `demo_surface_stats3d.scene.json`

Surface geoms can also render mesh, contour, and ridgeline surface objects:

```r
ggplot3() +
  geom_surface_mesh3d(surface_mesh(vertices, faces)) +
  geom_contour_stack3d(contour_stack(polylines, levels = levels)) +
  geom_ridgeline3d(ridgeline_stack(profiles))
```

Run the surface geom demo:

```sh
Rscript examples/demo_surface_geoms3d.R
```

This writes:

- `demo_surface_geoms3d.html`
- `demo_surface_geoms3d.scene.json`

## Face projection

Face projection is a separate rendering space. It maps an already-computed 2D
grid onto a named 3D plane or cube face. It is not an arbitrary ggplot2 layer
projection and it does not ask the browser to compute density.

```r
p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
  geom_face_density3d(
    aes3(x, y),
    plane = "zmin",
    axes = c("x", "y"),
    offset = -0.05
  ) +
  geom_point3d()
```

The emitted layer has `type = "face_projection"` and declares `plane`, `axes`,
`offset`, `clip`, grid data, and unlit style as JSON.

Run the face projection demo:

```sh
Rscript examples/demo_face_projection3d.R
```

This writes:

- `demo_face_projection3d.html`
- `demo_face_projection3d.scene.json`

## Axes and guides

`grid_3d()` controls grid planes, grid domain, and grid breaks. `axis_3d()`
controls axis appearance and label/tick placement. Coordinate origin and grid
domain remain coordinate concerns, not theme concerns.

```r
p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
  geom_point3d() +
  coord_3d(
    grid = grid_3d(domain = "positive", planes = "xy"),
    axis = axis_3d(length_fraction = 0.6, arrows = TRUE)
  ) +
  guide_legend_scene3d(
    aesthetic = "colour",
    title = "cluster",
    labels = c("A", "B"),
    values = c("#3366CC", "#CC6633")
  )
```

Guides compile into top-level `scene$guides` entries and are rendered as a
screen/UI-space overlay by the HTML renderer.

## ggplot2-like input

`ggplot3()` can accept ggplot2-like `aes()` mappings as input. They are
normalized immediately into `aes3()` string mappings. Scene3D does not contain
quosures, formulas, ggplot2 layer objects, or ggproto objects.

```r
ggplot3(df, ggplot2::aes(x, y, z = z, colour = group)) +
  geom_point3d()
```

`ggplot3_from_ggplot()` is a narrow adapter for simple ggplot point plots. It is
an input bridge only and is not part of the rendering core.

## ABS anchored annotations

ABS means anchored billboard space: the annotation starts from a real world-space
anchor, then follows screen-space route commands, and ends in a screen-facing
label. The leader is rendered as depth-tested WebGL geometry, so foreground
points can occlude it. The label is rendered as a WebGL sprite by default so it
stays in sync with camera motion.

```r
label_df <- data.frame(x = 0, y = 0, z = 0, label = "cluster core")

p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
  geom_point3d() +
  geom_abs_label3d(
    data = label_df,
    mapping = aes3(x, y, z = z, label = label),
    route = abs_route(abs_anchor(), abs_up(72), abs_right(150)),
    leader_occlusion = "depth-test",
    anchor_occlusion = "depth-test",
    label_occlusion = "none"
  )
```

The older shorthand remains valid:

```r
abs_route(up = 72, right = 150)
```

ABS theme entries control visual defaults only:

```r
theme_3d(
  abs.line = element_abs_line(color = "#111827", width = 4),
  abs.text = element_abs_text(size = 12),
  abs.label.background = element_abs_label_background(fill = "#FFFFFF")
)
```

Theme does not control anchor position, route, occlusion, or label data.

Run the ABS demos:

```sh
Rscript examples/demo_abs_label3d.R
Rscript examples/demo_abs_occlusion3d.R
Rscript examples/demo_abs_multi_labels3d.R
```

## UMAP density ABS showcase

The Stage 3 showcase combines the core spaces:

- world space point cloud;
- R-computed density surface;
- floor face projection;
- positive grid and shortened arrow axes;
- ABS cluster labels;
- guide overlay.

```sh
Rscript examples/demo_umap_density_abs_showcase.R
```

This writes:

- `demo_umap_density_abs_showcase.html`
- `demo_umap_density_abs_showcase.scene.json`

## Run Tests

```sh
Rscript -e 'testthat::test_dir("tests/testthat")'
```

## Current Limits

- No Shiny.
- No ggplot2 internals, quosures, grid, or grobs.
- `ggplot3_from_ggplot()` currently supports only simple point layers.
- No full scale system; colour mapping is limited to discrete character/factor columns.
- Surface stats are intentionally small: density uses an R Gaussian product-kernel estimator and smooth surfaces use a quadratic `lm`.
- Face projection currently supports density grids, not arbitrary 2D ggplot layer projection.
- No R-side headless PNG export.
- No local three.js vendor bundle yet.
- Camera interaction is intentionally minimal: drag rotates and wheel zooms; pan is not implemented yet.
- Point size is rendered as screen-space average size in the prototype renderer.
- Scene3D schema is intentionally minimal.
- Theme3D intentionally does not control camera, projection, stats, scale domains, or data transforms.
- Theme3D intentionally does not control axis length, axis arrows, guide domains, or guide entries.
- `coord_umap3d()` controls display conventions only; UMAP computation is out of scope.
- ABS annotations are stable enough for anchored labels, but collision avoidance is not implemented yet.

## Roadmap

1. Stabilize Scene3D v0.1 validation and clearer error messages.
2. Add local vendor fallback for three.js and OrbitControls.
3. Expand renderer controls for visibility, opacity, reset, PNG, and view import.
4. Add focused tests for theme resolution, colour mapping, and invalid layers.
5. Add face projection, axis guides, and a product-grade UMAP showcase on top of the new surface stat protocol.
