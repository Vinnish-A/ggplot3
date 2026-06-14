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
- `geom_point3d()` point cloud layer.
- `geom_surface_grid3d()` gridded surface layer.
- `coord_3d()` camera and projection settings.
- `theme_3d()` and `theme_3d_scientific()` JSON-compatible theme defaults.
- `element_material_3d()` and `element_light_3d()` for material/light theme entries.
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

## Run Tests

```sh
Rscript -e 'testthat::test_dir("tests/testthat")'
```

## Current Limits

- No Shiny.
- No ggplot2 internals, quosures, grid, or grobs.
- No full scale system; colour mapping is limited to discrete character/factor columns.
- No R-side headless PNG export.
- No local three.js vendor bundle yet.
- Camera interaction is intentionally minimal: drag rotates and wheel zooms; pan is not implemented yet.
- Point size is rendered as screen-space average size in the prototype renderer.
- Scene3D schema is intentionally minimal.
- Theme3D intentionally does not control camera, projection, stats, scale domains, or data transforms.

## Roadmap

1. Stabilize Scene3D v0.1 validation and clearer error messages.
2. Add local vendor fallback for three.js and OrbitControls.
3. Expand renderer controls for visibility, opacity, reset, PNG, and view import.
4. Add focused tests for theme resolution, colour mapping, and invalid layers.
5. Add R-side scientific stats such as KDE surface generation without moving stat work into JS.
