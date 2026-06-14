find_package_root <- function(path = getwd()) {
  path <- normalizePath(path, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not locate package root.", call. = FALSE)
    }
    path <- parent
  }
}

root <- find_package_root()
source_files <- file.path(root, "R", c(
  "aes3.R", "grid3d.R", "theme3d.R", "scene3d.R", "layers.R", "abs3d.R", "build.R", "export.R", "demo_data.R"
))
invisible(lapply(source_files, source))

test_that("ggplot3 creates plot object and accepts point layer", {
  df <- data.frame(x = 1:3, y = 2:4, z = 3:5, group = c("a", "b", "a"))
  p <- ggplot3(df, aes3(x, y, z = z, colour = group))
  expect_s3_class(p, "ggplot3scene_plot")

  p <- p + geom_point3d()
  expect_length(p$layers, 1)
})

test_that("as_scene3d compiles point cloud and surface grid", {
  df <- data.frame(x = 1:3, y = 2:4, z = 3:5, group = c("a", "b", "a"))
  xgrid <- c(1, 2)
  ygrid <- c(3, 4, 5)
  zmat <- outer(xgrid, ygrid, `+`)

  p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
    geom_surface_grid3d(x = xgrid, y = ygrid, z = zmat) +
    geom_point3d(size = 2, alpha = 0.8)

  scene <- as_scene3d(p)
  expect_type(scene, "list")
  expect_false(inherits(scene, "ggplot3scene_scene"))
  expect_equal(scene$schemaVersion, "0.1.0")
  expect_gte(length(scene$layers), 1)

  surface <- scene$layers[[1]]
  expect_equal(surface$type, "surface_grid")
  expect_equal(surface$data$shape, c(length(xgrid), length(ygrid)))

  points <- scene$layers[[2]]
  expect_equal(points$type, "point_cloud")
  expect_named(points$data$columns, c("x", "y", "z", "color", "size", "alpha"))
})

test_that("write_scene_json and export_html write files", {
  scene <- as_scene3d(demo_scene3d())
  json_file <- tempfile(fileext = ".json")
  html_file <- tempfile(fileext = ".html")

  write_scene_json(scene, json_file)
  export_html(scene, html_file)

  expect_true(file.exists(json_file))
  expect_true(file.exists(html_file))
  expect_type(jsonlite::read_json(json_file), "list")
  html <- readLines(html_file, warn = FALSE)
  expect_true(any(grepl("scene3d-data", html, fixed = TRUE)))
  expect_true(any(grepl("installPointerOrbitControls", html, fixed = TRUE)))
  expect_true(any(grepl("drag to rotate", html, fixed = TRUE)))
  expect_false(any(grepl("Static render", html, fixed = TRUE)))
})

test_that("point layers inherit and override data and mapping predictably", {
  plot_data <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    z = c(5, 6),
    group = c("a", "b")
  )
  layer_data <- data.frame(
    a = c(10, 20, 30),
    b = c(11, 21, 31),
    c = c(12, 22, 32),
    label = c("u", "v", "u")
  )

  p <- ggplot3(plot_data, aes3(x, y, z = z, colour = group)) +
    geom_point3d(data = layer_data, mapping = aes3(a, b, z = c, colour = label))

  scene <- as_scene3d(p)
  columns <- scene$layers[[1]]$data$columns

  expect_equal(columns$x, layer_data$a)
  expect_equal(columns$y, layer_data$b)
  expect_equal(columns$z, layer_data$c)
  expect_length(unique(columns$color), 2)
})

test_that("explicit point colour overrides mapped colour", {
  df <- data.frame(x = 1:4, y = 1:4, z = 1:4, group = c("a", "b", "a", "b"))

  p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
    geom_point3d(colour = "#112233")

  scene <- as_scene3d(p)
  expect_equal(unique(scene$layers[[1]]$data$columns$color), "#112233")
})

test_that("coord_3d compiles camera and axes without theme leakage", {
  df <- data.frame(x = 1:3, y = 1:3, z = 1:3)

  p <- ggplot3(df, aes3(x, y, z = z)) +
    geom_point3d() +
    coord_3d(
      projection = "perspective",
      position = c(2, -3, 4),
      target = c(1, 1, 1),
      up = c(0, 0, 1),
      zoom = 1.5,
      aspect = c(1, 2, 3)
    ) +
    theme_3d(scene.background = "#EEEEEE")

  scene <- as_scene3d(p)

  expect_equal(scene$camera$projection, "perspective")
  expect_equal(scene$camera$position, c(2, -3, 4))
  expect_equal(scene$camera$target, c(1, 1, 1))
  expect_equal(scene$camera$zoom, 1.5)
  expect_equal(scene$coordinateSystem$aspect$ratio, c(1, 2, 3))
  expect_equal(names(scene$axes), c("x", "y", "z", "grid"))
  expect_null(scene$theme$camera)
  expect_equal(scene$theme$scene$background, "#EEEEEE")
})

test_that("surface layer validates shape and preserves explicit material params", {
  xgrid <- c(0, 1)
  ygrid <- c(0, 1, 2)
  zmat <- outer(xgrid, ygrid, `+`)

  expect_error(
    geom_surface_grid3d(xgrid, ygrid, matrix(1, nrow = 3, ncol = 2)),
    "dim\\(z\\) must equal"
  )

  p <- ggplot3(data.frame(x = 1, y = 1, z = 1), aes3(x, y, z = z)) +
    geom_surface_grid3d(xgrid, ygrid, zmat, fill = "#AA0000", alpha = 0.25) +
    geom_point3d()

  scene <- as_scene3d(p)
  expect_equal(scene$layers[[1]]$material$fill, "#AA0000")
  expect_equal(scene$layers[[1]]$material$opacity, 0.25)
})

test_that("theme_3d controls non-data scene, material, and light defaults", {
  df <- data.frame(x = 1:3, y = 1:3, z = 1:3)
  xgrid <- c(0, 1)
  ygrid <- c(0, 1)
  zmat <- outer(xgrid, ygrid, `+`)

  p <- ggplot3(df, aes3(x, y, z = z)) +
    geom_surface_grid3d(xgrid, ygrid, zmat) +
    geom_point3d() +
    theme_3d(
      scene.background = "#FAFAFA",
      axis.grid.major = "#CCCCCC",
      axis.line = "#222222",
      axis.text = "#333333",
      material.point = element_material_3d(
        type = "points",
        sizeUnit = "screen",
        depthTest = FALSE,
        color = "#990099",
        size = 7,
        opacity = 0.4
      ),
      material.surface = element_material_3d(
        type = "surface",
        model = "unlit",
        fill = "#009999",
        opacity = 0.35,
        side = "double"
      ),
      light.ambient = element_light_3d(color = "#FFFFFF", intensity = 0.25),
      light.key = element_light_3d(color = "#FFEECC", intensity = 0.75, position = c(1, 2, 3))
    )

  scene <- as_scene3d(p)
  surface <- scene$layers[[1]]
  points <- scene$layers[[2]]

  expect_equal(scene$theme$scene$background, "#FAFAFA")
  expect_equal(scene$theme$axis$grid.major, "#CCCCCC")
  expect_equal(scene$theme$axis$line, "#222222")
  expect_equal(scene$theme$axis$text, "#333333")
  expect_equal(scene$lights$ambient$intensity, 0.25)
  expect_equal(scene$lights$key$position, c(1, 2, 3))

  expect_equal(surface$material$fill, "#009999")
  expect_equal(surface$material$opacity, 0.35)
  expect_false(points$material$depthTest)
  expect_equal(unique(points$data$columns$color), "#990099")
  expect_equal(unique(points$data$columns$size), 7)
  expect_equal(unique(points$data$columns$alpha), 0.4)
})

test_that("layer parameters override theme material defaults", {
  df <- data.frame(x = 1:3, y = 1:3, z = 1:3)
  xgrid <- c(0, 1)
  ygrid <- c(0, 1)
  zmat <- outer(xgrid, ygrid, `+`)

  p <- ggplot3(df, aes3(x, y, z = z)) +
    geom_surface_grid3d(xgrid, ygrid, zmat, fill = "#AA0000", alpha = 0.2) +
    geom_point3d(size = 3, alpha = 0.8, colour = "#00AA00") +
    theme_3d(
      material.point = element_material_3d(color = "#111111", size = 10, opacity = 0.1),
      material.surface = element_material_3d(fill = "#0000AA", opacity = 0.9)
    )

  scene <- as_scene3d(p)
  surface <- scene$layers[[1]]
  points <- scene$layers[[2]]

  expect_equal(surface$material$fill, "#AA0000")
  expect_equal(surface$material$opacity, 0.2)
  expect_equal(unique(points$data$columns$color), "#00AA00")
  expect_equal(unique(points$data$columns$size), 3)
  expect_equal(unique(points$data$columns$alpha), 0.8)
})

test_that("theme_3d rejects camera and computation concerns", {
  expect_error(theme_3d(camera.position = c(1, 2, 3)), "Unsupported theme_3d")
  expect_error(theme_3d(scale.domain = c(0, 1)), "Unsupported theme_3d")
  expect_error(theme_3d(stat.bandwidth = 1), "Unsupported theme_3d")
})

test_that("grid2d validates shape and alpha inputs", {
  x <- c(0, 1)
  y <- c(0, 1, 2)
  z <- outer(x, y, `+`)
  alpha <- matrix(seq(0, 1, length.out = length(z)), nrow = length(x), ncol = length(y))

  grid <- grid2d(x, y, z, alpha = alpha, metadata = list(source = "test"))
  expect_s3_class(grid, "ggplot3scene_grid2d")
  expect_equal(grid$shape, c(2, 3))
  expect_equal(grid$alpha, alpha)

  expect_error(grid2d(x, y, matrix(1, nrow = 3, ncol = 2)), "dim\\(z\\)")
  expect_error(grid2d(x, y, z, alpha = c(0.1, 0.2)), "alpha must")
  expect_error(grid2d(x, y, z, alpha = 2), "\\[0, 1\\]")
})

test_that("scene3d_table reserves json-columns table protocol", {
  tbl <- scene3d_table(data.frame(x = c(1, 2), label = c("a", "b")))
  compiled <- compile_scene3d_table(tbl)

  expect_s3_class(tbl, "ggplot3scene_table")
  expect_equal(compiled$kind, "table")
  expect_equal(compiled$encoding, "json-columns")
  expect_named(compiled$columns, c("x", "label"))
  expect_error(scene3d_table(data.frame(x = 1), encoding = "arrow-ipc"), "reserved")
})

test_that("surface grid geoms compile from grid2d and legacy x/y/z", {
  df <- data.frame(x = 1:3, y = 1:3, z = 1:3)
  xgrid <- c(0, 1)
  ygrid <- c(0, 1, 2)
  zmat <- outer(xgrid, ygrid, `+`)
  alpha <- matrix(0.5, nrow = length(xgrid), ncol = length(ygrid))

  p_grid <- ggplot3(df, aes3(x, y, z = z)) +
    geom_surface_grid3d(grid = grid2d(xgrid, ygrid, zmat, alpha = alpha)) +
    geom_point3d()
  scene_grid <- as_scene3d(p_grid)
  expect_equal(scene_grid$layers[[1]]$data$kind, "grid2d")
  expect_equal(scene_grid$layers[[1]]$data$order, "row-major")
  expect_equal(length(scene_grid$layers[[1]]$data$alpha), length(zmat))

  p_legacy <- ggplot3(df, aes3(x, y, z = z)) +
    geom_surface_grid3d(x = xgrid, y = ygrid, z = zmat) +
    geom_point3d()
  scene_legacy <- as_scene3d(p_legacy)
  expect_equal(scene_legacy$layers[[1]]$data$kind, "grid2d")
  expect_equal(scene_legacy$layers[[1]]$data$shape, c(length(xgrid), length(ygrid)))
})

test_that("alpha fade helpers preserve shape and clamp values", {
  z <- outer(seq(-1, 1, length.out = 12), seq(-1, 1, length.out = 9), function(x, y) exp(-(x^2 + y^2)))
  fades <- list(
    alpha_edge_fade(z),
    alpha_density_fade(z),
    alpha_combined_fade(z)
  )

  for (fade in fades) {
    expect_equal(dim(fade), dim(z))
    expect_true(all(is.finite(fade)))
    expect_true(all(fade >= 0 & fade <= 1))
  }
})

test_that("coord_3d grid protocol compiles origin and positive domain", {
  df <- data.frame(x = c(-1, 2), y = c(-2, 3), z = c(0, 1))

  p <- ggplot3(df, aes3(x, y, z = z)) +
    geom_point3d() +
    coord_3d(
      origin = c(1, 2, 0),
      grid = grid_3d(
        domain = "positive",
        planes = "xy",
        axis_length_fraction = 0.5,
        axis_arrows = TRUE
      )
    )

  scene <- as_scene3d(p)
  expect_equal(scene$coordinateSystem$origin, c(1, 2, 0))
  expect_equal(scene$coordinateSystem$originMode, "fixed")
  expect_equal(scene$axes$grid$domainMode, "positive")
  expect_equal(scene$axes$grid$origin, c(1, 2, 0))
  expect_equal(scene$axes$grid$planes, "xy")
  expect_equal(scene$axes$grid$axisLengthFraction, 0.5)
  expect_true(scene$axes$grid$axisArrows)
})

test_that("coord_umap3d compiles to positive xy grid", {
  df <- data.frame(UMAP1 = c(-3, 2), UMAP2 = c(-1, 4), z = 0)

  p <- ggplot3(df, aes3(UMAP1, UMAP2, z = z)) +
    geom_point3d() +
    coord_umap3d(origin_mode = "data_min", positive_grid = TRUE)

  scene <- as_scene3d(p)
  expect_equal(scene$coordinateSystem$originMode, "data_min")
  expect_equal(scene$axes$grid$domainMode, "positive")
  expect_equal(scene$axes$grid$planes, "xy")
  expect_equal(scene$coordinateSystem$origin, c(-3, -1, 0))
})

test_that("theme_3d_umap is visual only", {
  theme <- as_json_theme(theme_3d_umap())
  expect_true(is.null(theme$coordinateSystem))
  expect_true(is.null(theme$axes$grid))
  expect_true(is.null(theme$origin))
  expect_true(is.null(theme$grid))
  expect_true(is.null(theme$clip))
})

test_that("exported HTML uses custom grid and axis builders", {
  scene <- as_scene3d(demo_scene3d())
  html_file <- tempfile(fileext = ".html")
  export_html(scene, html_file)
  html <- paste(readLines(html_file, warn = FALSE), collapse = "\n")

  expect_false(grepl("GridHelper", html, fixed = TRUE))
  expect_false(grepl("AxesHelper", html, fixed = TRUE))
  expect_true(grepl("buildGridLines", html, fixed = TRUE))
  expect_true(grepl("buildAxisLines", html, fixed = TRUE))
})

test_that("UMAP-style scene includes positive grid and surface alpha", {
  df <- data.frame(UMAP1 = c(-1, 1), UMAP2 = c(-1, 1), z = c(0, 0), cluster = c("a", "b"))
  xgrid <- seq(-1, 1, length.out = 5)
  ygrid <- seq(-1, 1, length.out = 6)
  zmat <- outer(xgrid, ygrid, function(x, y) exp(-(x^2 + y^2)))

  p <- ggplot3(df, aes3(UMAP1, UMAP2, z = z, colour = cluster)) +
    geom_surface_grid3d(grid = grid2d(xgrid, ygrid, zmat, alpha = alpha_combined_fade(zmat))) +
    geom_point3d() +
    coord_umap3d(origin_mode = "data_min", positive_grid = TRUE) +
    theme_3d_umap()

  scene <- as_scene3d(p)
  expect_equal(scene$axes$grid$domainMode, "positive")
  expect_false(is.null(scene$layers[[1]]$data$alpha))
  expect_equal(length(scene$layers[[1]]$data$alpha), length(zmat))
})

test_that("abs_route returns pixel route commands", {
  route <- abs_route(up = 72, right = 140)

  expect_s3_class(route, "ggplot3scene_abs_route")
  expect_equal(route$units, "px")
  expect_equal(route$commands[[2]]$op, "screen_up")
  expect_equal(route$commands[[2]]$dy, 72)
  expect_equal(route$commands[[3]]$op, "screen_right")
  expect_equal(route$commands[[3]]$dx, 140)
})

test_that("geom_abs_label3d requires x y z and label aesthetics", {
  df <- data.frame(x = 1, y = 2, z = 3, label = "target")

  p_missing <- ggplot3(df, aes3(x, y, z = z)) +
    geom_abs_label3d()
  expect_error(as_scene3d(p_missing), "missing label")

  p_ok <- ggplot3(df, aes3(x, y, z = z, label = label)) +
    geom_abs_label3d()
  scene <- as_scene3d(p_ok)
  expect_equal(scene$layers[[1]]$type, "abs_annotation")
})

test_that("as_scene3d emits ABS annotation anchors and screen route", {
  df <- data.frame(x = c(1, 2), y = c(2, 3), z = c(3, 4), label = c("A", "B"))

  p <- ggplot3(df, aes3(x, y, z = z, label = label)) +
    geom_abs_label3d(route = abs_route(up = 64, right = 120), occlusion = "depth-test")
  scene <- as_scene3d(p)
  layer <- scene$layers[[1]]

  expect_equal(layer$type, "abs_annotation")
  expect_equal(layer$space$type, "anchored_billboard")
  expect_equal(layer$space$units, "px")
  expect_equal(layer$space$occlusion, "depth-test")
  expect_equal(layer$data$anchors[[1]]$position, c(1, 2, 3))
  expect_equal(layer$data$anchors[[1]]$label$text, "A")
  expect_true(any(vapply(layer$route, function(command) identical(command$op, "screen_up"), logical(1))))
  expect_true(any(vapply(layer$route, function(command) identical(command$op, "screen_right"), logical(1))))
  expect_true(layer$style$line$depthTest)
})

test_that("ABS annotation does not mutate point data or theme", {
  df <- data.frame(x = c(1, 2), y = c(2, 3), z = c(3, 4), group = c("a", "b"))
  label_df <- data.frame(x = 1, y = 2, z = 3, label = "A")

  p <- ggplot3(df, aes3(x, y, z = z, colour = group)) +
    geom_point3d() +
    geom_abs_label3d(data = label_df, mapping = aes3(x, y, z = z, label = label)) +
    theme_3d_scientific()
  scene <- as_scene3d(p)

  expect_equal(scene$layers[[1]]$type, "point_cloud")
  expect_equal(scene$layers[[1]]$data$columns$x, df$x)
  expect_equal(scene$layers[[2]]$type, "abs_annotation")
  expect_true(is.null(scene$theme$abs))
  expect_true(is.null(scene$theme$abs_annotation))
})

test_that("exported HTML contains ABS renderer builder", {
  df <- data.frame(x = 1, y = 2, z = 3, label = "target")
  scene <- as_scene3d(ggplot3(df, aes3(x, y, z = z, label = label)) + geom_abs_label3d())
  html_file <- tempfile(fileext = ".html")
  export_html(scene, html_file)
  html <- paste(readLines(html_file, warn = FALSE), collapse = "\n")

  expect_true(grepl("buildAbsAnnotations", html, fixed = TRUE))
  expect_true(grepl("unprojectScreenAtDepth", html, fixed = TRUE))
  expect_true(grepl("addAxisWithArrow", html, fixed = TRUE))
  expect_true(grepl("createAbsLabelSprite", html, fixed = TRUE))
  expect_true(grepl("CanvasTexture", html, fixed = TRUE))
  expect_true(grepl("SpriteMaterial", html, fixed = TRUE))
  expect_true(grepl("setDrawRange", html, fixed = TRUE))
})
