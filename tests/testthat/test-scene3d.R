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
  "aes3.R", "grid3d.R", "theme3d.R", "scene3d.R", "ggplot_adapter.R", "layers.R", "abs3d.R", "surface_stats.R", "surface_geoms.R", "face_projection.R", "build.R", "export.R", "demo_data.R"
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

test_that("polyline3d path and segment layers compile", {
  path_df <- data.frame(
    x = c(0, 1, 0, 1),
    y = c(0, 1, 1, 0),
    z = c(0, 0.2, 0.4, 0.6),
    group = c("a", "a", "b", "b")
  )
  seg_df <- data.frame(
    x = 0,
    y = 0,
    z = 0,
    xend = 1,
    yend = 1,
    zend = 1
  )

  scene <- as_scene3d(
    ggplot3(path_df, aes3(x, y, z = z, group = group)) +
      geom_path3d(line_width = 2) +
      geom_segment3d(data = seg_df, mapping = aes3(x, y, z = z, xend = xend, yend = yend, zend = zend))
  )

  expect_equal(scene$layers[[1]]$type, "polyline3d")
  expect_equal(scene$layers[[1]]$space$type, "world")
  expect_equal(length(scene$layers[[1]]$data$polylines), 2)
  expect_equal(scene$layers[[2]]$style$type, "segment")
  expect_equal(scene$layers[[2]]$data$polylines[[1]]$z, c(0, 1))
})

test_that("numeric colour mapping compiles to point hex colors", {
  df <- data.frame(x = 1:4, y = 1:4, z = 1:4, value = c(0, 0.25, 0.75, 1))
  scene <- as_scene3d(ggplot3(df, aes3(x, y, z = z, colour = value)) + geom_point3d())
  colors <- scene$layers[[1]]$data$columns$color
  expect_length(colors, 4)
  expect_true(all(grepl("^#", colors)))
  expect_gt(length(unique(colors)), 1)
})

test_that("ggplot2 adapter supports point path and segment without leaking ggplot objects", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    x = c(0, 1, 2),
    y = c(0, 1, 0),
    z = c(0, 0.5, 1),
    value = c(0, 1, 2),
    xend = c(0.5, 1.5, 2.5),
    yend = c(0.4, 0.6, 0.2)
  )
  gp <- ggplot2::ggplot(df, ggplot2::aes(x, y, z = z, colour = value)) +
    ggplot2::geom_point() +
    ggplot2::geom_path() +
    ggplot2::geom_segment(ggplot2::aes(xend = xend, yend = yend))

  scene <- as_scene3d_ggplot(gp)

  expect_equal(vapply(scene$layers, `[[`, character(1), "type"), c("point_cloud", "polyline3d", "polyline3d"))
  expect_equal(scene$layers[[2]]$style$type, "path")
  expect_equal(scene$layers[[3]]$style$type, "segment")
  expect_equal(scene$layers[[3]]$data$polylines[[1]]$z, c(0, 0))
  scene_text <- jsonlite::toJSON(scene, auto_unbox = TRUE)
  expect_false(grepl("quosure|ggproto|grob|ggplot_build|ggplot2", scene_text))
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

test_that("ggplot2-like aes mappings normalize without quosure leakage", {
  df <- data.frame(x = 1:3, y = 2:4, z = 3:5, group = c("a", "b", "a"))
  ggplot_like_mapping <- structure(
    list(x = stats::as.formula("~x"), y = stats::as.formula("~y"), z = stats::as.formula("~z"), colour = stats::as.formula("~group")),
    class = "uneval"
  )

  p <- ggplot3(df, ggplot_like_mapping) +
    geom_point3d(mapping = structure(list(x = quote(x), y = quote(y), z = quote(z)), class = "uneval"))

  expect_s3_class(p$mapping, "ggplot3scene_aes")
  expect_equal(as_mapping(p$mapping)$colour, "group")

  scene <- as_scene3d(p)
  json <- jsonlite::toJSON(scene, auto_unbox = TRUE)
  expect_equal(scene$layers[[1]]$data$columns$x, df$x)
  expect_false(grepl("quosure", json, fixed = TRUE))
  expect_false(grepl("uneval", json, fixed = TRUE))
  expect_false(grepl("formula", json, fixed = TRUE))
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
  expect_equal(names(scene$axes), c("x", "y", "z", "grid", "style", "labelPlacement", "tickPlacement"))
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

test_that("surface objects declare reusable protocol classes", {
  x <- c(0, 1)
  y <- c(0, 1)
  z <- outer(x, y, `+`)
  grid <- surface_grid(x, y, z, tessellation = "right2")

  expect_s3_class(grid, "ggplot3scene_grid2d")
  expect_equal(grid$tessellation, "right2")
  expect_equal(compile_grid2d_data(grid)$tessellation, "right2")

  mesh <- surface_mesh(
    vertices = matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE),
    faces = matrix(c(1, 2, 3), ncol = 3)
  )
  expect_s3_class(mesh, "ggplot3scene_surface_mesh")
  expect_s3_class(contour_stack(list()), "ggplot3scene_contour_stack")
  expect_s3_class(ridgeline_stack(list()), "ggplot3scene_ridgeline_stack")
})

test_that("mesh contour and ridgeline geoms compile to Scene3D surface protocols", {
  vertices <- matrix(
    c(
      0, 0, 0,
      1, 0, 0.2,
      0, 1, 0.1,
      1, 1, 0.3
    ),
    ncol = 3,
    byrow = TRUE
  )
  mesh <- surface_mesh(vertices, matrix(c(1, 2, 3, 2, 4, 3), ncol = 3, byrow = TRUE))
  contours <- contour_stack(
    list(data.frame(x = c(0, 0.5, 1), y = c(0, 0.4, 1), z = c(0.15, 0.15, 0.15))),
    levels = 0.15
  )
  ridges <- ridgeline_stack(
    list(data.frame(x = c(0, 0.5, 1), y = c(1.2, 1.2, 1.2), z = c(0, 0.3, 0)))
  )

  p <- ggplot3() +
    geom_surface_mesh3d(mesh, fill = "#99CCEE", alpha = 0.5) +
    geom_contour_stack3d(contours, colour = "#111111", line_width = 2) +
    geom_ridgeline3d(ridges, colour = "#CC3311", line_width = 2)
  scene <- as_scene3d(p)

  expect_equal(scene$layers[[1]]$type, "surface_mesh")
  expect_equal(scene$layers[[1]]$data$kind, "mesh3d")
  expect_equal(scene$layers[[1]]$data$vertexCount, 4)
  expect_equal(scene$layers[[1]]$data$faces, c(0, 1, 2, 1, 3, 2))
  expect_equal(scene$layers[[2]]$type, "contour_stack")
  expect_equal(scene$layers[[2]]$data$kind, "contour_stack")
  expect_equal(scene$layers[[2]]$data$polylines[[1]]$level, 0.15)
  expect_equal(scene$layers[[3]]$type, "ridgeline_stack")
  expect_equal(scene$layers[[3]]$data$kind, "ridgeline_stack")
  expect_equal(scene$coordinateSystem$domain$x, c(0, 1))
  expect_equal(scene$coordinateSystem$domain$y, c(0, 1.2))
  expect_equal(scene$coordinateSystem$domain$z, c(0, 0.3))

  json <- jsonlite::toJSON(scene, auto_unbox = TRUE)
  expect_false(grepl("ggplot3scene_surface_mesh", json, fixed = TRUE))
  expect_false(grepl("ggplot3scene_contour_stack", json, fixed = TRUE))
  expect_false(grepl("ggplot3scene_ridgeline_stack", json, fixed = TRUE))
})

test_that("surface mesh rejects invalid face indices", {
  vertices <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  expect_error(surface_mesh(vertices, matrix(c(1, 2, 4), ncol = 3)), "1-based vertex indices")
})

test_that("surface stats compile to surface_grid with R stat metadata", {
  set.seed(1)
  df <- data.frame(
    x = c(stats::rnorm(20, -1), stats::rnorm(20, 1)),
    y = c(stats::rnorm(20, -1), stats::rnorm(20, 1)),
    z = 0
  )

  p <- ggplot3(df, aes3(x, y, z = z)) +
    stat_density_surface3d(
      aes3(x, y),
      grid_size = c(16, 18),
      bandwidth = c(0.5, 0.6),
      bounds = list(x = c(-3, 3), y = c(-3, 3)),
      alpha = "combined_fade",
      tessellation = "right1"
    ) +
    geom_point3d()

  scene <- as_scene3d(p)
  surface <- scene$layers[[1]]

  expect_equal(surface$type, "surface_grid")
  expect_equal(surface$data$kind, "grid2d")
  expect_equal(surface$data$shape, c(16, 18))
  expect_equal(surface$data$tessellation, "right1")
  expect_false(is.null(surface$data$alpha))
  expect_equal(surface$data$metadata$stat$type, "density_surface")
  expect_equal(surface$data$metadata$stat$computedBy, "R")
  expect_equal(surface$data$metadata$stat$gridSize, c(16, 18))
  expect_equal(surface$metadata$stat$method, "gaussian_kde_product_kernel")
})

test_that("function and smooth surface stats compile without language objects in scene JSON", {
  df <- expand.grid(x = seq(-1, 1, length.out = 5), y = seq(-1, 1, length.out = 5))
  df$z <- df$x^2 - df$y

  p <- ggplot3(df, aes3(x, y, z = z)) +
    stat_function_surface3d(
      function(x, y) x^2 + y^2,
      xlim = c(-1, 1),
      ylim = c(-1, 1),
      grid_size = 12,
      tessellation = "right2"
    ) +
    stat_smooth_surface3d(grid_size = c(10, 11)) +
    geom_point3d()

  scene <- as_scene3d(p)
  expect_equal(scene$layers[[1]]$type, "surface_grid")
  expect_equal(scene$layers[[1]]$data$tessellation, "right2")
  expect_equal(scene$layers[[1]]$data$metadata$stat$type, "function_surface")
  expect_equal(scene$layers[[2]]$data$metadata$stat$type, "smooth_surface")

  json <- jsonlite::toJSON(scene, auto_unbox = TRUE)
  expect_false(grepl("function\\(", json))
  expect_false(grepl("ggplot3scene_surface_stat_layer", json, fixed = TRUE))
})

test_that("face projection protocol compiles density grid onto a plane", {
  set.seed(2)
  df <- data.frame(
    x = stats::rnorm(40),
    y = stats::rnorm(40),
    z = stats::rnorm(40)
  )
  pos <- position_on_plane3d("zmin", axes = c("x", "y"), offset = -0.1)
  expect_s3_class(pos, "ggplot3scene_plane_position")
  expect_equal(pos$plane, "zmin")
  expect_equal(pos$axes, c("x", "y"))

  p <- ggplot3(df, aes3(x, y, z = z)) +
    geom_face_density3d(
      aes3(x, y),
      plane = "zmin",
      grid_size = c(14, 15),
      bandwidth = c(0.5, 0.5),
      alpha = "combined_fade",
      offset = -0.05
    ) +
    geom_point3d()
  scene <- as_scene3d(p)
  layer <- scene$layers[[1]]

  expect_equal(layer$type, "face_projection")
  expect_equal(layer$space$type, "face_plane")
  expect_equal(layer$plane, "zmin")
  expect_equal(layer$axes, c("x", "y"))
  expect_equal(layer$offset, -0.05)
  expect_equal(layer$data$kind, "grid2d")
  expect_equal(layer$data$shape, c(14, 15))
  expect_false(is.null(layer$data$alpha))
  expect_equal(layer$data$metadata$stat$type, "face_density")
  expect_equal(layer$data$metadata$stat$computedBy, "R")
})

test_that("face projection supports points paths and contours", {
  df <- data.frame(
    x = c(0, 1, 2, 0, 1, 2),
    y = c(0, 0.5, 0, 1, 1.5, 1),
    group = c("a", "a", "a", "b", "b", "b")
  )
  contours <- contour_stack(
    list(data.frame(x = c(0, 1, 2), y = c(0.2, 0.8, 0.2), z = 0)),
    levels = 0.4
  )

  scene <- as_scene3d(
    ggplot3(df, aes3(x, y)) +
      geom_face_points3d(plane = "zmin", size = 4, alpha = 0.7) +
      geom_face_path3d(aes3(x, y, group = group), plane = "zmax", line_width = 2) +
      geom_face_contour3d(contours, plane = "xmax", axes = c("y", "z"))
  )

  expect_equal(vapply(scene$layers, function(layer) layer$type, character(1)), rep("face_projection", 3))
  expect_equal(scene$layers[[1]]$style$type, "points")
  expect_equal(scene$layers[[1]]$data$kind, "face_points")
  expect_named(scene$layers[[1]]$data$columns, c("x", "y", "color", "size", "alpha"))
  expect_equal(scene$layers[[2]]$style$type, "path")
  expect_equal(scene$layers[[2]]$data$kind, "face_path")
  expect_equal(length(scene$layers[[2]]$data$polylines), 2)
  expect_equal(scene$layers[[3]]$style$type, "contour_lines")
  expect_equal(scene$layers[[3]]$data$kind, "face_contour")
  expect_equal(scene$layers[[3]]$axes, c("y", "z"))
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

test_that("axis_3d separates axis styling from grid protocol", {
  df <- data.frame(x = c(-1, 2), y = c(-2, 3), z = c(0, 1))

  p <- ggplot3(df, aes3(x, y, z = z)) +
    geom_point3d() +
    coord_3d(
      grid = grid_3d(domain = "positive", planes = "xy"),
      axis = axis_3d(
        length_fraction = 0.42,
        arrows = TRUE,
        labels = FALSE,
        titles = TRUE,
        label_placement = "outside",
        tick_placement = "inside"
      )
    )

  scene <- as_scene3d(p)
  expect_equal(scene$axes$grid$domainMode, "positive")
  expect_equal(scene$axes$style$lengthFraction, 0.42)
  expect_true(scene$axes$style$arrows)
  expect_false(scene$axes$style$labels)
  expect_true(scene$axes$style$titles)
  expect_equal(scene$axes$labelPlacement, "outside")
  expect_equal(scene$axes$tickPlacement, "inside")
  expect_null(scene$theme$axis$lengthFraction)
  expect_null(scene$theme$axis$arrows)
})

test_that("guide protocol compiles language-neutral legends and colorbars", {
  df <- data.frame(x = 1:3, y = 1:3, z = 1:3)

  p <- ggplot3(df, aes3(x, y, z = z)) +
    geom_point3d() +
    guide_legend_scene3d(
      aesthetic = "colour",
      title = "cluster",
      labels = c("A", "B"),
      values = c("#3366CC", "#CC6633")
    ) +
    guide_colorbar_scene3d(
      aesthetic = "density",
      title = "density",
      domain = c(0, 1),
      palette = c("#FFFFFF", "#4477AA")
    )

  scene <- as_scene3d(p)
  expect_length(scene$guides, 2)
  expect_equal(scene$guides[[1]]$type, "legend")
  expect_equal(scene$guides[[1]]$entries[[1]]$label, "A")
  expect_equal(scene$guides[[2]]$type, "colorbar")
  expect_equal(scene$guides[[2]]$materialMode, "unlit")
  expect_false(inherits(scene$guides[[1]], "ggplot3scene_guide"))
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
  expect_true(grepl("buildAxisSprites", html, fixed = TRUE))
  expect_true(grepl("createAxisTextSprite", html, fixed = TRUE))
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
  expect_equal(route$commands[[2]], list(op = "screen_offset", dx = 0, dy = -72))
  expect_equal(route$commands[[3]], list(op = "screen_offset", dx = 140, dy = 0))

  route2 <- abs_route(abs_anchor(), abs_up(10), abs_right(20), abs_offset(-2, 3))
  expect_equal(
    vapply(route2$commands, function(command) command$op, character(1)),
    c("move_anchor", "screen_offset", "screen_offset", "screen_offset")
  )
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
    geom_abs_label3d(
      route = abs_route(abs_anchor(), abs_up(64), abs_right(120)),
      leader_occlusion = "depth-test",
      label_occlusion = "none",
      anchor_occlusion = "depth-test"
    )
  scene <- as_scene3d(p)
  layer <- scene$layers[[1]]

  expect_equal(layer$type, "abs_annotation")
  expect_equal(layer$space$type, "anchored_billboard")
  expect_equal(layer$space$units, "px")
  expect_equal(layer$occlusion$anchor, "depth-test")
  expect_equal(layer$occlusion$leader, "depth-test")
  expect_equal(layer$occlusion$label, "none")
  expect_equal(layer$data$anchors[[1]]$position, c(1, 2, 3))
  expect_equal(layer$data$anchors[[1]]$label$text, "A")
  expect_true(any(vapply(layer$route, function(command) identical(command$op, "screen_offset") && identical(command$dy, -64), logical(1))))
  expect_true(any(vapply(layer$route, function(command) identical(command$op, "screen_offset") && identical(command$dx, 120), logical(1))))
  expect_true(layer$style$line$depthTest)
  expect_equal(layer$style$line$geometry, "screen-ribbon")
  expect_equal(layer$style$line$widthUnit, "px")
  expect_true(layer$visibility$hideWhenAnchorOutsideFrustum)
})

test_that("ABS occlusion shorthand maps to component occlusion", {
  df <- data.frame(x = 0, y = 0, z = 0, label = "A")

  scene <- as_scene3d(
    ggplot3(df, aes3(x, y, z = z, label = label)) +
      geom_abs_label3d(occlusion = "none")
  )
  layer <- scene$layers[[1]]

  expect_equal(layer$occlusion$anchor, "none")
  expect_equal(layer$occlusion$leader, "none")
  expect_equal(layer$occlusion$label, "none")
  expect_false(layer$style$line$depthTest)
})

test_that("ABS anchors participate in scene bounds", {
  point_df <- data.frame(x = 0, y = 0, z = 0)
  label_df <- data.frame(x = 10, y = 20, z = 30, label = "far")

  scene <- as_scene3d(
    ggplot3(point_df, aes3(x, y, z = z)) +
      geom_point3d() +
      geom_abs_label3d(data = label_df, mapping = aes3(x, y, z = z, label = label)) +
      coord_3d(origin_mode = "data_center")
  )

  expect_equal(scene$coordinateSystem$domain$x, c(0, 10))
  expect_equal(scene$coordinateSystem$domain$y, c(0, 20))
  expect_equal(scene$coordinateSystem$domain$z, c(0, 30))
  expect_equal(scene$coordinateSystem$origin, c(5, 10, 15))
})

test_that("ABS style defaults come from theme visual elements", {
  df <- data.frame(x = 0, y = 0, z = 0, label = "A")

  scene <- as_scene3d(
    ggplot3(df, aes3(x, y, z = z, label = label)) +
      geom_abs_label3d() +
      theme_3d(
        abs.line = element_abs_line(color = "#AA0000", width = 9, opacity = 0.5),
        abs.point = element_abs_point(color = "#00AA00", size = 8),
        abs.text = element_abs_text(color = "#0000AA", size = 16),
        abs.label.background = element_abs_label_background(fill = "#EEEEEE", borderColor = "#333333", padding = c(9, 6))
      )
  )
  layer <- scene$layers[[1]]

  expect_equal(layer$style$line$color, "#AA0000")
  expect_equal(layer$style$line$width, 9)
  expect_equal(layer$style$point$color, "#00AA00")
  expect_equal(layer$style$point$size, 8)
  expect_equal(layer$style$text$color, "#0000AA")
  expect_equal(layer$style$text$size, 16)
  expect_equal(layer$style$text$backgroundColor, "#EEEEEE")
  expect_equal(layer$style$text$padding, c(9, 6))
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
  expect_false(is.null(scene$theme$abs$line))
  expect_true(is.null(scene$theme$abs$anchor))
  expect_true(is.null(scene$theme$abs$route))
  expect_true(is.null(scene$theme$abs$occlusion))
  expect_true(is.null(scene$theme$abs$label.text))
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
  expect_true(grepl("writeRibbonSegment", html, fixed = TRUE))
  expect_true(grepl("MeshBasicMaterial", html, fixed = TRUE))
  expect_true(grepl("CanvasTexture", html, fixed = TRUE))
  expect_true(grepl("SpriteMaterial", html, fixed = TRUE))
  expect_true(grepl("setDrawRange", html, fixed = TRUE))
  expect_true(grepl("buildSurfaceGridIndices", html, fixed = TRUE))
  expect_true(grepl("addSurfaceMesh", html, fixed = TRUE))
  expect_true(grepl("addPolylineStack", html, fixed = TRUE))
  expect_true(grepl("addFaceProjection", html, fixed = TRUE))
  expect_true(grepl("addFacePointsProjection", html, fixed = TRUE))
  expect_true(grepl("addFacePolylineProjection", html, fixed = TRUE))
  expect_true(grepl("faceProjectionPoint", html, fixed = TRUE))
  expect_true(grepl("buildGuides", html, fixed = TRUE))
  expect_true(grepl("scene3d-guide", html, fixed = TRUE))
})
