camera_3d <- function(view = NULL,
                      azimuth = -37.5,
                      elevation = 30,
                      roll = 0,
                      projection = c("orthographic", "perspective"),
                      fov = 35,
                      zoom = 1,
                      distance = NULL,
                      target = "auto",
                      target_data = NULL,
                      up = c(0, 0, 1),
                      lock_up = TRUE,
                      fit = TRUE,
                      fit_method = c("bbox", "sphere"),
                      padding = 0.08,
                      near = NULL,
                      far = NULL,
                      clipping = c("auto", "manual"),
                      data_aspect = NULL,
                      z_exaggeration = 1,
                      panels = c("visible", "all", "none", "dynamic"),
                      projection_faces = NULL,
                      orbit = TRUE,
                      min_elevation = -89,
                      max_elevation = 89,
                      name = NULL) {
  projection <- match.arg(projection)
  fit_method <- match.arg(fit_method)
  clipping <- match.arg(clipping)
  panels <- match.arg(panels)
  for (nm in c("azimuth", "elevation", "roll", "fov", "zoom", "padding", "z_exaggeration", "min_elevation", "max_elevation")) {
    value <- get(nm)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      stop(nm, " must be a finite numeric scalar.", call. = FALSE)
    }
  }
  if (zoom <= 0) {
    stop("zoom must be positive.", call. = FALSE)
  }
  if (fov <= 0 || fov >= 180) {
    stop("fov must be between 0 and 180.", call. = FALSE)
  }
  if (!is.null(distance) && (!is.numeric(distance) || length(distance) != 1L || !is.finite(distance) || distance <= 0)) {
    stop("distance must be NULL or a positive numeric scalar.", call. = FALSE)
  }
  if (!is.character(target) && !identical(target, "auto")) {
    stop("target must be 'auto' or use target_data for numeric coordinates.", call. = FALSE)
  }
  if (!is.null(target_data) && (!is.numeric(target_data) || length(target_data) != 3L || any(!is.finite(target_data)))) {
    stop("target_data must be NULL or a finite numeric vector of length 3.", call. = FALSE)
  }
  if (!is.numeric(up) || length(up) != 3L || any(!is.finite(up))) {
    stop("up must be a finite numeric vector of length 3.", call. = FALSE)
  }
  if (!is.null(near) && (!is.numeric(near) || length(near) != 1L || !is.finite(near) || near <= 0)) {
    stop("near must be NULL or a positive numeric scalar.", call. = FALSE)
  }
  if (!is.null(far) && (!is.numeric(far) || length(far) != 1L || !is.finite(far) || far <= 0)) {
    stop("far must be NULL or a positive numeric scalar.", call. = FALSE)
  }

  out <- list(
    view = view,
    projection = projection,
    azimuth = azimuth,
    elevation = elevation,
    roll = roll,
    fov = fov,
    zoom = zoom,
    distance = distance,
    target = target,
    targetData = if (is.null(target_data)) NULL else unname(as.numeric(target_data)),
    up = unname(as.numeric(up)),
    lockUp = isTRUE(lock_up),
    fit = isTRUE(fit),
    fitMethod = fit_method,
    padding = padding,
    near = near,
    far = far,
    clipping = clipping,
    dataAspect = data_aspect,
    zExaggeration = z_exaggeration,
    panels = panels,
    projectionFaces = projection_faces,
    orbit = isTRUE(orbit),
    minElevation = min_elevation,
    maxElevation = max_elevation,
    name = name
  )
  class(out) <- c("ggplot3scene_camera", "list")
  out
}

view_3d <- function(...) camera_3d(...)

camera_preset3d <- function(defaults, args) {
  defaults[names(args)] <- args
  do.call(camera_3d, defaults)
}

view_default3d <- function(...) {
  camera_preset3d(
    list(view = "ggplot3_default", azimuth = -125, elevation = 28, roll = 0, zoom = 1.18),
    list(...)
  )
}

view_isometric <- function(...) {
  camera_preset3d(
    list(view = "isometric", azimuth = -45, elevation = 35.264, roll = 0),
    list(...)
  )
}

view_top <- function(...) {
  camera_preset3d(
    list(view = "top", azimuth = 0, elevation = 90, roll = 0),
    list(...)
  )
}

view_front <- function(...) {
  camera_preset3d(
    list(view = "front", azimuth = -90, elevation = 0, roll = 0),
    list(...)
  )
}

view_right <- function(...) {
  camera_preset3d(
    list(view = "right", azimuth = 0, elevation = 0, roll = 0),
    list(...)
  )
}

compile_camera_view <- function(camera, coord, bounds) {
  if (is.null(camera)) {
    camera <- camera_3d(
      projection = coord$projection,
      zoom = coord$zoom,
      target_data = coord$target,
      up = coord$up,
      view = "coord_3d_legacy"
    )
    camera$position <- unname(coord$position)
  }
  if (!inherits(camera, "ggplot3scene_camera")) {
    stop("camera must be created with camera_3d().", call. = FALSE)
  }

  center <- c(
    mean(c(bounds$min[["x"]], bounds$max[["x"]])),
    mean(c(bounds$min[["y"]], bounds$max[["y"]])),
    mean(c(bounds$min[["z"]], bounds$max[["z"]]))
  )
  size <- c(
    bounds$max[["x"]] - bounds$min[["x"]],
    bounds$max[["y"]] - bounds$min[["y"]],
    bounds$max[["z"]] - bounds$min[["z"]]
  )
  radius <- max(size, 1) * 0.75
  target <- camera$targetData %||% center
  distance <- camera$distance %||% max(radius * 3.5, 1)
  position <- camera$position %||% (target + camera_direction(camera$azimuth, camera$elevation) * distance)
  up <- roll_up_vector(camera$up, target - position, camera$roll)
  near <- camera$near %||% max(radius / 1000, 0.001)
  far <- camera$far %||% max(radius * 1000, 1000)

  view <- strip_classes(camera)
  view$target <- unname(as.numeric(target))
  view$position <- unname(as.numeric(position))
  view$up <- unname(as.numeric(up))
  view$near <- near
  view$far <- far
  list(
    view = list(camera = view),
    camera = list(
      projection = camera$projection,
      position = unname(as.numeric(position)),
      target = unname(as.numeric(target)),
      up = unname(as.numeric(up)),
      zoom = camera$zoom,
      fov = camera$fov,
      near = near,
      far = far
    )
  )
}

camera_direction <- function(azimuth, elevation) {
  az <- azimuth * pi / 180
  el <- elevation * pi / 180
  c(cos(el) * cos(az), cos(el) * sin(az), sin(el))
}

roll_up_vector <- function(up, direction, roll) {
  if (roll == 0) {
    return(unname(as.numeric(up)))
  }
  axis <- direction / sqrt(sum(direction^2))
  theta <- roll * pi / 180
  v <- up / sqrt(sum(up^2))
  rotated <- v * cos(theta) + cross3(axis, v) * sin(theta) + axis * sum(axis * v) * (1 - cos(theta))
  unname(as.numeric(rotated))
}

cross3 <- function(a, b) {
  c(
    a[[2]] * b[[3]] - a[[3]] * b[[2]],
    a[[3]] * b[[1]] - a[[1]] * b[[3]],
    a[[1]] * b[[2]] - a[[2]] * b[[1]]
  )
}
