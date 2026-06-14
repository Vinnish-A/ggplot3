demo_scene3d <- function() {
  set.seed(42)
  n <- 220L
  df <- data.frame(
    x = stats::runif(n, -3, 3),
    y = stats::runif(n, -3, 3)
  )
  df$z <- sin(df$x) * cos(df$y) + stats::rnorm(n, sd = 0.12)
  df$group <- ifelse(df$x + df$y > 0, "ridge", "basin")

  xgrid <- seq(-3, 3, length.out = 45)
  ygrid <- seq(-3, 3, length.out = 45)
  zmat <- outer(xgrid, ygrid, function(x, y) sin(x) * cos(y))

  ggplot3(df, aes3(x, y, z = z, colour = group)) +
    geom_surface_grid3d(x = xgrid, y = ygrid, z = zmat, alpha = 0.55) +
    geom_point3d(size = 5, alpha = 0.9) +
    coord_3d(projection = "orthographic") +
    theme_3d_scientific()
}
