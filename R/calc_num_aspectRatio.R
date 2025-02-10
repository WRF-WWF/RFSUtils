#' Calculate (rough) dimensions of map based on lat/lon range.
#'
#' For a plot measured in degrees (i.e. WGS84) the distance represented by
#' 1 degree latitude will depend on how far you are from the equator. Therefore,
#' to create a plot where x and y axes are proportional the aspect ratio of
#' the plot will need to be different at different latitudes.
#'
#' When saving a plot, we want to height/width to match the aspect ratio of the
#' plot. We cannot calculate this based on ratio of latitude/longitude and
#' instead need to calculate the real world distance covered on x and y.
#'
#' This calculation is ROUGH, but will give a good starting values.
#'
#' @param lat_range Numeric. Latitudinal range of map (degrees)
#' @param lon_range Numeric. Longitudinal range of map (degrees)
#' @param lat_centre Numeric. Latitudinal value of map centre (degrees)
#' @param height Numeric. Expected height of plot. Width will be calculate based on
#' required aspect ratio. Default (6) is in units of inches (default unit
#' for `ggsave`).
#' @param offset Numeric. Proportional buffer around map. By default, plot
#' dimensions are designed for plots using theme_void(), without titles or
#' axis labels. Use `offset` > 1 to create more space for titles and labels.
#'
#' @return Numeric vector with values for width and height.
#' @export
#'
#' @examples
#' if(require("maps") & require("ggplot2") & require("sf")){
#'
#' world <- st_as_sf(map('world', plot = FALSE, fill = TRUE))
#'
#' ## Dimensions of plot at equator
#' plot1 <- ggplot() +
#' geom_sf(data = world) +
#' coord_sf(xlim = c(0, 30), ylim = c(-15, 15)) +
#' theme_void()
#'
#' plot_size <- calc_plot_dim(lat_range = 30, lon_range = 30, lat_centre = 0)
#'
#' ggplot2::ggsave(filename = "plot1.png", plot = plot1,
#' height = plot_size["height"], width = plot_size["width"])
#'
#' ## Same lat/lon range closer to the poles
#' plot2 <- ggplot() +
#' geom_sf(data = world) +
#' coord_sf(xlim = c(0, 30), ylim = c(45, 75)) +
#' theme_void()
#'
#' plot_size <- calc_plot_dim(lat_range = 30, lon_range = 30, lat_centre = 60)
#'
#' ggplot2::ggsave(filename = "plot2.png", plot = plot2,
#' height = plot_size["height"], width = plot_size["width"])
#'
#' ## More space for titles and labels
#' plot3 <- ggplot() +
#' geom_sf(data = world) +
#' labs(title = "Map of Northern Europe") +
#' coord_sf(xlim = c(0, 30), ylim = c(45, 75)) +
#' theme_void()
#'
#' plot_size <- calc_plot_dim(lat_range = 30, lon_range = 30,
#' lat_centre = 60, offset = 1.2)
#'
#' ggplot2::ggsave(filename = "plot3.png", plot = plot3,
#' height = plot_size["height"], width = plot_size["width"])
#' }
calc_plot_dim <- function(lat_range, lon_range, lat_centre, height = 6,
                          offset = 1){

  # Calculate aspect ratio
  aspect_ratio <- (lon_range * cos(lat_centre * pi / 180)) / lat_range

  # Choose a height in inches (e.g., 6 inches) and calculate width
  height_in <- height
  width_in <- as.numeric(height_in * aspect_ratio)

  return(c(width = width_in*offset, height = height_in*offset))

}
