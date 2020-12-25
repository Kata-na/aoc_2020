coordinates_change <- function(action, coordinates, size) {
  if (action == 'N') {
    coordinates <- coordinates + c(0, -size)
  } else if (action == 'S') {
    coordinates <- coordinates + c(0, size)
  } else if (action == 'W') {
    coordinates <- coordinates + c(size, 0)
  } else if (action == 'E') {
    coordinates <- coordinates + c(-size, 0)
  }
  return(coordinates)
}
