
# normalize min - max
normalize <- function(x, ...) {
  return((x - min(x,na.rm = TRUE)) /(max(x,na.rm = TRUE) - min(x,na.rm = TRUE)))
}

# Function to be applied on the raster values; return: SpatRaster object
rescale01 <- function(x) {
  
  val <- values(x)
  
  values(x) <- (val - min(val, na.rm = TRUE)) / (max(val, na.rm = TRUE) - min(val, na.rm = TRUE))
  
  x
}
