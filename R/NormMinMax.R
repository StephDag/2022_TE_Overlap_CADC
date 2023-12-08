
# normalize min - max
normalize <- function(x, ...) {
  return((x - min(x,na.rm = TRUE)) /(max(x,na.rm = TRUE) - min(x,na.rm = TRUE)))
}

