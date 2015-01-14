# rmse function

rmse <- function (y, y1) {
  rse <- sqrt(mean((y-y1)^2))
  return (rse)
} 
