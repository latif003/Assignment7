# 13 Januari 2015
# Nikoula; Latifah and Nikos
# Root mean square error (rmse) function

rmse <- function (ori, pred) {
  rmse <- round(sqrt(mean((ori - pred) ^ 2)), 3)
} 
