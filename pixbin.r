# source("~/Documents/cranvas/demos/pixbin.r")
library(qtpaint)
library(ash)

library(ggplot2)
df <- data.frame(x = diamonds$carat, y = diamonds$price)

"dim.QViz::RLayer" <- function(item) {
  qvBoundingRect(qtpaint:::qvPaintingView(item))[2, ]
}
pix_to_data <- function(data, layer) {
  mat <- qvDeviceMatrix(layer, inverted = TRUE)
  qvMap(mat, data)
}
data_to_pix <- function(data, layer) {
  mat <- qvDeviceMatrix(layer, inverted = FALSE)
  qvMap(mat, data)  
}

scale01 <- function(x) (x - min(x)) / diff(range(x))

scatterplot <- function(layer, painter, exposed) {
  binned <- subset(pixbin2(df$x, df$y, dim(layer)), value > 0)
  coords <- pix_to_data(cbind(binned$X1, binned$X2), layer)
  
  col <- grey(scale01(-log(binned$value)))
  qvPoint(painter, coords[, 1], coords[, 2], stroke = col)
}

pixbin2 <- function(x, y, n) {
  mat <- cbind(df$x, df$y)
  mat <- mat[complete.cases(mat), ]
  rng <- rbind(range(mat[ ,1]), range(mat[, 2]))

  melt(bin2(mat, rng, n)$nc)
}

scene <- qvScene()
root <- qvLayer(scene)

points <- qvLayer(root, scatterplot)
qvSetLimits(points, range(df$x), range(df$y))

view <- qvView(scene = scene)
print(view)