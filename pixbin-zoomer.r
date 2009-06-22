# source("~/Documents/cranvas/demos/pixbin-zoomer.r", chdir=T)
library(qtpaint)
library(reshape)
library(ash)

source("limits.r")
if (!exists("geo")) {
  load("geo.rdata")  
}
df <- data.frame(y = geo$lat * 100, x = geo$long * 100)
df <- df[complete.cases(df), ]


"dim.QViz::RLayer" <- function(item) {
  qvBoundingRect(qtpaint:::qvPaintingView(item))[2, ]
}
pix_to_data <- function(data, layer) {
  data[, 2] <- ncol(layer) - data[, 2]
  mat <- qvDeviceMatrix(layer, inverted = TRUE)
  qvMap(mat, data)
}

scale01 <- function(x) (x - min(x)) / diff(range(x))

scatterplot <- function(layer, painter, exposed) {
  visible <- limits$subset(df)
  binned <- subset(pixbin2(visible$x, visible$y, dim(layer)), value > 0)
  coords <- pix_to_data(cbind(binned$X1, binned$X2), layer)
  
  col <- grey(scale01(-log(binned$value)))
  qvPoint(painter, coords[, 1], coords[, 2], stroke = col)
}

pixbin2 <- function(x, y, n) {
  mat <- cbind(x, y)
  mat <- mat[complete.cases(mat), ]
  rng <- rbind(range(mat[ ,1]), range(mat[, 2]))

  melt(bin2(mat, rng, n)$nc)
}

zoom <- function(ev) {
  if (ev$delta > 0) {
    limits$zoom_in()
    qvUpdate(points)
  } else {
    limits$zoom_out()
    qvUpdate(points)
  }
}

scene <- qvScene()
root <- qvLayer(scene)

points <- qvLayer(root, scatterplot, wheelFun = zoom)
qvSetLimits(points, c(0, 1), c(0, 1))

limits <- new_limits(df$x, df$y)

view <- qvView(scene = scene)
print(view)