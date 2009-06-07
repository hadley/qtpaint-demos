# l(qtpaint); source("~/Documents/cranvas/qtpaint/demo/pixbin.r")
library(qtpaint)
library(ggplot2)

df <- data.frame(x = diamonds$carat, y = diamonds$price)

"dim.QViz::RLayer" <- function(item) {
  qvBoundingRect(qtpaint:::qvPaintingView(item))[2, ]
}

scatterplot <- function(layer, painter, exposed) {
  xbin <- cut(df$x, nrow(layer), include.lowest=TRUE)
  ybin <- cut(df$y, ncol(layer), include.lowest=TRUE)
  
  binned <- as.data.frame(xtabs( ~ xbin + ybin), responseName="count")
  binned <- subset(binned, count > 0)
  binned$xbin <- as.numeric(binned$xbin)
  binned$ybin <- as.numeric(binned$ybin)
  binned$count <- binned$count / max(binned$count) *  2
  print(nrow(binned))

  mat <- qvDeviceMatrix(layer)
  coords <- qvMap(mat, cbind(binned$xbin, binned$ybin))
  
  square <- qvPathRect(0, 0, 1, 1) # 
  
  qvAntialias(painter) <- FALSE
  qvStrokeColor(painter) <- NA
  qvFillColor(painter) <- "black"
  qvGlyph(painter, square, coords[, 1], coords[, 2])
}

scene <- qvScene()
root <- qvLayer(scene)

points <- qvLayer(root, scatterplot)
qvSetLimits(points, range(df$x), range(df$y))

view <- qvViewWidget(scene = scene, opengl = FALSE)
overlay <- qvOverlay(view)

print(view)