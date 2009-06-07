# l(qtpaint)
# source("~/Documents/cranvas/qtpaint/demo/pixbin.r")
library(qtpaint)
library(ggplot2)

df <- data.frame(x = diamonds$carat, y = diamonds$price)

"dim.QViz::RLayer" <- function(item) {
  qvBoundingRect(qtpaint:::qvPaintingView(item))[2, ]
}

colour_scale <- scale_colour_gradient()

scatterplot <- function(layer, painter, exposed) {
  xbin <- cut(df$x, nrow(layer), include.lowest=TRUE)
  ybin <- cut(df$y, ncol(layer), include.lowest=TRUE)
  
  binned <- as.data.frame(xtabs( ~ xbin + ybin), responseName="count")
  colour_scale$train(binned$count)
  cols <- colour_scale$map(binned$count)
  
  qvImage(painter, cols, ncol(layer), nrow(layer), 1, 1)
}

scene <- qvScene()
root <- qvLayer(scene)

points <- qvLayer(root, scatterplot)
qvSetLimits(points, range(df$x), range(df$y))

view <- qvViewWidget(scene = scene, opengl = FALSE)
overlay <- qvOverlay(view)

print(view)


print(system.time({
  for(i in 1:100) {
    df$X <- df$X + runif(nrow(df), -0.01, 0.01)
    qvUpdate(scene)
    Sys.sleep(1 / 66)
  }
}))