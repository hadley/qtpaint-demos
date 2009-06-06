# l(qtpaint)
# source("~/Documents/cranvas/qtpaint/demo/alpha.r")
library(qtpaint)
library(ggplot2)

n <- 50000
x <- runif(n)
y <- runif(n)
df <- data.frame(X = x, Y = y)

view_size <- function(item) {
  qvBoundingRect(qtpaint:::qvPaintingView(item))[2, ]
}

scatterplot <- function(item, painter, exposed) {
  # print(min(view_size(item)) / 100)
  circle <- qvPathCircle(0, 0, min(view_size(item)) / 100)
  qvFillColor(painter) <- alpha("red", 1/10)
  qvStrokeColor(painter) <- NA
  qvGlyph(painter, circle, df[,1], df[,2])
}

scene <- qvScene()
root <- qvLayer(scene)

points <- qvLayer(root, scatterplot)
qvSetLimits(points, range(df[,1]), range(df[,2]))

labels <- qvLayer(root)
qvSetLimits(labels, qvLimits(points))

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