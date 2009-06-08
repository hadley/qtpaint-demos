# l(qtpaint); source("~/Documents/cranvas/qtpaint/demo/scatterplot.r")
library(qtpaint)

n <- 10000
x <- rnorm(n, 50, 25)
y <- rnorm(n, 50, 25)
df <- data.frame(X = x, Y = y)

circle <- qvPathCircle(0, 0, 5)
render_plot <- function(layer, canvas, exposed) {
  qvFillColor(canvas) <- "red"
  qvStrokeColor(canvas) <- NA
  qvGlyph(canvas, circle, df[,1], df[,2])
}

handle_keys <- function(event) {
  print(str(event))
  browser()
}

scene <- qvScene()
root <- qvLayer(scene)

points <- qvLayer(root, render_plot, keyPressFun = handle_keys)
qvSetLimits(points, range(df$X), range(df$X))

view <- qvViewWidget(scene = scene, opengl = FALSE)
overlay <- qvOverlay(view)

print(view)