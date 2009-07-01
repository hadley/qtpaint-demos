# source("~/Documents/cranvas/demos/scatterplot.r")
library(qtpaint)

n <- 50000
x <- rnorm(n, 50, 25)
y <- rnorm(n, 50, 25)
df <- data.frame(X = x, Y = y)

size <- 3
alpha <- 1

render_plot <- function(layer, canvas, exposed) {
  circle <- qpathCircle(0, 0, size)

  qfillColor(canvas) <- ggplot2::alpha("blue", alpha)
  qstrokeColor(canvas) <- NA
  qdrawGlyph(canvas, circle, df[,1], df[,2])
}

handle_keys <- function(event) {
  if (event$key == "up") {
    size <<- size + 1
  } else if (event$key == "down") {
    size <<- max(size - 1, 1)
  } else if (event$key == "left") {
    alpha <<- max(alpha - 0.05, 0.05)
  } else if (event$key == "right") {
    alpha <<- min(alpha + 0.05, 1)
  }
  qupdate(scene)
}

scene <- qgraphicsScene()
view <- qplotView(scene = scene)

points <- qlayer(scene, render_plot, keyPressFun = handle_keys)
qlimits(points) <- qrect(range(df$X), range(df$X))

print(view)
