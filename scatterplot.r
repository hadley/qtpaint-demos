# source("~/Documents/cranvas/demos/scatterplot.r")
library(qtpaint)

n <- 1e6
x <- rnorm(n, 50, 25)
y <- rnorm(n, 50, 25)
df <- data.frame(X = x, Y = y)

size <- 3
alpha <- 1

render_plot <- function(item, painter, exposed) {

  
  if (size < 0.5) {
    qstrokeColor(painter) <- ggplot2::alpha("blue", alpha)
    qdrawPoint(painter, df[, 1], df[,2])
  } else {
    circle <- qpathCircle(0, 0, size)
    qfillColor(painter) <- ggplot2::alpha("blue", alpha)
    qstrokeColor(painter) <- NA
    qdrawGlyph(painter, circle, df[, 1], df[,2])
  }
}

handle_keys <- function(event) {
  if (event$key == "up") {
    size <<- size + 0.5
  } else if (event$key == "down") {
    size <<- max(size - 0.5, 0.5)
  } else if (event$key == "left") {
    alpha <<- max(alpha - 0.05, 0.05)
  } else if (event$key == "right") {
    alpha <<- min(alpha + 0.05, 1)
  }
  qupdate(scene)
}

if (exists("view")) qclose(view)

scene <- qgraphicsScene()
view <- qplotView(scene = scene)

points <- qlayer(scene, render_plot, keyPressFun = handle_keys)
qlimits(points) <- qrect(range(df$X), range(df$X))

print(view)
