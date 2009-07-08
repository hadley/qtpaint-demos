# source("~/Documents/cranvas/demos/scatterplot.r")
source("slider.r")
library(qtpaint)

n <- 1e6
df <- data.frame(X = rnorm(n, 50, 25), Y = rnorm(n, 50, 25))

size_slider <- new_slider(0.5, Inf, 0.5, 3)
alpha_slider <- new_slider(0.01, 1, 0.05, 1)

render_plot <- function(item, painter, exposed) {
  size <- size_slider$val()
  alpha <- alpha_slider$val()
  col <- ggplot2::alpha("grey20", alpha)
  qantialias(painter) <- FALSE
  
  if (size < 0.5) {
    qstrokeColor(painter) <- col
    qdrawPoint(painter, df[, 1], df[,2])
  } else {
    circle <- qpathCircle(0, 0, size)
    qfillColor(painter) <- col
    qstrokeColor(painter) <- NA
    qdrawGlyph(painter, circle, df[, 1], df[,2])
  }
}

handle_keys <- function(event) {
  if (event$key == "up") {
    size_slider$inc()
  } else if (event$key == "down") {
    size_slider$dec()
  } else if (event$key == "left") {
    alpha_slider$dec()
  } else if (event$key == "right") {
    alpha_slider$inc()
  }
  qupdate(points)
}

if (exists("view")) qclose(view)

scene <- qgraphicsScene()
root <- qlayer(scene)
view <- qplotView(scene = scene)

points <- qlayer(root, render_plot, keyPressFun = handle_keys)
qlimits(points) <- qrect(range(df$X), range(df$X))
print(view)
