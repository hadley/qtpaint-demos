# l(qtpaint)
# source("~/Documents/cranvas/demos/tour.r")
library(qtpaint)
library(ggplot2)
l(tourr)

view_size <- function(item) {
  qvBoundingRect(qtpaint:::qvPaintingView(item))[2, ]
}

f <- rescaler(flea[,1:4])
tour_anim <- tourer(f, grand_tour())
step <- tour_anim$step2(0.5 / 33)
data <- scale(as.matrix(f) %*% step$proj)

colour_scale <- scale_colour_hue()
colour_scale$train(flea$species, TRUE)
cols <- colour_scale$map(flea$species)

redraw <- function(item, painter, exposed) {
  circle <- qvPathCircle(0, 0, max(min(view_size(item) / 100), 1))
  
  qvStrokeColor(painter) <- NA
  qvGlyph(painter, circle, data[, 1], data[,2], fill = cols)
  # for(col in unique(cols)) {
  #   qvFillColor(painter) <- col
  #   qvGlyph(painter, circle, data[cols == col,1], data[cols == col,2])
  # }
}

scene <- qvScene()
root <- qvLayer(scene)

points <- qvLayer(root, redraw)
qvSetLimits(points, c(-3, 3), c(-3, 3))

view <- qvViewWidget(scene = scene, opengl = FALSE)
overlay <- qvOverlay(view)

print(view)


print(system.time({
  for(i in 1:100) {
    step <- tour_anim$step2(1 / 33)
    data <- scale(as.matrix(f) %*% step$proj)
    
    qvUpdate(scene)
    Sys.sleep(1 / 66)
  }
}))