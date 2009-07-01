# l(qtpaint)
# source("~/Documents/cranvas/demos/tour.r")
library(qtpaint)
library(ggplot2)
library(tourr)

view_size <- function(item) {
  qboundingRect(qtpaint:::qpaintingView(item))[2, ]
}

f <- rescaler(flea[,1:4])
tour_anim <- tourer(f, grand_tour())
step <- tour_anim$step2(0.5 / 33)
data <- scale(as.matrix(f) %*% step$proj)

colour_scale <- scale_colour_hue()
colour_scale$train(flea$species, TRUE)
cols <- colour_scale$map(flea$species)

redraw <- function(item, painter, exposed) {
  circle <- qpathCircle(0, 0, max(min(view_size(item) / 100), 1))
  
  qstrokeColor(painter) <- NA
  qdrawGlyph(painter, circle, data[, 1], data[,2], fill = cols)
}

scene <- qgraphicsScene()
root <- qlayer(scene)

points <- qlayer(root, redraw)
qlimits(points) <- qrect(c(-3, 3), c(-3, 3))

view <- qplotView(scene = scene)
print(view)


print(system.time({
  for(i in 1:100) {
    step <- tour_anim$step2(1 / 33)
    data <- scale(as.matrix(f) %*% step$proj)
    
    qupdate(scene)
    Sys.sleep(1 / 66)
  }
}))