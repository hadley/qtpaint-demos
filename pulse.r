# l(qtpaint)
# source("~/Documents/cranvas/qtpaint/demo/pulse.r")
library(qtpaint)
library(ggplot2)
l(tourr)

data <- rescaler(flea[1:2], "range")

view_size <- function(item) {
  qvBoundingRect(qtpaint:::qvPaintingView(item))[2, ]
}

transp_cur <- rep(0.61, nrow(data))
transp_delta <- runif(nrow(data), 0.01, 0.05)
transp_dir <- 1

redraw <- function(item, painter, exposed) {
  circle <- qvPathCircle(0, 0, max(min(view_size(item) / 20), 1))
  
  qvStrokeColor(painter) <- NA
  for(i in 1:nrow(data)) {
    # qvFillColor(painter) <- hcl(transp_cur[i] * 500 + 60, l = 50)
    qvFillColor(painter) <- hcl(240, l = 100 * transp_cur[i])
    qvGlyph(painter, circle, data[i,1], data[i,2])
  }
}

scene <- qvScene()
root <- qvLayer(scene)

points <- qvLayer(root, redraw)
qvSetLimits(points, c(-0.05, 1.05), c(-0.05, 1.05))

view <- qvViewWidget(scene = scene, opengl = FALSE)
overlay <- qvOverlay(view)

print(view)

for(i in 1:50) {
  transp_dir <<- transp_dir * 
    ifelse(transp_cur > 0.9 | transp_cur < 0.6, -1, 1)
  transp_cur <<- transp_cur + transp_delta * transp_dir
  
  qvUpdate(scene)
  Sys.sleep(1/33)
}