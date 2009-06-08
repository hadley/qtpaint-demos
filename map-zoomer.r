# l(qtpaint); source("~/Documents/cranvas/qtpaint/demo/map-zoomer.r")
library(qtpaint)
library(ggplot2)

if (!exists("geo")) {
  load("~/Documents/data/08-housing-ca/geo.rdata")  
}
df <- data.frame(y = geo$lat * 100, x = geo$long * 100)
df <- df[complete.cases(df), ]

scatterplot <- function(layer, painter, exposed) {
  square <- qvPathRect(0, 0, 1, 1)
  if (zoom_level > 10) {
    square <- qvPathCircle(0, 0, 2)    
  }
  
  qvAntialias(painter) <- FALSE
  qvStrokeColor(painter) <- NA
  qvFillColor(painter) <- "black"
  qvGlyph(painter, square, df$x, df$y)
}

pos <- NULL

zoom_level <- 1
xrng <- diff(range(df$x, na.rm=T)) / 2
yrng <- diff(range(df$y, na.rm=T)) / 2

mouse_zoom <- function(event) {
  if (event$modifiers == 0) {
    zoom_in()
  } else {
    zoom_out()
  }
}

dragging <- FALSE
mouse_down <- function(event) {
  dragging <<- TRUE
}
mouse_up <- function(event) {
  dragging <<- FALSE
}
mouse_move <- function(event) {
  mat <- qvDeviceMatrix(event$item, event$view)
  pos <<- qvMap(mat, event$screenPos) 
  if (dragging) zoom_update()
  qvUpdate(scene)
}



zoom_in <- function(...) {  
  zoom_level <<- zoom_level + 1
  zoom_update()
}
zoom_out <- function() {
  zoom_level <<- zoom_level - 1
  zoom_update()  
}
zoom_update <- function() {
  qvSetLimits(points, 
    c(pos[1] - xrng / 1.4 ^ zoom_level, pos[1] + xrng / 1.4 ^ zoom_level), 
    c(pos[2] - yrng / 1.4 ^ zoom_level, pos[2] + yrng / 1.4 ^ zoom_level)
  )
}

key_pressed <- function(event) {
  print("You pressed a key!")
}

scene <- qvScene()
root <- qvLayer(scene)

points <- qvLayer(root, scatterplot, mouseMove = mouse_move, mouseDoubleClickFun = mouse_zoom, mouseReleaseFun = mouse_up, mousePressFun = mouse_down)
qvSetLimits(points, range(df$x, na.rm=T), range(df$y, na.rm=T))



view <- qvViewWidget(scene = scene, opengl = FALSE)
overlay <- qvOverlay(view)

print(view)