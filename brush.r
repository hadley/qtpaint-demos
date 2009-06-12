# l(qtpaint); source("~/Documents/cranvas/qtpaint/demo/brush.r")
library(qtpaint)
library(ggplot2)

n=5000
df = data.frame(x=rnorm(n), y=rnorm(n))

bbase <- c(0,0) ; w = .5

underbrush <- function(df, bbrush) {
   df[,1] >= bbrush[1] & df[,1] <= bbrush[1] + w &
    df[,2] <= bbrush[2] & df[,2] >= bbrush[2] - h
}

view_size <- function(item) {
  qvBoundingRect(qtpaint:::qvPaintingView(item))[2, ]
}

scatterplot <- function(item, painter, exposed) {
  circle <- qvPathCircle(0, 0, min(view_size(item)) / 100)
  qvStrokeColor(painter) <- NA
    
  pt_underbrush <- underbrush(df, bbase)             
  qvFillColor(painter) <- alpha("red", 1/10)
  qvGlyph(painter, circle, df[!pt_underbrush, 1], df[!pt_underbrush, 2])

  qvFillColor(painter) <- alpha("blue", 1/10)
  qvGlyph(painter, circle, df[pt_underbrush, 1], df[pt_underbrush,2])

}

# Draw a brush
brushrect <- function(item, painter, exposed) {
  qvFillColor(painter) <- NA
  qvStrokeColor(painter) <- "darkblue"
  qvLineWidth(painter) <- 3
  qvRect(painter, bbase[1], bbase[2], bbase[1]+w, bbase[2]-h)
}  

moveBrush <- function(event) {
  if (!is.null(drag_start)) {
    drag_end <- event$screenPos
    mat <- qvDeviceMatrix(event$item, event$view)
    size <- qvMap(mat, drag_end) - qvMap(mat, drag_start)

    w <<- abs(size[1])
    h <<- abs(size[2])    
    
    pt <- drag_start
  } else {
    pt <- event$screenPos
    
  }

  mat <- qvDeviceMatrix(event$item, event$view)
  pt <- qvMap(mat, pt)
  bbase <<- pt
  #print (bbase)
  
  
  qvUpdate(scene)
}

drag_start <- NULL
start_drag <- function(event) {
  drag_start <<- event$screenPos 
}

end_drag <- function(event) {
  drag_start <<- NULL 
}


scene <- qvScene()
root <- qvLayer(scene)

points <- qvLayer(root, scatterplot, mouseMove = moveBrush, mouseReleaseFun = end_drag, mousePressFun = start_drag)

qvSetLimits(points, range(df[,1]), range(df[,2]))

brush <- qvLayer(root, brushrect)
qvSetLimits(brush, qvLimits(points))

view <- qvViewWidget(scene = scene, opengl = FALSE)
overlay <- qvOverlay(view)

print(view)
