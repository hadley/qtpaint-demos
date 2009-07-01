# source("~/Documents/cranvas/demos/brush.r")
library(qtpaint)
library(ggplot2)

n <- 5000
df <- data.frame(x = rnorm(n), y = rnorm(n))

bbase <- c(0,0)
h <- 0.5
w <- 0.5

underbrush <- function(df, bbrush) {
   df[,1] >= bbrush[1] & df[,1] <= bbrush[1] + w &
    df[,2] <= bbrush[2] & df[,2] >= bbrush[2] - h
}

view_size <- function(item) {
  qboundingRect(qtpaint:::qpaintingView(item))[2, ]
}

scatterplot <- function(item, painter, exposed) {
  circle <- qpathCircle(0, 0, min(view_size(item)) / 100)
  qstrokeColor(painter) <- NA
    
  qfillColor(painter) <- alpha("red", 1/10)
  qdrawGlyph(painter, circle, df[, 1], df[, 2])
}

brushrect <- function(item, painter, exposed) {
  # Draw a brush
  qfillColor(painter) <- NA
  qstrokeColor(painter) <- "darkblue"
  qlineWidth(painter) <- 3
  qdrawRect(painter, bbase[1], bbase[2], bbase[1]+w, bbase[2]-h)

  # Draw points under the brush
  circle <- qpathCircle(0, 0, min(view_size(item)) / 100)
  qfillColor(painter) <- "blue"
  qstrokeColor(painter) <- NA

  pt_underbrush <- underbrush(df, bbase)             
  qdrawGlyph(painter, circle, df[pt_underbrush, 1], df[pt_underbrush,2])
}  

moveBrush <- function(event) {
  if (!is.null(drag_start)) {
    drag_end <- event$screenPos
    mat <- qdeviceMatrix(event$item, event$view)
    size <- qmap(mat, drag_end) - qmap(mat, drag_start)

    w <<- abs(size[1])
    h <<- abs(size[2])    
    
    pt <- drag_start
  } else {
    pt <- event$screenPos    
  }

  mat <- qdeviceMatrix(event$item, event$view)
  pt <- qmap(mat, pt)
  bbase <<- pt
  
  qupdate(brush)
}

drag_start <- NULL
start_drag <- function(event) {
  drag_start <<- event$screenPos 
}

end_drag <- function(event) {
  drag_start <<- NULL 
}


scene <- qgraphicsScene()
root <- qlayer(scene)

view <- qplotView(scene = scene)

points <- qlayer(root, scatterplot, 
  mouseMove = moveBrush, 
  mouseReleaseFun = end_drag, 
  mousePressFun = start_drag)
qlimits(points) <- qrect(range(df[,1]), range(df[,2]))

brush <- qlayer(root, brushrect)
qlimits(brush) <- qlimits(points)

print(view)