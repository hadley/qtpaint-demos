# l(qtpaint)
# source("~/Documents/cranvas/qtpaint/demo/alpha.r")
library(qtpaint)
library(ggplot2)

n=5000
df = data.frame(x=rnorm(n), y=rnorm(n))

bbase <- c(0,0) ; w = .5

underbrush <- function(df, bbrush) {
   df[,1] >= bbrush[1] & df[,1] <= bbrush[1]+w &
    df[,2] <= bbrush[2] & df[,2] >= bbrush[2]-w
}

view_size <- function(item) {
  qvBoundingRect(qtpaint:::qvPaintingView(item))[2, ]
}

scatterplot <- function(item, painter, exposed) {
  # print(min(view_size(item)) / 100)
  circle <- qvPathCircle(0, 0, min(view_size(item)) / 100)
  qvStrokeColor(painter) <- NA
  
  pt_underbrush <- underbrush(df, bbase)
  
  qvFillColor(painter) <- alpha("red", 1/10)
  qvGlyph(painter, circle, df[!pt_underbrush,1], df[!pt_underbrush,2])

  qvFillColor(painter) <- alpha("blue", 1/10)
  qvGlyph(painter, circle, df[pt_underbrush,1], df[pt_underbrush,2])
}

# Draw a brush
brushrect <- function(item, painter, exposed) {
# b <- qvPathRect(bbase[1], bbase[2], bbase[1]+w, bbase[2]-w)
  qvFillColor(painter) <- NA
  qvStrokeColor(painter) <- "darkblue"
  qvLineWidth(painter) <- 3
#  qvGlyph(painter, b, bbase[1], bbase[2])
  qvPolyline(painter,
             c(bbase[1], bbase[1]+w, bbase[1]+w, bbase[1], bbase[1]),
             c(bbase[2], bbase[2], bbase[2]-w, bbase[2]-w, bbase[2]))
}  

moveBrush <- function(event) {
  pt <- event$screenPos
  mat <- qvDeviceMatrix(event$item, event$view)
  pt <- qvMap(mat, pt)
  bbase <<- pt
  #print (bbase)
  qvUpdate(scene)
}


scene <- qvScene()
root <- qvLayer(scene)

points <- qvLayer(root, scatterplot, mouseMove = moveBrush)
qvSetLimits(points, range(df[,1]), range(df[,2]))

brush <- qvLayer(root, brushrect)
qvSetLimits(brush, qvLimits(points))
                 
labels <- qvLayer(root)
qvSetLimits(labels, qvLimits(points))

view <- qvViewWidget(scene = scene, opengl = FALSE)
overlay <- qvOverlay(view)

print(view)

# Move the brush around
#for(i in 1:100) {
#    bbase[1] = bbase[1] + runif(1, -.2, +.2)    
#    bbase[2] = bbase[2] + runif(1, -.2, +.2)
#    qvUpdate(scene)
#    Sys.sleep(1 / 66)
#}
