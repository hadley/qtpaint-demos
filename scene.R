
library(qvizexpt)

myscene <- qvScene()

myview <- qvViewWidget(scene = myscene)
myview


qvScene.points(myscene, x = 1:100, y = rnorm(100), radius = 10)

### Bad things happen with NA :-(

## yy <- 1:10
## qvScene.points(myscene, 1:10, yy, radius = 0.5)
## yy[c(3, 6)] <- NA
## qvScene.points(myscene, 1:10, yy, radius = 1)

qvTransformView(myview, scale = 3)
qvTransformView(myview, yscale = 2)

qvTransformView(myview, rotate = 30)
qvTransformView(myview, rotate = -30)

qvTransformView(myview, translate = c(10, 0)) ## ??




qvScene.lines(myscene, x = 5 * (1:100), y = 3 + 5 * rnorm(100), lwd = 0:2)

lens <- 30 * runif(100)
qvScene.segments(myscene,
                 x1 = 5 * 1:100, y1 = 10,
                 x2 = 5 * 1:100, y2 = 10 + lens)

qvScene.rect(myscene, x = 500 * runif(50), y = 60 + 10 * rnorm(50))

qvScene.text(myscene, 500, c(0, 10, 50), labels = "some text")

qvScene.text(myscene, 200, 100,
             labels = "<i>Fancy</i> text with <a href='http://slashdot.org'>links</a>!",
             html = TRUE)

qvView.setantialias(myview, TRUE)

qvView.setdrag(myview, "scroll")

qvView.setdrag(myview, "select")




