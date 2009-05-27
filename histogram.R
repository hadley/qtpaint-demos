

## x11(width = 2, height = 2)

library(qtpaint)

## histo <- qhistogram(rnorm(100), breaks = NULL)

toplevel <- qvBasicWidget(minwidth = 500, minheight = 500)
tmp <- qvBasicWidget()
qvAddWidget(tmp, toplevel, 1, 1)
toplevel

topview <- qvViewWidget(toplevel)
topview

for (i in 1:10) 
{
    tmp[1, 1] <- qhistogram(rnorm(100))

    t <- sort(runif(10)) * 5 * pi

    tmp[2, 1] <-
        qscatter(sin(t), cos(t), fill = "yellow",
                 panel = function(x, y, ...) {
                     qv.panel.par(...)
                     qv.panel.polygon(x, y)
                 })

    tmp[2, 2] <-
        qv.xyplot(rnorm(100), rnorm(100),
                  alpha = 1, fill = "transparent")

}

qvRenderWidget(topview, view = TRUE)
qvRenderWidget(toplevel)



for (i in 1:60) 
{
    ## tmp[1, 1] <- qhistogram(rnorm(100))
    t <- seq(0, 20 * pi, length = i)
    Sys.sleep(0.1)
    tmp[2, 1] <-
        qscatter(sin(t), cos(t), fill = "yellow", col = "yellow",
                 panel = function(x, y, ...) {
                     qv.panel.par(...)
                     qv.panel.polygon(x, y)
                 })
}

qvSetMinHeight(toplevel, 1100)
qvSetMinWidth(toplevel, 1100)


foo <- qvTransform(topview, scale = 0.5)
foo <- qvTransform(topview, rotate = 30)

for (i in 1:100) {
    Sys.sleep(0.001)
    qvTransform(topview, rotate = 3.6)
}



for (i in 1:72) {
    Sys.sleep(0.001)
    qvTransform(topview, translate = c(5, 5))
}
