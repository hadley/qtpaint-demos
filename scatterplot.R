
## x11(width = 2, height = 2)

library(qvizexpt)
## dens <- qv.densityplot(rnorm(100), main = "Normal variates")
## rm(dens)
## gc()



dens <- qv.densityplot(rnorm(100), xlab = "Normal variates", main = "কিংকর্তব্যবিমূঢ")

xy <- with(iris, 
           qv.xyplot(Petal.Length, Petal.Width, pch = "+"))


toplevel <- qvBasicWidget(minwidth = 500, minheight = 500)
toplevel <- qvAddWidget(dens, toplevel, 1, 1, store = TRUE)
toplevel

toplevel <- qvAddWidget(xy, toplevel, 2, 1, store = TRUE)

str(toplevel[1, 1])

## qvAddWidget(xy, toplevel, 3, 3, store = TRUE)

if (FALSE)
{

    web <- qvBrowserWidget("http://slashdot.org")
    qvAddWidget(web, toplevel, 3, 1, store = FALSE)
    qvSetUrl(web, "http://cran.fhcrc.org")
    qvSetUrl(web, "http://lmdvr.r-forge.r-project.org")

}





## a different interface

## qscatter(sin(1:10))

foo <-
    with(list(x = seq(0, 2 * pi, length = 101)),
         qscatter(x, sin(3 * x), type = "l"))


bar <- qscatter(runif(1000), rnorm(1000), fill = "goldenrod",
                pch = rep(letters, length = 1000))

#str(qvGetRenderInfo(foo))
#str(qvGetRenderInfo(bar))

barunif <- qscatter(runif(1000), runif(1000), fill = "yellow")

## tmp <- qscatter(sort(runif(100)), sort(runif(100)), fill = "green")

toplevel <- qvBasicWidget(minwidth = 500, minheight = 500)
tmp <- qvBasicWidget()
qvAddWidget(tmp, toplevel, 1, 1)
            
toplevel

tmp[1, 1] <- bar
tmp[2, 1] <- foo
tmp[2, 2] <- qscatter(runif(1000), runif(1000),
                      fill = "yellow")


for (i in 1:100) 
{
    tmp[2, 1] <- 
        with(list(x = seq(0, i * pi / 10, length = 101)),
             qscatter(x, sin(x), type = "l"))
    tmp[2, 2] <-
        qscatter(runif(100), runif(100),
                 fill = "yellow", alpha = 0.5)
}


## no reference, so goes away on gc() after tmp is deleted

qscatter(sort(runif(100)), sort(runif(100)), fill = "green")



### labels

xlab <- qlabel("A quick brown fox", hadj = 0.5, vadj = -0.5)

ylab <- qlabel("Sin(x)", horizontal = FALSE)

tmp <- qvBasicWidget()

qvAddWidget(foo, tmp, 1, 2)
qvAddWidget(ylab, tmp, 1, 1)
qvAddWidget(xlab, tmp, 2, 2)
gc()


## a whole function

toplevel <- qvBasicWidget()


gc()
qvAddWidget(qv.densityplot(rnorm(100), main = "Normal variates"),
            toplevel, 1, 1)


