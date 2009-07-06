
pixbin2 <- function(x, y, n) {
  mat <- cbind(df$x, df$y)
  mat <- mat[complete.cases(mat), ]
  rng <- rbind(range(mat[ ,1]), range(mat[, 2]))

  subset(melt(bin2(mat, rng, n)$nc), value > 0)
}

# system.time(pixbin2(df$x, df$y, c(500, 500)))
# system.time(pixbin3(df$x, df$y, c(500, 500)))

pixbin3 <- function(x, y, n) {
  x <- cut(x, n[1], labels = FALSE)
  y <- cut(y, n[2], labels = FALSE)
  
  bin <- (x - 1) + (y - 1) * (n[1])

  bins <- tabulate(bin, n[1] * n[2])
  non_empty <- which(bins > 0)
  bins <- bins[bins > 0]
  
  data.frame(
    X1 = non_empty %% n[1] + 1,
    X2 = non_empty %/% n[1] + 1,
    value = bins
  )
  
  # y <- 
  
  # convert x & y to integers
  # compute binnum <- x + y * max(x)
  # use tabulate to count
  # use modular arithment to extract values
}
