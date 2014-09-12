
# function to grab bars out of a matrix in y
grab_bars <- function(y, col, ylim)
{
  # find range of y
  find_range <- function(x, col, xlim) {
    wch <- rep(F, length(x))
    wch[xlim[1]:xlim[2]] <- T
    y <- which(x == col & wch)
    if (length(y) > 0)
      diff(range(y))
    else
      0
  }

  vals <- apply(y, 2, find_range, col, ylim)
  vals <- rle(vals)$values
  return(vals[vals > 0])
}

# read bmp files into R and plot
library(bmp)
library(pixmap)

bmp <- read.bmp("test.bmp")
plot(pixmapRGB(bmp))

# convert to a matrix of colours
y <- apply(bmp, 1:2, function(x) { sprintf("#%02x%02x%02x", x[1], x[2], x[3]) })

# find horizontal lines (lines where we have a lot of colour?)
lines <- apply(y, 1, function(x) { sum(x != "#ffffff") })
lines <- which(lines > 900)
scale <- sum(diff(lines))

# figure out how many colours we have
z <- as.vector(y)
cols <- unique(z)

# grab out the colours we want
cols_to_grab <- c(38,40)

dat <- matrix(0, 21, length(cols_to_grab))
colnames(dat) <- 1:length(cols_to_grab)
for (i in 1:length(cols_to_grab)) {
  dat[,i] <- grab_bars(y, cols[cols_to_grab[i]], range(lines)) / scale
  colnames(dat)[i] <- paste("vacc", 2005 + i, sep="")
}
write.csv(dat, "mmr2_rates.csv", row.names=F)


########################## Do the same for MMR1

bmp <- read.bmp("test2.bmp")
plot(pixmapRGB(bmp))

# convert to a matrix of colours
y <- apply(bmp, 1:2, function(x) { sprintf("#%02x%02x%02x", x[1], x[2], x[3]) })

# find horizontal lines (lines where we have a lot of colour?)
lines <- apply(y, 1, function(x) { sum(x != "#ffffff") })
lines <- which(lines > 900)
scale <- sum(diff(lines))

# figure out how many colours we have
y <- apply(bmp, 1:2, function(x) { sprintf("#%02x%02x%02x", x[1], x[2], x[3]) })
z <- as.vector(y)
cols <- unique(z)

# grab out the colours we want
cols_to_grab <- c(38,40)

dat <- matrix(0, 21, length(cols_to_grab))
colnames(dat) <- 1:length(cols_to_grab)
for (i in 1:length(cols_to_grab)) {
  dat[,i] <- grab_bars(y, cols[cols_to_grab[i]], range(lines)) / scale
  colnames(dat)[i] <- paste("vacc", 2005 + i, sep="")
}
write.csv(dat, "mmr1_rates.csv", row.names=F)




