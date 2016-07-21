#Ian Gallmeister
#Statistical Computing + Machine Learning
#4 February 2016

#Task 1 ---------------------------------

library(mosaicData)
nbins<- 10

#Task 2 ---------------------------------

evenly_spaced <- seq(min(Galton$height), max(Galton$height), length = nbins + 1)

#Task 3 ---------------------------------

bin_counts <- table(cut(Galton$height, evenly_spaced))

#Task 4 ---------------------------------

xL <- evenly_spaced[-11]
xR <- evenly_spaced[-1]
count <- as.numeric(bin_counts)
hist_basics <- data.frame(xL, xR, count)

#Task 5 ---------------------------------

make_one_bar <- function(point){
  xLeft <- point$xL
  xRight <- point$xR
  height <- point$count
  res <- data.frame(x = c(xLeft, xLeft, xRight, xRight, NA), y = c(0, height, height, 0, NA))
  res
}

one_to_five <- function(hist_data){
  bars <- NULL
  for (k in 1:nrow(hist_data)){
    new_bar <- make_one_bar(hist_data[k,])
    bars <- rbind(bars, new_bar)
  }
  bars
}

My_bars <- data.frame(one_to_five(hist_basics))

#Task 6 ---------------------------------

plot(My_bars, type = "n")
lines(My_bars)
polygon(My_bars, col = "violet")

#Scoring --------------------------------

scoreActivity::score253(day = 5)

