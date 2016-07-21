#Ian Gallmeister
#Statistical Computing + Machine Learning
#11 February 2016
#Create a my_hist function for the Day05 activity.

my_hist <- function(nbins, data, ...){
 
  evenly_spaced <- seq(min(data), max(data), length = nbins + 1)
  bin_counts <- table(cut(data, evenly_spaced))
  xL <- evenly_spaced[-11]
  xR <- evenly_spaced[-1]
  count <- as.numeric(bin_counts)
  hist_basics <- data.frame(xL, xR, count)
  
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
  plot(My_bars, type = "n")
  lines(My_bars)
  polygon(My_bars, ...)
   
}

#scoreActivity::score253(day = 6)