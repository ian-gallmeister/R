#Class 11

coefs <- c(intercept = 2,
           income = 7,
           balance = -3)

linear_combination <- function(coefficient, data){
  start_with <- 0
  #k in coefficient gives 2, 7, -3.  Use 1:length(coefficient) to iterate through 1, 2, 3, ...
  for (k in 1:length(coefficient)){
    #Now need to go through the names, so need those
    if( names(coefficient)[k] == "intercept") {
      start_with <- start_with + coefficient$intercept
    } else { 
      if (! names(coefficient)[k] %in% names(data)) { #Here, look to see if coefficient is in data.  
        stop("Coefficient", names(coefficient)[k], "not in data") #If not, stop and return the bad coeff
      }
      #Now, can use k to get a specific coefficient and use names(coef)[k] to get the associated data
      start_with <- start_with + coefficient[k] * data[[names(coefficient)[k]]]
    }
  }
}