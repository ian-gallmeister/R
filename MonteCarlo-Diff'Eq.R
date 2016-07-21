#Monte Carlo Sim
#10,000,000 Matrices - classify fixed points

0 -> saddle
0 -> unstable_node
0 -> unstable_spiral
0 -> center
0 -> stable_spiral
0 -> stable_node
0 -> nonisolated_fixed_point
0 -> stars_degenerate_etc
0 -> this_is_embarrasing
0 -> done_fucked_up

for(i in 1:100000000){ #Small thousand trial run
  #twobytwo <- matrix(data = runif(4, min = -1, max = 1), nrow = 2, ncol = 2)
  #twobytwo <- matrix(data = c(runif(2, min = -1, max = 1), runif(1, min = -0.5, max = 0.5), runif(1, min = 0, max = 2)), nrow = 2, ncol = 2, byrow = FALSE) #biased
  twobytwo <- matrix(data = c(rnorm(4, mean = 0, sd = 0.5)), nrow = 2, ncol = 2) #normal
  determinant <- twobytwo[1,1]*twobytwo[2,2] - twobytwo[1,2]*twobytwo[2,1]
  trace <- twobytwo[1,1] + twobytwo[2,2]
  if(determinant < 0){
    saddle <- saddle + 1 #Saddle is only det > 0
  } else if(determinant == 0){nonisolated_fixed_point <- nonisolated_fixed_point + 1 #Nonis fp is only det = 0
  } else if(determinant > 0){ # For all the det > 0
      if(trace == 0){center <- center + 1 #Trace = 0 --> center
    } else if((trace*trace - 4*determinant) == 0){stars_degenerate_etc <- stars_degenerate_etc + 1 #Thingy = 0 --> stars,degenerate, etc...
    } else if(trace < 0){ #Trace less than zero
        if((trace*trace - 4*determinant)  > 0){stable_node <- stable_node + 1 #Thingy less than zero --> stable spiral
        } else if((trace*trace - 4*determinant) < 0){stable_spiral <- stable_spiral + 1 #Thingy greater than zero --> stable node
        } else{this_is_embarrasing <- this_is_embarrasing + 1} #If neither stable spiral nor stable node, code done fucked up
    }
    else if(trace > 0){ #If trace greather than zero
        if((trace*trace - 4*determinant)  > 0){unstable_node <- unstable_node + 1 #Thingy less than zero --> unstable spiral
      } else if((trace*trace - 4*determinant) < 0){unstable_spiral <- unstable_spiral + 1 #Thingy greater than zero --> unstable node
      } else{this_is_embarrasing <- this_is_embarrasing + 1} #If neither stable spiral nor stable node, code done fucked up
    }
    else{done_fucked_up <- done_fucked_up + 1} #First nesting fuckup
  }
}