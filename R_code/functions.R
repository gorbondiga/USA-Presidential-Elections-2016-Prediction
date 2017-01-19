
#changing the probability of vote for clinton and trump, getting to 1 clinton
# and 0 voting for turmp:
clint_01 <- function(clint, trump) {
  count <- 0
  for(i in 1:length(clint)) {
    if(clint[i] > trump[i]) {
      clint[i] = 1
      #fd$prob_trump[i] = 0
    }else if(clint[i] < trump[i]) {
      clint[i] = 0
      #fd$prob_trump[i] = 1
    } else if(clint[i] == trump[i]) {
      if(count %% 2 == 0) {
        clint[i] = 1
        count = count +1
      } else {
        clint[i] = 0
        count = count +1
      }
    }
  }
  clint = as.factor(clint)   
  return(clint) 
}


#Get states subsets:
states_list <- function(fd) {
  states = list()

  for(j in 1:56) {
    if(j != 3 || j != 52 || j != 14 || j != 43) {
      aux <- subset(fd, statereside == j, drop = FALSE)
      states[[j]] <- drop.levels(aux, reorder = FALSE)
    }
  }
  
  return(states)
}

#Return the data with the variables that you pas in the var vector from the fd dataset:
getNewData <- function(fd, var) {
    data = subset(fd, select = var)
    data = drop.levels(data, reorder = FALSE)
  
    data$clint <- clint_01(fd$prob_clint, fd$prob_trump)
    data$clint <- as.factor(data$clint)
  
    data[data == "NaN"] = NA
  
    return(data)
}

#fix the data of the states, cause there are some states that one or more variable(s) can have the same value for all of the rows
fixData <- function(data) {
  aux = data
  count = 1
  a <- apply(aux, 2, function(x) length(unique(x)) == 1)
  for(i in a) {
    if(i == TRUE) {
      aux[[count]] <- NULL
      count = count -1
    }
    count = count +1
  }
  return(aux)
}
