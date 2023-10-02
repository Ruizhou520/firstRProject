"rm(list=ls())

animal <- c('snake', 'ostrich', 'cat', 'spider')
nums_leg <- c(0, 2, 4, 8)

animal_df <- data.frame(animal, nums_leg)

print(animal_df)
"

myFirstRFunc <- function(n){
  "
  argument: n ---> number
  function: output the sum of those numbers strictly below n which are divisible by either 2 or 7 or both
  "
  
  sum <- 0
  iter <- 2
  while(iter<n){
    if(iter %% 7 == 0){
      iter <- iter+2
      next
    }
    
    sum <- sum + iter
    #print(iter)
    iter <- iter + 2
    
  }
  iter <- 7
  while(iter < n){
    sum <- sum + iter
    #print(iter)
    iter <- iter + 7
    
  }
  
  return(sum)
}

print(myFirstRFunc(1000))