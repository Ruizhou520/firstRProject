# function:  calculates the final amount of money based on certain financial inputs

final_amount <- function(
        principal, 
        interest_rate,
        years,
        compound
){
  "
  input: 
  1. principal: the initial amount of money
  2. interest_rate: annual return rate in percentage
  3. years: number of years the money saved
  4. compound: boolean indicating compound or simple interst
  
  output:final amount of money by year frequency
  "
  
  sum <- principal
  year <- 0
  while(year < years){
    if(compound){
      interest <- sum * interest_rate
      sum <- sum+interest
      year <- year + 1
    }else{
      interest <- principal * interest_rate
      sum <- sum + interest
      year <- year + 1
    }
  }
  
  return(sum)
}

test1 <- final_amount(1000, 0.05, 20, TRUE)
test2 <- final_amount(1000, 0.05, 20, FALSE)

principals <- seq(500, 2500, 500)
finals <- c()
finals_simple <- c()
for(principal in principals){
  finals[length(finals)+1] <- final_amount(principal, 0.05, 20, TRUE)
  finals_simple[length(finals_simple)+1] <- final_amount(principal, 0.05, 20, FALSE)
  
}

plot(principals, finals, col=2, type = "b", ylim = c(min(finals_simple), max(finals)))  # ylim = c(500, 7000) set min-max for y
lines(principals, finals_simple, col=3, type="b")
legend("bottomright", pch = c(15,15), legend=c("compound", "simple"), col=c(2,3), bty = 'l')