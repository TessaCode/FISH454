#lab group 6
library(tidyverse)

# Step 1: program the Gompertz model

#given parameters
K <- 1
a <- 0.5
n0 <- 0.1
nyears <- 50

#gompertz function
gompertz <- function(K, a, nt) {
  ntplus1 <- nt + a * nt * log(K / nt)
  return(ntplus1)
}

# initialize an empty array, call it nt, to hold the model results
nt <- rep(NA, nyears)
# set the first element of nt equal to N0
nt[1] <- n0


# now loop through 2 to nyears
for (i in 2 : nyears) {
  # call the function to get nt+1
  nt[i] <- gompertz( K = K,
                     a = a,
                     nt = nt[i - 1] # use the i-1 element of nt array to send to function
  )
}

# make a plot

plot(1:nyears,
     nt,
     type = "l",
     col = "blue",
     xlab = "time",
     ylab = "Nt")

#Adjust values of the parameter to determine whether you can find values that give
#rise to the same types of dynamic behavior that you discovered for the logistic model.

#create loop from function
gompertzloop <- function(K, a, nt, nyears) {
  for (i in 2:nyears) {
    # call the function to get nt+1
    nt[i] <- gompertz(K = K,
                      a = a,
                      nt = nt[i - 1])
  }

  plot <- plot(1:nyears,
       nt,
       type = "l",
       col = "blue",
       xlab = "time",
       ylab = "Nt")

  print(plot)

}

# explerimenting with levels of a

gompertzloop(K = 1, a = 1.75, nt = 0.1, nyears = 50) #dampened oscillations around carrying capacity

gompertzloop(K = 1, a = 2.0, nt = 0.1, nyears = 50) #this reaches 2 point limit cycle

gompertzloop(K = 1, a = 3, nt = 0.1, nyears = 50) #this causes the population to crash

gompertzloop(K = 1, a = 2.6, nt = 0.1, nyears = 50) #potentially the infinite point cycle
