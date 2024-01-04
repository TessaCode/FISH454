install.packages("tidyverse")
library(tidyverse)

#exercises
a <- 3
b <- 2
a + b
cats <- c(1, 2, 3, 4)
cats2 <- seq(1, 4, by = 1)
cats3 <- 1:4

cats3[3]
cats3[3] <- 45

#practice loops
n.2.count <- 30
count <- rep(NA, n.2.count)
count[1] <- 0

for(i in 2:n.2.count){
  count[i] <- count[i-1] + 1
}

# practicum
r <- 0.15
k <- 100
years <- 20
nt + r*nt*(1-(nt/k)) #this is the formula given

wolf_pop <- rep(NA, years) #open vector with 20 places
wolf_pop[1] <- 25 #designate year 1
for(i in 2:years){

  wolf_pop[i] <- wolf_pop[i-1] + r*wolf_pop[i-1]*(1-(wolf_pop[i-1]/k))  
  
}

wolves <- data.frame(years = seq(1, 20, 1), 
                     pop = wolf_pop)
wolves %>%
  ggplot(aes(x = years, y = pop)) + geom_point() + theme_bw() + ylab("wa wolf population")



nt.minus.one <- nt[i-1]