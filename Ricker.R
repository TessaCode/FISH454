# Ricker Model Demo

run.ricker<-function(alphal,K){
  tmax <- 100
  nstart<- 0.5
  years <- 0:tmax
  
  n <- rep(NA, times = length(years))
  n[1] <- nstart
  
  # Simulate trajectory
  s <- 1- alphal
  for (i in 1:tmax) n[i+1] <- n[i] * exp(alphal * (1-n[i] / K))
  
  
  #Create Ricker Plot
  nmax <- 2
  nt<- seq(0,nmax,0.01)
  
  ntplus1 <- nt * exp(alphal * (1 - nt/K))
  
  ## Make Plots
  par(mfrow=c(2,2),
      mar=c(4,4,2,2))

  
  plot(x=years,y=n,
       type='l',
       lwd = 2,
       xlim=c(0,50),
       ylim=c(0,2.0),
       col="black",
       xlab='Time',
       ylab='Abundance',
       main = paste0("slope (s) =", round(s, 2))
  )
  
  # repeat but zooming in near K
  plot(x=years,y=n,
       type='l',
       xlim=c(0,50),
       ylim=c(K-0.99*K,K + 1.01 * K),
       col="black",
       xlab='Time',
       ylab='Abundance',
       main = "zoom in near K"
  )
  
  # Make cobweb plots, all time points
  plot(x = nt, y = ntplus1,
       type = "l",
       lwd = 3,
       xlab = "Nt",
       ylab = "N t+1",
       ylim = c(0, 2 * K),
       xlim <- c(0, 2 * K),
       main = "cobweb"
  )
  
  # add replacement line
  abline(a = 0, b = 1,
         col = "gray",
         lwd = 3)
  
  # Create Cobweb
  # plot vertical lines
  for (i in 1:tmax){
    xs <- rep(x = n[i], times = 2)
    ys <- c(n[i], n[i+1])
    lines(xs, ys,
          lwd = 1
    )
  }
  
  # plot horizontal lines
  for (i in 1:tmax) {
    xs <- c(n[i], n[i+1])
    ys <- rep (x = n[i+1], times = 2)
    lines(xs, ys,
          lwd = 1
    )
  }
  
  
# make cobweb plot, remove first 50 points
  
  plot(x = nt, y = ntplus1,
       type = "l",
       lwd = 3,
       xlab = "Nt",
       ylab = "N t+1",
       ylim = c(0, 2 * K),
       xlim <- c(0, 2 * K),
       main = "Transient Dynamics Removed"
  )

  # add replacement line
  abline(a = 0, b = 1,
         col = "gray",
         lwd = 3)
  
  # Create Cobweb
  # plot vertical lines
  for (i in 50:tmax){
    xs <- rep(x = n[i], times = 2)
    ys <- c(n[i], n[i+1])
    lines(xs, ys,
          lwd = 1
    )
  }
  
  # plot horizontal lines
  for (i in 50:tmax) {
    xs <- c(n[i], n[i+1])
    ys <- rep (x = n[i+1], times = 2)
    lines(xs, ys,
          lwd = 1
    )
  }
  
  par(mfrow=c(1,1))
}

library(manipulate)
manipulate(run.ricker(alphal,K),
           alphal=slider(0.5,3.0,initial=0.8),
           K=slider(0.5,2,initial=1)
)

