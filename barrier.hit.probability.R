require(quantmod)

getHitProbability <- function(spot, strike, drift, vol, time=1/260){
  m <- drift/vol
  term1 <- log(strike/spot)/sqrt(vol^2*time) - (m-0.5*vol)*sqrt(time)
  term2 <- 2*(m-0.5*vol)*log(strike/spot)/vol
  term3 <- log(spot/strike)/sqrt(vol^2*time) - (m-0.5*vol)*sqrt(time)
  ret <- pnorm(term1) - exp(term2)*pnorm(term3)
  return(1-ret)
}

getBinaryProbability <- function(spot, strike, drift, vol, time=1/260){
  d1 <- (log(spot/strike) + (drift+0.5*(vol^2))*time)/(vol*sqrt(time))
  d2 <- d1 - vol*sqrt(time)
  ret <- pnorm(d2)
  return(ret)
}

vols <- seq(0.05,0.3,0.01)
sharpes <- seq(0,1.5,0.1)
spot <- 8500
time <- 1/260
#strike <- spot*1.0025
#stops <- spot*0.99
#upside <- strike - spot
#downside <- spot - stops
probs <- matrix(0,length(vols),length(sharpes))
stop.probs <- matrix(0,length(vols),length(sharpes))
binary.probs <- matrix(0,length(vols),length(sharpes))
pnls <- matrix(0,length(vols),length(sharpes))
strikes <- matrix(0,length(vols),length(sharpes))
stops <- matrix(0,length(vols),length(sharpes))
upsides <- matrix(0,length(vols),length(sharpes))
downsides <- matrix(0,length(vols),length(sharpes))
binary.ratio <- matrix(0,length(vols),length(sharpes))

for (i in 1:length(vols)){
  for (j in 1:length(sharpes)){
    vol <- vols[i]
    drift <- sharpes[j]*vol*sqrt(1/time)
    strikes[i,j] <- spot + vol*sqrt(time)*spot
    stops[i,j] <- spot - 2*vol*sqrt(time)*spot
    probs[i,j] <- getHitProbability(spot,strikes[i,j],drift,vol,time)
    stop.probs[i,j] <- -(1 - getHitProbability(spot,stops[i,j],drift,vol,time))
    binary.probs[i,j] <- getBinaryProbability(spot,strikes[i,j],drift,vol,time)
    binary.ratio[i,j] <- probs[i,j]/binary.probs[i,j]
    upsides[i,j] <- strikes[i,j] - spot
    downsides[i,j] <- spot - stops[i,j]
    pnls[i,j] <- probs[i,j]*upsides[i,j] - stop.probs[i,j]*downsides[i,j]
  }
}

persp(vols, sharpes, probs, phi = 25, theta = 40, expand=0.25,
      col = "lightblue",ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "volatility (annualized %)", ylab = "Sharpe Ratio (daily)",
      main = "Hit Probability"
)

persp(vols, sharpes, binary.ratio, phi = 25, theta = 40, expand=0.25,
      col = "lightblue",ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "volatility (annualized %)", ylab = "Sharpe Ratio (daily)",
      main = "Hit Probability to Digital Probability Ratio"
)

persp(vols, sharpes, pnls, phi = 25, theta = 40, expand=0.25,
      col = "lightblue",ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "volatility (annualized %)", ylab = "Sharpe Ratio (daily)",
      main = "PnL Distribution"
)

plot(sharpes,pnls[26,],type="n",ylab="PnLs", xlab="Sharpe ratios", main="PnL Profiles for Different Volatilites")
lines(sharpes,pnls[1,])
text(x=0.1,y=pnls[1,1],"5% vol",cex=0.75)
lines(sharpes,pnls[6,])
text(x=0.1,y=pnls[6,1],"10% vol",cex=0.75)
lines(sharpes,pnls[11,])
text(x=0.1,y=pnls[11,1],"15% vol",cex=0.75)
lines(sharpes,pnls[16,])
text(x=0.1,y=pnls[16,1],"20% vol",cex=0.75)
lines(sharpes,pnls[21,])
text(x=0.1,y=pnls[21,1],"25% vol",cex=0.75)
lines(sharpes,pnls[26,])
text(x=0.1,y=pnls[26,1],"30% vol",cex=0.75)
lines(sharpes,rep(0,length(sharpes)))

all.pnls <- as.vector(pnls)
min.pnl <- floor(min(all.pnls)/5)*5
max.pnl <- ceiling(max(all.pnls)/5)*5
steps <- (max.pnl - min.pnl)/20
breaks <- seq(min.pnl,max.pnl,steps)
hist(all.pnls,breaks, col="lightgrey", freq=F,xlab="PnLs",
     ylab="Probabilities", cex = 0.75,
     main="Histogram of Daily PnLs"
     )
mean(all.pnls)
quantile(all.pnls,c(0.1,0.25,0.5,0.75,0.9))
abs(mean(all.pnls)/quantile(all.pnls,0.1))
