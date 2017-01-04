require(quantmod)
require(techchart)
require(beepr)
require(quantstrat)

################# order sizing function ################################
osFixedDollar <- function(timestamp,orderqty, portfolio, symbol, 
                            ruletype, tradeSize, ...){
  px <- as.numeric(Cl(mktdata[timestamp,]))
  orderqty <- round(tradeSize/px,2)*sign(orderqty)
  return(orderqty)
}

################## download data and set up pattern search #############
x <- getSymbols("^GSPC", auto.assign = F)
n <- NROW(x)
k <- 200; l <- 1; carry <- 5

################## run the HS search ##########################
pattern.matches <- list()

for(i in k:n){
  y <- x[(i-k+1):i]
  tpattern <- find.pattern(y, pip.tolerance = c(1.5))
  if(!is.null(tpattern)){
    print(paste("pattern found on",as.Date(index(x[i]))))
    pattern.matches[[l]] <- tpattern
    l <- l+1
  }
}
beep()

l <- NROW(pattern.matches)
sig <- as.xts(rep(0,n),as.Date(index(x)))
colnames(sig) <- c("sig")

for(i in 1:l){
  match_date <- pattern.matches[[i]]$matches[[1]]$date
  match_idx <- match(match_date,index(sig))
  sig[match_idx] <- 1
}

exit_sig <- as.xts(rep(1,n),as.Date(index(x)))
colnames(exit_sig) <- c("exit")

for(i in 1:l){
  match_date <- pattern.matches[[i]]$matches[[1]]$date
  match_idx <- match(match_date,index(sig))
  exit_idx <- match_idx+carry
  exit_sig[match_idx:exit_idx] <- 0
  #if(sum(sig[(exit_idx-carry+1):exit_idx]) >0){
  #  exit_sig[exit_idx] <- 0
  #}
}

sigs <- merge(sig, exit_sig)
x <- merge(OHLC(x),sigs)

#################### run the IHS search #######################
l <- 1; carry <- 5
# run the IHS search
pattern.matches <- list()

for(i in k:n){
  y <- x[(i-k+1):i]
  tpattern <- find.pattern(y,pattern = pattern.db("IHS")[[1]], 
                           pip.tolerance = c(1.5))
  if(!is.null(tpattern)){
    print(paste("pattern found on",as.Date(index(x[i]))))
    pattern.matches[[l]] <- tpattern
    l <- l+1
  }
}
beep()

l <- NROW(pattern.matches)
sig <- as.xts(rep(0,n),as.Date(index(x)))
colnames(sig) <- c("sig1")

for(i in 1:l){
  match_date <- pattern.matches[[i]]$matches[[1]]$date
  match_idx <- match(match_date,index(sig))
  sig[match_idx] <- 1
}

exit_sig <- as.xts(rep(1,n),as.Date(index(x)))
colnames(exit_sig) <- c("exit1")

for(i in 1:l){
  match_date <- pattern.matches[[i]]$matches[[1]]$date
  match_idx <- match(match_date,index(sig))
  exit_idx <- match_idx+carry
  exit_sig[match_idx:exit_idx] <- 0
  #if(sum(sig[(exit_idx-carry+1):exit_idx]) > 0){
  #  exit_sig[exit_idx] <- 0
  #}
}
sigs <- merge(sig, exit_sig)
x <- merge(x,sigs)

########################### set up the back-tester ######################

# clean-up
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 
suppressWarnings(rm(list = ls(envir = .blotter), envir = .blotter))
suppressWarnings(rm(list = ls(envir = .strategy), envir = .strategy))

# instrument setup
underlying <- "x"
currency("INR")
stock(underlying, currency = "INR", multiplier = 1)

# required quantstrat variables
initDate <- index(x)[1]
initEq <- 1e5 
.txnfees = -0
.orderqty = 100
.tradeSize <- initEq
.maxSize <- 100000
.slow = 100
.fast = 50
short.threshold <-0.5
long.threshold <- 0.5
qs.account <- "pattern.act"
qs.portfolio <- "pattern.port"
qs.strategy <- "pattern.strat"

# initialize quantstrat
initPortf(name = qs.portfolio, symbols = underlying, initDate = initDate, 
          currency='INR')
initOrders(portfolio = qs.portfolio, initDate = initDate)
initAcct(name = qs.account, portfolios = qs.portfolio, initDate = initDate, 
         initEq = initEq, currency='INR')
#addPosLimit(portfolio=qs.portfolio, symbol=underlying, timestamp=initDate, 
#            maxpos=.orderqty)

# define strategy
strategy(name = qs.strategy, store = TRUE)

# add indicators
add.indicator(qs.strategy, name="SMA",
              arguments=list(x=quote(Cl(mktdata)[,1]), n=.fast),
              label="fstMA")
add.indicator(qs.strategy, name="SMA",
              arguments=list(x=quote(Cl(mktdata)[,1]), n=.slow),
              label="slwMA")

# add signals
# long signal from patterns
add.signal(strategy=qs.strategy, name="sigThreshold",
           arguments=list(column="sig1", threshold=short.threshold,
                          relationship="gt", cross=TRUE),
           label="longpattern")
# short signal from patterns
add.signal(strategy=qs.strategy, name="sigThreshold",
           arguments=list(column="sig", threshold=short.threshold,
                          relationship="gt", cross=TRUE),
           label="shortpattern")

# timed out exit from long and short positions
add.signal(strategy=qs.strategy, name="sigThreshold",
           arguments=list(column="exit1", threshold=0,
                          relationship="gt", cross=FALSE),
           label="timedoutlong")

add.signal(strategy=qs.strategy, name="sigThreshold",
           arguments=list(column="exit", threshold=0,
                          relationship="gt", cross=FALSE),
           label="timedoutshort")

# momentum signals
add.signal(strategy=qs.strategy, name="sigComparison",
           arguments=list(column=c("slwMA","fstMA"),
                          relationship="gt", cross=FALSE),
           label="momshort")

add.signal(strategy=qs.strategy, name="sigComparison",
           arguments=list(column=c("slwMA","fstMA"),
                          relationship="lt", cross=FALSE),
           label="momlong")

# entry to long and short positions
add.signal(strategy=qs.strategy, name="sigAND",
           arguments=list(column=c("longpattern","longpattern"), cross=TRUE),
           label="longEntry")

add.signal(strategy=qs.strategy, name="sigAND",
           arguments=list(column=c("shortpattern","shortpattern"), 
                          cross=TRUE),
           label="shortEntry")

# exit from long and short positions
add.signal(strategy=qs.strategy, name="sigAND",
           arguments=list(column=c("timedoutlong","momshort"), cross=TRUE),
           label="longExit")

add.signal(strategy=qs.strategy, name="sigAND",
           arguments=list(column=c("timedoutshort","momlong"), 
                          cross=TRUE),
           label="shortExit")

# add long entry rule
add.rule(strategy=qs.strategy, name='ruleSignal',
         arguments=list(sigcol='longEntry' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty= .orderqty,
                        #tradeSize=.tradeSize,
                        #maxSize = .maxSize,
                        #osFUN=osMaxPos,
                        osFUN="osFixedDollar", tradeSize=.tradeSize,
                        orderset='ocolong',
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)
# add short entry rule
add.rule(strategy=qs.strategy, name='ruleSignal',
         arguments=list(sigcol='shortEntry' , sigval=TRUE,
                        orderside='short' ,
                        ordertype='market',
                        orderqty= -.orderqty,
                        #tradeSize=.tradeSize,
                        #maxSize = .maxSize,
                        #osFUN=osMaxPos,
                        osFUN="osFixedDollar", tradeSize=.tradeSize,
                        orderset='ocoshort',
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)
# add short exit rule
add.rule(strategy=qs.strategy, name='ruleSignal',
         arguments=list(sigcol='longExit', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        orderset='ocolong',
                        replace=FALSE
         ),
         type='exit',
         label='longExit'
)
# add short exit rule
add.rule(strategy=qs.strategy, name='ruleSignal',
         arguments=list(sigcol='shortExit', sigval=TRUE,
                        orderside='short' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        orderset='ocoshort',
                        replace=FALSE
         ),
         type='exit',
         label='shortExit'
)

# run back-test
applyStrategy(strategy = qs.strategy, portfolios = qs.portfolio)
updatePortf(Portfolio = qs.portfolio)
updateAcct(qs.account)
updateEndEq(qs.account)

# get trading data
book    = getOrderBook(qs.portfolio)
stats   = tradeStats(qs.portfolio, use = "trades", inclZeroDays = TRUE)
ptstats = perTradeStats(qs.portfolio)
txns    = getTxns(qs.portfolio, underlying)

# analyze performance
equity.curve <- getAccount(qs.account)$summary$End.Eq
daily.returns <- Return.calculate(equity.curve$End.Eq, "discrete")
names(daily.returns) <- "return"
#hist(daily.returns)

# get annualized summary
table.AnnualizedReturns(daily.returns, scale = 252) 

# chart performance
charts.PerformanceSummary(daily.returns, main = "Pattern Trading Performance")

# get some summary trade statistics
stats[,c("Symbol", "Num.Trades", "Percent.Positive", "Net.Trading.PL",
         "Profit.Factor", "Max.Drawdown")]

# get table of monthly returns
monthly.returns <-  Return.calculate(to.monthly(equity.curve)[, 4],"discrete")
names(monthly.returns) <- "Total"
table.CalendarReturns(monthly.returns)

# more diagnositics
chart.Posn(qs.portfolio, underlying, 
           TA='add_BBands(n=20);add_SMA(n=10);add_SMA(n=50)')
View(t(stats))
View(ptstats)
