require(RSelenium)
require(XML)

clean.futures.table <- function(tbl){
  if(length(grep("expiry",tolower(colnames(tbl)))) > 0){
    cols <- colnames(tbl)
  } else{
    cols <- apply(tbl[1,],1,FUN=function(x) as.character(x))
    tbl <- tbl[-1,]
  }
  
  n <- NROW(tbl)
  futures <- data.frame(forward=rep(0,n),expiry=rep(0,n))
  futures$forward <- as.numeric(gsub(",","",tbl[,grep("last",tolower(cols))]))
  futures$expiry <- as.Date(tbl[,grep("expiry",tolower(cols))],"%d%b%Y")

  return(futures)
}
clean.options.table <- function(tbl, futures, series){
  tbl <- na.omit(tbl)
  tbl <- tbl[!sapply(tbl, function(x) all(x == ""))]
  
  if(is.numeric(grep("oi",tolower(colnames(tbl))))){
    cols <- colnames(tbl)
  } else{
    cols <- apply(tbl[1,],1,FUN=function(x) as.character(x))
    tbl <- tbl[-1,]
  }
  
  tbl <- apply(tbl, 2, gsub, pattern=",", replacement="")
  tbl <- apply(tbl,2, gsub, pattern="-", replacement="0")
  tbl <- apply(tbl,2,FUN = function(x)return(as.numeric(as.character(x))))
  tbl <- data.frame(tbl); colnames(tbl) <- cols
  
  split_idx <- grep("strike",tolower(colnames(tbl)))
  call_tbl <- tbl[,c(1:split_idx)]
  put_tbl <- tbl[,c(split_idx:NCOL(tbl))]
  call_tbl[call_tbl==0] <- NA; put_tbl[put_tbl==0] <- NA
  call_tbl <- na.omit(call_tbl); put_tbl <- na.omit(put_tbl)
  
  forward <- futures$forward[series]
  call_tbl <- call_tbl[call_tbl[,grep("strike",tolower(colnames(call_tbl)))]>forward,]
  put_tbl <- put_tbl[put_tbl[,grep("strike",tolower(colnames(put_tbl)))]<forward,]
  
  call_tbl <- call_tbl[call_tbl[,grep("strike",tolower(colnames(call_tbl)))]<forward*1.05,]
  put_tbl <- put_tbl[put_tbl[,grep("strike",tolower(colnames(put_tbl)))]>forward*0.95,]
  
  call_tbl <- call_tbl[call_tbl[,grep("ltp",tolower(colnames(call_tbl)))]>5,]
  put_tbl <- put_tbl[put_tbl[,grep("ltp",tolower(colnames(put_tbl)))]>5,]
  
  nc <- NROW(call_tbl); np <- NROW(put_tbl)
  types <- c(rep("put",np),rep("call",nc))
  strikes <- c(put_tbl[,grep("strike",tolower(colnames(put_tbl)))],
               call_tbl[,grep("strike",tolower(colnames(call_tbl)))])
  prices <- c(put_tbl[,grep("ltp",tolower(colnames(put_tbl)))],
               call_tbl[,grep("ltp",tolower(colnames(call_tbl)))])
  options <- data.frame(strike=strikes,price=prices,type=types)
  options$forward <- forward; options$expiry <- futures$expiry[series]
  return(options)
}

##### set up the selemium web driver ################################
# use docker to launch selenium server
# docker run -d -P selenium/standalone-firefox
# run docker ps and docker-machine ip to note the ip and port
# use the actual port instead of the exposed port from the container
remDr <- remoteDriver(remoteServerAddr="192.168.99.100",port = 32770L, 
                      browserName="firefox")
remDr$open()

#diagnostics
remDr$getStatus()

##### download the current 3 futures data ###########################
url <- "http://www.nseindia.com/live_market/dynaContent/live_watch/fomwatchsymbol.jsp?key=NIFTY&Fut_Opt=Futures"
remDr$navigate(url)
x <- remDr$getPageSource()
y <- htmlParse(x[[1]], asText=T)
tbl <- data.frame(readHTMLTable(y, which = 2))
futures <- clean.futures.table(tbl)

####### download the options data ####################################
series <- 1
base.url <- "http://www.nseindia.com/live_market/dynaContent/live_watch/option_chain/optionKeys.jsp?segmentLink=17&instrument=OPTIDX&symbol=NIFTY&date="
chain <- toupper(as.character(futures$expiry[series],"%d%b%Y"))
url <- paste(base.url,chain,sep="")
remDr$navigate(url)
x <- remDr$getPageSource()
y <- htmlParse(x[[1]], asText=T)
tbl <- data.frame(readHTMLTable(y, which = 3))
options <- clean.options.table(tbl, futures, series)

