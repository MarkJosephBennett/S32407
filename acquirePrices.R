#details of this function: Financial Analytics with R Chapter 4 cambridge.org/FAR
acquirePrices <- function(prices,lab,len,D,D1,D2,dir,
                 start,end,isSubDir=TRUE,verbose=TRUE) {
  isSuccessfulQuote <- FALSE
  for(d in 1:D) {
    if(d == 1 || (isSubDir && d == (D1+1)))
      if(d == 1 && isSubDir) {
        setwd(paste(homeuser,"/FinAnalytics/",dir,"/NYSE",sep=""))
        unlink('bad*')
        print(paste("NYSE=======:",d))
      } else if(d == (D1+1) && isSubDir) {
        setwd(paste(homeuser,"/FinAnalytics/",dir,"/NASDAQ",sep=""))
        unlink('bad*')
        print(paste("NASDAQ=======:",d))
      } else {
        setwd(paste(homeuser,"/FinAnalytics/",dir,sep=""))
        unlink('bad*')
        print(paste("ETF==========:",d))
      }
    if(verbose) print(paste(d,lab[d]))
    fileName = paste("cached",lab[d],".csv",sep="")
    usingCacheThisFileName <- FALSE
    if(file.exists(fileName)) {
      usingCacheThisFileName <- TRUE
      pricesForStock <- read.csv(fileName,header=TRUE,sep="")[,1]
      if(!is.na(pricesForStock[1]))
        isSuccessfulQuote <- TRUE
    }
    if(!usingCacheThisFileName ||
         (usingCacheThisFileName && length(pricesForStock) != len)) {
      usingCacheThisFileName <- FALSE
      tryCatch( {
        #print(start);print(end)
        Sys.sleep(1)
        pricesForStock <- get.hist.quote(lab[d],quote="Adj",
                                         start=start,end=end)
        if(!is.na(pricesForStock[1]))
          isSuccessfulQuote <- TRUE
      }, error = function(err) {
        print(err);cat(lab[d],file="badsyms.txt",
                       append=TRUE,sep="\n")
        isSuccessfulQuote <- FALSE
      } )
    }
    if(length(pricesForStock) == len) {
      prices[,d] <- pricesForStock
      if(sum(is.na(prices[,d])) > 0 || (sum(is.na(prices[,d-1])) == 0 &&
            d > 1 && prices[1,d] == prices[1,d-1])) {
        print(paste(lab[d],"has NA prices"))
        cat(lab[d],file="badsyms.txt",
            append=TRUE,sep="\n")
        isSuccessfulQuote <- FALSE
      }
    } else {
      cat(lab[d],file="badsyms.txt",append=TRUE,sep="\n")
    }
    if(!isSuccessfulQuote)
      cat(lab[d],file="badsyms.txt",append=TRUE,sep="\n")
    if(isPlotInAdjCloses) {
      if(d == 1)
        plot(prices[,d]/prices[1,d],type="l",col="blue",ylim=c(.2,6))
      else
        lines(prices[,d]/prices[1,d],type="l",col="blue")
      text(len,(prices[len,d]/prices[1,d]),lab[d],cex=.6)
    }
    if(isCacheEnabled && !usingCacheThisFileName &&
         isSuccessfulQuote) {
      #save redundant re-write
      fileName = paste("cached",lab[d],".csv",sep="")
      print(fileName)
      write.csv(prices[,d],file=fileName,row.names = FALSE)
    }
    isSplitAdjusted = TRUE
  }
  prices
}
