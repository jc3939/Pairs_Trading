rm(list = ls(all = TRUE))

library(quantmod)
library(tseries)

library(timeDate)
library(fUnitRoots)

symbols<-read.csv("S&P500.csv", header=F)

nrStocks = length(symbols[,1])

z <- zoo()
for (i in 1:nrStocks) {
    
       cat("Downloading ", i, " out of ", nrStocks , "\n")
   	x <- get.hist.quote(instrument = symbols[i,], start = as.Date("2003-01-01"), quote = "AdjClose", retclass = "zoo", quiet = T)
   	z <- merge(z, x)
   }

beta <- matrix(data = NA, ncol = nrStocks, nrow = nrStocks)
ht <- matrix(data = NA, ncol = nrStocks, nrow = nrStocks)
sprd <- list()
z_old <- z;
nDays <- length(z[,1])
# seting learning and testing periods
#testPeriod <- 0
learningPeriod <- 241
#testDates <- (nDays-testPeriod):nDays
learningDates <- learningPeriod:(learningPeriod+720-1)
data <- z		
z <- z[learningDates,]
#zTest <- data[testDates,]

# here we go! let's find the cointegrated pairs
for (j in 1:(nrStocks-1)) {
   	for (i in (j+1):nrStocks) {
      
       		cat("Calculating ", j, " - ", i , "\n")
     		if (length(na.omit(z[, i])) == 0 || length(na.omit(z[, j])) == 0) {
       			beta[j,i] <- NA
       			ht[j,i] <- NA
       			next
       		}
      
        
       		m <- lm(z[, j] ~ z[, i] + 0)
     		beta[j,i] <- coef(m)[1]
      
       		sprd <- resid(m)
      
       		ht[j,i] <- adfTest(na.omit(coredata(sprd)), type="nc")@test$p.value
      
       	}
   }

# prepare variables
zscore <- 0;
rscore <- matrix(data = NA, ncol = 4, nrow = (nrStocks^2)/2)
pairSummary <- matrix(data = NA, ncol = 6, nrow = (nrStocks^2)/2)

ii <- 1;

# lets evaluate the spreads
for (j in 1:(nrStocks-1)) {
  for (i in (j+1):nrStocks) {
    
    # if no data, skip
    if (is.na(ht[j, i])) {
      next
    }
    
    # is spread stationary (i.e. pair is co-integrated)
    if (ht[j, i] < 0.05) {
      
      sprd <- z[,j] - beta[j, i]*z[,i]
      sprd <- na.omit(sprd)
      
      # calculate z-score
      zscore <- sum(abs(scale((sprd))))/length(sprd)
      rscore[ii, 3] <- sd((sprd))
      rscore[ii, 4] <- zscore
      rscore[ii, 1] <- j
      rscore[ii, 2] <- i
      
      pairSummary[ii, ] = summary(coredata(sprd))[1:6]
      ii <- ii + 1
    }
  }
  
  cat("Calculating ", j, "\n")
}

# set up boundaries for 1st and 3rd quartiles
badSprd_up <- 1.5
badSprd_down <- -1.5 
# re-order spreads
rscore <- na.remove(rscore)
pairSummary <- na.remove(pairSummary)

order_id <- order((rscore[,3]), decreasing = T)
rscore <- rscore[order_id,]
pairSummary <- pairSummary[order_id,] 

goodSprd_id <- (pairSummary[, 2] >  badSprd_down) & (pairSummary[, 5] <  badSprd_up)

rscore <- rscore[goodSprd_id, ]
pairSummary <- pairSummary[goodSprd_id, ]

