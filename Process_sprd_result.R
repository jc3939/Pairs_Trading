library(zoo)

zooToDf <- function(z) {
  df <- as.data.frame(z) 
  df$Date <- time(z) #create a Date column
  rownames(df) <- NULL #so row names not filled with dates
  df <- df[,c(ncol(df), 1:(ncol(df)-1))] #reorder columns so Date first
  return(df)
}

z_table=zooToDf(z)

colnames(z_table)=c('Date',seq(1:500))

row_number=nrow(rscore)
#quantiles=matrix('NA',row_number,2)
sprd_result=matrix(NA,1,4)
for (i in 1:row_number){
  cat(i,'\n')
  A=rscore[i,1]
  B=rscore[i,2]
  sprd=z_table[,as.character(A)]-beta[A,B]*z_table[,as.character(B)]
  sprd=cbind(sprd,z_table[,1])
  A=rep(A,nrow(sprd))
  B=rep(B,nrow(sprd))
  sprd=cbind(A,B,sprd)
  sprd=na.omit(sprd)
  sprd_result=rbind(sprd_result,sprd)
}

sprd_result=sprd_result[-1,]