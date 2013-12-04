write.table(rscore,'rscore.csv',sep=',')

nrows=nrow(rscore)
betalist=matrix(NA,nrows,1)
for (i in 1:nrows){
  A=rscore[i,1]
  B=rscore[i,2]
  betalist[i,1]=beta[A,B]
}

quantiles=matrix(NA,nrows,2)
for (i in 1:nrows){
  cat(i,'\n')
  quantiles[i,]=pairSummary[i,c(2,5)]
}


result=cbind(rscore[,1],rscore[,2],rscore[,3],betalist,quantiles)

write.table(result,'result.csv',sep=',')

nrows=nrow(rscore)
result=cbind(rscore,matrix(NA,nrows,1),matrix(NA,nrows,1),matrix(NA,nrows,1),matrix(NA,nrows,1))


for (i in 1:nrows){
  cat(i,'\n')
  A=rscore[i,1]
  B=rscore[i,2]
  index=sprd_result[,1]==A &sprd_result[,2]==B
  tmp=sprd_result[index,]
  count=nrow(tmp)
  start=tmp[1,4]
  end=tmp[count,4]
  sprd_mean=mean(tmp[,3])
  result[i,c(8,9,10,11)]=c(count,start,end,sprd_mean)
  
}
colnames(result)=c('symbolA','symbolB','SD','Beta',
                   'Sector','1st_quantile','3rd_quantile',
                   'sample_size','start_date','end_date','sprd_mean')

write.table(sprd_result,'sprd.csv',sep=',')
result=cbind(result,Half_life)