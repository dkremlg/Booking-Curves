# library(partykit)

Data=read.csv('R_Training_Pax.csv')
Data[Data[,'NumPax']<0,'NumPax']=0

class(eq.mob <- NumPax ~ 1 + Dprio | dday + dtime + Direction + month)  

curve.model <- glmtree(
  eq.mob, 
  data = Data,
  family = poisson, 
  alpha = 0.01,
  bonferroni = TRUE,
  verbose = TRUE,
  prune = "BIC",
  minsize = 10,
  breakties = TRUE,
  restart = TRUE,
  maxdepth=5)

forecast_bookings=as.numeric(predict(curve.model,newdata=Data,type='response'))
forecast_node=as.numeric(predict(curve.model,newdata=Data,type='node'))
Data=cbind(Data,forecast_bookings,forecast_node)
write.csv(Data,'R_Output_Training_Pax.csv',row.names=FALSE)

#######################################################

Forecast_Data=read.csv('R_Test_Pax.csv')
forecast_bookings=as.numeric(predict(curve.model,newdata=Forecast_Data,type='response'))
forecast_node=as.numeric(predict(curve.model,newdata=Forecast_Data,type='node'))
Forecast_Data=cbind(Forecast_Data,forecast_bookings,forecast_node)
write.csv(Forecast_Data,'R_Output_Test_Pax.csv',row.names=FALSE)