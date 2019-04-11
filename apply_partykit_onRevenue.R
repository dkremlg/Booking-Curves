# library(partykit)

setwd('C:/Users/dkoch/Documents/Python Scripts/Booking Curves/Booking-Curves_git')

Data=read.csv('R_Training_Revenue.csv')
Data[Data[,'Coupons.YQ.on.current.bookings']<0,'Coupons.YQ.on.current.bookings']=0
Data[,'Coupons.YQ.on.current.bookings']=round(Data[,'Coupons.YQ.on.current.bookings'])

class(eq.mob <- Coupons.YQ.on.current.bookings ~ 1 + Dprio | dday + dtime + Direction + month)  

curve.model <- glmtree(
  eq.mob, 
  data = Data,
  family = poisson, 
  alpha = 0.25,
  bonferroni = TRUE,
  verbose = TRUE,
  prune = "BIC",
  minsize = 10,
  breakties = TRUE,
  restart = TRUE,
  maxdepth=5)

forecast_revenue=as.numeric(predict(curve.model,newdata=Data,type='response'))
forecast_node=as.numeric(predict(curve.model,newdata=Data,type='node'))
Data=cbind(Data,forecast_revenue,forecast_node)
write.csv(Data,'R_Output_Training_Revenue.csv',row.names=FALSE)

#######################################################

Forecast_Data=read.csv('R_Test_Revenue.csv')
forecast_revenue=as.numeric(predict(curve.model,newdata=Forecast_Data,type='response'))
forecast_node=as.numeric(predict(curve.model,newdata=Forecast_Data,type='node'))
Forecast_Data=cbind(Forecast_Data,forecast_revenue,forecast_node)
write.csv(Forecast_Data,'R_Output_Test_Revenue.csv',row.names=FALSE)