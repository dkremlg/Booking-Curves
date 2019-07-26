poisson_reg <- function(route){

library(partykit)

path='/home/ubuntu/Data/'

Data=read.csv(paste0(path,'Intermediate_Output/R_Training_Pax.csv'))
Data[Data[,'NumPax']<0,'NumPax']=0

class(eq.mob <- NumPax ~ 1 + Dprio | dday + dtime)  

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

#######################################################

cluster=predict(curve.model,newdata=Data,type='node')

cluster=cbind(Data,cluster)
cluster[,'cluster']=as.character(cluster[,'cluster'])
cluster=cluster[,c('dday','dtime','cluster')]
cluster=cluster[!duplicated(cluster),]

coefficients=coef(curve.model)
coefficients=cbind(rownames(coefficients),coefficients)
colnames(coefficients)[1]='cluster'
coefficients=merge(coefficients,cluster,by='cluster',all=TRUE)
coefficients=coefficients[,c(colnames(coefficients)[is.na(match(colnames(coefficients),c('(Intercept)','Dprio')))],colnames(coefficients)[!is.na(match(colnames(coefficients),c('(Intercept)','Dprio')))])]
coefficients=coefficients[,colnames(coefficients)!='cluster']

write.csv(coefficients,paste0(path,'Intermediate_Output/Coeff_',route,'.csv'),row.names=FALSE)

}
