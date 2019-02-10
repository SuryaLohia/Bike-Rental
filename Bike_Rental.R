setwd("C:/Users/Surya/Desktop/Project_Ed/Bike_Rental_Count")
data=read.csv('day.csv')
data=data[-c(1,2)]
colnames(data)

boxplot(x=data$casual)
boxplot(x=data$registered)


hist(data$casual,xlim=c(0,3500),breaks=8)
hist(data$windspeed,breaks=4)
hist(data$registered,breaks=8)

data$casual=sqrt(data$casual)
data$registered=log2(data$registered)
data$windspeed=1/data$windspeed

hist(data$casual,breaks=8)
hist(data$windspeed,breaks=4)
hist(data$registered,breaks=8)

data1=data[-c(9,13,1)]
cnames=c('temp','hum','windspeed','casual')

data$temp=as.numeric(unlist(data$temp))
data$hum=as.numeric(unlist(data$hum))
data$windspeed=as.numeric(unlist(data$windspeed))
data$casual=as.numeric(unlist(data$casual))
for(i in cnames){
  data1[,i]=(data[,i]-mean(data[,i]))/sd(data[,i])
}

train_index=sample(1:nrow(data1),.8*nrow(data1))
train=data1[train_index,]
test=data1[-train_index,]

row.names(train)=NULL
row.names(test)=NULL

library(usdm)
library(DMwR)
vif(data1[,-11])
vifcor(data1[,-11],th=0.9)
lm_model=lm(cnt~.,data=train)
summary(lm_model)
pred_lm=predict(lm_model,test[,-11])
regr.eval(test[,11],pred_lm,stats = c("mae","mse",'mape','rmse'))

library(rpart)
reg_tree=rpart(cnt~.,train,method = 'anova')
pred_rt=predict(reg_tree,test[,-11])
regr.eval(test[,11],pred_rt,stats=c('mae','mse','mape','rmse'))

library(randomForest)
library(inTrees)
rf_model=randomForest(cnt~.,train,importance=TRUE,ntree=500)
treeList=RF2List(rf_model)
exec=extractRules(treeList,train[,-11])
read_rule=presentRules(exec,colnames(train))
ruleMatrix=getRuleMetric(exec,train[,-11],train$cnt)
ruleMatrix[1:2,]
rf_pred=predict(rf_model,test[,-11])
rf_pred
regr.eval(test[,11],rf_pred,stats=c('mae','mse','mape','rmse'))


