
########### Bagging 
rm(list=ls())

data=read.csv("C:/Users/stats2/Desktop/heart.csv",header=T)
n=nrow(data)
s=sample(1:n,round(0.7*n)) # training indices
ts=seq(1,n)[-s]    # testing indices
y1=rep(0,91)
for(i in 1:10)
{
train=sample(s,length(s),replace=T)
data1=data[train,]
model=glm(target~.,family="binomial",data=data1)
y=predict(model,data[ts,-14],type="response")
y.pred=rep(0,91)
y.pred[y>.5]=1

print(summary(model))
accuracy=mean(y.pred==data[ts,14])
print(accuracy)
y1=y1+y.pred
}
#creating y*
y_star=rep(0,91)
y_star[y1>5]=1
#creatingconfusion matrix
table(y_star,data[ts,14])
#storingaccuracy
accuracy1=mean(y_star==data[ts,14])
accuracy1
