# ANN_classifier

rm(list=ls())
install.packages('ISLR')
library(ISLR)
data=read.csv("C:/Users/stats2/Desktop/heart.csv",header=T)
head(data,2)
# Create Vector of Column Max and Min Values
maxs <- apply(data[,1:14], 2, max)
mins <- apply(data[,1:14], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(data[,1:14],center = mins, scale = maxs - mins))

# Check out results
print(head(scaled.data,2))

data$target

library(caTools)
set.seed(101)

# Create Split (any column is fine)
split = sample.split(data$target, SplitRatio = 0.70)

# Split based off of split Boolean Vector
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)
dim(train)
dim(test)
length(which(split == FALSE))

feats <- names(scaled.data)
feats <- feats[-14]

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('target ~',f)

# Convert to formula
f <- as.formula(f)
f

#install.packages('neuralnet')
library(neuralnet)
nn <- neuralnet(f,train,hidden=2,linear.output=FALSE)
plot(nn)

# Compute Predictions off Test Set
predicted.nn.values <- compute(nn,test[1:14])

# Check out net.result
print(head(predicted.nn.values$net.result))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)


table(Actual=test$target,predicted=predicted.nn.values$net.result)































##############################
###Logistic Classifier###

rm(list=ls())
###Different libraries required:-
install.packages("ggcorrplot")        #For data visualization.
require(ggcorrplot)
library(caTools)                      #To split the data in to training and testing part.
require(caTools)
install.packages("ROCR")              #To determine the threshold.
require(ROCR)

d=read.csv("C:/Users/stats2/Desktop/heart.csv",header=T);d
str(d)
summary(d)
d_cor=round(cor(d[,c(1,4,5,8,10)]));d_cor       #corrletion matrix
g=ggcorrplot(d_cor);g


###To split the data set into training and testing data:-
data=d
sample=sample.split(data$Dataset,SplitRatio=0.70);sample
train_data=subset(data,sample==TRUE)
test_data=data.frame(subset(data,sample==FALSE))
attach(train_data)
model=glm(Dataset~Age+Gender+Total_Bilirubin+Direct_Bilirubin+Alkaline_Phosphotase+Alamine_Aminotransferase+Aspartate_Aminotransferase+Total_Protiens+Albumin+Albumin_and_Globulin_Ratio,family=binomial(link='logit'),data=train_data)
model
summary(model)
attach(test_data)
y1=predict(model,test_data[,-11],type="response");y1     #Predicted probabilities
y.pred=as.vector(ifelse(y1>0.2,1,0))
table(y.pred,test_data[,11])
mean(y.pred==test_data$Dataset)*100


##Threshold
threshold1= function(y1, Dataset){
perf=ROCR::performance(ROCR::prediction(y1, Dataset),"sens","spec")
df=data.frame(cut=perf@alpha.values[[1]],sens=perf@x.values[[1]],spec=perf@y.values[[1]])
df[which.max(df$sens+df$spec),"cut"]
}
y1=as.vector(y1)
y2=as.vector(test_data[,11])
threshold1(y1,y2)

































roc.perf = performance(y1, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)



































































































