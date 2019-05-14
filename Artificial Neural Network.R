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
