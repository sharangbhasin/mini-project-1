#remove all objects from R
rm(list=ls())

#set current working directory
setwd("E:/R/CaseStudyOnWines/CaseStudyOnWines")

#get current working directory
getwd()

list.files()

#load data into R
data1 = read.csv("winequality-red.csv", header=T)
data2 = read.csv("winequality-white.csv", header=T)
data1[, 'color'] = 'red'
data2[, 'color'] = 'white'

# Load packages
library('ggplot2')
library('ggthemes')
library('scales') 
library('dplyr') 
library('randomForest')
library("outliers")
library("caret")


#combinning the the two data set
data=rbind(data1,data2)

#Exploratory data Analysis
#understand the data type
str(data)

#Unique values in a column
#Count of unique values in a column
unique(data$quality)
length(unique(data$quality))
range(data$quality)


#binning quality
data$qualitycat[data$quality <=4] = "BAD"
data$qualitycat[data$quality>4 & data$quality <=6] = "NORMAL"
data$qualitycat[data$quality >6 ] = "GOOD"
#Distribution of unique values in a column
table(data$qualitycat)

#Convert into proper data types
data$color = as.factor(data$color)
data$qualitycat = as.factor(data$qualitycat)
data$quality= NULL


#Look at the block of data
head(data, 10)
tail(data, 10)


summary(data)

list(colnames(data))

#Normalized Data
library(clusterSim)
for(i in c(1:12))(
  data[,i] = data.Normalization(data[,i], type = "n4", normalization = "column"))

#Missing Analysis
sum(is.na(data))
#Analyze missing values by varaible. You can omit variabl;e which have morethan 50% missing values
apply(data,2, function(x)sum(is.na(x)))


#OUTLIER
for(i in 1:ncol(data))
{ if(class(data[,i]) == "numeric"){ outlier_tf = outlier(data[,i],logical=TRUE) 
find_outlier = which(outlier_tf==TRUE, arr.ind=TRUE) 
data = data[-find_outlier,] } 
  else {data = data} }


train = data[sample(nrow(data), 4550, replace = F), ]
test = data[!(1:nrow(data)) %in% as.numeric(row.names(train)), ]



#correlation matrix
library(corrplot)
numericColumns = !colnames(train) %in% c('quality', 'color')
correlationMatrix = cor(train[, 1:11])
highlyCorrelated = findCorrelation(correlationMatrix, cutoff = 0.6)
colnames(correlationMatrix)[highlyCorrelated]
corrplot(correlationMatrix, method = 'number', tl.cex = 0.5)

#remove highly correlated variables from the data set
data$density= NULL
data$total.sulfur.dioxide= NULL


#decision tree 
library(C50)
ruleModel = C5.0(qualitycat ~ ., data = train, rules = TRUE)
summary(ruleModel)

#predict using test data
test_pred = predict(ruleModel, test[,-13])
table(test_pred, test[,13])

#Visualize the confusion matrix
library(caret)
xtab = table(observed = test[,13], predicted = test_pred)
confusionMatrix(xtab)

#write rules into disk
write(capture.output(summary(ruleModel)), "c50Rules.txt")
write(capture.output(summary(ruleModel)), "c50Rules.csv")

library(rpart)
library(Metrics)
fit = rpart(qualitycat ~ ., data = train, method = "anova")
predictions = predict(fit, test[,-13])
mape <- function(y, yhat)
  mean(abs((y - yhat)/y))
mape(test[,1], predictions)

library(DMwR)
regr.eval(test[,1], predictions, stats = c('mae','rmse','mape'))



#apply CART model
library(ipred)
fit_cart <- bagging(qualitycat ~., data=train)

#summary of the model

library(plyr)
library(dplyr)

#summary(fit_cart)

importance_fit_cart= varImp(fit_cart)
varImportance_fit_cart = data.frame(importance_fit_cart)
varImportance_fit_cart$variable= row.names(varImportance_fit_cart)                            
row.names(varImportance_fit_cart)=NULL
varImportance_fit_cart=varImportance_fit_cart[,c(2,1)]

# Create a rank variable based on importance
rankImportance_fit_car = varImportance_fit_cart %>%
  mutate(Rank = paste0('#',dense_rank(desc(importance_fit_cart))))

write(capture.output(rankImportance_fit_car), "rankImportance_fit_car.csv")


# Predict using the test set

test_pred_cart = predict(fit_cart, test[,-13], type="class")
table(test_pred_cart, test[,13])

# summarize accuracy
xtab_cart = table(observed = test[,13], predicted = test_pred_cart)
confusionMatrix(xtab_cart)




#random forest

rf_model =randomForest(factor(qualitycat) ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + 
                         free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol +  color
                       , data = train)
summary(rf_model)
# Show model error
plot(rf_model, ylim=c(0,0.50))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:4)


# Get importance
importance_rf_model = importance(rf_model)
varImportance_rf_model = data.frame(Variables = row.names(importance), 
Importance = round(importance[ , 'MeanDecreaseGini'], 2))
                            
row.names(varImportance_rf_model)=NULL
varImportance_rf_model=varImportance_rf_model[,c(2,1)]



# Create a rank variable based on importance
rankImportance_rf_model = varImportance_rf_model %>%
  mutate(Rank = paste0('#',dense_rank(desc(importance_rf_model))))

write(capture.output(rankImportance_rf_model), "rankImportance_rf_model.csv")

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance_rf_model, aes(x = reorder(Variables, Importance), 
                                    y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + ggtitle('random forest model')+
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') + coord_flip() + theme_few()


## Predict using the test set
prediction_rf_model = predict(rf_model, test[,c(1:12)])
xtab_random = table(observed = test[,13], predicted = prediction_rf_model)
confusionMatrix(xtab_random)
