#################### Logistic Regression ######################

#https://www.r-bloggers.com/evaluating-logistic-regression-models/

library(caret)
library(e1071)
library(survey)
library(pROC)
library(ROCR)

#create training / test groups
index = 1:nrow(sub.df)
test_sample = sample(index, trunc(length(index)/2))
train_sample = sample(index[-test_sample])
if(length(train_sample)>length(test_sample)){
  train_sample = train_sample[1:(length(train_sample)-1)]haha
}
train = sub.df[train_sample,]
test = sub.df[test_sample,] 

mod_fit <- train(as.factor(Died_group) ~ ASA+Age,family="binomial", method="glm",data=train)
summary(mod_fit)

#calculate odds ratio for each predictor
exp(coef(mod_fit$finalModel))

#use model parameters to predict value of in new set of observations (using test data set)
predict(mod_fit, newdata=test)
predict(mod_fit, newdata=test, type="prob")

#testing model fit
mod_fit1 <- glm(Died_group ~ ASA+Age,family="binomial",data=train)
mod_fit2 <- glm(Died_group ~ ASA,family="binomial",data=train)

#test goodness of fit
anova(mod_fit1, mod_fit2, test ="Chisq")

#extract pseudo Rsqure
PR2 = 1 - (mod_fit1$deviance / mod_fit1$null.deviance)

#Wald Test
regTermTest(mod_fit1, "Age") #not significant, removing this variable wont harm the model
regTermTest(mod_fit1, "ASA") #significant, shouldnt remove this variable as would harm the model

#variable importance
varImp(mod_fit)

#how well does the model predict target variable on out of sample observations (use test dataset)
pred = predict(mod_fit, newdata=test)
accuracy = table(pred, test[, "Died_group"])
sum(diag(accuracy))/sum(accuracy) 
#82 accuracy 

confusionMatrix(data=pred, as.factor(test$Died_group))

#ROC curves (concerned about area under ROC curve, AUROC, which ranges from 0.5 to 1 - values above .8 indicate model does good job discriminating b/w the two categories)

#with target x predictor pairsings
f1 = roc(Died_group ~ ASA, data=train) #AUC .78
plot(f1, col="red")

#with target x model performance
prob <- predict(mod_fit1, newdata=test, type="response")
pred <- prediction(prob, test$Died_group)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc #.855

# k fold cross validation
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(as.factor(Died_group) ~ ASA + Age,  data=sub.df, method="glm", family="binomial",trControl = ctrl, tuneLength = 5)

pred = predict(mod_fit, newdata=test)
confusionMatrix(data=pred, as.factor(test$Died_group))
#accuracy .82