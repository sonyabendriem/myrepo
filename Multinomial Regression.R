#################### Multinomial Regression ######################
library(nnet)

#http://r-statistics.co/Multinomial-Regression-With-R.html

#create test and training data sets
set.seed(100)
trainingRows <- sample(1:nrow(sub.df), 0.7*nrow(sub.df))
train <- sub.df[trainingRows, ]
test <- sub.df[-trainingRows, ]


multinomModel <- multinom(Destination_group ~ ASA+Age, data=train) # multinom Model
summary(multinomModel) # model summary

predicted_scores <- predict(multinomModel, test, "probs")
predicted_class <- predict(multinomModel, test)
table(predicted_class, test$Destination_group)
mean(as.character(predicted_class) != as.character(test$Destination_group))
#error rate of about .32; accuracy of .68

#https://www.analyticsvidhya.com/blog/2016/02/multinomial-ordinal-logistic-regression/
m <- polr(as.factor(Destination_group) ~ ASA + Age + Sex_group, data = sub.df, Hess=TRUE)
summary(m)
ctable <- coef(summary(m))
ctable
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
ci <- confint(m)
ci
exp(coef(m))
exp(cbind(OR = coef(m), ci))
summary(m)
summary(update(m, method = "probit", Hess = TRUE), digits = 3)
summary(update(m, method = "logistic", Hess = TRUE), digits = 3)
summary(update(m, method = "cloglog", Hess = TRUE), digits = 3)
head(predict(m, sub.df, type = "p"))
addterm(m, ~.^2, test = "Chisq")    
m2 <- stepAIC(m, ~.^2) #ends up removing sex and just keeping ASA and Age
m2   
summary(m2)
m2$anova
anova(m, m2) #adding sex group doesnt significantly add to model
m3 <- update(m, Hess=TRUE)
pr <- profile(m3)
confint(pr)
plot(pr)
pairs(pr)


ctable2 <- coef(summary(m2))
ctable2
p <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE) * 2
ctable2 <- cbind(ctable2, "p value" = p)
ctable2
ci2 <- confint(m2)
ci2
exp(coef(m2)) #chekc how to interpret this again for multinomial but waht we need
exp(cbind(OR = coef(m2), ci2))
