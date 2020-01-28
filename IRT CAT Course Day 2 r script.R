install.packages("mirt")
install.packages("shiny")
install.packages('mokken')
install.packages('lordif')

library(mirt)
library(shiny)
itemplot(shiny=TRUE)


#### archive - go to CESD PROMIS Teaching practical 
require(mokken)
require(lordif)

#working on CESD - screening tool for depressive disorder; 20 questions; 4-likert responses
data = read.csv("IRT_cesd.csv", header=TRUE, sep=",")
items = data[, -c(1:4)]

str(data)

#look at frequency of responses per items 
for (i in 1:ncol(items)){
  print(names(items[i]))
  print(table(items[[i]]))
}
#add for loop for histograms (and save to pdf)
histogram(items$q1)

item.cors <- cor(items)

#Many IRT analyses assume that data are scored from 0, not 1. Many of the programs
#will do this for you automatically, but you can do it yourself using the code below.
#The code will only operate if your data aren't already scored from 0.
#Do not run if your data are correctly scored but some variables might have no 0
#responses.

head(items)

#converts scores to start from 0 (if not alraedy done)
for(i in 1:ncol(items)){
  if(0%in%items[,i]==FALSE){
    items[,i] = items[,i]-1
  }}

head(items)

old <- c(1,2,3,4) #creates a vector iwth the old scoring structure
new <- c(4,3,2,1) # creates a vector with the new scoring structure

#recode reverse coded items (4,8,12,16)

items$q4 <-recode(items$q4, old, new) 
items$q8 <-recode(items$q8, old, new) 
items$q12 <-recode(items$q12, old, new) 
items$q416<-recode(items$q16, old, new) 

#Remember that model fit statistics, included the chi square, are negatively affected by large sample sizes. We are at risk of Type II error (discarding 'good' items) when we use large samples.
#So we make two smaller random samples. We can do the analysis in one "evaluation"
#sample and then, to make sure we haven't overfitted the psychometric model to our
#data, we can assess the fit in the "validation" samples.

index=1:nrow(items) #number of observations
validation_sample = sample(index, trunc(length(index)/3)) #get a sample of 1/3 of observations
evaluation_sample = sample(index[-validation_sample], trunc(length(index)/3)) #get another sample of 1/3 of observations (exluding the ones from first sample)

print(validation_sample) # Check out what's in the vector.
eval= items[evaluation_sample,]  #Take the items for evaluation
valid = items[validation_sample,] #Take the items for validation

# You could make a 3rd validation sample here using the
valid_2 = items[-c(evaluation_sample,validation_sample),]

install.packages('lavaan')
require(lavaan)

