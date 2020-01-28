## CESD analysis using Rasch parital credit model ##
##  Key - if there are Three hashtags (###) then you have to do something. ##  Two hashtags (##) is a comment relating to what's happening after the comment.
##  One hashtage (#) is a comment realting to what's happening before the comment.
##  Four hashtags (####) demarcates a new section. You can search sections right in the bottom left hand corner of this window.

install.packages("mokken")
install.packages("mirt")
install.packages("lavaan")
install.packages("lordif")
install.packages('semPlot')

### Install and load the latest versions of 'mirt' 'lavaan' 'mokken' and 'lordif' 

#set working directory 
require(mokken)
require(mirt)
require(lavaan)
require(lordif)
require(semPlot)
### Loads the "IRT_cesd.csv" file from your working directory. Save it to an object called cesd. You can use the dim() and names() functions to learn a bit more about the object. 
data = read.csv("IRT_cesd.csv", header=TRUE, sep=",")


### Make an object containing just the items from the cesd data frame. Again use the names() and head() functions to learn some more about the items.
items = data[, -c(1:4)]


## The function below checks to see if there are any items scored to 0 (which is what an IRT model expects). If there is not items with a score of 0 then it rescores them. 
for(k in 1:ncol(items)){ # Means the fucntion will run on each column
  if(0%in%items[,k]==FALSE){items[,k] = items[,k]-1} #If there is not value of 0, -1 to all values
}


### Use the cor() function to correlate items in order to check item scoring is correct. Negatively scored items will correlate negatively with the rest of the items. 
cor(items)

## Reverse Scoring.
## Then we use the recode function from the lordif package to rescore the individual items

old <- c(0,1,2,3) #  Make a vector with standard scoring
new <- c(3,2,1,0) # Make a vector with reversed scoring 

### Use the recode() function to rescore negatively-worded items. You may have to use lordif::recode() to recode items if you have another package with a recode() function loaded.

#reverse coded items: 4, 8, 12,16
items$q4 <- recode(items$q4, old, new)
items$q8 <- recode(items$q8, old, new)
items$q12 <- recode(items$q12, old, new)
items$q16 <- recode(items$q16, old, new)


### Use the cor() function to check the scoring is now correct. 
cor(items)

## The code below splits your data into evaluation and validation samples. You have enough data to make an additional validation sample, see if you can write the code for it. 

index = 1:nrow(items)
validation_sample = sample(index, trunc(length(index)/3))
evaluation_sample = sample(index[-validation_sample], trunc(length(index)/3))
print(evaluation_sample) # See which participants are in the evaluation sample
eval = items[evaluation_sample,]
valid = items[validation_sample,] 

#### CFA Analysis (check factor structure)####

### Load the Lavaan package. If necessary install.packages("lavaan")###

### Check the CFA for the CESD ###
## Remember the structure of a CFA model 
## model = 'data =~ item1+item2+...itemn' 
## fit = cfa(model, data=data)
## summary(fit, fit.measures=T)

cesd.model= 'cesd =~ q1+q2+q3+q4+q5+
q6+q7+q8+q9+q10+
q11+q12+q13+q14+q15+
q16+q17+q18+q19+q20'

#fit=cfa(cesd.model, data=cesd)
fit =cfa(cesd.model, data=items)
summary(fit, fit.measures=T)
#check for CFI (should be greater than .9), ours is borderline .861
#check for TLI (should be greater than .9), ours is borderline .844
#check for RMSEA (should be less than .05), ours is borderline .085
#check for SRMR (should be ???), ours is borderline .053

semPaths(fit)


####### CHeck for Monotonicity (scalability) #########
#### Mokken analysis ####

require(mokken) # Loads `mokken`. Install if necessary. 

### check the H values and asip using coefH() and aisp() ###
coefH(items) # Check the Loevinger's H value (>3 is ok, >5 is excellent)
# items 2, 11, 15 do not scale with the rest of the items

aisp(items) # Check the unidimensionality
#items 2 and 11 seem to load on to different factor 


### Remove the items with low scalability Ho < .30 
##  data$item = NULL

## Before you modify an object containing items, save a new version so you keep a copy of the original. 
items2 <-items # Save the items to a new dataframe before we remove them, so that the original 'items' frame stays unedited 

### Remove items that violate the assumptions of Mokken analysis
##  data$item = NULL

items2$q2 = NULL
items2$q11 = NULL

names(items2)

#### PCM model fit ####

require(mirt)# Loads `mirt`. Install if necessary. 

### Fit the data to the partial credit model ###
## Remember model = mirt(items, model=1, type="Rasch")
pcm=mirt(items2, model=1, type="Rasch") #actually should be type "pcm" (bc don't have dichotomous variable, luckily it automatically changes it for us)

#grm = mirt(new_items, model=1, itemtype="graded") #greater response model?? 
#model is an argument to say how many dimensions the data has
#itemtype is an argument to state which model you'd like to use.

#iterations, tries to create a model, improves the model bit by bit to get as close to the expected model 

### Check the thresholds using the plot(model, type="trace") function ###
plot(pcm, type="trace")
# checks for category threshold ordering (if any of the answer options' curves were below all otehr curves under any ability level (pointless answer), would need to change the answer options) in this case good to go

## Make sure your graphics pane is large enough. If it's not you'll get an
## error which says "figure margins too large". 

### check item fit  ###
## itemfit(model)
packageVersion('mirt')
itemfit(pcm)
#look at outfit / infit 
#if you see a number that is > 1 it is an underfit
# outfit is very sensitive to outliers so recommend looking at infit
#tries to average difference b/w two curves -f or outfit it doesnt care about hte variance (unwieghted)
# a bunch of stuff said 
# just check if the infit is close to 1
#somehow need to get the fits to work (infit = T, need a different version)

#### Local dependency ####
### Assess local dependency using Yen's Q3, with values under .2 suppressed ###
## Use the residuals() command with type="Q3" and suppress=.2
residuals(pcm, type="Q3", digits=2, suppress=+.2)
## Remember it's positive correlations that are problematic, not negative. 

## Because we have removed items we need to see which row each item in now in so we can look at the information for the correct items. issue items: q12, q8, q16

# These represent items (because we have deleted some already = 10(q12), 7(q8), 14(q16), 

itemplot(pcm, item=10, type="info", ylim=c(0,1.5), main="Information for item 12")  
itemplot(pcm, item=7,  type="info", ylim=c(0,1.5), main="Information for item 8")  
itemplot(pcm, item=14, type="info", ylim=c(0,1.5), main="Information for item 16")  

# all 3 items have high correlations, so need to remove one 
## Even though item 16 has good information, removing item 16 means less overall information is lost from the scale because it is locally dependent with two other items. or could remove number 12 bc it has the lowest information 

### Make a new item dataframe
items3 <- items2

### Remove locally dependent items here 
items3$q16 <- NULL
#items3$q8 <- NULL
#items3$q12 <- NULL


### Refit the model and check for local dependency
pcm2=mirt(items3, model=1, type="Rasch")
plot(pcm2)
itemfit(pcm2 )
residuals(pcm2, type="Q3", digits=2, suppress=+.2)

items4 <- items3
items4$q19 <- NULL

pcm3=mirt(items4, model=1, type="Rasch")
residuals(pcm3, type="Q3", digits=2, suppress=+.2)

items5<- items4
items5$q12 <- NULL
pcm4=mirt(items5, model=1, type="Rasch")
residuals(pcm4, type="Q3", digits=2, suppress=+.2)

items6<- items5
items6$q8 <- NULL
pcm5=mirt(items6, model=1, type="Rasch")
residuals(pcm5, type="Q3", digits=2, suppress=+.2)


#### Local dependency ####
### Assess local dependency using Yen's Q3, with values under .2 suppressed ###
## Use the residuals() command with type="Q3" and suppress=.2

## Is there still some issues with local dependency between 8 and 12? If so, remove the item with less information. 


### Assess model fit using the M2() function 
M2(pcm5)
# RMSEA .06 (decent), TLI .95 (whoop), CFI .96 (whoop)

marginal_rxx(pcm5) #checks reliability of the test (.92 oh ya)

#### DIFFERENTIAL ITEM FUNCTIONING ####  

#check for item biases (area between the item curves for same item but different groups - if for same ability level, one demo group performs better than teh other - tehre is bias;)

## Select gender information for the evaluation dataset
eval_gender = data$gender[evaluation_sample]

### check for DIF using lordif() ###
# R squared change greater than .13 indicates there is differential item functioning (according to literature)
dif=lordif(items6[evaluation_sample,], eval_gender, R2.change=.13)
plot(dif)

## Use summary() to see the summary 
#summary(dif)

dif
#items flagged: 10,12 (which is equivalent to q14, q17

# Make plots for all of the items which have DIF 
dev.off()
plot(dif, labels=c("Female", "Male"))
plot(dif)
#need to find a way to see each item's (rather than just the last one)
#seems to be hidden 

## Do any items have R^2 change greater than .02? 
# this part doesnt work! (should be showing up on the item graphs)

### Remove items with DIF here ###
items7 <- items6
items7$q14 <- NULL
items7$q17 <- NULL

### Refit the model, check item fit ###
pcm6=mirt(items7, model=1, type="Rasch")
itemfit(pcm6)
plot(pcm6)

### check DIF ###
dif2=lordif(items7[evaluation_sample,], eval_gender)

#no items flagged whoop

### check local dependency ###
residuals(pcm6, type="Q3", digits=2, suppress=+.2)
# good to go


### check model fit ###

M2(pcm6)
# RMSEA .06 (decent), TLI .945 (whoop), CFI .96 (whoop)

marginal_rxx(pcm6) #checks reliability of the test (.91 oh ya)

### check the marginal reliability using marginal_rxx()

### plot the test information curve
plot(pcm6)

## Ensure that the final solution also works well in the Validation dataset. Remove the same items, check the item fit, local dependency, DIF and model fit statistics. Remember to rescore!
# need to do this! 

##  Export parameter values for catR 
#IRTpars=T is important will give you item difficulty (rather than easiness)
thresh <- coef(pcm6, simplify=TRUE, IRTpars=T, digits=2)
thresh <- thresh$items

cat = 4 
firestar = cbind(thresh, cat)

write.csv(thresh, "cesdPar.csv", row.names = F) 
write.table(firestar, "firePar.csv", row.names = F, col.names = F, sep=",") 

# check your working director to see the final files, well done! 
