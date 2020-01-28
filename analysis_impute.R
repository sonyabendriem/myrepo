#SB ADDED FOR IMPUTING DATA
data = df2[, traits$label]
seed = 1
data = mice(data, print=T, method="pmm", seed=seed) #Aidens method to impute data using predictive matrix

## working with no missing data now ###
## using imputed datasets ##

require(readxl)
require(psych)
require(mirt)
require(ggplot2)
require(lavaan)
require(tidyverse)
require(plyr)
library(VIM)
require(mice)

# set wd
setwd("C:/Users/aiden/Dropbox/Cambridge/PhD/Psychometrics Centre/aptology")

# check wd
dir()

# read data
ori_dat = read_excel("All IRT Info.xlsx", sheet = 6)
ori_dat[ ori_dat == 'NA'] = NA # set 'NA' as NAs
demog =ori_dat[,415:431]

a =readRDS(file = "imp_data.rds")
dat1 =mice::complete(a,1)
dat1

# response matrix
resp = sapply(dat1, as.numeric) %>%
  data.frame()
head(resp)
names(resp)
dim(resp) # 411 items


# labels
labels_all  = read_excel("All IRT Info.xlsx", sheet = 1)

# get labels from attributes
labels = list()
for(i in 1: length(unique(labels_all$attribute))){ # 19 attributes
  name <- unique(labels_all$attribute)[i]
  labels[[name]] = labels_all[labels_all$attribute == (unique(labels_all$attribute)[i]), ]
}
labels

# get response matrix from attributes
attributes = list()
for(i in 1: length(unique(labels_all$attribute))){ # 19 attributes
  name <- unique(labels[[i]]$attribute) # names
  attributes[[name]] = resp[,names(resp) %in% labels[[i]]$label]
}


# cal row means
ave = lapply(attributes, rowMeans, na.rm=TRUE)
ave2 = do.call('cbind', ave)

# cal correlations
ave2_cor  = round(cor(ave2),2 )
ave2_cor[upper.tri(ave2_cor)] <- NA
ave2_cor[ave2_cor < 0.3] <- NA # find cor greater than 0.5
ave2_cor

#################################
#### Factor Analysis ###########
## Check if Fa is possible
fa(attributes[[1]]) # test

## created my own function
## Run fa ##
myefa =function(data=data, no_final_item=8, noOfAttributes = 19){

  if(length(names(data)) != noOfAttributes){
    stop('Number of attributes arg must match the number of columns in the dataset')
  }

attributes = data
variance = NULL
count_items = NULL
floadings = list()
efa_items = list()
fscores = list()
response_matrix = list()


for(i in 1:noOfAttributes){

  # part 1
  fit =fa(attributes[[i]])
  fload =loadings(fit)[1:length(attributes[[i]]),1]
  fnames =names(fload[fload > 0.5])
  gooditems = attributes[[i]][names(attributes[[i]]) %in% fnames]

  # part 2 (loadings > 0.5) approxi
  fit2 =fa(gooditems)

  if(ncol(gooditems) < no_final_item){
    fload2 = loadings(fit2)[1:ncol(gooditems),1] # save loadings
  }else{
    highestloadings=sort(loadings(fit2)[1:ncol(gooditems),1], decreasing= TRUE)
    fload2 = highestloadings[1:no_final_item] # save loadings
  }

  # keep top items
  fnames2 =names(fload2)
  (topitems = attributes[[i]][names(attributes[[i]]) %in% fnames2])

  (fit3 =fa(topitems))

  ## save information
  (ci = ncol(topitems))
  count_items = c(ci, count_items)
  names <- names(attributes)[i] # names
  floadings[[names]] = loadings(fit3)[1:ncol(topitems),1] # save loadings
  efa_items[[names]]=names(topitems) # save remaining item names
  response_matrix[[names]]=topitems #  save response_matrix of remaining items
  fscores[[names]] = fit3$scores
  var = fit3$Vaccounted[2,]
  variance = c(variance, var) # capture variance


  }

  # factor scores
  fscores =do.call('cbind.data.frame',fscores)
  names(fscores) = names(attributes)


  final=list(count_items = count_items, floadings = floadings,
           efa_items = efa_items, response_matrix = response_matrix,
           fscores = fscores, variance = variance)

  return(final)
}



## creat a function to correlation factor scores ###########
fs_cor =function(data = data){
  fscores_2d = data$fscores
  fscores_2d_cor =cor(fscores_2d)
  fscores_2d_cor[upper.tri(fscores_2d_cor)] = NA
  fscores_2d_cor =round(fscores_2d_cor,2)
  fscores_2d_cor[fscores_2d_cor < 0.5 & fscores_2d_cor > -0.5] <- NA
  return(fscores_2d_cor)
}

######################################
######################################
# check my efa
check =myefa(data=attributes, no_final_item=10, noOfAttributes = 19)
head(check$fscores)
check$efa_items
sum(check$count_items)
mean(check$variance)

# check factor corrlations
fs_cor(data= check)

attributes1 <- attributes

########################################################################################
###### EA.3 is negative correlative
## merged columns in attributes (for E11, E12, EA.3)
attributes1$e11_e12_ea3 <- cbind(attributes1$E.1.1, attributes1$E.1.2, attributes1$EA.3)
remove =c('E.1.1', 'E.1.2', 'EA.3')
attributes2 = attributes1[!names(attributes1) %in% remove]
length(names(attributes2))

check2 =myefa(data=attributes2, no_final_item=10, noOfAttributes = 17)
fs_cor(data = check2)
check2$variance


#################
## merged columns in attributes (for N.EI.4, EI.3)
attributes2$nei4_ei3 <- cbind(attributes2$N.EI.4, attributes2$EI.3)
remove =c('N.EI.4', 'EI.3')
attributes3 = attributes2[!names(attributes2) %in% remove]
length(names(attributes3))

check3 =myefa(data=attributes3, no_final_item=10, noOfAttributes = 16)
fs_cor(data = check3)
check3$variance

################
## merged columns in attributes (for TL.1.4, EI.3)
attributes3$tl14_c2 <- cbind(attributes3$TL.1.4, attributes3$C.2)
remove =c('TL.1.4', 'C.2')
attributes4 = attributes3[!names(attributes3) %in% remove]
length(names(attributes4))

check4 =myefa(data=attributes4, no_final_item=10, noOfAttributes = 15)
fs_cor(data = check4)
check4$variance
check4$efa_items

################
## negative correlation
## merged columns in attributes (for E.1.3, A.4)
attributes4$ei3_a4 <- cbind(attributes4$E.1.3, attributes4$A.4)
remove =c('E.1.3', 'A.4')
attributes5 = attributes4[!names(attributes4) %in% remove]
length(names(attributes5))

check5 =myefa(data=attributes5, no_final_item=10, noOfAttributes = 14)
fs_cor(data = check5)
check5$variance
check5$efa_items
check5$floadings
##################################################################################
names(attributes$EA.3)
ea3 =attributes$EA.3[,names(attributes$EA.3) %in% c('WSSA.268.2', 'WSSA.274', 'WSSA.278', 'WSSA.280', 'WSSA.272', 'WSSA.277', 'WSSA.279', 'WSSA.271', 'WSSA.267.3', 'WSSA.268', 'WSSA.267.2')]


ea3 =attributes$EA.3[,names(attributes$EA.3) %in% c('WSSA.268.2', 'WSSA.274', 'WSSA.280', 'WSSA.277')]
fa(ea3,1)


## check paired correlations
#### merge this two together ######
e11_e12_ea3 =cbind(attributes$E.1.1, attributes$E.1.2, attributes1$EA.3)
nei_ei3 =cbind(attributes$N.EI.4, attributes$EI.3)
tl14_c2 =cbind(attributes$TL.1.4, attributes$C.2)
ei3_a4 <- cbind(attributes4$E.1.3, attributes4$A.4)

fa.parallel(e11_e12_ea3) # originial merged dataset
fa.parallel(nei_ei3) # merged dataset
fa.parallel(tl14_c2) # merged dataset
fa.parallel(ei3_a4) # merged dataset

fa.parallel(check5$response_matrix$e11_e12_ea3) # remove poor items and final itemset
fa.parallel(check5$response_matrix$nei4_ei3) # remove poor items and final itemset
fa.parallel(check5$response_matrix$tl14_c2) # remove poor items and final itemset
fa.parallel(check5$response_matrix$ei3_a4) # remove poor items and final itemset


############# individual cfa #############

(model = paste('f1 =~ ', noquote(paste(names(check5$response_matrix$e11_e12_ea3), collapse = '+'))))
fit_e11_e12_ea3=cfa(model = model, data = check5$response_matrix$e11_e12)
summary(fit_e11_e12_ea3, fit.measure=TRUE, standardized= TRUE)

(model = paste('f1 =~ ', noquote(paste(names(check5$response_matrix$nei4_ei3), collapse = '+'))))
fit_nei4_ei3 =cfa(model = model, data = check5$response_matrix$nei4_ei3)
summary(fit_nei4_ei3, fit.measure=TRUE, standardized= TRUE)

(model = paste('f1 =~ ', noquote(paste(names(check5$response_matrix$tl14_c2), collapse = '+'))))
fit_tl14_c2 =cfa(model = model, data = check5$response_matrix$tl14_c2)
summary(fit_tl14_c2, fit.measure=TRUE, standardized= TRUE)

(model = paste('f1 =~ ', noquote(paste(names(check5$response_matrix$ei3_a4), collapse = '+'))))
fit_ei3_a4 =cfa(model = model, data = check5$response_matrix$ei3_a4)
summary(fit_ei3_a4, fit.measure=TRUE, standardized= TRUE)


########################################################################
########################################################################
########################################################################
## cfa

mycfa =function(efa_rm = efa_response_matrix , noOfAttributes=14){

  if(length(names(efa_rm$response_matrix)) != noOfAttributes){
    stop('Number of attributes arg must match the number of columns in the dataset')
  }

check5  =efa_rm

all_floadings = list()
allFitMeasures = list()
cfa_items = list()
fscores = NULL
response_matrix = list()

for(i in 1:noOfAttributes){ # working with 15 traits
  ## part 1
  (model = paste('f1 =~ ', noquote(paste(names(check5$response_matrix[[i]]), collapse = '+'))))
  fit1 =cfa(model, data = check5$response_matrix[[i]], missing = 'fiml')
  summary(fit1, standardized= TRUE, fit.measures = TRUE)
  (std_loadings1 =inspect(fit1,what="std")$lambda)
  (keep = which(std_loadings1 > 0.5))
  (keep_items =rownames(std_loadings1)[keep])
  oldFitMeasures = fitmeasures(fit1)

  ## part 2 (loadings greater than 0.5)
  (model = paste('f1 =~ ', noquote(paste(keep_items, collapse = '+'))))

  fit2 = cfa(model, data = check5$response_matrix[[i]], missing = 'fiml')
  summary(fit2, standardized= TRUE, fit.measures = TRUE)
  factor_scores =predict(fit2)
  fscores = cbind(fscores, factor_scores) # save factor scores
  (std_loadings2 =inspect(fit2,what="std")$lambda) # factor loadings
  newFitMeasures=fitmeasures(fit2)
  FitMeasures = round(cbind(oldFitMeasures, newFitMeasures),3) # fit measures
  FitMeasures2 = FitMeasures[rownames(FitMeasures) %in% c('cfi', 'tli','rmsea', 'srmr', 'chisq'),] # save specific measures
  loadings = merge(round(std_loadings1,3), round(std_loadings2,3), by='row.names', all=TRUE)
  names(loadings) = c(unique(labels_all$attribute)[i], 'old', 'new') # capture loadings


  # keep top items
  fnames2 = rownames(std_loadings2)
  (resp_mat =check5$response_matrix[[i]])
  (resp_mat2 = resp_mat[names(resp_mat) %in% fnames2,])

  ## save information
  names <- names(check5$floadings)[i] # names
  all_floadings[[names]] = loadings
  allFitMeasures[[names]] = FitMeasures2
  cfa_items[[names]]=keep_items
  response_matrix[[names]]=resp_mat2 #  save response_matrix of remaining items

}

# factor scores
fscores=as.data.frame(fscores)
names(fscores) = names(check5$response_matrix)


final=list( floadings = all_floadings,
            cfa_items = cfa_items, response_matrix = response_matrix,
           fscores = fscores, allFitMeasures = allFitMeasures)

return(final)

}

cfa_result_dat5 =mycfa(efa_rm=check5, noOfAttributes=14)
cfa_result_dat5$floadings

#################################################################################
######### compare imputation data to original missing data ######################
# read data
dat = read_excel("All IRT Info.xlsx", sheet = 6)

# set 'NA' as NAs
dat[ dat == 'NA'] = NA


# response matrix
resp = dat[,c(4:414)]
resp = sapply(resp, as.numeric) %>%
  data.frame()
head(resp)
names(resp)
dim(resp) # 411 items


# keep items identified from efa
dataWithMissingness = list()
loadings_miss=list()
for(i in 1:14){
names =names(check5$floadings)[i]
dataWithMissingness[[names]]=resp[,names(resp) %in%  check5$efa_items[[i]]]
loadings_miss[[names]] = names(check5$floadings[[i]])
}

# make sure it is in a list format understood by mycfa()
df =list(response_matrix=dataWithMissingness, floadings = loadings_miss)

cfa_result_missing =mycfa(efa_rm=df, noOfAttributes=14)
cfa_result_missing$cfa_items
cfa_result_missing$response_matrix
cfa_result_missing$allFitMeasures
cfa_result_missing$floadings


#### compared items that were not in all datasets #########
## remove unwanted items
# EI.2 [[7]] WSSA.303
# N1 [[8]] WSSA.334.2
# EI3_A4 [[14]] WSSA256.2
length(unlist(cfa_result_missing$cfa_items))
length(unlist(cfa_result$cfa_items))
length(unlist(cfa_result_dat2$cfa_items))
length(unlist(cfa_result_dat3$cfa_items))
length(unlist(cfa_result_dat4$cfa_items))
length(unlist(cfa_result_dat5$cfa_items))

## remove the unwanted items
check6 <- check5[2:4]
check6$response_matrix[[7]] <- check6$response_matrix[[7]][,(!names(check6$response_matrix[[7]]) %in% 'WSSA.303')]
check6$response_matrix[[8]] <- check6$response_matrix[[8]][,(!names(check6$response_matrix[[8]]) %in% 'WSSA.334.2')]
check6$response_matrix[[14]] <- check6$response_matrix[[14]][,(!names(check6$response_matrix[[14]]) %in% 'WSSA256.2')]

check6$floadings[[7]] <- check6$floadings[[7]][(!names(check6$floadings[[7]]) %in% 'WSSA.303')]
check6$floadings[[8]] <- check6$floadings[[8]][(!names(check6$floadings[[8]]) %in% 'WSSA.334.2')]
check6$floadings[[14]] <- check6$floadings[[14]][(!names(check6$floadings[[14]]) %in% 'WSSA256.2')]

(check6$efa_items[[7]] <- check6$efa_items[[7]][!check6$efa_items[[7]] %in% 'WSSA.303'])
check6$efa_items[[8]] <- check6$efa_items[[8]][!check6$efa_items[[8]] %in% 'WSSA.334.2']
check6$efa_items[[14]] <- check6$efa_items[[14]][!check6$efa_items[[14]] %in% 'WSSA256.2']

## now run cfa again
result =mycfa(efa_rm=check6, noOfAttributes=14)
result$allFitMeasures

######## C2 ########
(model = paste('f1 =~ ', noquote(paste(names(check6$response_matrix$C.4), collapse = '+'))))
fit_C.4 =cfa(model = model, data = check6$response_matrix$C.4)
summary(fit_C.4, fit.measure=TRUE, standardized= TRUE)
modificationindices(fit_C.4) # WSSA.167 is a problematic item # WSSA.162 high residual # WSSA.163 high residual

# remove 453 because it has lower factor loadings
check6$response_matrix[[2]] <- check6$response_matrix[[2]][,(!names(check6$response_matrix[[2]]) %in% c('WSSA.167', 'WSSA.162', 'WSSA.163'))]
check6$floadings[[2]] <- check6$floadings[[2]][(!names(check6$floadings[[2]]) %in% c('WSSA.167', 'WSSA.162', 'WSSA.163'))]
check6$efa_items[[2]] <- check6$efa_items[[2]][!check6$efa_items[[2]] %in% c('WSSA.167', 'WSSA.162', 'WSSA.163')]

######## CE1.2 ########
(model = paste('f1 =~ ', noquote(paste(names(check6$response_matrix$CE.1.2), collapse = '+'))))
fit_CE.1.2 =cfa(model = model, data = check6$response_matrix$CE.1.2)
summary(fit_CE.1.2, fit.measure=TRUE, standardized= TRUE)
modificationindices(fit_CE.1.2) ## high residuals WSSA.199 ~~   WSSA.200

check6$response_matrix[[4]] <- check6$response_matrix[[4]][,(!names(check6$response_matrix[[4]]) %in% c('WSSA.193','WSSA.200', 'WSSA.201'))]
check6$floadings[[4]] <- check6$floadings[[4]][(!names(check6$floadings[[4]]) %in% c('WSSA.193','WSSA.200', 'WSSA.201'))]
check6$efa_items[[4]] <- check6$efa_items[[4]][!check6$efa_items[[4]] %in% c('WSSA.193','WSSA.200', 'WSSA.201')]

######## EI.1.O.1 ########
(model = paste('f1 =~ ', noquote(paste(names(check6$response_matrix$EI.1.O.1), collapse = '+'))))
fit_EI.1.O.1 =cfa(model = model, data = check6$response_matrix$EI.1.O.1)
summary(fit_EI.1.O.1, fit.measure=TRUE, standardized= TRUE)
modificationindices(fit_EI.1.O.1) ## WSSA.289 ~~ WSSA.296 83.026  ## wssa.292 flagged by MIRT and very wordy

check6$response_matrix[[6]] <- check6$response_matrix[[6]][,(!names(check6$response_matrix[[6]]) %in% c('WSSA.296', 'WSSA.292'))]
check6$floadings[[6]] <- check6$floadings[[6]][(!names(check6$floadings[[6]]) %in% c('WSSA.296', 'WSSA.292'))]
check6$efa_items[[6]] <- check6$efa_items[[6]][!check6$efa_items[[6]] %in% c('WSSA.296', 'WSSA.292')]

######## EI.2 ########
(model = paste('f1 =~ ', noquote(paste(names(check6$response_matrix$EI.2), collapse = '+'))))
fit_EI.2 =cfa(model = model, data = check6$response_matrix$EI.2)
summary(fit_EI.2, fit.measure=TRUE, standardized= TRUE)
modificationindices(fit_EI.2) ##  WSSA.305 ~~ WSSA.308 146.994

check6$response_matrix[[7]] <- check6$response_matrix[[7]][,(!names(check6$response_matrix[[7]]) %in% 'WSSA.305')]
check6$floadings[[7]] <- check6$floadings[[7]][(!names(check6$floadings[[7]]) %in% 'WSSA.305')]
check6$efa_items[[7]] <- check6$efa_items[[7]][!check6$efa_items[[7]] %in% 'WSSA.305']


######## O.1 ########
(model = paste('f1 =~ ', noquote(paste(names(check6$response_matrix$O.1), collapse = '+'))))
#model2 =paste(model, '\n WSSA.370 ~~ WSSA.453')
fit_O.1 =cfa(model = model, data = check6$response_matrix$O.1)
summary(fit_O.1, fit.measure=TRUE, standardized= TRUE)
modificationindices(fit_O.1) # remove WSSA 453

# remove 453 because it has lower factor loadings
check6$response_matrix[[9]] <- check6$response_matrix[[9]][,(!names(check6$response_matrix[[9]]) %in% 'WSSA.453')]
check6$floadings[[9]] <- check6$floadings[[9]][(!names(check6$floadings[[9]]) %in% 'WSSA.453')]
check6$efa_items[[9]] <- check6$efa_items[[9]][!check6$efa_items[[9]] %in% 'WSSA.453']

######## O.2 ########
(model = paste('f1 =~ ', noquote(paste(names(check6$response_matrix$O.2), collapse = '+'))))
  fit_O.2 =cfa(model = model, data = check6$response_matrix$O.2)
summary(fit_O.2, fit.measure=TRUE, standardized= TRUE)
modificationindices(fit_O.2) #  WSSA.389 ~~   WSSA.390 54.727 #  WSSA.388 ~~   WSSA.390 42.548 ## WSSA.388 ~~   WSSA.389 86.459

# remove 453 because it has lower factor loadings
check6$response_matrix[[10]] <- check6$response_matrix[[10]][,(!names(check6$response_matrix[[10]]) %in% c('WSSA.390', 'WSSA.388'))]
check6$floadings[[10]] <- check6$floadings[[10]][(!names(check6$floadings[[10]]) %in% c('WSSA.390', 'WSSA.388'))]
check6$efa_items[[10]] <- check6$efa_items[[10]][!check6$efa_items[[10]] %in% c('WSSA.390', 'WSSA.388')]

######## e11_e12_ea3 ########
(model = paste('f1 =~ ', noquote(paste(names(check6$response_matrix$e11_e12_ea3), collapse = '+'))))
fit_e11_e12_ea3 =cfa(model = model, data = check6$response_matrix$e11_e12_ea3)
summary(fit_e11_e12_ea3, fit.measure=TRUE, standardized= TRUE)
modificationindices(fit_e11_e12_ea3) ## WSSA.251 ~~ WSSA.252 89.369

check6$response_matrix[[11]] <- check6$response_matrix[[11]][,(!names(check6$response_matrix[[11]]) %in% c('WSSA.252'))]
check6$floadings[[11]] <- check6$floadings[[11]][(!names(check6$floadings[[11]]) %in% c('WSSA.252'))]
check6$efa_items[[11]] <- check6$efa_items[[11]][!check6$efa_items[[11]] %in% c('WSSA.252')]

######## nei4_ei3 ########
(model = paste('f1 =~ ', noquote(paste(names(check6$response_matrix$nei4_ei3), collapse = '+'))))
fit_nei4_ei3 =cfa(model = model, data = check6$response_matrix$nei4_ei3)
summary(fit_nei4_ei3, fit.measure=TRUE, standardized= TRUE)
modificationindices(fit_nei4_ei3) ##WSSA.319 ~~ WSSA.367 159.752 ##   WSSA.318 ~~ WSSA.319 120.836

# remove 453 because it has lower factor loadings
check6$response_matrix[[12]] <- check6$response_matrix[[12]][,(!names(check6$response_matrix[[12]]) %in% c('WSSA.367','WSSA.319'))]
check6$floadings[[12]] <- check6$floadings[[12]][(!names(check6$floadings[[12]]) %in% c('WSSA.367','WSSA.319'))]
check6$efa_items[[12]] <- check6$efa_items[[12]][!check6$efa_items[[12]] %in% c('WSSA.367','WSSA.319')]


######## ei3_a4 ########
(model = paste('f1 =~ ', noquote(paste(names(check6$response_matrix$ei3_a4), collapse = '+'))))
fit_ei3_a4 =cfa(model = model, data = check6$response_matrix$ei3_a4)
summary(fit_ei3_a4, fit.measure=TRUE, standardized= TRUE)
modificationindices(fit_ei3_a4) ## WSSA.263 ~~   WSSA.264 60.658 ## WSSA.260 ~~   WSSA.264 39.244 ## WSSA.257 ~~   WSSA.264 41.379

# remove 453 because it has lower factor loadings
check6$response_matrix[[14]] <- check6$response_matrix[[14]][,(!names(check6$response_matrix[[14]]) %in% 'WSSA.264')]
check6$floadings[[14]] <- check6$floadings[[14]][(!names(check6$floadings[[14]]) %in% 'WSSA.264')]
check6$efa_items[[14]] <- check6$efa_items[[14]][!check6$efa_items[[14]] %in% 'WSSA.264']
check6$efa_items

## final model
check7 <- check6
result =mycfa(efa_rm=check7, noOfAttributes=14)
result$allFitMeasures
check7$efa_items
check7$floadings[[7]]

# factor correlations
fs_cor(data=result)

# variance explained
round(fs_cor(data = result)^2,2)

# cronbach alpha
lapply(check7$response_matrix, psych::alpha)

### simulate with random groups
check8 <- check7
set.seed(102)
check8$response_matrix =lapply(check8$response_matrix, sample_n, 500)
result_s =mycfa(efa_rm=check8, noOfAttributes=14)
result_s$allFitMeasures


# keep professional services
demog$job.function =as.numeric(demog$job.function)
(keep =which(demog$job.function %in% c(1,3,4,6,7,8,9,11,12,13,14,16,17,20,21,22,24)))
check8 <- check7
for(i in 1:14){
check8$response_matrix[[i]]<- check8$response_matrix[[i]][keep,]
}
lapply(check8$response_matrix, nrow)
result_s =mycfa(efa_rm=check8, noOfAttributes=14)
result_s$allFitMeasures

# final number of items
lapply(result_s$cfa_items, length)

## Test specific attribute
(model = paste('f1 =~ ', noquote(paste(names(check7$response_matrix$EI.2), collapse = '+'))))
fit_ei3_a4 =cfa(model = model, data = check7$response_matrix$EI.2)
summary(fit_ei3_a4, fit.measure=TRUE, standardized= TRUE)
modificationindices(fit_ei3_a4) #

trait_demog <- cbind(factor = result$fscores$e11_e12_ea3, demog)
unique(trait_demog$age_group)
aa =trait_demog[trait_demog$age_group == 'Less than 40',]
bb =trait_demog[trait_demog$age_group == '40 or over',]
require(ggplot2)
trait_demog$age_group_5 =cut(trait_demog$age,5)
ggplot( data = trait_demog) + geom_histogram(aes(x= factor, fill = age_group_5))
hist(aa$factor)
hist(bb$factor)


###################### actual dataset #################
########## Exploring with IRT ######################
# read data
dat = read_excel("All IRT Info.xlsx", sheet = 6)

# set 'NA' as NAs
dat[ dat == 'NA'] = NA
names(dat)
# response matrix
resp = dat[,c(4:414)]
resp = sapply(resp, as.numeric) %>%
  data.frame()
head(resp)
names(resp)
dim(resp) # 411 items


# keep items identified from efa
dataWithMissingness = list()
loadings_miss=list()
for(i in 1:14){
  names =names(check7$floadings)[i]
  dataWithMissingness[[names]]=resp[,names(resp) %in%  check7$efa_items[[i]]]
  loadings_miss[[names]] = names(check7$floadings[[i]])
}

# make sure it is in a list format understood by mycfa()
df =list(response_matrix=dataWithMissingness, floadings = loadings_miss)

###########################################################################
names(check8$efa_items)

cfa


#grm model
fit_1 =mirt(check7$response_matrix[[1]], 1, itemtype='graded') ;itemfit(fit_1) ;plot(fit_1, type = 'info')
fit_1 =mirt(check7$response_matrix[[2]], 1, itemtype='graded');itemfit(fit_1) ;plot(fit_1, type = 'info')
fit_1 =mirt(check7$response_matrix[[3]], 1, itemtype='graded');itemfit(fit_1);plot(fit_1, type = 'info') # maybe 185
fit_1 =mirt(check7$response_matrix[[4]], 1, itemtype='graded');itemfit(fit_1);plot(fit_1, type = 'info') # 195
fit_1 =mirt(check7$response_matrix[[5]], 1, itemtype='graded');itemfit(fit_1);plot(fit_1, type = 'info') # 205.2 (check 7)
fit_1 =mirt(check7$response_matrix[[6]], 1, itemtype='graded');itemfit(fit_1);plot(fit_1, type = 'info') ## 292 (removed), 293 (now okay) (not okay for checky 7) # maybe 289 (check 7)
fit_1 =mirt(check8$response_matrix[[7]], 1, itemtype='graded');itemfit(fit_1);plot(fit_1, type = 'info')
fit_1 =mirt(check8$response_matrix[[8]], 1, itemtype='graded');itemfit(fit_1);plot(fit_1, type = 'info')
fit_1 =mirt(check8$response_matrix[[9]], 1, itemtype='graded');itemfit(fit_1);plot(fit_1, type = 'info')
fit_1 =mirt(check8$response_matrix[[10]], 1, itemtype='graded');itemfit(fit_1);plot(fit_1, type = 'info')
fit_1 =mirt(check8$response_matrix[[11]], 1, itemtype='graded');itemfit(fit_1);plot(fit_1, type = 'info')
fit_1 =mirt(check8$response_matrix[[12]], 1, itemtype='graded');itemfit(fit_1);plot(fit_1, type = 'info')
fit_1 =mirt(check8$response_matrix[[13]], 1, itemtype='graded');itemfit(fit_1);plot(fit_1, type = 'info')
fit_1 =mirt(check8$response_matrix[[14]], 1, itemtype='graded');itemfit(fit_1);plot(fit_1, type = 'info')


# gpcm model (result not as good as graded)
fit_1_g =mirt(check8$response_matrix[[1]], 1, itemtype='gpcm') ;itemfit(fit_1_g);plot(fit_1_g, type = 'info')
fit_1_g =mirt(check8$response_matrix[[2]], 1, itemtype='gpcm');itemfit(fit_1_g);plot(fit_1_g, type = 'info')
fit_1_g =mirt(check8$response_matrix[[3]], 1, itemtype='gpcm');itemfit(fit_1_g);plot(fit_1_g, type = 'info')
fit_1_g =mirt(check8$response_matrix[[4]], 1, itemtype='gpcm');itemfit(fit_1_g);plot(fit_1_g, type = 'info') # 195, 201
fit_1_g =mirt(check8$response_matrix[[5]], 1, itemtype='gpcm');itemfit(fit_1_g);plot(fit_1_g, type = 'info')
fit_1_g =mirt(check8$response_matrix[[6]], 1, itemtype='gpcm');itemfit(fit_1_g);plot(fit_1_g, type = 'info')# 293 (okay after 292 is removed)
fit_1_g =mirt(check8$response_matrix[[7]], 1, itemtype='gpcm');itemfit(fit_1_g);plot(fit_1_g, type = 'info')
fit_1_g =mirt(check8$response_matrix[[8]], 1, itemtype='gpcm');itemfit(fit_1_g);plot(fit_1_g, type = 'info')
fit_1_g =mirt(check8$response_matrix[[9]], 1, itemtype='gpcm');itemfit(fit_1_g);plot(fit_1_g, type = 'info')
fit_1_g =mirt(check8$response_matrix[[10]], 1, itemtype='gpcm');itemfit(fit_1_g);plot(fit_1_g, type = 'info')
fit_1_g =mirt(check8$response_matrix[[11]], 1, itemtype='gpcm');itemfit(fit_1_g);plot(fit_1_g, type = 'info')
fit_1_g =mirt(check8$response_matrix[[12]], 1, itemtype='gpcm');itemfit(fit_1_g);plot(fit_1_g, type = 'info')
fit_1_g =mirt(check8$response_matrix[[13]], 1, itemtype='gpcm');itemfit(fit_1_g);plot(fit_1_g, type = 'info')
fit_1_g =mirt(check8$response_matrix[[14]], 1, itemtype='gpcm');itemfit(fit_1_g);plot(fit_1_g, type = 'info')
itemfit(fit_1_g)

anova(fit_1, fit_1_g)
