library(pacman)
pacman::p_load(MASS, data.table, tidyr, dplyr, ggplot2, car, zoo, reshape2, Hmisc, openxlsx, nnet, caret,
               splines, ggpubr, effects, pscl, visreg, lime, broom, readr, e1071,sqldf,
               viridis,rJava,FSelector,Boruta,pROC)

dfPrep = function(df, comp, atts){
  raw_y = df[complete.cases(df[[comp]]), c('MID', comp)]
  names(raw_y) = c('MID', 'performance')
  raw_y$cat = as.factor(ifelse(raw_y$performance==1, 'high', 'low'))
  raw_y$cat = relevel(raw_y$cat, ref="high")
  dat = left_join(raw_y[, c(1,3)], df[, c('MID', atts)])
  names(dat)[3:ncol(dat)] = unlist(lapply(atts, function(x) traits$attributeLabel[match(gsub("_theta", "", x), traits$attribute)]))
  names(dat) = gsub(" ", ".", tolower(names(dat)))
  names(dat) = gsub("-", ".", names(dat))
  names(dat)[1] = 'member_id'
  return(dat)
}

#theDat = dfPrep(df2, compTopBins[[2]], atts_theta)

JustLR =  function(dat, TL){
  
  #do feature selection by linear regression
  #note: this will only work for a binary classifier for now
  ogDat = dat[complete.cases(dat),]
  dat = ogDat
  dvList <- names(dat)[3:ncol(dat)]
  
  model <- lapply(dvList, function(x) {
    lm(substitute(i~cat, list(i = as.name(x))), data = dat)})
  test <- lapply(model, summary)
  
  anto <- NA #initialize output file for regression
  
  for (i in 1:length(test)){
    ok <- test[[i]]$coefficients 
    anto[i] <- TRUE %in% c(ok[2,4] <.1)
  }
  
  #variables passing linear regression significance cut-off
  ok <- which(anto==T)
  sub <- dvList[ok]
  sub <- c('cat', sub)
  
  dat <- subset(dat, select=sub)
  
  
  #followed by stepwise AIC
  model <- multinom(cat ~ . , data = dat)
  varselect <- stepAIC(model, data = dat, direction = "both", trace = FALSE)
  
  moresel <- c('cat', varselect$vcoefnames[-1])
  
  #build the model
  dat <- subset(dat, select=moresel)
  train_control <- trainControl(method="repeatedcv", number=5, repeats=10, savePredictions = T)
  set.seed(1)
  garbage = capture.output(model <- train(cat ~ . , data=dat, trControl=train_control, 
                                          method='multinom'),file = NULL)
  
  #build the null model
  Nullmodel = nnet::multinom(cat ~1, data = dat)
  
  #internal and cross-validated diagnostics
  newdat <- dat %>% dplyr::select(-cat)
  
  newdat <- cbind( member_id = ogDat$member_id, 
                   newdat, 
                   pred = predict(model, newdat, type = "raw"), 
                   orig = dat$cat,
                   predict(model, newdat, type = "prob"))
  
  #build confusion matrix for internals
  conf <- confusionMatrix(newdat$pred, newdat$orig)
  
  over <- cbind(as.matrix(conf,what="overall"), '')
  class <- cbind(as.matrix(conf, what = "classes"), '')
  
  internal <- rbind(
    as.matrix(conf), 
    rep('', ncol(over)),
    class, 
    rep('', ncol(over)),
    over)
  
  internal = as.data.frame(internal)
  
  #build cross-validated confusion matrix
  pred_cv <- model$pred %>% group_by(rowIndex, pred) %>% summarise(n=n())
  try <- dcast(pred_cv, rowIndex ~ pred, value.var = 'n')
  try[is.na(try)] <- 0
  
  names(try)[names(try) == 'low'] <- 'cv_low'
  names(try)[names(try) == 'high'] <- 'cv_high'
  if(is.null(try$cv_high)==TRUE){try$cv_high=0}
  if(is.null(try$cv_low)==TRUE){try$cv_high=0}  
  try$cv_high <- round(try$cv_high/30,3)
  try$cv_low <- round(try$cv_low/30,3)
  try$cv_pred <- factor(ifelse(try$cv_high>try$cv_low, 'high', 'low'))
  
  total <- cbind(newdat, try)
  
  
  conf <- confusionMatrix(total$cv_pred, as.factor(tolower(total$orig)))
  
  over <- cbind(as.matrix(conf,what="overall"), '')
  class <- cbind(as.matrix(conf, what = "classes"), '')
  
  testing <- rbind(
    as.matrix(conf), 
    rep('', ncol(over)),
    class, 
    rep('', ncol(over)),
    over)
  
  testing = as.data.frame(testing)
  
  #generate model summary
  sumz = summary(model)
  
  #calculate McFadden's pseudo R2
  FinalM = model$finalModel
  FullD = FinalM$deviance
  NullD = Nullmodel$deviance
  McFad = 1-(FullD/NullD)
  
  #calculate p-value from ANOVA test
  AnovaTest = anova(FinalM,Nullmodel, test = "Chisq")
  ATP = AnovaTest$`Pr(Chi)`[2]
  
  #evaluate whether predictors can be removed from the model by the Wald test
  #relax criteria to reject H0 to p > 0.1 
  WaldTest = Anova(FinalM, type = "II", test = "Wald")
  WaldTest$variable = rownames(WaldTest)
  Rejects = as.character(WaldTest[which(WaldTest$`Pr(>Chisq)` > 0.1),c(4)])
  RejectThis = ifelse(length(Rejects) > 0, "","None to Remove")
  
  #put everything into a single table
  #confusion matrix
  CM = cbind(as.data.frame(unlist(internal[1:2,])),
             as.data.frame(unlist(testing[1:2,])))
  colnames(CM) = c("internal","crossval")
  CM$group = rownames(CM)
  
  #diagnostics - note: can add more if needed
  Dx = cbind(internal[c(8,9,16),],
             testing[c(8,9,16),])
  Dx = Dx[c(1,3)]
  colnames(Dx) = c("internal","crossval")
  Dx$group = rownames(Dx)
  
  #model summary
  msumz = as.data.frame(sumz$coefficients)
  msumz$variable = rownames(msumz)
  msumz = msumz[c(2:nrow(msumz)),]
  msumz$varsign = ifelse(sign(msumz[c(1)]) == +1, paste0("-",msumz$variable), msumz$variable)
  
  #percent high as defined by thermal line
  PctHi = ogDat %>% 
    group_by(cat) %>%
    summarise(count = n()) %>%
    mutate(pct_high = round(((count/(nrow(ogDat)))*100),2))
  
  #model descriptors
  DR = "Full_Logistic_w_step_AIC"
  Mod = "Logistic"
  Pct = as.numeric(PctHi[which(PctHi$cat == "high"),c(3)])
  Diff = as.character(unlist(msumz$varsign), stringsAsFactors = FALSE)
  
  #put everything together
  AllThat = rbind(CM,Dx)
  AllThat = melt(AllThat, id.vars = c("group"))
  AllThat = dcast(AllThat, variable ~ group)
  colnames(AllThat)[c(1)] = c("diagnostic")
  AllThat$thermal_line = TL
  AllThat$pct_high = Pct
  AllThat$dimensionality_reduction = DR
  AllThat$model_method = Mod
  AllThat$differentiators = toString(Diff)
  AllThat$diffs_to_remove = ifelse(length(Rejects) >0,toString(Rejects),RejectThis)
  AllThat$McFadden_pseudoR2 = McFad
  AllThat$FullvNull_X2_pvalue = ATP
  AllThat$FullvNull_H0 = ifelse(ATP <= 0.05,"Reject","Accept")
  
  AllThatJazz = AllThat %>% dplyr::select(model_method, dimensionality_reduction,thermal_line,
                                          pct_high, differentiators, diffs_to_remove,diagnostic,
                                          high.high,high.low,low.high,low.low,
                                          Accuracy,Precision,Recall,
                                          McFadden_pseudoR2,FullvNull_X2_pvalue,FullvNull_H0)
  
  
  
  return(AllThatJazz)
}

#same thing but generate catdf within the function - use this as the unit function for auto
MinLR = function(df, comp, atts, TL) {
  theDat = dfPrep(df, comp, atts)
  MResult = JustLR(theDat, TL)
  MResult$perf = comp
  return(MResult)
}
#eg
output = do.call(rbind, (lapply(compTopBins, function(x) MinLR(df2, x, atts_theta, 'strongly agree'))))