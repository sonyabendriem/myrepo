##ANOVA with Effect Sizes
library(lsr)
library(car) 
library(plyr)
library(reshape2)


########################################
### FUNCTION VERSION ###################
#######################################
anovaEff <- function(df,DV,IV){
  #extract complete cases: 
  sub.df <- df[complete.cases(df[, c(DV,IV)]), c(DV,IV)]
  sub.df[IV] = as.factor(sub.df[[IV]])
  sub.df[DV] = as.numeric(sub.df[[DV]])
  ##create a table of summary stats (sample, mean, sd)
  the.sumStats <- tryCatch(plyr::ddply(sub.df, IV, .fun=function(x) {c(N=length(x[,DV]), mean=mean(x[,DV], na.rm=TRUE), sd=sd(x[,DV], na.rm=TRUE))}),error=function(e) "error")
  names(the.sumStats)[1] <- "Group"
  theDV <- rep(DV,nrow(the.sumStats))
  theIV <- rep(IV, nrow(the.sumStats))
  the.sumStats <- cbind(theDV, theIV, the.sumStats)
  the.sumStats$id <- c(1:nrow(the.sumStats))
  
  #run levenes test:
  lev <- tryCatch(leveneTest(sub.df[[DV]], as.factor(sub.df[[IV]]), center=mean),error=function(e) "error")
  lev.F <- tryCatch(lev$`F value`[1], error=function(e) "error")
  lev.p <- tryCatch(lev$`Pr(>F)`[1], error=function(e) "error")
  
  #run anova
  the.aov <- tryCatch(aov(sub.df[,DV]~sub.df[,IV], data=sub.df),error=function(e) "error")
  sum.aov <- tryCatch(unlist(summary(the.aov)),error=function(e) "error")
  eff.aov <- tryCatch(etaSquared(the.aov), error=function(e) "error")
  mag.aov <- tryCatch(ifelse(eff.aov[2]<.01, "Negligible", ifelse(eff.aov[2]<.06, "Small", ifelse(eff.aov[2]<.14, "Medium", ifelse(eff.aov[2]>=.14, "Large", NA)))), error=function(e) "error")
  
  anova.df <- data.frame(DV=character(0), IV=character(0), LevenesF=numeric(0), LevenesP= numeric(0), DF=numeric(0), Sum.sq=numeric(0), Mean.sq=numeric(0), F.value=numeric(0),P.value=numeric(0), EtaSq=numeric(0), EtaSq.Part = numeric(0), Magnitude = character(0), stringsAsFactors = F)
  
  row1 <- tryCatch(c(as.character(DV), as.character(IV), as.numeric(lev.F), as.numeric(lev.p), as.numeric(sum.aov[1]), as.numeric(sum.aov[3]), as.numeric(sum.aov[5]), as.numeric(sum.aov[7]), as.numeric(sum.aov[9]), as.numeric(eff.aov[1]), as.numeric(eff.aov[2]), as.character(mag.aov)), error=function(e) "error")
  row2 = tryCatch(c(as.character(DV),"Residuals", NA, NA, as.numeric(sum.aov[2]), as.numeric(sum.aov[4]), as.numeric(sum.aov[6]), as.numeric(sum.aov[8]),as.numeric(sum.aov[10]), NA, NA, NA), error=function(e) "error")
  anova.df[1,] <- row1 #assign row1
  anova.df[2,] <- row2 #assign row2 
  anova.df$id <- c(1:nrow(anova.df))
  merged <- merge(the.sumStats, anova.df, by="id", all=T)
  
  #create pairwise t.tests comparisons output table
  pair.comp <- tryCatch(pairwise.t.test(sub.df[, DV], sub.df[,IV], p.adjust.method="none"),error=function(e) "error")
  if (nlevels(sub.df[[IV]])<2){
    PC <- data.frame(X1=c(NA, NA), X2=c(NA, NA))
  } else {
    PC <- tryCatch(data.frame(pair.comp$p.value),error=function(e) "error")}
  lower.triangle <- tryCatch(lower.tri(PC, diag=T),error=function(e) "error")
  PC[!lower.triangle]<- NA
  PC <- cbind(Var=row.names(PC), PC)
  PC <- na.omit(reshape2::melt(PC, value.name="PairwiseP", id.vars='Var'))
  colnames(PC) <- c("Var1", "Var2", "PairwiseP")
  PC$Var1 <- as.character(PC$Var1)
  PC$Var2 <- as.character(PC$Var2)
  PC$PairwiseP <- as.numeric(PC$PairwiseP)
  if(nrow(PC)==0){
    PC[nrow(PC)+1,]<-rep("error", ncol(PC))
  }
  PC$id <- c(1:nrow(PC))
  final <- merge(merged, PC, by="id", all=T)
  return(final)
}


withAnovaEff = function(DF, DV, VectorIV){
  output = do.call(rbind, (lapply(VectorIV, function(x) anovaEff(DF, DV, x))))
  return(output)
}
#use it like this: insert df that will get passed into the anovaEFF function, the vector of group variables that will get passed into the lapply function that will get passed into the anovaEFF function as the IV, and the vector of DV variables
do.call(rbind, (lapply('insert vector of DV variables', function(x) withAnovaEff('insert name of DF', x, 'insert vector of IV variables'))))


##################################
## original function with rbind ##
##################################

the.anova.Output <- data.frame()
anovafunk <- function(df,DV,IV){
  #extract complete cases: 
  sub.df <- df[complete.cases(df[, c(DV,IV)]), c(DV,IV)]
  sub.df[IV] = as.factor(sub.df[[IV]])
  sub.df[DV] = as.numeric(sub.df[[DV]])
  ##create a table of summary stats (sample, mean, sd)
  the.sumStats <- tryCatch(plyr::ddply(sub.df, IV, .fun=function(x) {c(N=length(x[,DV]), mean=mean(x[,DV], na.rm=TRUE), sd=sd(x[,DV], na.rm=TRUE))}),error=function(e) "error")
  names(the.sumStats)[1] <- "Group"
  theDV <- rep(DV,nrow(the.sumStats))
  theIV <- rep(IV, nrow(the.sumStats))
  the.sumStats <- cbind(theDV, theIV, the.sumStats)
  the.sumStats$id <- c(1:nrow(the.sumStats))
  
  #run levenes test:
  lev <- tryCatch(leveneTest(sub.df[[DV]], as.factor(sub.df[[IV]]), center=mean),error=function(e) "error")
  lev.F <- tryCatch(lev$`F value`[1], error=function(e) "error")
  lev.p <- tryCatch(lev$`Pr(>F)`[1], error=function(e) "error")
  
  #run anova
  the.aov <- tryCatch(aov(sub.df[,DV]~sub.df[,IV], data=sub.df),error=function(e) "error")
  sum.aov <- tryCatch(unlist(summary(the.aov)),error=function(e) "error")
  eff.aov <- tryCatch(etaSquared(the.aov), error=function(e) "error")
  mag.aov <- tryCatch(ifelse(eff.aov[2]<.01, "Negligible", ifelse(eff.aov[2]<.06, "Small", ifelse(eff.aov[2]<.14, "Medium", ifelse(eff.aov[2]>=.14, "Large", NA)))), error=function(e) "error")
  
  anova.df <- data.frame(DV=character(0), IV=character(0), LevenesF=numeric(0), LevenesP= numeric(0), DF=numeric(0), Sum.sq=numeric(0), Mean.sq=numeric(0), F.value=numeric(0),P.value=numeric(0), EtaSq=numeric(0), EtaSq.Part = numeric(0), Magnitude = character(0), stringsAsFactors = F)
  
  row1 <- tryCatch(c(as.character(DV), as.character(IV), as.numeric(lev.F), as.numeric(lev.p), as.numeric(sum.aov[1]), as.numeric(sum.aov[3]), as.numeric(sum.aov[5]), as.numeric(sum.aov[7]), as.numeric(sum.aov[9]), as.numeric(eff.aov[1]), as.numeric(eff.aov[2]), as.character(mag.aov)), error=function(e) "error")
  row2 = tryCatch(c(as.character(DV),"Residuals", NA, NA, as.numeric(sum.aov[2]), as.numeric(sum.aov[4]), as.numeric(sum.aov[6]), as.numeric(sum.aov[8]),as.numeric(sum.aov[10]), NA, NA, NA), error=function(e) "error")
  anova.df[1,] <- row1 #assign row1
  anova.df[2,] <- row2 #assign row2 
  anova.df$id <- c(1:nrow(anova.df))
  merged <- merge(the.sumStats, anova.df, by="id", all=T)
  
  #create pairwise t.tests comparisons output table
  pair.comp <- tryCatch(pairwise.t.test(sub.df[, DV], sub.df[,IV], p.adjust.method="none"),error=function(e) "error")
  if (nlevels(sub.df[[IV]])<2){
    PC <- data.frame(X1=c(NA, NA), X2=c(NA, NA))
  } else {
    PC <- tryCatch(data.frame(pair.comp$p.value),error=function(e) "error")}
  lower.triangle <- tryCatch(lower.tri(PC, diag=T),error=function(e) "error")
  PC[!lower.triangle]<- NA
  PC <- cbind(Var=row.names(PC), PC)
  PC <- na.omit(reshape2::melt(PC, value.name="PairwiseP", id.vars='Var'))
  colnames(PC) <- c("Var1", "Var2", "PairwiseP")
  PC$Var1 <- as.character(PC$Var1)
  PC$Var2 <- as.character(PC$Var2)
  PC$PairwiseP <- as.numeric(PC$PairwiseP)
  if(nrow(PC)==0){
    PC[nrow(PC)+1,]<-rep("error", ncol(PC))
  }
  PC$id <- c(1:nrow(PC))
  final <- merge(merged, PC, by="id", all=T)
  the.anova.Output <<- rbind(the.anova.Output, final)
}

for (i in raws){
  for (j in plus3.groups){
    trait <- i
    perf <- j
    anovafunk(df2, trait, perf)
  }
}
head(the.anova.Output)

############### ARCHIVE 
output.es = data.frame(id = numeric(0),
                       DV = character(0),
                       IV = character(0),
                       LevenesF = numeric(0),
                       LevenesP = numeric(0),
                       DF = numeric(0),
                       Sum.sq = numeric(0),
                       Mean.Sq = numeric(0),
                       F.value = numeric(0),
                       P.value = numeric(0), 
                       EtaSq = numeric(0),
                       EtaSq.Part = numeric(0),
                       Magnitude = character(0),
                       Var1= character(0),
                       Var2 = character(0),
                       PairwiseP = numeric(0),
                       stringsAsFactors = FALSE)

AnovaEffectSize <- function(df, IV, DV){
  sub.df <- df[complete.cases(df[, c(DV,IV)]),]
  sub.df[[IV]] <- as.factor(sub.df[[IV]])
  sub.df[[DV]] <- as.numeric(sub.df[[DV]])
  
  #run levenes test:
  lev <- tryCatch(leveneTest(sub.df[[DV]], as.factor(sub.df[[IV]]), center=mean),error=function(e) "error")
  lev.F <- tryCatch(lev$`F value`[1], error=function(e) "error")
  lev.p <- tryCatch(lev$`Pr(>F)`[1], error=function(e) "error")
  
  #run anova
  the.aov <- tryCatch(aov(sub.df[,DV]~sub.df[,IV], data=sub.df),error=function(e) "error")
  sum.aov <- tryCatch(unlist(summary(the.aov)),error=function(e) "error")
  eff.aov <- tryCatch(etaSquared(the.aov), error=function(e) "error")
  mag.aov <- ifelse(eff.aov[2]<.01, "Negligible", ifelse(eff.aov[2]<.06, "Small", ifelse(eff.aov[2]<.14, "Medium", ifelse(eff.aov[2]>=.14, "Large", NA))))
  #create table of output:
  the.output = data.frame(DV=as.character(DV), IV=as.character(IV),LevenesF=as.numeric(lev.F), LevenesP= as.numeric(lev.p), DF=as.numeric(sum.aov[1]), Sum.sq=as.numeric(sum.aov[3]), Mean.sq=as.numeric(sum.aov[5]), F.value=as.numeric(sum.aov[7]),P.value=as.numeric(sum.aov[9]), EtaSq=as.numeric(eff.aov[1]), EtaSq.Part = as.numeric(eff.aov[2]), Magnitude = as.character(mag.aov), stringsAsFactors = F)
  #need to do this
  the.output$IV = as.character(the.output$IV)
  the.output = rbind(the.output,list(as.character(DV),"Residuals", NA, NA, as.numeric(sum.aov[2]), as.numeric(sum.aov[4]), as.numeric(sum.aov[6]), as.numeric(sum.aov[8]),as.numeric(sum.aov[10]), NA, NA, NA))
  the.output$id = c(1:nrow(the.output))
  
  #create pairwise t-tests comparisons output table
  pair.comp <- tryCatch(pairwise.t.test(sub.df[, DV], sub.df[,IV], p.adjust.method="none"),error=function(e) "error")
  if (nlevels(sub.df[[IV]])<2){
    PC <- data.frame(X1=c(NA, NA), X2=c(NA, NA))
  } else {
    PC <- tryCatch(data.frame(pair.comp$p.value),error=function(e) "error")}
  lower.triangle <- tryCatch(lower.tri(PC, diag=T),error=function(e) "error")
  PC[!lower.triangle]<- NA
  PC <- cbind(Var=row.names(PC), PC)
  PC <- na.omit(melt(PC, value.name="PairwiseP", id.vars='Var'))
  colnames(PC) <- c("Var1", "Var2", "PairwiseP")
  PC$Var1 <- as.character(PC$Var1)
  PC$Var2 <- as.character(PC$Var2)
  PC$PairwiseP <- as.numeric(PC$PairwiseP)
  if(nrow(PC)==0){
    PC[nrow(PC)+1,]<-rep("error", ncol(PC))
  }
  PC$id <- c(1:nrow(PC))
  final.output <- merge(the.output, PC, by="id", all=T)
  output.es <<- rbind(output.es, final.output)
}


AnovaEffectSize(df2, "Conversion_Group", "Group_MOT")

### in a loop: 

dv.list <- c()
iv.list <- c()
for (i in c(PPA_vars)){
  for (j in Perf_Group_vars){
    trait <- i
    perf <- j
    AnovaEffectSize(df2, perf, trait)
    dv.list <- c(dv.list, trait, trait)
    iv.list <- c(iv.list, perf, "Residuals")
  }
}

output.es$DV <- dv.list
output.es$IV <- iv.list

current_date = format(Sys.time(), "%b_%d_%Y")
write.csv(output.es, paste(file = tclvalue(tcl("tk_getSaveFile")), " ", current_date, ".csv", sep=''))

#http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize

