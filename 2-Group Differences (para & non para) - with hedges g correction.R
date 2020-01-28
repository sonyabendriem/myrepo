##################################################################################################################################
##################################### Group Differences (T tests and Wilconcox) #################################################
##################################################################################################################################
library(psych)
library(effsize)
library(car)

#################################
###  FUNCTION VERSION ##########
################################
GroupDiffsHedgeCorrect <- function(df,dv,iv){
  df = df[complete.cases(df[, c(dv, iv)]), c(dv,iv)]
  name <- as.character(substitute(dv))
  group <- as.character(substitute(iv))
  #get summary stats:
  summary <- unlist(describeBy(df[dv], group=df[iv]))
  group0 <- as.character(gsub("[.].*", "", names(summary)[1]))
  group1 <- as.character(gsub("[.].*", "", names(summary)[14]))
  n0 <- as.numeric(summary[[2]])
  m0 <- as.numeric(summary[[3]])
  sd0 <- as.numeric(summary[[4]])
  n1 <- as.numeric(summary[[15]])
  m1 <- as.numeric(summary[[16]])
  sd1 <- as.numeric(summary[[17]])
  #run levenes test:
  lev <- tryCatch(leveneTest(df[[dv]], as.factor(df[[iv]]), center=mean),error=function(e) "error")
  lev.F <- tryCatch(lev$`F value`[1], error=function(e) "error")
  lev.p <- tryCatch(lev$`Pr(>F)`[1], error=function(e) "error")
  the.t.test <- tryCatch(t.test(df[[dv]]~df[[iv]]), error=function(e) "error")
  the.t.stat <- tryCatch(as.numeric(the.t.test$statistic), error=function(e) "error")
  t.degf <- tryCatch(as.numeric(the.t.test$parameter), error=function(e) "error")
  t.pval <- tryCatch(as.numeric(the.t.test$p.value),error=function(e) "error")
  t.d <- tryCatch(cohen.d(df[[dv]]~as.factor(df[[iv]]),hedges.correction=T, na.rm=TRUE))
  t.effect <- tryCatch(as.numeric(t.d$estimate), error=function(e) "error")
  t.mag <- tryCatch(as.character(t.d$magnitude[1]), error=function(e) "error")
  the.w.test <- tryCatch(wilcox.test(df[[dv]]~df[[iv]]), error=function(e) "error")
  the.w.stat <- tryCatch(as.numeric(the.w.test$statistic), error=function(e) "error")
  w.degf <- NA
  w.pval <- tryCatch(as.numeric(the.w.test$p.value), error=function(e) "error")
  z <- tryCatch(qnorm(the.w.test$p.value/2), error=function(e) "error")
  N <- n1 + n0
  w.effect <- tryCatch(z/sqrt(N), error=function(e) "error")
  w.mag <- tryCatch(ifelse(is.na(w.effect)==TRUE, NA, ifelse(abs(w.effect)>.5, "large", ifelse(abs(w.effect)>.3, "medium", ifelse(abs(w.effect)>.1, "small", "negligible")))), error=function(e) "error")
  the.output =  data.frame(DV = as.character(name), IV = as.character(group),N.0 = n0, Mean.0 = m0, SD.0 = sd0, N.1 = n1, Mean.1 = m1, SD.1 = sd1, Levenes.F = lev.F, Levenes.p = lev.p, Ttest = the.t.stat, DF = t.degf, pVal = t.pval, CohenD = t.effect, Cohen.Mag = t.mag, Wtest = the.w.stat, W.Df = w.degf, W.pVal = w.pval, W.Effect = w.effect, W.Mag = w.mag)
  return(the.output)
}


#eg change srCats in there to whatever vector of group names
with2GroupDiffs = function(outcome){
  output = do.call(rbind, (lapply(compTopBins, function(groups) GroupDiffsHedgeCorrect(df2, outcome, groups))))
  return(output)
}
do.call(rbind, (lapply(atts_theta, function(x) withAnovaEff(x))))

## old version
rm(the.output.g)
the.output.g = data.frame(DV = character(0),
                        IV = character(0),
                        N.0 = numeric(0),
                        Mean.0 = numeric(0),
                        SD.0 = numeric(0),
                        N.1 = numeric(0),
                        Mean.1 = numeric(0), 
                        SD.1 = numeric(0),
                        levenes.F = numeric(0),
                        levens.p = numeric(0),
                        Ttest = numeric(0),
                        T.DF = numeric(0),
                        T.p.value = numeric(0), 
                        T.cohens.d = numeric(0), 
                        T.magnitude = character(0),
                        Wtest = numeric(0),
                        W.DF = numeric(0),
                        W.p.value = numeric(0), 
                        W.eff.r = numeric(0), 
                        W.magnitude = character(0),
                        stringsAsFactors = FALSE)

groupd.funk.wG <- function(df,dv,iv){
  df = df[complete.cases(df[, c(dv, iv)]), c(dv,iv)]
  name <- as.character(substitute(dv))
  group <- as.character(substitute(iv))
  #get summary stats:
  summary <- unlist(describeBy(df[dv], group=df[iv]))
  group0 <- as.character(gsub("[.].*", "", names(summary)[1]))
  group1 <- as.character(gsub("[.].*", "", names(summary)[14]))
  n0 <- as.numeric(summary[[2]])
  m0 <- as.numeric(summary[[3]])
  sd0 <- as.numeric(summary[[4]])
  n1 <- as.numeric(summary[[15]])
  m1 <- as.numeric(summary[[16]])
  sd1 <- as.numeric(summary[[17]])
  #run levenes test:
  lev <- tryCatch(leveneTest(df[[dv]], as.factor(df[[iv]]), center=mean),error=function(e) "error")
  lev.F <- tryCatch(lev$`F value`[1], error=function(e) "error")
  lev.p <- tryCatch(lev$`Pr(>F)`[1], error=function(e) "error")
  the.t.test <- tryCatch(t.test(df[[dv]]~df[[iv]]), error=function(e) "error")
  the.t.stat <- tryCatch(as.numeric(the.t.test$statistic), error=function(e) "error")
  t.degf <- tryCatch(as.numeric(the.t.test$parameter), error=function(e) "error")
  t.pval <- tryCatch(as.numeric(the.t.test$p.value),error=function(e) "error")
  t.d <- tryCatch(cohen.d(df[[dv]]~as.factor(df[[iv]]),hedges.correction=T, na.rm=TRUE))
  t.effect <- tryCatch(as.numeric(t.d$estimate), error=function(e) "error")
  t.mag <- tryCatch(as.character(t.d$magnitude[1]), error=function(e) "error")
  the.w.test <- tryCatch(wilcox.test(df[[dv]]~df[[iv]]), error=function(e) "error")
  the.w.stat <- tryCatch(as.numeric(the.w.test$statistic), error=function(e) "error")
  w.degf <- NA
  w.pval <- tryCatch(as.numeric(the.w.test$p.value), error=function(e) "error")
    z <- tryCatch(qnorm(the.w.test$p.value/2), error=function(e) "error")
    N <- n1 + n0
  w.effect <- tryCatch(z/sqrt(N), error=function(e) "error")
  w.mag <- tryCatch(ifelse(is.na(w.effect)==TRUE, NA, ifelse(abs(w.effect)>.5, "large", ifelse(abs(w.effect)>.3, "medium", ifelse(abs(w.effect)>.1, "small", "negligible")))), error=function(e) "error")
  the.output.g[nrow(the.output.g)+1,] <<- list(as.character(name), as.character(group),n0, m0, sd0, n1, m1, sd1, lev.F, lev.p, the.t.stat, t.degf, t.pval, t.effect, t.mag, the.w.stat, w.degf, w.pval, w.effect, w.mag)
}

# run it manually for one DV & IV
groupd.funk.wG(df, "continuous_DV", "categorical_IV")


# loop through multiple DVs & IVs
dv.list <- c()
iv.list <- c()
for (i in c(DV_vars)){
  for (j in c(group_vars)){  #insert list / vector of variable names of all groups (IVs)
    the.dvs <- i
    the.ivs <- j
    groupd.funk.wG(df, the.dvs, the.ivs)
    dv.list <- c(dv.list, the.dvs)
    iv.list <- c(iv.list, the.ivs)
  }
}
the.output.g$DV = dv.list
the.output.g$IV = iv.list




######################################
## Get dat T test ###################
#######################################

### para 
rm(output.tt)
output.tt = data.frame(DV = character(0),
                       IV = character(0),
                       N.0 = numeric(0),
                       Mean.0 = numeric(0),
                       SD.0 = numeric(0),
                       N.1 = numeric(0),
                       Mean.1 = numeric(0),
                       SD.1 = numeric(0),
                       Ttest = numeric(0),
                       DF = numeric(0),
                       p.value = numeric(0), 
                       cohens.d.effect = numeric(0), 
                       cohens.d.magnitude = character(0),
                       stringsAsFactors = FALSE)

tfunkdfunk <- function(x,y){
  name <- as.character(substitute(x))[4]
  group <- gsub(".*\\$", "",as.character(substitute(x))[3])
  group <- gsub("=.*", "",group)
  n0 <- length(x[!is.na(x)]) 
  m0 <- mean(x, na.rm=TRUE)
  sd0 <- sd(x, na.rm=TRUE)
  m1 <- mean(y, na.rm=TRUE)
  sd1 <- sd(y, na.rm=TRUE)
  n1 <- length(y[!is.na(y)])
  tt <- as.numeric(t.test(x,y)$statistic)
  df <- as.numeric(t.test(x,y)$parameter)
  pval <- as.numeric(t.test(x,y)$p.value)
  cohD <- as.numeric(cohen.d(x,y, na.rm=TRUE)$estimate)
  cohDmag <- as.character(cohen.d(x,y, na.rm=TRUE)$magnitude[1])
  output.tt[nrow(output.tt)+1,] <<- list(as.character(name), as.character(group),n0, m0, sd0, n1, m1, sd1, tt, df, pval, cohD, cohDmag)
}

#eg 
tfunkdfunk(df2[df2$ADH.PassRate.Binary==0, "GTi"], df2[df2$ADH.PassRate.Binary==1, "GTi"])

### non para t-test 

##wilcox tests - data frame of outputs 
rm(output.wt)
output.wt = data.frame(DV = character(0),
                       IV = character(0),
                       N.0 = numeric(0),
                       Mean.0 = numeric(0),
                       SD.0 = numeric(0),
                       N.1 = numeric(0),
                       Mean.1 = numeric(0), 
                       SD.1 = numeric(0),
                       Ttest = numeric(0),
                       p.value = numeric(0), 
                       r = numeric(0),
                       effect.size = character(0),
                       # cohens.d.effect = numeric(0), #cant use coehns d for non parametric data
                      # cohens.d.magnitude = character(0),
                       stringsAsFactors = FALSE)

wtfunkdfunk <- function(x, y){
  name <- as.character(substitute(x))[4]
  group <- gsub(".*\\$", "",as.character(substitute(x))[3])
  group <- gsub("=.*", "",group)
  n0 <- length(x[!is.na(x)])
  m0 <- mean(x, na.rm=TRUE)
  sd0 <- sd(x, na.rm=TRUE)
  n1 <- length(y[!is.na(y)])
  m1 <- mean(y, na.rm=TRUE)
  sd1 <- sd(y, na.rm=TRUE)
  wt <- as.numeric(wilcox.test(x,y)$statistic)
  pval <- as.numeric(wilcox.test(x,y)$p.value)
  #cohD <- as.numeric(cohen.d(x,y, na.rm=TRUE)$estimate)
  #cohDmag <- as.character(cohen.d(x,y, na.rm=TRUE)$magnitude[1])
  #to get effect size
  z <- qnorm(wilcox.test(x,y)$p.value/2)
  N <- n1 + n0
  r <- z/sqrt(N)
  effect.size <- ifelse(is.na(r)==TRUE, NA, ifelse(abs(r)>.5, "large", ifelse(abs(r)>.3, "medium", ifelse(abs(r)>.1, "small", "negligible"))))
  output.wt[nrow(output.wt)+1,] <<- list(as.character(name), as.character(group),n0, m0, sd0, n1, m1, sd1, wt,pval, r, effect.size)
}

#eg 
wtfunkdfunk(df2[df2$ADH.PassRate.Binary==0, "GTi"], df2[df2$ADH.PassRate.Binary==1, "GTi"])
wtfunkdfunk(acc_agg_df$Mismatch, acc_agg_df$Match)


### in a loop: 

dv.list <- c()
iv.list <- c()
for (i in c(GIA.vars2, PPA.vars[c(13:16)], TEIQue.vars)){
  for (j in Perf.vars.bin){
    trait <- i
    perf <- j
    wtfunkdfunk(df2[df2[[perf]]==0, trait], df2[df2[[perf]]==1, trait])
    dv.list <- c(dv.list, trait)
    iv.list <- c(iv.list, perf)
  }
}

output.wt$DV <- dv.list
output.wt$IV <- iv.list

current_date = format(Sys.time(), "%b_%d_%Y")
write.csv(output.wt, paste(file = tclvalue(tcl("tk_getSaveFile")), " ", current_date, ".csv", sep=''))


#### paired t test: https://rcompanion.org/rcompanion/d_09.html
rm(output.pt)
output.pt = data.frame(Variable = character(0),
                       t = numeric(0),
                       df = numeric(0),
                       p.value = numeric(0),
                       CI95L = numeric(0),
                       CI95U = numeric(0), 
                       Meandiff = numeric(0),
                       cohens.d.effect = numeric(0), 
                       cohens.d.magnitude = character(0),
                       stringsAsFactors = FALSE)

pairedtfunk <- function(DV,IV,DF){
  the.var <- as.character(substitute(DV))
  the.group <- as.character(substitute(IV))
  the.df <- DF
  the.df$var <- DF[[DV]]
  the.df$group <- as.factor(DF[[IV]])
  the.test <- t.test(var~group, data=the.df, paired=TRUE, conf.level=0.95)
  the.t <- the.test$statistic
  the.degf <- the.test$parameter
  the.p <- the.test$p.value
  the.CI95L <- the.test$conf.int[1]
  the.CI95U <- the.test$conf.int[2]
  the.meandiff <- the.test$estimate
  #the.cohen <- cohen.d(var~group, data=the.df, paired=TRUE)
  cohD <- as.numeric(cohen.d(var~group, data=the.df, paired=TRUE)$estimate)
  cohDmag <- as.character(cohen.d(var~group, data=the.df, paired=TRUE, na.rm=TRUE)$magnitude[1])
  output.pt[nrow(output.pt)+1,] <<- list(the.var, the.t, the.degf, the.p, the.CI95L, the.CI95U, the.meandiff, cohD, cohDmag)
}

dv.list <- c()
for (i in c(GIA.vars)){
  trait <- i
  pairedtfunk(trait, "CompanyName", df2)
  dv.list <- c(dv.list, trait)
}
output.pt$Variable <- dv.list
current_date = format(Sys.time(), "%b_%d_%Y")
write.csv(output.pt, paste(file = tclvalue(tcl("tk_getSaveFile")), " ", current_date, ".csv", sep=''))

rm(output.pt2)
output.pt2 = data.frame(Variable = character(0),
                       t = numeric(0),
                       df = numeric(0),
                       p.value = numeric(0),
                       cohens.d.effect = numeric(0), 
                       cohens.d.magnitude = character(0),
                       stringsAsFactors = FALSE)
pairedtfunk2 <- function(x,y, DF){
  var1 <- as.character(substitute(x))
  var2 <- as.character(substitute(x))
  the.df <- DF
  the.df$var1 <- DF[[var1]]
  the.df$var2 <- DF[[var2]]
  
  the.test <- t.test(df$var1, df$var2, paired=TRUE)
  the.t <- the.test$statistic
  the.degf <- the.test$parameter
  the.p <- the.test$p.value
  #the.cohen <- cohen.d(var~group, data=the.df, paired=TRUE)
  cohD <- as.numeric(cohen.d(df$var1, df$var2, paired=TRUE)$estimate)
  cohDmag <- as.character(cohen.d(df$var1, df$var2, paired=TRUE, na.rm=TRUE)$magnitude[1])
  output.pt2[nrow(output.pt2)+1,] <<- list(var1, the.t, the.degf, the.p, cohD, cohDmag)
}


