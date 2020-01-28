########################## Kruskal Wallis Test with one Table Output ###################
library(plyr)
library(reshape2)

#will need to add effect size to this 
the.KW.Output <- data.frame()
kruwafunk <- function(df,DV,IV){
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
  #create anova output table 
  the.kw <- tryCatch(kruskal.test(sub.df[,DV]~sub.df[,IV], data=sub.df),error=function(e) "error") #run anova
  sum.kw <- tryCatch(unlist(the.kw),error=function(e) "error") #save summary output - unlist
  row1 <- tryCatch(c(as.numeric(sum.kw[1]), as.numeric(sum.kw[2]), as.numeric(sum.kw[3])) ,error=function(e) "error") #extract kw chi, df, pvalue
  kw.df <- data.frame(KW.chisq = numeric(0), Df = numeric(0), P.value = numeric(0)) #create empty data frame
  kw.df[1,] <- row1 #assign row1
  kw.df$id <- c(1:nrow(kw.df))
  merged <- merge(the.sumStats, kw.df, by="id", all=T)
  #create pairwise Mann Whitney U tests comparisons output table
  pair.comp <- tryCatch(pairwise.wilcox.test(sub.df[, DV], sub.df[,IV], p.adjust.method="none"),error=function(e) "error")
  if (nlevels(sub.df[[IV]])<2){
    PC <- data.frame(X1=c(NA, NA), X2=c(NA, NA))
    print(PC)
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
  the.KW.Output <<- rbind(the.KW.Output, final)
}

##### run my kruskal wallis function in a loop across multiple DVs and IVs
#atts = vector of column names holding attributes 
#perf_groups = vector of column names holding the performance quad groupings (can just use the one quad group if not looking at individual ones)
for (i in atts){
  for (j in perf_groups){
    trait <- i
    perf <- j
    kruwafunk(df2, trait, perf)
  }
}
write.csv(the.KW.Output, paste(file = tclvalue(tcl("tk_getSaveFile")),".csv", sep=''))

#for getting effect size for significant ones: 
library(coin)
wt <- wilcox_test(Empathetic ~ factor(Performance.Group), data=df2[df2$Performance.Group=="Top"|df2$Performance.Group=="Bottom",], distribution="exact")
es <- wt@statistic@teststatistic/sqrt(nrow(df2[df2$Performance.Group=="Top"|df2$Performance.Group=="Bottom",]))
