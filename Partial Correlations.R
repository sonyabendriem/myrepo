library(ppcor)

#partial correlations 
rm(output.pcor)
output.pcor = data.frame(estimate = numeric(0),
                         p.value= numeric(0),
                         statistic = numeric(0),
                         n = numeric(0),
                         gp = numeric(0),
                         X = character(0),
                         Y = character(0),
                         Z = character(0),
                         stringsAsFactors = FALSE)
                    
for (k in "Known"){
  for (j in c(PPA.vars, TEIQue.vars, GIA.vars)){
    for (i in c(perf_groups)){
      perf <- i 
      trait <- j 
      control <- k
      sub.df <- df2[complete.cases(df2[, c(perf,trait, control)]), c(perf,trait, control)]
      pcor <- ppcor::pcor.test(sub.df[,perf], sub.df[,trait], sub.df[,control], method="spearman")
      pcor$Method <- NULL
      pcor$X = as.character(perf)
      pcor$Y = as.character(trait)
      pcor$Z = as.character(control)
      output.pcor <- rbind(output.pcor, pcor)
    }
  }
}
