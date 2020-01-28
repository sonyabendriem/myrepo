library(xlsx)
library(stats)


df = read.csv(file.choose(), header=TRUE,sep=",", as.is=T)
df2 = df
atts = names(df2)[which(colnames(df2)=="abstract"):which(colnames(df2)=="understanding")]

#correlations
rm(output.cor)
output.cor = data.frame(X = character(0),
                            Y = character(0),
                            spearman.estimate = numeric(0),
                            spearman.p= numeric(0),
                            pearson.estimate = numeric(0),
                            pearson.p = numeric(0),
                            n = numeric(0),
                            stringsAsFactors = FALSE)
for (i in atts){
  for (j in "performance"){
    att <- i 
    perf <- j 
    sub.df <- df2[complete.cases(df2[, c(att,perf)]), c(att,perf)]
    the.cor.s <- cor.test(sub.df[,att], sub.df[,perf], method="spearman")
    the.cor.p <- cor.test(sub.df[,att], sub.df[,perf], method="pearson")
    the.output <- data.frame(X=as.character(perf), Y=as.character(att), spearman.estimate=as.numeric(the.cor.s$estimate), spearman.p=as.numeric(the.cor.s$p.value), pearson.estimate=as.numeric(the.cor.p$estimate), pearson.p=as.numeric(the.cor.p$p.value), n=as.numeric(nrow(sub.df)))
    output.cor = rbind(output.cor, the.output)
  }
}
write.xlsx(output.cor, file="./Pooled Enterprise Correlations.xlsx", sheetName="output", row.names=T)
