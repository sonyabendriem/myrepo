library(rowr)
library(plyr)
library(varhandle)

###### FUNCTION VERSION
chiSqrFunk  <- function(df,x,y){
  df <- df[complete.cases(df[, c(x,y)]),]
  tbl <- as.data.frame.matrix(table(df[[x]],df[[y]])) ##creates count table for chisqtest 
  conditions = nrow(tbl)*ncol(tbl)
  #append variable name info front of group level
  colnames(tbl)=paste(y, colnames(tbl), sep="_")
  rownames(tbl)=paste(x, rownames(tbl), sep="_")
  #run chisq
  chisq <- tryCatch(chisq.test(tbl), error=function(e) "error")
  tbl$Extra=c("NA")
  ###put output tables together into one 
  if(chisq=="error"){
    the.chi.output = data.frame(X1=x, X2=y, X3="error")
  } else if (conditions <4){
    the.chi.output = data.frame(X1=x, X2=y, X3 = "error")
  } else {
    #extract chi square test info 
    test.stats.row = c(chisq$statistic, chisq$parameter, chisq$p.value, rep("NA", (ncol(tbl)-3)))
    tbl[nrow(tbl)+1,] = c("X-Squared", "df", "p.value", rep("NA", (ncol(tbl)-3)))
    tbl[nrow(tbl)+1,] = test.stats.row
    ##first edit row names in each table (observed/expected/residuals/stdre)
    rownames(chisq$observed) <- paste(rownames(chisq$observed), "observed", sep=".")
    chisq$observed <- cbind(chisq$observed, num = c(1:nrow(chisq$observed)))
    
    rownames(chisq$expected) = paste(rownames(chisq$expected), "expected", sep=".")
    chisq$expected <- cbind(chisq$expected, num = c(1:nrow(chisq$expected)))
    
    rownames(chisq$residuals) = paste(rownames(chisq$residuals), "residuals", sep=".") 
    chisq$residuals <- cbind(chisq$residuals, num = c(1:nrow(chisq$residuals)))
    
    rownames(chisq$stdres) = paste(rownames(chisq$stdres), "stdres", sep=".")
    chisq$stdres <- cbind(chisq$stdres, num = c(1:nrow(chisq$stdres)))
    
    #rbind output tables from chi square
    chisq.table <- data.frame(rbind(chisq$observed, chisq$expected, chisq$residuals, chisq$stdres))
    
    chisq.table <- chisq.table[with(chisq.table, order(num)),]
    chisq.table$Variable = rownames(chisq.table)
    chisq.table = chisq.table[,c("Variable", names(chisq.table)[1:(ncol(chisq.table)-1)])]
    
    #get odds ratio (if 2X2 chi square):
    if (conditions == 4){
      a = as.numeric(tbl[1,1])
      b = as.numeric(tbl[1,2])
      c = as.numeric(tbl[2,1])
      d = as.numeric(tbl[2,2])
      
      p1=a/(a+b) 
      p2=c/(c+d)
      
      odds.ratio = p1/p2
      odds.ratio= as.data.frame(odds.ratio)
      
      chisq.table = cbind.fill(chisq.table, odds.ratio, fill=NA)}
    
    #put together chisq out put and counts 
    tbl$Groups = rownames(tbl)
    tbl = tbl[,c("Groups", names(tbl)[1:(ncol(tbl)-1)])]
    
    the.chi.output = rowr::cbind.fill(tbl, chisq.table, fill=NA)
    
    for (i in 1:ncol(the.chi.output)){
      if(class(the.chi.output[[i]])=="factor"){
        the.chi.output[[i]] = unfactor(the.chi.output[[i]])}}
    
    the.chi.output[nrow(the.chi.output)+1,] = colnames(the.chi.output)
    the.chi.output$order = as.numeric(c(1:(nrow(the.chi.output)-1),0)) 
    the.chi.output = the.chi.output[order(the.chi.output$order),]
    colnames(the.chi.output) = paste("X", 1:length(the.chi.output), sep=".")
  }
  return(the.chi.output)
}

withChiSqrFunk = function(outcome){
  output = do.call(rbind.fill, (lapply('insert vector of groups', function(groups) chiSqrFunk(df2, outcome, groups))))
  return(output)
}
do.call(rbind.fill, (lapply('insert vector of other groups', function(x) withChiSqrFunk(x))))

## original version

chi.output = data.frame(X1=character(0), X2=character(0))
chifunk  <- function(df,x,y){
  df <- df[complete.cases(df[, c(x,y)]),]
  tbl <- as.data.frame.matrix(table(df[[x]],df[[y]])) ##creates count table for chisqtest 
  conditions = nrow(tbl)*ncol(tbl)
  #append variable name info front of group level
  colnames(tbl)=paste(y, colnames(tbl), sep="_")
  rownames(tbl)=paste(x, rownames(tbl), sep="_")
  #run chisq
  chisq <- tryCatch(chisq.test(tbl), error=function(e) "error")
  tbl$Extra=c("NA")
  ###put output tables together into one 
  if(chisq=="error"){
    the.chi.output = data.frame(X1=x, X2=y, X3="error")
  } else if (conditions <4){
    the.chi.output = data.frame(X1=x, X2=y, X3 = "error")
  } else {
  #extract chi square test info 
    test.stats.row = c(chisq$statistic, chisq$parameter, chisq$p.value, rep("NA", (ncol(tbl)-3)))
    tbl[nrow(tbl)+1,] = c("X-Squared", "df", "p.value", rep("NA", (ncol(tbl)-3)))
    tbl[nrow(tbl)+1,] = test.stats.row
    ##first edit row names in each table (observed/expected/residuals/stdre)
  rownames(chisq$observed) <- paste(rownames(chisq$observed), "observed", sep=".")
  chisq$observed <- cbind(chisq$observed, num = c(1:nrow(chisq$observed)))

    rownames(chisq$expected) = paste(rownames(chisq$expected), "expected", sep=".")
  chisq$expected <- cbind(chisq$expected, num = c(1:nrow(chisq$expected)))
  
    rownames(chisq$residuals) = paste(rownames(chisq$residuals), "residuals", sep=".") 
  chisq$residuals <- cbind(chisq$residuals, num = c(1:nrow(chisq$residuals)))
  
    rownames(chisq$stdres) = paste(rownames(chisq$stdres), "stdres", sep=".")
  chisq$stdres <- cbind(chisq$stdres, num = c(1:nrow(chisq$stdres)))
  
  #rbind output tables from chi square
  chisq.table <- data.frame(rbind(chisq$observed, chisq$expected, chisq$residuals, chisq$stdres))
  
  chisq.table <- chisq.table[with(chisq.table, order(num)),]
  chisq.table$Variable = rownames(chisq.table)
  chisq.table = chisq.table[,c("Variable", names(chisq.table)[1:(ncol(chisq.table)-1)])]
  
  #get odds ratio (if 2X2 chi square):
  if (conditions == 4){
    a = as.numeric(tbl[1,1])
    b = as.numeric(tbl[1,2])
    c = as.numeric(tbl[2,1])
    d = as.numeric(tbl[2,2])
 
    p1=a/(a+b) 
    p2=c/(c+d)
    
    odds.ratio = p1/p2
    odds.ratio= as.data.frame(odds.ratio)
    
    chisq.table = cbind.fill(chisq.table, odds.ratio, fill=NA)}
  
#put together chisq out put and counts 
tbl$Groups = rownames(tbl)
tbl = tbl[,c("Groups", names(tbl)[1:(ncol(tbl)-1)])]

the.chi.output = rowr::cbind.fill(tbl, chisq.table, fill=NA)

for (i in 1:ncol(the.chi.output)){
  if(class(the.chi.output[[i]])=="factor"){
    the.chi.output[[i]] = unfactor(the.chi.output[[i]])}}

the.chi.output[nrow(the.chi.output)+1,] = colnames(the.chi.output)
the.chi.output$order = as.numeric(c(1:(nrow(the.chi.output)-1),0)) 
the.chi.output = the.chi.output[order(the.chi.output$order),]
colnames(the.chi.output) = paste("X", 1:length(the.chi.output), sep=".")
  }
  chi.output <<- plyr::rbind.fill(chi.output, the.chi.output)
}

#eg

for (i in c(PPA_vars[1:4])){
  for (j in "Still.Employed."){
    trait <- i
    perf <- j
    chifunk(df2, trait, perf)
  }
}
head(chi.output)