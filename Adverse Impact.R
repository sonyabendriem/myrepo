library(rowr)
library(plyr)
library(varhandle)

advImp.output = data.frame(X1=character(0), X2=character(0))
advImpfunk  <- function(df,x,y){
  df <- df[complete.cases(df[, c(x,y)]),]
  tbl <- as.data.frame.matrix(prop.table(table(df[[x]],df[[y]]),2)) ##creates col frequency count table
  conditions = nrow(tbl)*ncol(tbl)

  #get what percentage is selection rate of var 1 of selection rate of var 2 
  tbl[[paste(names(tbl)[1], names(tbl)[2], sep="_")]] = tbl[[names(tbl)[1]]]/tbl[[names(tbl)[2]]]
  #and vice versa
  tbl[[paste(names(tbl)[2], names(tbl)[1], sep="_")]] = tbl[[names(tbl)[2]]]/tbl[[names(tbl)[1]]]
  
  #append variable name info front of group level
  names(tbl)[1:2]=paste(y, colnames(tbl), sep="_")
  rownames(tbl)=paste(x, rownames(tbl), sep="_")
  
 
    #put together chisq out put and counts 
    tbl$Groups = rownames(tbl)
    tbl = tbl[,c("Groups", names(tbl)[1:(ncol(tbl)-1)])]
    
    for (i in 1:ncol(tbl)){
      if(class(tbl[[i]])=="factor"){
        tbl[[i]] = unfactor(tbl[[i]])}}
    
    tbl[nrow(tbl)+1,] = colnames(tbl)
    tbl$order = as.numeric(c(1:(nrow(tbl)-1),0)) 
    tbl = tbl[order(tbl$order),]
    colnames(tbl) = paste("X", 1:length(tbl), sep=".")
    
  advImp.output <<- plyr::rbind.fill(advImp.output, tbl)
}


#eg

for (i in c(PPA_vars[1:4])){
  for (j in "Still.Employed."){
    trait <- i
    perf <- j
    advImpfunk(df2, trait, perf)
  }
}
head(advImp.output)