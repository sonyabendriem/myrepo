########################  Outliers function for +-2.5SD #######################

#requires that you have a vector of the column names holding your attributes (atts_z_list)
# requires that you have a vector of the column names holding each perf measure (perf_raw)

#create data frame of all attribute outliers in the sample
outliers_atts = data.frame(var=c(1))
for (i in seq_along(atts_z_list)){
  the.att <- atts_z_list[i]
  y = df[!is.na(df[[the.att]]), the.att]
  mean = mean(y, na.rm=T)
  sd = sd(y, na.rm=T)
  the.min = max(mean-2.5*sd, min(y, na.rm=T))
  the.max = min(mean+2.5*sd, max(y, na.rm=T))
  the.outs <- as.data.frame(y[y<the.min | y > the.max & !is.na(y)])
  names(the.outs)<- paste(the.att)
  if (nrow(the.outs)>0){
    the.outs$var <- c(1:nrow(the.outs))
    outliers_atts <- merge(outliers_atts, the.outs, by="var", all=T)
  }
}

#create data frame of all performance outliers in the sample
outliers_perfs = data.frame(var=c(1))
for (i in seq_along(perf_raw)){
  the.perf <- perf_raw[i]
  y = df[!is.na(df[[the.perf]]), the.perf]
  mean = mean(y, na.rm=T)
  sd = sd(y, na.rm=T)
  the.min = max(mean-2.5*sd, min(y, na.rm=T))
  the.max = min(mean+2.5*sd, max(y, na.rm=T))
  the.outs <- as.data.frame(y[y<the.min | y > the.max & !is.na(y)])
  names(the.outs)<- paste(the.perf)
  if (nrow(the.outs)>0){
    the.outs$var <- c(1:nrow(the.outs))
    outliers_perfs <- merge(outliers_perfs, the.outs, by="var", all=T)
  }
}

#for each person (row), count number of attribute outliers and performance outliers 
for (i in 1:nrow(df)){
  the.count <- 0
  #starts at 2 because the first column is not an attribute
  for (j in 2:length(outliers_atts)){
    the.att <- names(outliers_atts)[j]
    count <- sum(df[i, the.att] %in% outliers_atts[complete.cases(outliers_atts[[the.att]]), the.att])
    the.count = the.count + count
  }
  df$SD_att_outlier[i] = the.count
  the.count <- 0
  #starts at 2 because the first column is not a perf measure
  for (j in 2:length(outliers_perfs)){
    the.att <- names(outliers_perfs)[j]
    count <- sum(df[i, the.att] %in% outliers_perfs[complete.cases(outliers_perfs[[the.att]]), the.att])
    the.count = the.count + count
  }
  df$SD_perf_outlier[i] = the.count
}
table(df$SD_att_outlier) #4 people iwth 1 outlier, 1 with 3 (remove)
table(df$SD_perf_outlier) #1 person with 3 outliers
# if want to see total outlieres per person
df$outlier = df$SD_att_outlier + df$SD_perf_outlier
table(df$outlier) #no one has outliers in both atts & perf

## Decisions made Feb 19, 2018
# Remove row with performance outliers
# Remove rows with attribute outlier counts >=4
# Remove attribute score for people with attribute outlier count < 4
# Rescale performance and attribute scores 
