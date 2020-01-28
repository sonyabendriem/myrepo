########## INSTALL NECESSARY PACKAGES (only do this once per computer) ###################

install.packages('stringdist')
install.packages('stringr')
install.pacakges('plyr')


######## PREPARE WORKING ENVIRONMENT & DATA FILES ##############

## Save excel documents as CSVs in same folder as r script 
#**remember that CSV only takes in first sheet of excel documents so if you have two sheets in excel of assessment data, separate it out into two CSVs)
#**make sure that the csvs are formatted so that the first line has the variable names and that each case of data is on one row)

### Set working directory to that folder - Session, Set Working Directory, Choose directory, choose that folder

##load stringdist package
library(stringdist)
library(stringr)
library(plyr)
library(dplyr)
#require(tcltk)
######### LET THE FUN BEGIN ############## 


######## 2. Import your files #########
## Employee performance data first!! (file to be used as base for final merging)

#wm1 <- tktoplevel()
#tktitle(wm1) <- "Oh hey!"
#tcl(wm1, topmost=TRUE) 
#df1 <- read.csv(tk_choose.files(caption = "Choose Performance Data First!"), header=TRUE,sep=",", as.is=T)
#df2 <- read.csv(tk_choose.files(caption = "Choose Assessment Data Second!"), header=TRUE,sep=",", as.is=T)

#adjusted for my stupid mac that cant use tckltk
df1 <- read.csv(file.choose(), header=TRUE,sep=",", as.is=T)
df2 <- read.csv(file.choose(), header=TRUE,sep=",", as.is=T)

######## 3. Clean yo files, they so dirty #########

## Function to clean strings (eg take out extra spaces, strip unwanted characters, lowercase everything etc)
stringCleaning <- function(x) {
  #   x <- stringr::str_trim(x)
  #   x <- tolower(x)
  #   x <- gsub("'", "", x)
  #   x <- gsub("\\s+", " ", x)
  #   x <- gsub("[^[:space:]A-z0-9]", " ", x)
  #   x <- gsub('[[:digit:]]+', '', x)
  stringr::str_trim(tolower(gsub("'", "", gsub("\\s+", " ", gsub("[^[:space:]A-z0-9]", " ", gsub("[[:digit:]]+", "", x))))))
}

#To-DO**: Change the $Name and $Candidate to the name of the column with the names to match (leave the $ there)
# eg in assessment files its called Candidate
#NOTE: Column names in csv that include space get condensed in R (eg Full Name = Full.Name)
#REMEMBER: df1 is performance data, df2 is assessment data

df1$clean <- stringCleaning(df1$Original)
df2$clean <- stringCleaning(df2$combo)

##Remove duplicates from asssessment data, keep most recent completion date 
# Create new date variable based on DateCompleted
df2$DTime <- as.Date(df2$PPA_Date_Complete,"%d/%m/%Y %H:%M")
df2 <- arrange(df2, clean, desc(DTime))
df2 <- df2[!duplicated(df2$clean),]

######## 4. Split out the names #########

##File 1: Performance 

#extract the cleaned names
names <- df1$clean
# split on comma
names <- strsplit(names, " ")
# find the largest element
maxLen <- max(sapply(names, length))


##subset out first name 
firstname=sapply(names, function(x) x[1])
##subset out last name
lastname=sapply(names,function(x) x[length(x)])
##put them together
last_first.p <- paste(lastname, firstname, sep=" ")

##to retain each of the names...just in case
# fill in any blanks with NAs. The t() is to transpose the return from sapply
names <- 
  t(sapply(names, function(x)
    # append to x, NA's.  Note that if (0 == (maxLen - length(x))), then no NA's are appended 
    c(x, rep(NA, maxLen - length(x)))
  ))

as.data.frame(names) -> names
##create vector of column names for the separated name fields
col_names <- c()
for (i in 1:maxLen) { 
  col_name <- paste('name', i, sep="_")
  colnames(names)[i] <- col_name
  col_names <- c(col_names,col_name)}


# Put it all back together
data.frame(names, last_first.p, clean=df1$clean) -> names
merge(df1, names, by = "clean", all=F) -> df1


##File 2: Assessment data

#extract the cleaned names
names2 <- df2$clean
# split on comma
names2 <- strsplit(names2, " ")
# find the largest element
maxLen2 <- max(sapply(names2, length))

##subset out first name 
firstname2=sapply(names2, function(x) x[1])
##subset out last name
lastname2=sapply(names2,function(x) x[length(x)])
##put them together
last_first.a <- paste(lastname2, firstname2, sep=" ")

##to retain each of the names...
# fill in any blanks with NAs. The t() is to transpose the return from sapply
names2 <- 
  t(sapply(names2, function(x)
    # append to x, NA's.  Note that if (0 == (maxLen - length(x))), then no NA's are appended 
    c(x, rep(NA, maxLen2 - length(x)))
  ))

as.data.frame(names2) -> names2
##create vector of column names for the separated name fields
col_names2 <- c()
for (i in 1:maxLen2) { 
  col_name2 <- paste('name', i, sep="_")
  colnames(names2)[i] <- col_name2
  col_names2 <- c(col_names2,col_name2)}


# Put it all back together
data.frame(names2, last_first.a, clean=df2$clean) -> names2
merge(df2, names2, by = "clean", all=F) -> df2

#order alphabetically on cleaned last_first.p name 
df2 <- arrange(df2, last_first.a)
df1 <- arrange(df1, last_first.p)

######## 4. Match it like its hot ########


#copy df2 to be able to subset matched rows
df2ext <- df2
#This will find the closest match between the last_first name columns
for (i in 1:nrow(df1)) {
  stringdistmatrix(df1$last_first.p[i], df2ext$last_first.a, method='osa', weight = c(.2,.2, 1, .2)) -> dist
  min(dist) -> mindist
  
  #if it is a perfect match, it'll assign the assessment name to a new column (check) in performance data
  # if its not a perfect match, it'll assign the value NA to that new column
  # Then it takes out the matched rows so they can't be matched again
  if (mindist == 0) {
    df1$check[i] <- as.character(df2ext$last_first.a[which.min(dist)])
    df2ext <- df2ext[-c(which.min(dist)),]
  }
  else {
    df1$check[i] <- NA
  }} 


#Finds the closest match between the last_first name columns for the ones that didn't get a perfect match
for (i in 1:nrow(df1)) {
  if (is.na(df1$check[i]) == TRUE){ 
    stringdistmatrix(df1$last_first.p[i], df2ext$last_first.a, method='osa', weight = c(.2,.2, 1, .2)) -> dist
    min(dist) -> mindist
    
    #if the minimum distance is 1 or less, it'll assign the assessment name to a new column (checkme) in performance data
    # if the distance is greater than 1, it'll assign the value NA to that new column
    # Then it takes out the matched rows so they can't be matched again
    if (mindist <= 1) {
      df1$checkme[i] <- as.character(df2ext$last_first.a[which.min(dist)])
      df2ext <- df2ext[-c(which.min(dist)),]
    }
    else {
      df1$checkme[i] <- NA
    }}
  else {
    df1$checkme[i] <- NA
  }}

#Finds the closest match between the last_first name columns for the ones that are still not matched
for (i in 1:nrow(df1)) {
  if (is.na(df1$checkme[i]) == TRUE && is.na(df1$check[i] == TRUE)){ 
    stringdistmatrix(df1$last_first.p[i], df2ext$last_first.a, method='osa', weight = c(.2,.2, 1, .2)) -> dist
    min(dist) -> mindist
    
    #if the minimum distance is 2 or less, it'll assign the assessment name to a new column (checkme2) in performance data
    # if the distance is greater than 2, it'll assign the value NA to that new column
    # Then it takes out the matched rows so they can't be matched again
    if (mindist <= 2) {
      df1$checkme2[i] <- as.character(df2ext$last_first.a[which.min(dist)])
      df2ext <- df2ext[-c(which.min(dist)),]
    }
    else {
      df1$checkme2[i] <- NA
    }}
  else {
    df1$checkme2[i] <- NA
  }} 

##Combines check, checkme, and checkme2 together into new column named match
for (i in 1:nrow(df1)) {
  if (is.na(df1$check[i]) == FALSE) { 
    df1$match[i] <- df1$check[i]}
  else if ((is.na(df1$check[i]) == TRUE) && (is.na(df1$checkme[i]) == FALSE)) {
    df1$match[i] <- df1$checkme[i]
  }
  else {
    df1$match[i] <- df1$checkme2[i]
  }
} 

######## 5. Merge yo files: LETS GET TOGETHER YA YA YA ########

## Merge the two files together based on the matched names while keeping rows that don't get a match
merged_df <- merge(df1, df2, by.x = "match", by.y = "last_first.a", all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".p", ".a"))


##This is a very roundabout code to put all the columns that involve the separated name fields (from earlier) to the front of the datafile
group_names <- c()
col_ref <- c()
for (i in 1:ncol(merged_df)) {
  if (grepl("^(name_)", names(merged_df)[i]) == TRUE) {
    thename <- names(merged_df)[i]
    thecol <- i
    group_names <- c(group_names, thename)
    col_ref <- c(col_ref, thecol)}
}

merged_df[,group_names] -> dfnames
merged_df2 <- merged_df[, -col_ref]

cbind(dfnames, merged_df2) -> merged_df

#puts all name columns at the beginning
merged_df %>% select(match, last_first.p, check, checkme, checkme2, clean.p, clean.a, everything()) -> merged_df

##arrange it so that any names that didn't get perfect matches get placed at the top for you to check in columns "checkme" & "checkme2"
merged_df <- arrange(merged_df, checkme, checkme2, check, match)

######## 6. Save as CSV ########

#save the current date and time
current_date = format(Sys.time(), "%b_%d_%Y_at_%H_%M_%S")

#will pop up a save file window (check behind R if it doesn't pop up in front)
#write.csv(merged_df, paste(file = tclvalue(tcl("tk_getSaveFile")), " ", current_date, ".csv", sep=''))

write.csv(merged_df, paste(file = "Nuvox AE - Merged", " ", current_date, ".csv", sep=''))


