#######################################
######### playing with glassdoor ######
#######################################
library(rvest)
library(tcltk)
library(xlsx)
library(qdapRegex)
library(stringr)

df = read.csv(file.choose(), header=T, sep=",")
#df = read.xlsx(file.choose(), header=T, sheetIndex=2)

comp.df <- df
comp.df$Cleaned <- df$Company
comp.df$Ref <- df$ID

companies <- as.vector(comp.df$Cleaned)

output =  data.frame(CompanyName = character(0),
                     RefID = integer(0),
                     URL = character(0),
                     Rating = numeric(0),
                     Website = character(0),
                     Headquarters = character(0),
                     Size = character(0),
                     Founded = character(0),
                     Type = character(0),
                     Industry = character(0),
                     Revenue = character(0),
                     Competitors = character(0),
                     stringsAsFactors = FALSE)
#### trying again with overview

for (i in seq_along(c(companies))){
  original.name <- companies[i]
  name <- gsub(" ", "+", original.name)
  url <- paste("https://www.google.co.uk/search?q=working+at+", name, "+glassdoor+overview", sep="")
  html <- read_html(url)
  the.list <- html %>% html_nodes("a")
  reviews <- the.list[grep("Overview", the.list)][1]
  if(length(reviews)==0){
    reviews = "error"
  }
  extract.url <- tryCatch(gsub(".*q=|&amp;.*", "", reviews), error=function(e) "error")
  
  if(grepl("glassdoor(.*)Overview", extract.url)==T){
    test.html <- tryCatch(read_html(extract.url), error=function(e) "error")
    
      if(test.html!="error"){
    gd.html <- read_html(extract.url)
    gd.rate <- gd.html %>% html_nodes(".empStatsBody")
    rating.num <- tryCatch(str_extract_all(gd.rate, "ratingNum(.*?)div"), error=function(e) "error") #extract
    rating.num <- gsub(".*>|<.*", "", rating.num)
    
    gd.list.info <- gd.html %>% html_nodes(".infoEntity")
    
    gd.website <- gd.list.info[grep("Website", gd.list.info)][1]
    gd.website <- tryCatch(str_extract_all(gd.website, "href=(.*?) target"), error=function(e) "error")  #extract
    #will need to clean that one up but lifes too short
    
    gd.Headquarters <- gd.list.info[grep("Headquarters", gd.list.info)][1]
    gd.Headquarters <- tryCatch(str_extract_all(gd.Headquarters, "value\">(.*?)</span"), error=function(e) "error")  #extract
    gd.Headquarters <- gsub(".*>|<.*", "", gd.Headquarters)
    
    gd.Size <- gd.list.info[grep("Size", gd.list.info)][1]
    gd.Size <- tryCatch(str_extract_all(gd.Size, "value\">(.*?)</span"), error=function(e) "error")  #extract
    gd.Size <- gsub(".*>|<.*", "", gd.Size)
    
    gd.Founded <- gd.list.info[grep("Founded", gd.list.info)][1]
    gd.Founded <- tryCatch(str_extract_all(gd.Founded, "value\">(.*?)</span"), error=function(e) "error")  #extract
    gd.Founded <- gsub(".*>|<.*", "", gd.Founded)
    
    gd.Type <- gd.list.info[grep("Type", gd.list.info)][1]
    gd.Type <- tryCatch(str_extract_all(gd.Type, "value\">(.*?)</span"), error=function(e) "error")  #extract
    gd.Type <- gsub(".*>|<.*", "", gd.Type)
    
    gd.Industry <- gd.list.info[grep("Industry", gd.list.info)][1]
    gd.Industry <- tryCatch(str_extract_all(gd.Industry, "value\">(.*?)</span"), error=function(e) "error")  #extract
    gd.Industry <- gsub(".*>|<.*", "", gd.Industry)
    
    gd.Revenue <- gd.list.info[grep("Revenue", gd.list.info)][1]
    gd.Revenue <- tryCatch(str_extract_all(gd.Revenue, "value\">(.*?)</span"), error=function(e) "error")  #extract
    gd.Revenue <- gsub(".*>|<.*", "", gd.Revenue)
    
    gd.Competitors <- gd.list.info[grep("Competitors", gd.list.info)][1]
    gd.Competitors <- tryCatch(str_extract_all(gd.Competitors, "value\">(.*?)</span"), error=function(e) "error") #extract
    gd.Competitors <- gsub(".*>|<.*", "", gd.Competitors)
    
    ##run all the things
  } else {
    rating.num <- NA
    gd.website <- NA
    gd.Headquarters <- NA
    gd.Headquarters <- NA
    gd.Size <- NA
    gd.Founded <- NA
    gd.Type <- NA
    gd.Industry <- NA
    gd.Revenue <- NA
    gd.Competitors <- NA
    
  }}
    else {
      rating.num <- NA
      gd.website <- NA
      gd.Headquarters <- NA
      gd.Headquarters <- NA
      gd.Size <- NA
      gd.Founded <- NA
      gd.Type <- NA
      gd.Industry <- NA
      gd.Revenue <- NA
      gd.Competitors <- NA
      
    }
    
  
  the.output <- c(original.name, comp.df$Ref[i], extract.url, rating.num, gd.website, gd.Headquarters,gd.Size, gd.Founded, 
                  gd.Type, gd.Industry, gd.Revenue, gd.Competitors)
  
  
  #print(the.output)
  output[i,] <- the.output
}


current_date = format(Sys.time(), "%b_%d_%Y")
write.csv(output, paste(file = tclvalue(tcl("tk_getSaveFile")), " ", current_date, ".csv", sep=''))




################# archive trials ###################

## format: https://www.google.co.uk/?gws_rd=ssl#q=working+at+arv+solutions+glassdoor+reviews

# will need to take the trimmed names of the companies, and replace white space with + 
#the.name <- #gsbub(" ", "+", company)
the.name <- "ARV+solutions"
the.url <- paste("https://www.google.co.uk/search?q=working+at+", the.name, "+glassdoor+reviews", sep="")
html <- read_html(the.url)

the.list <- html %>% html_nodes("a")
#the.list2 <- html %>% html_nodes("a href")
reviews <- the.list[grep("Reviews", the.list)][1]
extract.url <- gsub(".*q=|&amp;.*", "", reviews)


## in a loop
#file name: Company Names 
df = read.csv(file.choose(), header=T, sep=",")

companies <- as.vector(df$Cleaned)
company.names <- gsub(" ", "+", companies)
output = data.frame(CompanyName = character(0),
                    RefID = integer(0),
                    URL = character(0),
                    stringsAsFactors = FALSE)

#testing
#name <- company.names[1]
for (i in seq_along(c(company.names))){
  name <- company.names[i]
  url <- paste("https://www.google.co.uk/search?q=working+at+", name, "+glassdoor+reviews", sep="")
  html <- read_html(url)
  the.list <- html %>% html_nodes("a")
  reviews <- the.list[grep("Reviews", the.list)][1]
  extract.url <- tryCatch(gsub(".*q=|&amp;.*", "", reviews), error=function(e) "error")
  the.output <- c(df$Cleaned[i], df$Ref[i], extract.url)
  #print(the.output)
  output[i,] <- the.output
}

comp.glassdoor <- output

#now need to extract glassdoor info 
gd.url <- comp.glassdoor$URL[1]
gd.html <- read_html(gd.url)
gd.list <- gd.html %>% html_nodes(".empStatsBody")
# will need to add a trycatch on this for those urls that dont have glassdoor
## probably will make it aruond if 'glassdoor' in url 
rating.num <- str_extract_all(gd.list, "ratingNum(.*?)div") #extract
rating.num <- gsub(".*>|<.*", "", rating.num)

gd.list.info <- gd.html %>% html_nodes(".infoEntity")


current_date = format(Sys.time(), "%b_%d_%Y")
write.csv(merged, paste(file = tclvalue(tcl("tk_getSaveFile")), " ", current_date, ".csv", sep=''))

output2 = data.frame(CompanyName = character(0),
                     RefID = integer(0),
                     URL = character(0),
                     stringsAsFactors = FALSE)
#### trying again with overview
for (i in seq_along(c(company.names))){
  name <- company.names[i]
  url <- paste("https://www.google.co.uk/search?q=working+at+", name, "+glassdoor+overview", sep="")
  html <- read_html(url)
  the.list <- html %>% html_nodes("a")
  reviews <- the.list[grep("Overview", the.list)][1]
  extract.url <- tryCatch(gsub(".*q=|&amp;.*", "", reviews), error=function(e) "error")
  the.output <- c(df$Cleaned[i], df$Ref[i], extract.url)
  #print(the.output)
  output2[i,] <- the.output
}

comp.gd.overview <- output2

#now need to extract glassdoor info 
gd.url <- comp.gd.overview$URL[1]
gd.html <- read_html(gd.url)
gd.list <- gd.html %>% html_nodes(".empStatsBody")
# will need to add a trycatch on this for those urls that dont have glassdoor
## probably will make it aruond if 'glassdoor' in url 
rating.num <- str_extract_all(gd.list, "ratingNum(.*?)div") #extract
rating.num <- gsub(".*>|<.*", "", rating.num)

gd.list.info <- gd.html %>% html_nodes(".infoEntity")

gd.website <- gd.list.info[grep("Website", gd.list.info)][1]
gd.website <- str_extract_all(gd.website, "href=(.*?) target") #extract
#will need to clean that one up but lifes too short

gd.Headquarters <- gd.list.info[grep("Headquarters", gd.list.info)][1]
gd.Headquarters <- str_extract_all(gd.Headquarters, "value\">(.*?)</span") #extract
gd.Headquarters <- gsub(".*>|<.*", "", gd.Headquarters)

gd.Size <- gd.list.info[grep("Size", gd.list.info)][1]
gd.Size <- str_extract_all(gd.Size, "value\">(.*?)</span") #extract
gd.Size <- gsub(".*>|<.*", "", gd.Size)

gd.Founded <- gd.list.info[grep("Founded", gd.list.info)][1]
gd.Founded <- str_extract_all(gd.Founded, "value\">(.*?)</span") #extract
gd.Founded <- gsub(".*>|<.*", "", gd.Founded)

gd.Type <- gd.list.info[grep("Type", gd.list.info)][1]
gd.Type <- str_extract_all(gd.Type, "value\">(.*?)</span") #extract
gd.Type <- gsub(".*>|<.*", "", gd.Type)

gd.Industry <- gd.list.info[grep("Industry", gd.list.info)][1]
gd.Industry <- str_extract_all(gd.Industry, "value\">(.*?)</span") #extract
gd.Industry <- gsub(".*>|<.*", "", gd.Industry)

gd.Revenue <- gd.list.info[grep("Revenue", gd.list.info)][1]
gd.Revenue <- str_extract_all(gd.Revenue, "value\">(.*?)</span") #extract
gd.Revenue <- gsub(".*>|<.*", "", gd.Revenue)

gd.Competitors <- gd.list.info[grep("Competitors", gd.list.info)][1]
gd.Competitors <- str_extract_all(gd.Competitors, "value\">(.*?)</span") #extract
gd.Competitors <- gsub(".*>|<.*", "", gd.Competitors)
