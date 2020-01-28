library(rvest)
library(xlsx)
library(qdapRegex)
library(stringr)
library(installr)
library(curl)

mainDir = "C:/Users/sonya/Box Sync/2018 - Aptology/Client Projects/Company Info Scraping"
setwd(file.path(mainDir))

# file
#df = read.xlsx("List of Customers.xlsx", sheetIndex=1, header=TRUE)

df = read.xlsx("ALL LEADS.xls", sheetIndex=1, header=TRUE)
df2 =df
df2$Company = as.character(df2$Company)

companies = unique(df2$Company)


#######################################
######### playing with glassdoor ######
#######################################


new.output =  data.frame(Company = character(0),
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

#run it in chunks, seeing how many i can do per hour / day 
for (i in 1:nrow(missing.output)){
  original.name <- missing.output$Company[i]
  name <- gsub(" ", "+", original.name)
  
  url <- paste("https://www.google.co.uk/search?q=working+at+", name, "USA+glassdoor+overview", sep="")
  #url <- paste("https://www.google.co.uk/search?q=working+at+", name, "+glassdoor+overview", sep="")
  #if finding overview doesn't work (use these)
  #url <- paste("https://www.google.co.uk/search?q=working+at+", name, "+glassdoor", sep="")
  
  html <- read_html(url, handle = new_handle("useragent" = "Mozilla/5.0"))
  the.list <- html %>% html_nodes("a")
  reviews <- the.list[grep("Overview", the.list)][1]
  
  #if finding overview doesnt work (with or without usa, use these)
  #reviews <- the.list[grep("https://www.glassdoor.", the.list)]
  #reviews <- reviews[-grep("Reviews|Salary|Salaries|Jobs", reviews)][1]
  
  if(length(reviews)==0){
    reviews = "error"
  }
  extract.url <- tryCatch(gsub(".*q=|&amp;.*", "", reviews), error=function(e) "error")
  closeAllConnections()
  
  if(grepl("glassdoor(.*)Overview", extract.url)==T){
  #if(grepl("glassdoor(.*)", extract.url)==T){
    test.html <- tryCatch(read_html(extract.url, handle = new_handle("useragent" = "Mozilla/5.0")), error=function(e) "error")
    
    if(test.html!="error"){
      #gd.html <- read_html(extract.url)
      gd.rate <- test.html %>% html_nodes(".empStatsBody")
      rating.num <- tryCatch(str_extract_all(gd.rate, "ratingNum(.*?)div"), error=function(e) "error") #extract
      rating.num <- gsub(".*>|<.*", "", rating.num)
      if(is.empty(rating.num)==T){
        rating.num = "Section not included"
      }
      
      gd.list.info <- test.html %>% html_nodes(".infoEntity")
      
      gd.website <- gd.list.info[grep("Website", gd.list.info)][1]
      gd.website <- tryCatch(str_extract_all(as.character(gd.website), "href=(.*?) target"), error=function(e) "error")  #extract
      gd.website = unlist(gd.website)
      #will need to clean that one up but lifes too short
      if(is.null(gd.website)==T){
        gd.website = "Section not included"
      }
      
      gd.Headquarters <- gd.list.info[grep("Headquarters", gd.list.info)][1]
      gd.Headquarters <- tryCatch(str_extract_all(gd.Headquarters, "value\">(.*?)</span"), error=function(e) "error")  #extract
      gd.Headquarters <- gsub(".*>|<.*", "", gd.Headquarters)
      if(is.empty(gd.Headquarters)==T){
        gd.Headquarters = "Section not included"
      }
      
      gd.Size <- gd.list.info[grep("Size", gd.list.info)][1]
      gd.Size <- tryCatch(str_extract_all(gd.Size, "value\">(.*?)</span"), error=function(e) "error")  #extract
      gd.Size <- gsub(".*>|<.*", "", gd.Size)
      if(is.empty(gd.Size)==T){
        gd.Size = "Section not included"
      }
      
      gd.Founded <- gd.list.info[grep("Founded", gd.list.info)][1]
      gd.Founded <- tryCatch(str_extract_all(gd.Founded, "value\">(.*?)</span"), error=function(e) "error")  #extract
      gd.Founded <- gsub(".*>|<.*", "", gd.Founded)
      if(is.empty(gd.Founded)==T){
        gd.Founded = "Section not included"
      }
      
      gd.Type <- gd.list.info[grep("Type", gd.list.info)][1]
      gd.Type <- tryCatch(str_extract_all(gd.Type, "value\">(.*?)</span"), error=function(e) "error")  #extract
      gd.Type <- gsub(".*>|<.*", "", gd.Type)
      if(is.empty(gd.Type)==T){
        gd.Type = "Section not included"
      }
      
      gd.Industry <- gd.list.info[grep("Industry", gd.list.info)][1]
      gd.Industry <- tryCatch(str_extract_all(gd.Industry, "value\">(.*?)</span"), error=function(e) "error")  #extract
      gd.Industry <- gsub(".*>|<.*", "", gd.Industry)
      if(is.empty(gd.Industry)==T){
        gd.Industry = "Section not included"
      }
      
      gd.Revenue <- gd.list.info[grep("Revenue", gd.list.info)][1]
      gd.Revenue <- tryCatch(str_extract_all(gd.Revenue, "value\">(.*?)</span"), error=function(e) "error")  #extract
      gd.Revenue <- gsub(".*>|<.*", "", gd.Revenue)
      if(is.empty(gd.Revenue)==T){
        gd.Revenue = "Section not included"
      }
      
      gd.Competitors <- gd.list.info[grep("Competitors", gd.list.info)][1]
      gd.Competitors <- tryCatch(str_extract_all(gd.Competitors, "value\">(.*?)</span"), error=function(e) "error") #extract
      gd.Competitors <- gsub(".*>|<.*", "", gd.Competitors)
      if(is.empty(gd.Competitors)==T){
        gd.Competitors = "Section not included"
      }
      
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
      
    }} else {
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
closeAllConnections()
  the.output <- data.frame(Company = original.name, URL=extract.url, Rating=rating.num, Website=gd.website, Headquarters=gd.Headquarters, Size=gd.Size, Founded=gd.Founded, Type=gd.Type, Industry=gd.Industry, Revenue=gd.Revenue, Competitors=gd.Competitors, stringsAsFactors = FALSE)
  
  
  #print(the.output)
  complete.output2 <- rbind(complete.output2, the.output)
}

#complete.output = new.output[new.output$URL!="error",]
#missing.output = new.output[new.output$URL=="error",]
#rerun script removing 'USA' from search and change the rbind at end to rbind to complete.output

#complete.output2 = complete.output[complete.output$URL!="error",]
#missing.output = complete.output[complete.output$URL=="error",]
#rerun wiht searching regular search and using first url that shows up that doesn't say 'Reviews, Salary or Jobs and rbind to complete.output2; and #out the logic that says if the html has overview

final = df2 %>% left_join(new.output, by="Company")
#final = df2 %>% left_join(complete.output, by="Company")
#final = df2 %>% left_join(complete.output2, by="Company")

write.xlsx(final, file=paste("ALL LEADS.xls", sep=''), append=T, sheetName="Glassdoor Info", row.names=T)

##############################
### For FInding Linkedin URL # 
##############################

#name in file should be 'Company'
Output = data.frame(Company = character(0),
                    URL = character(0),
                    Count = integer(0),
                    stringsAsFactors = FALSE)

# change name appropriately to match column name for 'delegate list'
for (i in seq_along(companies)){
  
  original.account <- companies[i]
  account <- gsub(" ", "+", original.account)
  
  #first search for them by name and their account name (current / previous employer)
  url <- paste("https://www.google.com/search?q=", account, "+company+linkedin", sep="")
  
  html <- read_html(curl(url, handle = new_handle("useragent" = "Mozilla/5.0")))
  
  #html <- read_html(url)
  the.list <- html %>% html_nodes("a")
  
  linkedin <- the.list[grep(".?linkedin.com/company?", the.list)][1]
  linkedin <- tryCatch(gsub(".*q=|&amp;.*", "", linkedin), error=function(e) "error")
  
  if(is.empty(linkedin)==TRUE){
    linkedin = "none found"
  }
  
  the.count = length(the.list[grep(".?linkedin.com.?in", the.list)])
  
  the.output <- data.frame(Company = original.account, URL = linkedin, Count = the.count, stringsAsFactors = FALSE)
  #print(the.output)
  Output = rbind(Output, the.output)
}

#current_date = format(Sys.time(), "%b_%d_%Y")
#write.csv(Output, paste(file = tclvalue(tcl("tk_getSaveFile")), " ", current_date, ".csv", sep=''))

output2 = Output

#unfortunately linked in doesnt let us scrape bc you have to log in
for (i in 1:nrow(output2)){
  LI.url <- paste(output2$URL[i], "/about/", sep="")
  test = download.file(output2$URL[1], destfile = "scrapedpage.html", quiet=TRUE)
  LI.html <- read_html(curl(LI.url, handle = new_handle("useragent" = "Mozilla/5.0")))
  LI.list <- LI.html %>% html_nodes(".overflow-hidden")
  #need to figure out the right node for the about info ("a")? (".overflow-hidden")?
  
}


final = merge(new.output, Output, by="Company")

#left join

#write.xlsx(final, file=paste("List of Customers.xlsx", sep=''), sheetName="Glassdoor Info", row.names=T)
#write.xlsx(new.output2, file=paste("List of Customers.xlsx", sep=''), sheetName="Glassdoor Info searching USA", row.names=T)




