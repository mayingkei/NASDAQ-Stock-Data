rm(list=ls())
library(XML)
library(httr)
library(RCurl)

#get info from url
url_complist <- "https://www.slickcharts.com/nasdaq100"
doc_complist <- htmlTreeParse(rawToChar(GET(url_complist)$content), useInternalNodes = TRUE)
comp <- xpathSApply(doc_complist,"//tbody/tr/td",xmlValue)

#declare vector to store company names and tickers
comp_name <- c()
comp_ticker <- c()

#store company names and tickers
for (i in 1:length(comp)){
  if(i %% 7 == 2){
    comp_name <- c(comp_name, trimws(comp[i]))
  }
  if(i %% 7 == 3){
    comp_ticker <- c(comp_ticker, trimws(comp[i]))
  }
}

complist <- cbind(comp_name, comp_ticker)
complist<-data.frame(complist)
colnames(complist)<-c("Company","Symbol")
print(complist)




#continue from q3a for saving time
library(foreach)
library(doParallel)

# Register
cl <- makeCluster(8)
registerDoParallel(cl)

#get data for each tickers (speed up by parallel)
result <- c()

result <- foreach(i = comp_ticker, .combine = rbind) %dopar%{
  library(XML)
  library(httr)
  library(RCurl)
  url_compdata <- paste0("https://ycharts.com/companies/", i)
  doc_compdata <- htmlTreeParse(rawToChar(GET(url_compdata)$content), useInternalNodes = TRUE)
  data <- xpathSApply(doc_compdata,"//tbody/tr/td",xmlValue)
  
  res <- c(i, trimws(data[28]), trimws(data[38]), trimws(data[24]))
  res
}

stopCluster(cl)
result<-data.frame(result)
colnames(result)<-c("Symbol", "Market Cap", "P/B Value", "Dividend Yield")
print(result) #takes around 15s for 4core, around 8s for 8core


result_sort <- result[order(result["P/B Value"]),]
print(result_sort)