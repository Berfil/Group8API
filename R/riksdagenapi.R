
get_riksdagen_api_data <- function() {
  #run the API query to fetch voting data from 2009-21 and store it as a list
  years <- c(2009:2020)
  api_result_list <- list()
  i = 2009
  j = 1
  for (i in years) {
    api_url <- paste0("https://data.riksdagen.se/voteringlista/?rm=",i,"%2F",(i-1999),"&bet=&punkt=&valkrets=&rost=&iid=&sz=10000&utformat=json&gruppering=namn")
    #x <- httr::GET(api_url)
    api_result_list[j] <- jsonlite::fromJSON(api_url)
    names(api_result_list[j]) <- i
    j = j + 1
  }
  
  i <- NULL
  j <- NULL
  
  
  #naming the result list with years
  names(api_result_list) <- as.character(years)
  
  #creating a resultant list that contains yearly data in a separate list element by year to be used for final analysis
  
  result_list = as.list(c())
  result_list <- lapply(api_result_list, "[[", 12)
}
get_riksdagen_api_data
