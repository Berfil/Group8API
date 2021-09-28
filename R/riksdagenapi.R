
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
# Create the DF we will use

df_1 <- get_riksdagen_api_data()

#add years column to each DF in list
year_labels <- c(2009:2020)
df_1 <- mapply(cbind, df_1, "year" = year_labels, SIMPLIFY = F)


# Merge them all together
master_df <- dplyr::bind_rows(df_1)

# Create a new variable for party ID
master_df$parti <- gsub(".*[(]([^)]+)[)].*", "\\1", master_df$namn)
master_df$parti[master_df$parti == "fp"] <- "l"
master_df$parti <- toupper(master_df$parti)


Center_party <- master_df[master_df$parti == "C",]
Krist_demokraterna <- master_df[master_df$parti == "KD",]
Liberal_party <- master_df[master_df$parti == "L",]
Moderat_party <-master_df[master_df$parti == "M",]
Enviorment_party <-master_df[master_df$parti == "MP",]
Social_democrats <-master_df[master_df$parti == "S",]
Sweden_democrats <-master_df[master_df$parti == "SD",]
Left_party <-master_df[master_df$parti == "V",]






