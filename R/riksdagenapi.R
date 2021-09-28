
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

# # get all the data per year
# testing_1 <-df_1$`2009`
# testing_1$year = 2009
#
# testing_2 <-df_1$`2010`
# testing_2$year = 2010
#
#
#
# testing_3 <-df_1$`2011`
# testing_3$year = 2011
#
# testing_4 <-df_1$`2012`
# testing_4$year = 2012
#
# testing_5 <-df_1$`2013`
# testing_5$year = 2013
#
# testing_6 <-df_1$`2014`
# testing_6$year = 2014
#
# testing_7 <-df_1$`2015`
# testing_7$year = 2015
#
# testing_8 <-df_1$`2016`
# testing_8$year = 2016
#
# testing_9 <-df_1$`2017`
# testing_9$year = 2017
#
# testing_10 <-df_1$`2018`
# testing_10$year = 2018
#
# testing_11 <-df_1$`2019`
# testing_11$year = 2019
#
# testing_12 <-df_1$`2020`
# testing_12$year = 2020

# Merge them all together
master_df <- dplyr::bind_rows(df_1)


# new_df_thing <- merge(testing_1 , testing_2 , all = TRUE)
# new_df_thing <- merge(new_df_thing , testing_3 , all = TRUE)
# new_df_thing <- merge(new_df_thing , testing_4 , all = TRUE)
# new_df_thing <- merge(new_df_thing , testing_5 , all = TRUE)
# new_df_thing <- merge(new_df_thing , testing_6 , all = TRUE)
# new_df_thing <- merge(new_df_thing , testing_7 , all = TRUE)
# new_df_thing <- merge(new_df_thing , testing_8 , all = TRUE)
# new_df_thing <- merge(new_df_thing , testing_9 , all = TRUE)
# new_df_thing <- merge(new_df_thing , testing_10 , all = TRUE)
# new_df_thing <- merge(new_df_thing , testing_11 , all = TRUE)
# new_df_thing <- merge(new_df_thing , testing_12 , all = TRUE)


# Create a new variable for party ID
master_df$parti <- gsub(".*[(]([^)]+)[)].*", "\\1", master_df$namn)
master_df$parti[master_df$parti == "fp"] <- "l"
master_df$parti <- toupper(master_df$parti)

# new_df_thing$SVP_label1 = ifelse(grepl("(L)", new_df_thing$namn, fixed = T), "(L)", "")
# new_df_thing$SVP_label2 = ifelse(grepl("(KD)", new_df_thing$namn, fixed = T), "(KD)", "")
# new_df_thing$SVP_label3 = ifelse(grepl("(S)", new_df_thing$namn, fixed = T), "(S)", "")
# new_df_thing$SVP_label4 = ifelse(grepl("(M)", new_df_thing$namn, fixed = T), "(M)", "")
# new_df_thing$SVP_label5 = ifelse(grepl("(MP)", new_df_thing$namn, fixed = T), "(MP)", "")
# new_df_thing$SVP_label6 = ifelse(grepl("(SD)", new_df_thing$namn, fixed = T), "(SD)", "")
# new_df_thing$SVP_label7 = ifelse(grepl("(C)", new_df_thing$namn, fixed = T), "(C)", "")
# new_df_thing$SVP_label8 = ifelse(grepl("(V)", new_df_thing$namn, fixed = T), "(V)", "")
# new_df_thing$SVP_label9 = ifelse(grepl("(FP)", new_df_thing$namn, fixed = T), "(L)", "")
#
# new_df_thing$SVP_label = paste0(new_df_thing$SVP_label1, new_df_thing$SVP_label2, new_df_thing$SVP_label3, new_df_thing$SVP_label4, new_df_thing$SVP_label5, new_df_thing$SVP_label6, new_df_thing$SVP_label7, new_df_thing$SVP_label8, new_df_thing$SVP_label9)
#
# # Drop all the ugly variables created
# drops <- c("SVP_label1","SVP_label2","SVP_label3", "SVP_label4" , "SVP_label5" , "SVP_label6" ,"SVP_label7" , "SVP_label8" , "SVP_label9")
# new_df_thing <- new_df_thing[ , !(names(new_df_thing) %in% drops)]

#party mapping
party_map <- as.data.frame(Name = c("Center_Party",
                                      "Krist_demokraterna",
                                      "Liberal_party",
                                      "Moderat_party",
                                      "Enviorment_party",
                                      "Social_democrats",
                                      "Sweden_democrats",
                                      "Left_party"),
                           Abbrev = c("C",
                                      "KD",
                                      "L",
                                      "M",
                                      "MP",
                                      "S",
                                      "SD",
                                      "V"))

# # Start sorting
# ir <- master_df %>%
#   group_by(parti)


# Create a DF for each party!
# Center_party <- group_split(ir)[[2]]
# Krist_demokraterna <- group_split(ir)[[3]]
# Liberal_party <- group_split(ir)[[4]]
# Moderat_party <-group_split(ir)[[5]]
# Enviorment_party <-group_split(ir)[[6]]
# Social_democrats <-group_split(ir)[[7]]
# Sweden_democrats <-group_split(ir)[[8]]
# Left_party <-group_split(ir)[[9]]

Center_party <- master_df[master_df$parti == "C",]
Krist_demokraterna <- master_df[master_df$parti == "KD",]
Liberal_party <- master_df[master_df$parti == "L",]
Moderat_party <-master_df[master_df$parti == "M",]
Enviorment_party <-master_df[master_df$parti == "MP",]
Social_democrats <-master_df[master_df$parti == "S",]
Sweden_democrats <-master_df[master_df$parti == "SD",]
Left_party <-master_df[master_df$parti == "V",]






