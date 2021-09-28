
clean_api_data <- function(api_data) {
  # Create the DF shiny will use
  df_1 <- api_data

  #add years column to each DF in list
  year_labels <- c(2009:2020)
  df_1 <- mapply(cbind, df_1, "year" = year_labels, SIMPLIFY = F)


  # Merge them all together
  master_df <- dplyr::bind_rows(df_1)

  master_df[,2:5] <- lapply(master_df[,2:5], as.numeric)

  # Create a new variable for party ID
  master_df$parti <- gsub(".*[(]([^)]+)[)].*", "\\1", master_df$namn)
  master_df$parti[master_df$parti == "fp"] <- "l"
  master_df$parti <- toupper(master_df$parti)

  cleaned_list <- list()

  cleaned_list$Center_party <- master_df[master_df$parti == "C",]
  cleaned_list$Krist_demokraterna <- master_df[master_df$parti == "KD",]
  cleaned_list$Liberal_party <- master_df[master_df$parti == "L",]
  cleaned_list$Moderat_party <-master_df[master_df$parti == "M",]
  cleaned_list$Enviorment_party <-master_df[master_df$parti == "MP",]
  cleaned_list$Social_democrats <-master_df[master_df$parti == "S",]
  cleaned_list$Sweden_democrats <-master_df[master_df$parti == "SD",]
  cleaned_list$Left_party <-master_df[master_df$parti == "V",]

  return(cleaned_list)
}




