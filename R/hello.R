# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(httr)
library(dplyr)

api_url <- paste("https://data.riksdagen.se/voteringlista/?rm=2020%2F21&bet=&punkt=&valkrets=&rost=&iid=&sz=10000&utformat=json&gruppering=namn")

raw_api_result <- httr::GET(api_url)

httr::headers(raw_api_result)

str(content(raw_api_result))


result_list <- content(raw_api_result)
str(result_list)
result_df <- dplyr::bind_rows(result_list$votering$votering)
