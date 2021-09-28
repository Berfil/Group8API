library(testthat)
library(Group8API)

test_check("Group8API")

test_that("API connection is established correctly", {
  years <- c(2009:2020)
  i = 2009
  j = 1
  y = list()
  for (i in years) {
    api_url <- paste0("https://data.riksdagen.se/voteringlista/?rm=",i,"%2F",(i-1999),"&bet=&punkt=&valkrets=&rost=&iid=&sz=10000&utformat=json&gruppering=namn")
    x <- httr::GET(api_url)
    y <- httr::status_code(x)
    expect_output(print(y[1]), "200")
    j = j + 1
  }
})

test_that("Output of APi call is a list", {
  df_test <- get_riksdagen_api_data()
  expect_equal(class(df_test), "list")
})

test_that("API output is a nested list", {
  df_test <- get_riksdagen_api_data()
  for (i in 1:12) {
    expect_equal(class(df_test[i]), "list")
  }
})

test_that("API output nested list elements have data from years 2009:2020", {
  df_test <- get_riksdagen_api_data()
  correct_listnames <- c(2009:2020)
  for (i in 1:12) {
   expect_equal(names(df_test[i]), as.character(correct_listnames[i]))
  }
})

test_that("API output nested list elements have right column names", {
  df_test <- get_riksdagen_api_data()
  correct_colnames <- c("namn", "Ja", "Nej", "Frånvarande", "Avstår")
  for (i in 1:12) {
    expect_equal(names(df_test[[i]]), correct_colnames)
  }
})


