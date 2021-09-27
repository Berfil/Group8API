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
library(shiny)
library(stringr)



api_url <- paste("https://data.riksdagen.se/voteringlista/?rm=2020%2F21&bet=&punkt=&valkrets=&rost=&iid=&sz=10000&utformat=json&gruppering=namn")

raw_api_result <- httr::GET(api_url)

httr::headers(raw_api_result)

str(content(raw_api_result))


result_list <- content(raw_api_result)
str(result_list)
result_df <- dplyr::bind_rows(result_list$votering$votering)





a <- c("Hi", "Hi lol")

str_detect(a, "lol")








result_df$SVP_label1 = ifelse(grepl("(L)", result_df$namn, fixed = T), "(L)", "")
result_df$SVP_label2 = ifelse(grepl("(KD)", result_df$namn, fixed = T), "(KD)", "")
result_df$SVP_label3 = ifelse(grepl("(S)", result_df$namn, fixed = T), "(S)", "")
result_df$SVP_label4 = ifelse(grepl("(M)", result_df$namn, fixed = T), "(M)", "")
result_df$SVP_label5 = ifelse(grepl("(MP)", result_df$namn, fixed = T), "(MP)", "")
result_df$SVP_label6 = ifelse(grepl("(SD)", result_df$namn, fixed = T), "(SD)", "")
result_df$SVP_label7 = ifelse(grepl("(C)", result_df$namn, fixed = T), "(C)", "")
result_df$SVP_label8 = ifelse(grepl("(V)", result_df$namn, fixed = T), "(V)", "")
result_df$SVP_label = paste0(result_df$SVP_label1, result_df$SVP_label2, result_df$SVP_label3, result_df$SVP_label4, result_df$SVP_label5, result_df$SVP_label6, result_df$SVP_label7, result_df$SVP_label8)


ui <- fluidPage(
  sliderInput(inputId="ws", label="Choose bandwidth size", value=0.01, min=0.1, max=1),
  plotOutput("densPlot")
)

server <- function(input, output) {

  output$densPlot <- renderPlot({
    ggplot(result_df, aes(x=result_df$Ja , fill=result_df$SVP_label))+
      stat_density(alpha=0.8, bw=input$ws, position="identity")
  })
}
# Run the application
shinyApp(ui = ui, server = server)
