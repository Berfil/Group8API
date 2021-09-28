shinyFunction <- function(dataframe_1) {

#require(shiny)
shinyApp(
# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Votes by Parliment party"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a Party:",
                  choices = c("Center partiet", "Kristdemokraterna", "Liberalerna" ,"Moderaterna" , "Miljö partiet"  , "Socialdemokraterna", "Svergiedemokraterna" , "Vänster partiet")),

      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),

      # Output: HTML table with requested number of observations ----
      tableOutput("view") ,

      plotOutput("pie")

    )
  )
),


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Center partiet" = dataframe_1$Center_party,
           "Kristdemokraterna" = dataframe_1$Krist_demokraterna,
           "Liberalerna" = dataframe_1$Liberal_party,
           "Moderaterna" = dataframe_1$Moderat_party ,
           "Miljö partiet" = dataframe_1$Enviorment_party,
           "Socialdemokraterna" = dataframe_1$Social_democrats,
           "Svergiedemokraterna" = dataframe_1$Sweden_democrats,
           "Vänster partiet" = dataframe_1$Left_party
           )
  })


  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })

  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })

  output$pie <- renderPlot({
    dataset <- datasetInput()
    value <- values <- as.vector(c(sum(dataset[,2] , na.rm = T), sum(dataset[,3], na.rm = T) , sum(dataset[,4] , na.rm = T) , sum(dataset[,5], na.rm = T)))
    namn = as.vector(c("Ja" , "nej", "Frånvarande" , "Avstår"))
    pie_df <- data.frame(values , namn)


    pie_df <- pie_df %>%
      mutate(freq = round(values / sum(values), 3)) %>%
      arrange(desc(freq))  %>%
      mutate(labels = scales::percent(freq))

    ggplot(pie_df, aes(x = "", y = freq, fill = namn)) +
      geom_col() +
      geom_text(aes(label = labels),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y")




  })

}
)}
