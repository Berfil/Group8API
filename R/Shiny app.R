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
      tableOutput("view")

    )
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Center partiet" = Center_party,
           "Kristdemokraterna" = Krist_demokraterna,
           "Liberalerna" = Liberal_party,
           "Moderaterna" = Moderat_party ,
           "Miljö partiet" = Enviorment_party,
           "Socialdemokraterna" = Social_democrats,
           "Svergiedemokraterna" = Sweden_democrats,
           "Vänster partiet" =Left_party
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

}

shinyApp(ui = ui, server = server)
