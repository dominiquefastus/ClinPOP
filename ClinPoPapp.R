rm(list = ls())

library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(DT)

population_plot <- read.csv('/Users/dominiquefastus/ClinPop/populations_readin.txt',
                            sep = "\t")

clinvar_table <- read.csv('/Users/dominiquefastus/ClinPop/clinvar.txt',
                          sep = "\t")

# Group by country and rsID, calculate frequencies every 1000 years
frequencies <- population_plot %>%
  group_by(Country, rsID, Time %/% 1000) %>%
  summarize(Frequency = n()) %>%
  ungroup() %>%
  rename('Year' = `Time%/%1000`)

# Group the data by country, sex, and rsID, and calculate the frequencies
frequencies_sex <- population_plot %>%
  filter(Sex != "U") %>%
  group_by(Country, Sex, rsID) %>%
  summarize(Frequency = n()) %>%
  ungroup()

# Subset the frequencies data frame to only include the top 10 rsIDs by frequency
top_rsIDs <- frequencies_sex %>%
  arrange(desc(Frequency)) %>%
  head(9) %>%
  pull(rsID)

frequencies_top10 <- frequencies_sex %>%
  filter(rsID %in% top_rsIDs)

# Define UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "ClinPOP - ClinVAR marker visualization in populations",
    titleWidth = 600),
  
  dashboardSidebar(
    width = 320,
    
    tags$div(
      # Add the centered text with borders
      tags$h2("Select",
              style = "font-weight:bold;")),
    
    # selection option for ClinVAR marker
    selectInput("ClinVAR_marker", "Select ClinVAR marker:",
                choices = c("all Clinvar Markers",unique(population_plot$rsID))),
    
    # selection option for populations
    selectInput("Populations", "Select Population:",
                choices = c("Europe & Asia", unique(population_plot$Country))),
    
    sliderInput("range", "Time to show frequencies:",
                min = 0, max = 9,
                step = 1, # set step size to 1
                value = c(0,1)*1000), # set initial value in 1000s
    
    div(style="display:inline-block; align-items: normal;",actionButton("action", label = "Apply")),
    div(style="display:inline-block; align-items: normal;",downloadButton("downloadPDF", label = "Download plot as PDF")),
    
    # Center the text in the middle of the sidebar
    tags$style(".sidebar {display: flex; flex-direction: column; justify-content: center; align-items: center;}"),
    tags$div(
      # Add the centered text with borders
      tags$h2("About",
              style = "font-weight:bold; border-left: 2px solid black; border-right: 2px solid black; padding: 0 10px; text-align: center;"),
      tags$p(""),
      tags$p("Last update: 14.03.2023 - Version 1.02", style = "font-weight:bold; border-left: 2px solid black; border-right: 2px solid black; padding: 0 10px; text-align: justify;"),
      tags$p("The first update contains an improved data visualization and faster loading of the page.",
             style = "border-left: 2px solid black; border-right: 2px solid black; padding: 0 10px; text-align: justify;")
    ),
    # Add the rest of the text
    tags$h4("Background:", style = "font-weight: bold;"),
    tags$p("ClinPOP is a Shiny application designed to display the frequency of ClinVAR markers across different populations over time. Users can select a specific ClinVAR marker and population of interest, as well as specify a time range for the frequency data.", 
           style = "border-left: 2px solid black; border-right: 2px solid black; padding: 0 10px; text-align: justify;"),
    tags$h4("Author:", style = "font-weight: bold;"),
    tags$p("This Shiny application was created by Dominique Fastus",
           style = "border-left: 2px solid black; border-right: 2px solid black; padding: 0 10px; text-align: justify;"),
    tags$h4("Contact:",  style = "font-weight: bold;"),
    tags$p("For any inquiries or feedback, please contact .......@lu.se.",
           style = "border-left: 2px solid black; border-right: 2px solid black; padding: 0 10px; text-align: justify;"),
    tags$h4("Code:", style = "font-weight: bold;"),
    tags$p("The code and input data used to generate this Shiny mapping tool are available on ",
           style = "border-left: 2px solid black; border-right: 2px solid black; padding: 0 10px; text-align: justify; margin-bottom: 0;"),
    tags$a("Github", href = "https://github.com/dominiquefastus/ClinPOP", style = "font-weight: bold; font-size: 28px;")
  ),
  
  dashboardBody(
    # Main panel with plot
    fluidRow(
      box(
        width = 12,
        height = 560,
        title = "Frequency of rsIDs in populations every 1000 years",
        plotlyOutput("line_plot")),
  
      box(
        title = "Frequency of the top 10 rsIDs in populations by gender",
        plotlyOutput("bar_plot")),
      
      box(
        title = "Clinvar marker Information",
        DTOutput("clinvar_table")
      ),
      
      box(
        textOutput("text")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter the population data based on user inputs
  filtered_data <- reactive({
    frequencies %>%
      filter(rsID == input$ClinVAR_marker, Country == input$Populations,
             Year >= input$range[1] & Year <= input$range[2])
  })
  
  # Create an interactive plot of the frequencies
  line_plot <- ggplot(frequencies, aes(x = Year, y = Frequency, color = rsID)) +
    geom_line() +
    facet_wrap(~Country, nrow = 3) +
    # scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    labs(x = "Year",
         y = "Frequency") +
    theme_bw()
  
  # Create a stacked barplot showing the frequencies of each rsID in each country, separated by sex
  bar_plot <- ggplot(frequencies_top10, aes(x = Country, y = Frequency, fill = rsID)) +
    geom_col(position = "dodge", color = "white") +
    facet_wrap(~Sex, ncol = 2) +
    coord_flip() + 
    scale_x_discrete(labels= c("S. Asia", "ME. & N. Africa", "Europe & C. Asia", "E. Asia & Pacifc")) +
    scale_fill_brewer(palette = 4) +
    labs(x = "Country",
         y = "Frequency") +
    theme_bw()
  
  # Make the plot interactive with plotly and apply black and white theme
  plotly_line_plot <- ggplotly(line_plot) %>%
    layout(plot_bgcolor = "white",
           paper_bgcolor = "white") 
  
  # Make the plot interactive with plotly and apply black and white theme
  plotly_bar_plot <- ggplotly(bar_plot) %>%
    layout(plot_bgcolor = "white",
           paper_bgcolor = "white")
  
  # Render the plotly plot in the Shiny app
  output$line_plot <- renderPlotly({
    if (input$Populations == "Europe & Asia") {
      plotly_line_plot
    } 
    else {
      filtered_data() %>%
        plot_ly(x = ~Year, y = ~Frequency, color = ~Country, type = 'scatter', mode = 'lines+markers') %>%
        layout(title = paste("Frequency of", input$ClinVAR_marker, "in", input$Populations, "over time"),
               xaxis = list(title = "Year"), yaxis = list(title = "Frequency"))
    }
  })
  
  # Create a plot of the frequencies of the top 10 rsIDs by gender
  output$bar_plot <- renderPlotly({
    if (input$Populations == "Europe & Asia") {
      plotly_bar_plot
  } 
    else {
      frequencies_sex %>%
        filter(rsID %in% top_rsIDs, Country == input$Populations) %>%
        plot_ly(x = ~Frequency, y = ~rsID, color = ~Sex, type = 'bar', orientation = 'h') %>%
        layout( xaxis = list(title = "Frequency", tickangle = 0), yaxis = list(title = "rsID"),
               margin = list(l = 100, r = 20, t = 50, b = 50),
               plot_bgcolor = "white", paper_bgcolor = "white")
    }
  })
  
  
  output$clinvar_table <- renderDT({
    if (input$ClinVAR_marker == "all Clinvar Markers") {
      clinvar_table %>%
        datatable(rownames = FALSE, options = list(pageLength = 6, dom = "tp")) 
    } else {
      clinvar_table %>%
        filter(rsID == input$ClinVAR_marker) %>%
        distinct() %>%
        datatable(rownames = FALSE, options = list(pageLength = 6, dom = "t")) 
    }
  })
  
  # Print selected input values to sidebar
  output$text <- renderText({
    paste("You have selected", input$ClinVAR_marker, "in", input$Populations,
          "from time", input$range[1], "to", input$range[2], "bp")
  })
  
  # Download plot as PDF
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste("ClinPOP_", input$Populations, input$ClinVAR_marker, ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      if (!is.null(filtered_data())) {
        filtered_data() %>%
          group_by(Country, rsID, Time %/% 1000) %>%
          summarize(Frequency = n()) %>%
          ungroup() %>%
          rename('Year' = `Time%/%1000`) %>%
          plot_ly(x = ~Year, y = ~Frequency, color = ~Country, type = 'scatter', mode = 'lines+markers') %>%
          layout(title = paste("Frequency of", input$ClinVAR_marker, "in", input$Populations, "over time"),
                 xaxis = list(title = "Year"), yaxis = list(title = "Frequency"))
      } else {
        plot_ly() %>%
          layout(title = "No data available for selected inputs")
      }
      dev.off()
    }
  )
}


# Run app
shinyApp(ui = ui, server = server)