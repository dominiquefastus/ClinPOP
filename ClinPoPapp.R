rm(list = ls())

# load libraries to calculate frequencies, plot the destributions, implent it in shiny and
# make a shiny dashboard with the interactive plots, as well as implement a data table
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(DT)

# read in the population dataset, created from the previous pre-processing, with the
# MasterID, Time, Continent, Sex and rsID column
population_plot <- read.csv('/Users/dominiquefastus/ClinPop/populations_readin.txt',
                            sep = "\t")

# also read in the clinar information for the found hits and only pathogenic significance
clinvar_table <- read.csv('/Users/dominiquefastus/ClinPop/clinvar.txt',
                          sep = "\t")

# Group by Continent and rsID, calculate frequencies every 1000 years and rename the column regarding
frequencies <- population_plot %>%
  group_by(Continent, rsID, Time %/% 1000) %>%
  summarize(Frequency = n()) %>%
  ungroup() %>%
  rename('Year' = `Time%/%1000`)

# Group the data by Continent, Sex, and rsID, and calculate the frequencies
# filter out rows with undefined sex
frequencies_sex <- population_plot %>%
  filter(Sex != "U") %>%
  group_by(Continent, Sex, rsID) %>%
  summarize(Frequency = n()) %>%
  ungroup()

# Subset the frequencies dataframe to only include the top 9 rsIDs by frequency
top_rsIDs <- frequencies_sex %>%
  arrange(desc(Frequency)) %>%
  head(9) %>%
  pull(rsID)

# create a frequency dataframe for only the top 10 of each seperated gender group
frequencies_top10 <- frequencies_sex %>%
  filter(rsID %in% top_rsIDs)

# Define UI for the shiny dashboard
ui <- dashboardPage(
  # create dashboard header in a black style with the tiltle
  skin = "black",
  dashboardHeader(
    title = "ClinPOP - ClinVAR marker visualization in populations",
    titleWidth = 600),
  
  # the sidebar to input user selection and provide further information about
  # the code, usage and more, using css tags to make it user frendlier
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
                choices = c("Europe & Asia", unique(population_plot$Continent))),
    
    # slider option to select the time range to plot
    sliderInput("range", "Time to show frequencies:",
                min = 0, max = 9,
                step = 1, # set step size to 1
                value = c(0,1)*1000), # set initial value in 1000s
    
    # div(style="display:inline-block; align-items: normal;",actionButton("action", label = "Apply")),
    # div(style="display:inline-block; align-items: normal;", downloadButton("downloadPDF", label = "Download plot as PDF")),
    
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
    tags$p("ClinPOP is a Shiny application designed to display the frequency of ClinVAR markers across different populations over time. Users can select a specific ClinVAR marker and population of interest, as well as specify a time range for the frequency data to be displayed.", 
           style = "border-left: 2px solid black; border-right: 2px solid black; padding: 0 10px; text-align: justify;"),
    tags$h4("Author:", style = "font-weight: bold;"),
    tags$p("This Shiny application was created by Dominique Fastus",
           style = "border-left: 2px solid black; border-right: 2px solid black; padding: 0 10px; text-align: justify;"),
    tags$h4("Contact:",  style = "font-weight: bold;"),
    tags$p("For any inquiries or feedback, please contact fastus.dominique@gmail.com",
           style = "border-left: 2px solid black; border-right: 2px solid black; padding: 0 10px; text-align: justify;"),
    tags$h4("Code:", style = "font-weight: bold;"),
    tags$p("The code and input data used to generate this Shiny mapping tool are available on ",
           style = "border-left: 2px solid black; border-right: 2px solid black; padding: 0 10px; text-align: justify; margin-bottom: 0;"),
    tags$a("Github", href = "https://github.com/dominiquefastus/ClinPOP", style = "font-weight: bold; font-size: 28px;")
  ),
  
  dashboardBody(
    # Main panel with plots, table and other box tools
    # can be easily updated or extended with further plots, etc.
    fluidRow(
      # create box for frequency over time in population plotting
      box(
        width = 12,
        height = 480,
        title = "Frequency of rsIDs in populations every 1000 years",
        plotlyOutput("line_plot")),
      
      # crate box for gender seperated top ClinVAR marker frrequency destribution
      box(
        title = "Frequency of the top 10 rsIDs in populations by gender",
        plotlyOutput("bar_plot")),
      
      # create box with the clinvar table
      box(
        title = "Clinvar marker Information",
        DTOutput("clinvar_table")
      ),
      
      # create box to show user what was selected
      box(
        height = 76,
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
      filter(rsID == input$ClinVAR_marker, Continent == input$Populations,
             Year >= input$range[1] & Year <= input$range[2])
  })
  
  # Create an interactive plot of the frequencies over time in years bp
  line_plot <- ggplot(frequencies, aes(x = Year, y = Frequency, color = rsID)) +
    geom_line() +
    facet_wrap(~Continent, nrow = 3) +
    # scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    labs(x = "Year",
         y = "Frequency") +
    theme_bw()
  
  # Create a stacked barplot showing the frequencies of each rsID in each Continent, separated by sex
  bar_plot <- ggplot(frequencies_top10, aes(x = Continent, y = Frequency, fill = rsID)) +
    geom_col(position = "dodge", color = "white") +
    facet_wrap(~Sex, ncol = 2) +
    coord_flip() + 
    scale_x_discrete(labels= c("W. Europe", "W. Asia", "S. Europe", "S. Asia", "S.E. Asia",
                               "N. Europe", "E. Europe", "E. Asia", "C. Asia")) +
    scale_fill_brewer(palette = 4) +
    labs(x = "Continent",
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

  # general idea for the plots: if the user select both countries, show the over destribution 
  # for all, else make it specific to the selected ClinVAR marker, population and time range
  
  # Render the plotly plot in the Shiny app
  output$line_plot <- renderPlotly({
    if (input$Populations == "Europe & Asia") {
      plotly_line_plot
    } 
    else {
      filtered_data() %>%
        plot_ly(x = ~Year, y = ~Frequency, color = ~Continent, type = 'scatter', mode = 'lines+markers') %>%
        layout(title = paste("Frequency of", input$ClinVAR_marker, "in", input$Populations, "over time"),
               xaxis = list(title = "Year"), yaxis = list(title = "Frequency"))
    }
  })
  
  # Create a plot of the frequencies of the top 9 rsIDs by gender
  output$bar_plot <- renderPlotly({
    if (input$Populations == "Europe & Asia") {
      plotly_bar_plot
  } 
    else {
      frequencies_sex %>%
        filter(rsID %in% top_rsIDs, Continent == input$Populations) %>%
        plot_ly(x = ~Frequency, y = ~rsID, color = ~Sex, type = 'bar', orientation = 'h') %>%
        layout( xaxis = list(title = "Frequency", tickangle = 0), yaxis = list(title = "rsID"),
               margin = list(l = 100, r = 20, t = 50, b = 50),
               plot_bgcolor = "white", paper_bgcolor = "white")
    }
  })
  
  # table shows either all clinvar markers or only the selected
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
  
  # Print selected input values to mainboard for the user
  output$text <- renderText({
    paste("You have selected", input$ClinVAR_marker, "in", input$Populations,
          "from time", input$range[1], "to", input$range[2], "bp")
  })
}


# Run app with defined ui and server structure
shinyApp(ui = ui, server = server)