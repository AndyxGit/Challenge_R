install.packages(c("shiny", "tidyverse", "lubridate", "ggplot2", "here"))
library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(here)

canada_births <- read_csv(here("canada_births_1991_2022.csv"))
nhl_player_births <- read_csv(here("nhl_player_births.csv"))
nhl_rosters <- read_csv(here("nhl_rosters.csv"))
nhl_teams <- read_csv(here("nhl_teams.csv"))

ui <- fluidPage(
  titlePanel("Análisis de Nacimientos de Jugadores de Hockey Canadienses"),
  sidebarLayout(
    sidebarPanel(
      selectInput("teamSelection", "Seleccione un Equipo:", 
                  choices = unique(nhl_rosters$team_code))
    ),
    mainPanel(
      plotOutput("birthMonthPlot"),
      plotOutput("comparisonPlot"),
      plotOutput("lineTrendPlot"),
      plotOutput("teamDiversityPlot"),
      plotOutput("heightDistributionPlot"),
      plotOutput("birthMonthHeightPlot")
    )
  )
)

server <- function(input, output) {
  filtered_rosters <- reactive({
    nhl_rosters %>%
      filter(team_code == input$teamSelection)
  })
  
  filtered_player_births <- reactive({
    nhl_player_births %>%
      filter(player_id %in% filtered_rosters()$player_id)
  })
  
  output$birthMonthPlot <- renderPlot({
    filtered_player_births() %>%
      group_by(birth_month) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = birth_month, y = count)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribución de Meses de Nacimiento de Jugadores",
           x = "Mes de Nacimiento",
           y = "Número de Jugadores")
  })
  
  output$comparisonPlot <- renderPlot({
    total_births_per_month <- canada_births %>%
      group_by(month) %>%
      summarize(total_births = sum(births))
    
    nhl_births_per_month <- filtered_player_births() %>%
      group_by(birth_month) %>%
      summarize(nhl_births = n())
    
    comparison_df <- left_join(total_births_per_month, nhl_births_per_month, by = c("month" = "birth_month"))
    
    ggplot(comparison_df, aes(x = month)) +
      geom_bar(aes(y = total_births), stat = "identity", fill = "blue", alpha = 0.5) +
      geom_bar(aes(y = nhl_births), stat = "identity", fill = "red", alpha = 0.5) +
      labs(title = "Comparación de Nacimientos en Canadá vs Jugadores de la NHL",
           x = "Mes",
           y = "Número de Nacimientos/Jugadores")
  })
  
  output$lineTrendPlot <- renderPlot({
    filtered_player_births() %>%
      group_by(birth_year, birth_month) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = birth_year, y = count, group = birth_month, color = as.factor(birth_month))) +
      geom_line() +
      labs(title = "Tendencia de Meses de Nacimiento a lo Largo de los Años",
           x = "Año de Nacimiento",
           y = "Número de Jugadores",
           color = "Mes de Nacimiento")
  })
  
  output$teamDiversityPlot <- renderPlot({
    filtered_data <- filtered_rosters() %>%
      count(birth_country)
    
    if (nrow(filtered_data) > 0) {
      ggplot(filtered_data, aes(x = "", y = n, fill = birth_country)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y") +
        labs(fill = "País", title = "Diversidad de Lugares de Nacimiento por Equipo")
    } else {
      print("No hay datos para mostrar")
    }
  })
  
  output$heightDistributionPlot <- renderPlot({
    if (nrow(filtered_rosters()) > 0) {
      filtered_rosters() %>%
        ggplot(aes(x = height_in_inches)) +
        geom_histogram(binwidth = 1, fill = "blue", color = "white") +
        labs(title = "Distribución de Alturas de Jugadores", x = "Altura (pulgadas)", y = "Frecuencia")
    } else {
      print("No hay datos para mostrar")
    }
  })
  
}

shinyApp(ui = ui, server = server)

