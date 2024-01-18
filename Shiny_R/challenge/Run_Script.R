install.packages(c("shiny", "tidyverse", "lubridate", "ggplot2", "here"))

library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(here)

if (rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
  if (!is.null(script_path)) {
    script_dir <- dirname(script_path)
    setwd(script_dir)
    getwd()
  }
}

canada_births <- read_csv("canada_births_1991_2022.csv")
nhl_player_births <- read_csv("nhl_player_births.csv")
nhl_rosters <- read_csv("nhl_rosters.csv")
nhl_teams <- read_csv("nhl_teams.csv")

ui <- fluidPage(
  titlePanel("Análisis de Nacimientos de Jugadores de Hockey Canadienses"),
  sidebarLayout(
    sidebarPanel(
      selectInput("teamSelection", "Seleccione un Equipo:", choices = unique(nhl_rosters$team_code))
    ),
    mainPanel(
      
      strong("¿Cual es la cantidad de jugadores nacidos por mes?"),
      plotOutput("birthMonthPlot"),
      br(), br(),
      
      strong("¿Cual es el porcentaje de Jugadores de la NHL en comparacion con los jugadores de Canada?"),
      plotOutput("comparisonPlot"),
      br(), br(),
      
      strong("¿Cuales son los meses en donde nacieron mas jugadores a lo largo de los años?"),
      plotOutput("lineTrendPlot"),
      br(), br(),
      
      strong("¿Porcentaje de Jugadores segun su nacionalidad?"),
      plotOutput("teamDiversityPlot"),
      br(), br(),
      
      strong("¿Cuantos Jugadores existe segun su estatura?"),
      plotOutput("heightDistributionPlot")
      
    )
  )
)

server <- function(input, output) {
  filtered_rosters <- reactive({
    nhl_rosters %>% filter(team_code == input$teamSelection)
  })
  
  filtered_player_births <- reactive({
    nhl_player_births %>% filter(player_id %in% filtered_rosters()$player_id)
  })
  
  output$birthMonthPlot <- renderPlot({
    filtered_player_births() %>%
      group_by(birth_month) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = birth_month, y = count)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribución de Meses de Nacimiento de Jugadores",
           x = "Mes de Nacimiento", y = "Número de Jugadores")
  })
  
  output$comparisonPlot <- renderPlot({
    total_births_per_month <- canada_births %>%
      group_by(month) %>%
      summarize(total_births = sum(births))
    
    nhl_births_per_month <- filtered_player_births() %>%
      group_by(birth_month) %>%
      summarize(nhl_births = n())
    
    comparison_df <- left_join(total_births_per_month, nhl_births_per_month, by = c("month" = "birth_month")) %>%
      mutate(Porcentaje_NHL = nhl_births / sum(nhl_births) * 100,
             Porcentaje_Canada = total_births / sum(total_births) * 100) %>%
      select(month, Porcentaje_NHL, Porcentaje_Canada) %>%
      pivot_longer(cols = c(Porcentaje_NHL, Porcentaje_Canada), 
                   names_to = "Tipos", values_to = "percentage")
    
    ggplot(comparison_df, aes(x = percentage, y = month, color = Tipos)) +
      geom_point() +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                position = position_nudge(x = 0.5), hjust = 0, size = 3) +
      scale_color_manual(values = c("red", "black")) +
      theme_minimal() +
      labs(title = "Comparación de Porcentajes de Nacimientos por Mes",
           x = "Porcentaje de Nacimientos (%)", y = "Mes de Nacimiento")
  })
  
  output$lineTrendPlot <- renderPlot({
    filtered_player_births() %>%
      group_by(birth_year, birth_month) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = birth_year, y = count, group = birth_month, color = as.factor(birth_month))) +
      geom_line() +
      labs(title = "Tendencia de Meses de Nacimiento a lo Largo de los Años",
           x = "Año de Nacimiento", y = "Número de Jugadores", color = "Mes de Nacimiento")
  })
  
  output$teamDiversityPlot <- renderPlot({
    filtered_data <- filtered_rosters() %>% count(birth_country)
    
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
