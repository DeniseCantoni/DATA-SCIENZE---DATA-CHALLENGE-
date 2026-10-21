#terza data challenge - Fuel

library(shiny)
library(tidyverse)
library(gapminder)

# User interface
ui <- fluidPage(
  
  titlePanel("Shiny Gapminder"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #seleziona gli anni
      sliderInput(
        "Anni",
        "Year",
        min = min(gapminder$year),
        max = max(gapminder$year),
        value = min(gapminder$year),
        step=5
      ),
      
      #tendina per selezionare i continenti
      selectInput(
        "Continenti",
        "Continent",
        choices = c(sort(unique(as.character(gapminder$continent))), "All"),
        selected = "All",
      ),
      
      #scelta con più opzioni per asse x
      radioButtons(
        "asse_x",
        "X",
        choices = c("Life expectancy", "Population", "GDP per capita"),
        selected = "GDP per capita"
      ),
      
      #scelta con più opzioni per asse y
      radioButtons(
        "asse_y",
        "Y",
        choices = c("Life expectancy", "Population", "GDP per capita"),
        selected = "Life expectancy"
      )
    ),
    
    mainPanel(
      plotOutput("scatter")
    )
  )
)





# Server logic
server <- function(input, output, session) {
  
  # Reactive filtered dataset
  filtered_gapminder <- reactive({
    
    #filtro nel grafico solo i dati che mi servono relativi all'anno inserito dall'utente
    gapminder |>
      filter(
        year == input$Anni,
        # Se è "All" prende tutto, altrimenti filtra per continente
        input$Continenti == "All" | continent == input$Continenti 
      )
    
  })
  
  
  # Interactive scatterplot
  output$scatter <- renderPlot({
    
    data <- filtered_gapminder()

    p <- data |>
      ggplot(aes(
        x = case_when(
        input$asse_x == "Life expectancy" ~ lifeExp,
        input$asse_x == "Population" ~ pop,
        input$asse_x == "GDP per capita" ~ gdpPercap
        ), 
        y = case_when(
        input$asse_y == "Life expectancy" ~ lifeExp,
        input$asse_y == "Population" ~ pop,
        input$asse_y == "GDP per capita" ~ gdpPercap
        )
      )) +
    
      geom_point(
        alpha = 0.5,
        aes(color = country, size = pop),
        show.legend = FALSE
      ) +
      
      labs(
        x = case_when(
          input$asse_x == "Life expectancy" ~ "Life expectancy",
          input$asse_x == "Population" ~ "Population",
          input$asse_x == "GDP per capita" ~ "GDP per capita"
        ),
        y = case_when(
          input$asse_y == "Life expectancy" ~ "Life expectancy",
          input$asse_y == "Population" ~ "Population",
          input$asse_y == "GDP per capita" ~ "GDP per capita"
        )
      ) +
      
      scale_size(range = c(2, 20)) +
      theme_minimal() +
      scale_color_manual(values = country_colors)
    
    # Applica la scala logaritmica sull'asse X 
    if (input$asse_x %in% c("Population", "GDP per capita")) {
      p <- p + scale_x_log10()
    }
    
    # Applica la scala logaritmica sull'asse Y 
    if (input$asse_y %in% c("Population", "GDP per capita")) {
      p <- p + scale_y_log10()
    }
    
    p

  })
  
}

# Launch application
shinyApp(ui, server)