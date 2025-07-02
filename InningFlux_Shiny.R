library(shiny)
library(tidyverse)
library(ggthemes)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  titlePanel("InningFlux: Game Momentum Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_date", "Choose a Date:",
                  choices = unique(inning_flux$date),
                  selected = unique(inning_flux$date)[1]),
      
      selectInput("selected_matchup", "Choose a Matchup:",
                  choices = NULL)  # We'll populate this dynamically
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Net Flux Plot",
                 plotOutput("flux_plot")
        ),
        tabPanel("Inning Summary Table",
                 tableOutput("summary_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Update matchup options based on selected date
  observeEvent(input$selected_date, {
    available_matchups <- inning_flux %>%
      filter(date == input$selected_date) %>%
      distinct(matchup) %>%
      pull(matchup)
    
    updateSelectInput(session, "selected_matchup", choices = available_matchups)
  })
  
  # Reactive: filter for the selected game
  game_data <- reactive({
    req(input$selected_date, input$selected_matchup)
    
    inning_flux %>%
      filter(date == input$selected_date, matchup == input$selected_matchup) %>%
      mutate(half_inning = paste("Inning", inning, inn_half))
  })
  
  # Net Flux Plot
  output$flux_plot <- renderPlot({
    df <- game_data()
    req(nrow(df) > 0)
    
    home_team <- unique(df$home)
    away_team <- unique(df$away)
    game_date <- format(unique(df$date), "%B %d, %Y")
    final_scores <- df %>% slice_tail(n = 1)
    
    ggplot(df, aes(x = factor(half_inning, levels = unique(half_inning)), y = net_flux)) +
      geom_line(group = 1, color = "gold", size = 1.2) +
      geom_point(size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        title = paste("Net Flux —", home_team, "vs.", away_team, "(", game_date, ")"),
        subtitle = paste0("Final Score: ", home_team, " ", final_scores$home_score,
                          " - ", final_scores$away_score, " ", away_team),
        x = "Half-Inning",
        y = paste("Net Flux (", home_team, " Perspective)")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Table output
  output$summary_table <- renderTable({
    game_data() %>%
      select(inning, inn_half, batting_team, total_flux, home, away,
             home_score, away_score, net_flux, score)
  })
}

shinyApp(ui, server)
