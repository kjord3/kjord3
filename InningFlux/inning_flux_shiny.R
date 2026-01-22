library(shiny)
library(tidyverse)
library(bslib)
library(DT)

# Ensure inning_flux has matchup & Team Momentum
inning_flux <- inning_flux %>%
  mutate(
    matchup = paste0(home, " vs. ", away),
    `Team Momentum` = ifelse(net_flux >= 0, home, away)
  )

# ----- UI -----
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  titlePanel("InningFlux: Game Momentum Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "selected_date",
        "Choose a Date:",
        choices = sort(unique(as.character(inning_flux$date))),
        selected = max(as.character(inning_flux$date))
      ),
      selectInput(
        "selected_matchup",
        "Choose a Game:",
        choices = NULL
      ),
      radioButtons(
        "flux_view",
        "Flux View:",
        choices = c("Net Flux", "Half-Inning Flux"),
        inline = TRUE
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Momentum Plot",
          br(),
          uiOutput("key_moment"),
          plotOutput("flux_plot", height = 500)
        ),
        tabPanel(
          "Inning Summary Table",
          DTOutput("summary_table")
        )
      )
    )
  )
)

# ----- Server -----
server <- function(input, output, session) {
  
  # Update matchups based on selected date
  observeEvent(input$selected_date, {
    matchups <- inning_flux %>%
      filter(as.character(date) == input$selected_date) %>%
      distinct(matchup) %>%
      arrange(matchup) %>%
      pull(matchup)
    
    updateSelectInput(
      session,
      "selected_matchup",
      choices = matchups,
      selected = matchups[length(matchups)]
    )
  }, ignoreInit = TRUE)
  
  # Reactive game data
  game_data <- reactive({
    req(input$selected_date, input$selected_matchup)
    
    df <- inning_flux %>%
      filter(
        as.character(date) == input$selected_date,
        matchup == input$selected_matchup
      ) %>%
      mutate(
        half_inning_label = paste0("Inning ", inning, " ", inn_half),
        half_inning_label = factor(half_inning_label, levels = unique(half_inning_label)),
        # Extra innings adjustment
        extra_innings = max(inning, na.rm = TRUE) > 9,
        is_final_half = row_number() == n(),
        home_extra_run = if_else(extra_innings & inn_half == "Bot" & home_score <= away_score & is_final_half, 1, 0),
        home_score = home_score + home_extra_run,
        flux_signed = if_else(batting_team == home, total_flux, -total_flux),
        net_flux = cumsum(flux_signed),
        impact = case_when(
          total_flux >= 8 ~ "🔥 Big Swing",
          total_flux <= -8 ~ "❄️ Momentum Kill",
          TRUE ~ ""
        ),
        score_diff = home_score - away_score
      )
    df
  })
  
  # Key moment UI
  output$key_moment <- renderUI({
    df <- game_data()
    req(nrow(df) > 0)
    
    key_play <- df %>%
      mutate(delta = net_flux - lag(net_flux, default = 0)) %>%
      slice_max(abs(delta), n = 1)
    
    wellPanel(
      style = "background-color:#2C3E50; color: #F1C40F; border-left: 5px solid #F39C12;",
      strong("Key Moment: "),
      paste0(
        key_play$batting_team, " ",
        ifelse(key_play$delta > 0, "+", ""),
        key_play$delta,
        " Flux in ",
        key_play$half_inning_label,
        ifelse(key_play$impact != "", paste0(" (", key_play$impact, ")"), "")
      )
    )
  })
  
  # Flux Plot
  output$flux_plot <- renderPlot({
    df <- game_data()
    req(nrow(df) > 0)
    
    home_team <- unique(df$home)
    away_team <- unique(df$away)
    game_date <- format(as.Date(unique(df$date)), "%B %d, %Y")
    final <- df %>% slice_tail(n = 1)
    
    if (input$flux_view == "Net Flux") {
      ggplot(df, aes(x = half_inning_label, y = net_flux)) +
        geom_line(group = 1, color = "gold", size = 1.2) +
        geom_point(aes(color = `Team Momentum`), size = 3) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_line(aes(y = score_diff), linetype = "dotted", color = "white", size = 0.8) +
        geom_point(data = df %>% slice_max(abs(net_flux - lag(net_flux, default = 0)), n = 1),
                   aes(x = half_inning_label, y = net_flux),
                   color = "red", size = 4, shape = 8) +
        scale_color_manual(values = c("#3498DB", "#E74C3C")) +
        labs(
          title = paste("Net Flux —", home_team, "vs.", away_team),
          subtitle = paste0(game_date, " | Final Score: ", home_team, " ", final$home_score,
                            " - ", final$away_score, " ", away_team),
          x = "Half-Inning",
          y = paste("Net Flux (", home_team, " perspective)", sep = "")
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else {
      ggplot(df, aes(x = half_inning_label, y = total_flux, fill = total_flux)) +
        geom_col() +
        geom_text(aes(label = ifelse(events %in% c("home_run","triple","strikeout"), events, "")),
                  vjust = -0.5, size = 3) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_fill_gradient2(low = "#E74C3C", mid = "grey20", high = "#2ECC71", midpoint = 0) +
        labs(
          title = paste("Half-Inning Flux —", home_team, "vs.", away_team),
          subtitle = game_date,
          x = "Half-Inning",
          y = "Flux"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Summary Table
  output$summary_table <- renderDT({
    df <- game_data()
    datatable(
      df %>%
        select(inning, inn_half, batting_team, total_flux, net_flux, home_score, away_score, impact),
      rownames = FALSE,
      options = list(pageLength = 12, autoWidth = TRUE)
    ) %>%
      formatStyle(
        "total_flux",
        backgroundColor = styleInterval(c(-7, 7), c("#E74C3C", NA, "#2ECC71")),
        color = "white"
      )
  })
}

# Run App
shinyApp(ui, server)