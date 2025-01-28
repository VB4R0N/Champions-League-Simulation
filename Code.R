#### Load relevant packages (install them if necessary) ----
library(tidyverse)
library(rvest)
library(httr)
library(plotly)
library(magick)
library(grid)
library(shiny)
library(DT)
#### Import data sets ----
setwd("define_you_working_directory_here")
scores<-read_csv("./scores.csv")
probs<-read_csv("./probs.csv")
table<-read_csv("./table.csv")

#### Subset scores into home, draw, away ----
scores_home <- scores %>%
  filter(home>away) %>% 
  mutate(perc=frequency/sum(frequency))
scores_draw <- scores %>%
  filter(home==away) %>% 
  mutate(perc=frequency/sum(frequency))
scores_away <- scores %>%
  filter(home<away) %>% 
  mutate(perc=frequency/sum(frequency))

#### Monte Carlo Simulation ----
results <- matrix(NA, nrow = 50000, ncol = nrow(table))
all_simulations <- data.frame()

for (sim in 1:nrow(results)) {
  # Re-initialize the table
  table_sim <- table
  pts_home <- rep(NA, nrow(probs))
  pts_away <- rep(NA, nrow(probs))
  
  # Random point generation and goal difference calculation
  for (i in 1:nrow(probs)) {
    pts_home[i] <- sample(x = c(3, 1, 0), size = 1, prob = probs[i, 4:6])
    pts_away[i] <- if (pts_home[i] == 0) {
      3
    } else if (pts_home[i] == 1) {
      1
    } else {
      0
    }
    
    # Generate random result based on points
    if (pts_home[i] == 3) {
      finalscore <- sample(scores_home$fulltime, size = 1, prob = scores_home$perc)
    } else if (pts_home[i] == 1) {
      finalscore <- sample(scores_draw$fulltime, size = 1, prob = scores_draw$perc)
    } else {
      finalscore <- sample(scores_away$fulltime, size = 1, prob = scores_away$perc)
    }
    
    # Extract goals from final score (e.g., "2 - 1")
    goals_home <- as.numeric(sub("-.*", "", finalscore))
    goals_away <- as.numeric(sub(".*-", "", finalscore))
    
    # Update points and goals in table_sim
    table_sim$Pts[table_sim$club == probs$home[i]] <- 
      table_sim$Pts[table_sim$club == probs$home[i]] + pts_home[i]
    table_sim$Pts[table_sim$club == probs$away[i]] <- 
      table_sim$Pts[table_sim$club == probs$away[i]] + pts_away[i]
    
    table_sim$F[table_sim$club == probs$home[i]] <- 
      table_sim$F[table_sim$club == probs$home[i]] + goals_home
    table_sim$A[table_sim$club == probs$home[i]] <- 
      table_sim$A[table_sim$club == probs$home[i]] + goals_away
    
    table_sim$F[table_sim$club == probs$away[i]] <- 
      table_sim$F[table_sim$club == probs$away[i]] + goals_away
    table_sim$A[table_sim$club == probs$away[i]] <- 
      table_sim$A[table_sim$club == probs$away[i]] + goals_home
  }
  
  # Rank teams by Points, Goal Difference, and Goals Scored
  table_sim <- table_sim %>%
    arrange(desc(Pts), desc(F - A), desc(F)) %>%
    mutate(pos = 1:36, simulation = sim) 
  
  # Accumulate the simulation results in all_simulations dataframe
  all_simulations <- rbind(all_simulations, table_sim)
  
  # Store the sorted club names in results to track ranking positions
  results[sim, ] <- table_sim$club
}

results<- data.frame(results) %>% set_names(1:36)
# Data Cleaning & Make sure it's properly ordered
all_simulations <- all_simulations %>%
  mutate(goaldiff = F - A) %>% 
  group_by(simulation) %>% 
  arrange(simulation, desc(Pts), desc(goaldiff), desc(F)) %>% 
  mutate(pos = row_number()) %>%
  ungroup()
#### Plots of Probabilities ----
simulation_summary <- all_simulations %>%
  group_by(club) %>%
  summarise(
    `xP`= round(mean(Pts), 1),
    `1st` = sum(pos == 1)/n_distinct(simulation),
    `Top 8` = sum(pos <= 8)/n_distinct(simulation),
    `Play-off` = sum(pos >= 9 & pos <= 24)/n_distinct(simulation),
    `Eliminated` = sum(pos > 24)/n_distinct(simulation)
  ) %>% 
  arrange(desc(xP))

#### Plot actual probabilities ----
# long format
simulation_summary_long <- simulation_summary %>%
  pivot_longer(cols = c(`1st`, `Top 8`, `Play-off`, `Eliminated`), names_to = "position", values_to = "probability")

# Order
position_order <- c("1st", "Top 8", "Play-off", "Eliminated")

# Split data into two groups
top_teams <- simulation_summary_long %>%
  filter(club %in% c("Liverpool", "Barcelona", "Inter Milan", "Arsenal", "Atlético Madrid", "AC Milan", 
                     "Bayer 04 Leverkusen", "Aston Villa", "Bayern Munich", "Atalanta", "Lille", "Real Madrid", 
                     "Borussia Dortmund", "Feyenoord", "Monaco", "Brest", "Juventus", "Celtic"))

other_teams <- simulation_summary_long %>%
  filter(!club %in% top_teams$club)

# Custom heatmap function
generate_heatmap <- function(data, title) {
  ggplot(data, aes(x = position, y = reorder(club, xP), fill = probability)) +
    geom_tile(color = "white", size = 0.3) +  
    geom_text(aes(label = scales::percent(probability, accuracy = 0.1)), 
              color = "black", size = 7, fontface = "bold") +  
    scale_fill_gradient(low = "white", high = "indianred3", labels = scales::percent) +
    scale_x_discrete(limits = position_order) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(hjust = 1, size = 14, face = "bold"),  
      axis.text.y = element_text(size = 17, face = "bold"),  
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
      axis.ticks.y = element_blank()
    ) +
    labs(
      title = title,
      fill = "Probability"
    )
}


# Generate both plots
plot_top_teams <- generate_heatmap(top_teams, "Top Teams League Phase")
plot_other_teams <- generate_heatmap(other_teams, "Other Teams League Phase")


# Dashboard ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      label, .control-label { 
        font-size: 18px !important;  /* Increase label font size */
      }
      .shiny-input-container { 
        font-size: 16px !important;  /* Increase input font size */
      }
      .irs-grid-text { 
        font-size: 14px !important;  /* Increase slider grid number size */
      }
      .irs-single, .irs-bar-edge, .irs-bar {
        font-size: 16px !important;  /* Increase slider selection font */
      }
    "))
  ),
  
  titlePanel("Football Simulation Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select Team:", choices = sort(unique(all_simulations$club))),
      sliderInput("points", "Filter by Points:", 
                  min = min(all_simulations$Pts), 
                  max = max(all_simulations$Pts), 
                  value = c(min(all_simulations$Pts), max(all_simulations$Pts)), 
                  step = 1),
      sliderInput("simulation", "Filter by Simulation:", 
                  min = min(all_simulations$simulation), 
                  max = max(all_simulations$simulation), 
                  value = c(min(all_simulations$simulation), max(all_simulations$simulation)), 
                  step = 1)
    ),
    mainPanel(
      plotOutput("rankPlot", height = "600px", width = "100%"),
      DT::dataTableOutput("summaryTable"),
      verbatimTextOutput("summaryStats")
    )
  )
)


server <- function(input, output, session) {
  filtered_data <- reactive({
    all_simulations %>%
      filter(club == input$team, 
             Pts >= input$points[1], 
             Pts <= input$points[2],
             simulation >= input$simulation[1],
             simulation <= input$simulation[2])
  })
  
  output$rankPlot <- renderPlot({
    df <- filtered_data()
    total_simulations <- nrow(df)
    
    ggplot(df, aes(x = pos, y = ..count../total_simulations)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "white", alpha = 0.7) +
      labs(title = paste("Relative Position Distribution for", input$team), 
           x = "Position", y = "Relative Frequency") +
      scale_y_continuous(labels = scales::percent) +  
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),  
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18),  
        axis.text.x = element_text(size = 16),   
        axis.text.y = element_text(size = 16)    
      )
  })
  
  output$summaryTable <- DT::renderDataTable({
    df <- filtered_data()
    total_simulations <- nrow(df)
    
    summary_data <- df %>%
      group_by(pos) %>%
      summarise(Frequency = n(), 
                Relative_Frequency = n() / total_simulations) %>%
      arrange(pos)
    
    DT::datatable(summary_data, 
                  options = list(pageLength = 10, 
                                 lengthMenu = c(5, 10, 15, 20)),
                  rownames = FALSE)
  })
  
  output$summaryStats <- renderPrint({
    summary_stats <- filtered_data() %>%
      summarise(
        Mean_Points = mean(Pts),
        Median_Points = median(Pts),
        Min_Points = min(Pts),
        Max_Points = max(Pts),
        Total_Simulations = n()
      )
    print(summary_stats)
  })
}

shinyApp(ui = ui, server = server)