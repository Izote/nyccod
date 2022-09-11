library(shiny)
library(shinyWidgets)
library(tidyverse)

clean_string <- function(x) {
  x %>% 
    str_remove_all("\\(.+\\)|Non-Hispanic|Race/ Ethnicity|Not Stated/") %>%
    str_trim()
}

columns <- c("year", "cause", "sex", "race", "deaths", "death_rate", "aa_rate")
race_lvl <- c("Asian and Pacific Islander", "Black", 
              "Hispanic", "White", "Other", "Unknown")

df <- read_csv(
  "nyccod.csv", col_names = columns, na = c("", "NA", "."), skip = 1,
  show_col_types = FALSE
  ) %>%
  mutate(
    cause = str_replace_all(clean_string(cause), "Posion", "Poison")
    ) %>% 
  drop_na()

year_value <- c(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE))
cause_lvl <- unique(df$cause)[order(unique(df$cause))]

df <- df %>% 
  mutate(
    cause = factor(cause, levels = cause_lvl, ordered = TRUE),
    race = factor(clean_string(race), levels = race_lvl, ordered = TRUE)
    )

ui <- fluidPage(
  titlePanel("Exploring the Leading Causes of Death in NYC"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "yearSlider", 
        label = "Year(s)", 
        min = year_value[1], max = year_value[2], value = year_value, sep = ""
      ),
      pickerInput(
        inputId = "racePick", label = "Race", 
        choices = race_lvl, selected = race_lvl, multiple = TRUE,
        options = list(`selected-text-format` = "count")
        ),
      pickerInput(
        inputId = "causePick", label = "Cause(s) of Death",
        choices = cause_lvl, selected = c("Assault", "Accidents Except Drug Poisoning", "Intentional Self-Harm"), multiple = TRUE,
        options = list(`selected-text-format`= "count")
        )
    ),
    mainPanel(
      plotOutput("barplot"),
      plotOutput("lineplot")
      ),
    fluid = FALSE
  )
)

server <- function(input, output) {
  output$barplot <- renderPlot({
    df %>% 
      filter(
        input$yearSlider[1] <= year & year <= input$yearSlider[2] & 
          cause %in% input$causePick & race %in% input$racePick
        ) %>% 
      group_by(race, cause) %>% 
      summarize(across("deaths", sum, na.rm = TRUE), .groups = "drop") %>% 
      ggplot(aes(x = deaths, y = forcats::fct_rev(cause), fill = race)) +
      geom_col(position = position_dodge()) +
      theme(
        axis.title = element_blank()
      )
  })
  
  output$lineplot <- renderPlot({
    df %>% 
      filter(
        input$yearSlider[1] <= year & year <= input$yearSlider[2] & 
          cause %in% input$causePick & race %in% input$racePick
      ) %>% 
      group_by(year, race, cause) %>% 
      summarize(across("deaths", sum, na.rm = TRUE), .groups = "drop") %>% 
      ggplot(aes(year, deaths, color = race, group = race)) +
      geom_line(alpha = 0.6, size = 1.5, linetype = 5) +
      geom_point(alpha = 0.6, size = 3) +
      facet_wrap("cause", ncol = 1, scales = "free_y") +
      scale_x_continuous(breaks = seq(year_value[1], year_value[2])) +
      theme(
        axis.title = element_blank(),
        legend.position = "none"
        )
  })
}

shinyApp(ui = ui, server = server)