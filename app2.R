library(shiny)
library(tidyverse)
library(thematic)
library(bslib)
library(DT)
library(plotly)
library(kableExtra)

theme_set(theme_light())

thematic_shiny()
theme_update(text = element_text(size = 17))

df <- read_rds("df_all_countries.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty", font_scale = 1.3),

  # Application title
  titlePanel("Growth Decomposition"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      p("Text here"),
      selectizeInput("country",
                     "Select country",
                     choices = unique(df$country),
                     selected = "Italy",
                     multiple = F
      ),
      sliderInput("alpha",
                  "Capital share of income (alpha):",
                  min = 0.2,
                  max = 0.4,
                  value = .3,
                  step = .01
      ),
      sliderInput("time_chunk",
                  "How many years to average over:",
                  min = 1,
                  max = 15,
                  value = 6,
                  step = 1
      ),
      sliderInput("range_1",
                  "First date range:",
                  min = 1950,
                  max = 2018,
                  value = c(1964, 1977),
                  step = 1
      ),
      sliderInput("range_2",
                  "Second date range:",
                  min = 1950,
                  max = 2018,
                  value = c(1990, 2005),
                  step = 1
      ),
      sliderInput("range_3",
                  "Third date range:",
                  min = 1950,
                  max = 2018,
                  value = c(2006, 2018),
                  step = 1
      ),

    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Growth rates",
                           selectizeInput("evol_opts",
                                          "Choose indicators:",
                                          choices = unique(df$indicator),
                                          selected = c("GDP (volume)", "Total Hours", "Capital Stock (K)", "Human Capital Stock (HK)"),
                                          multiple = T),
                           plotlyOutput("evolution_plot"),
                           # tableOutput("evolution_table")
                           ),
                  tabPanel("Decomposition for Crude TFP",
                           h2("GDP Method"),
                           plotlyOutput("contribs_plot"),
                           h2("Labour Prductivity Method"),
                           plotlyOutput("contribs_plot_2")),
                  tabPanel("Decomposition of Capital Growth",
                           h2("Evolution of ICT and Non-ICT Capital"),
                           plotlyOutput("capital_decomp")),
                  tabPanel("Period Decomposition",
                           h2("GDP Method"),
                           plotlyOutput("range_plot_1"),
                           h2("Labour Prductivity Method"),
                           plotlyOutput("range_plot_2")),
                  tabPanel("Period Decomposition 2",
                           h2("GDP Method"),
                           plotlyOutput("range_plot_3")))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$evolution_plot <- renderPlotly({

    g <- df_time_chunks() %>%
      filter(indicator %in% input$evol_opts,
             !is.na(compound_gr)) %>%
      ggplot(aes(year, compound_gr, colour = indicator)) +
      geom_point() +
      geom_line() +
      geom_hline(yintercept = 0, lty = 2) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(y = "Compound Growth Rate",
           x = NULL,
           colour = "Indicator")

    ggplotly(g)
  })

  output$contribs_plot <- renderPlotly({
    f <- df_calcs() %>%
      pivot_wider(names_from = name) %>%
      pivot_longer(-c(year, gdp_volume)) %>%
      filter(name %in% c("capital_contribution", "labour_contribution", "crude_tfp_via_gdp", "gdp_volume")) %>%
      ggplot() +
      geom_col(aes(year, value, fill = name),
               position = "dodge",
               alpha = .8) +
      geom_point(aes(year, gdp_volume,colour = "GDP Growth")) +
      geom_line(aes(year, gdp_volume,colour = "GDP Growth")) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x = NULL,
           y = "GDP growth rate\n contribution to growth",
           fill = "Contribution to GDP Growth",
           colour = "Level of GDP Growth")

    ggplotly(f)
  })

  output$contribs_plot_2 <- renderPlotly({
    f <- df_calcs() %>%
      pivot_wider(names_from = name) %>%
      pivot_longer(-c(year, gdp_volume)) %>%
      filter(name %in% c("gdp_volume", "labour_productivity",  "crude_tfp_via_labour_productivity", "contribution_of_capital_deepening")) %>%
      ggplot() +
      geom_col(aes(year, value, fill = name),
               position = "dodge",
               alpha = .8) +
      geom_point(aes(year, gdp_volume,colour = "GDP Growth")) +
      geom_line(aes(year, gdp_volume,colour = "GDP Growth")) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x = NULL,
           y = "GDP growth rate\n contribution to growth",
           fill = "Contribution to GDP Growth",
           colour = "Level of GDP Growth")

    ggplotly(f)
  })

  output$capital_decomp <- renderPlotly({
    h <- df_capital() %>%
      mutate(
        non_ict_capital_contribution = non_ict_capital_volume * non_ict_capital_share,
        ict_capital_contribution = ict_capital_volume * ict_capital_share
      ) %>%
      pivot_longer(-time_chunk) %>%
      mutate(
        sector = ifelse(str_detect(name, "non"), "Non-ICT", "ICT"),
        type = case_when(
          str_detect(name, "contribution") ~ "Contribution to GDP growth",
          str_detect(name, "share") ~ "Share of capital",
          TRUE ~ "Growth of capital"
        )
      ) %>%
      ggplot(aes(time_chunk, value, colour = type)) +
      geom_point() +
      geom_line() +
      facet_wrap(~sector) +
      labs(x = NULL,
           y = NULL,
           colour = NULL)

    ggplotly(h)
  })

  output$range_plot_1 <- renderPlotly({
    h <- df_ranges() %>%
      filter(name %in% c("capital_contribution", "labour_contribution", "crude_tfp_via_gdp", "gdp_volume")) %>%
      pivot_wider(names_from = name) %>%
      pivot_longer(-c(range, gdp_volume)) %>%
      ggplot() +
      geom_col(aes(range, value, fill = name), alpha = .5) +
      geom_point(aes(range, gdp_volume, colour = "GDP Growth")) +
      # geom_line(aes(as.numeric(range), gdp_volume, colour = "GDP Growth")) +
      labs(x = NULL,
           y = "GDP growth rate\n contribution to growth",
           colour = NULL,
           fill = NULL)

    ggplotly(h)
  })

  output$range_plot_2 <- renderPlotly({
    h <- df_ranges() %>%
      filter(name %in% c("gdp_volume", "labour_productivity",  "crude_tfp_via_labour_productivity", "contribution_of_capital_deepening")) %>%
      pivot_wider(names_from = name) %>%
      pivot_longer(-c(range, gdp_volume)) %>%
      ggplot() +
      geom_col(aes(range, value, fill = name), alpha = .5) +
      geom_point(aes(range, gdp_volume, colour = "GDP Growth")) +
      # geom_line(aes(as.numeric(range), gdp_volume, colour = "GDP Growth")) +
      labs(x = NULL,
           y = "GDP growth rate\n contribution to growth",
           colour = NULL,
           fill = NULL)

    ggplotly(h)
  })

  output$range_plot_3 <- renderPlotly({
    h <- df_ranges() %>%
      filter(name %in% c("gdp_volume", "capital_contribution", "contribution_of_human_capital", "labour_augmented_tfp_via_gdp")) %>%
      pivot_wider(names_from = name) %>%
      pivot_longer(-c(range, gdp_volume)) %>%
      ggplot() +
      geom_col(aes(range, value, fill = name), alpha = .5) +
      geom_point(aes(range, gdp_volume, colour = "GDP Growth")) +
      # geom_line(aes(as.numeric(range), gdp_volume, colour = "GDP Growth")) +
      labs(x = NULL,
           y = "GDP growth rate\n contribution to growth",
           colour = NULL,
           fill = NULL)

    ggplotly(h)
  })

  df_time_chunks <- reactive({

    tc <- input$time_chunk

    df %>%
      filter(country == input$country) %>%
      group_by(indicator) %>%
      mutate(time_chunk = year - year %% tc) %>%
      ungroup() %>%
      group_by(indicator, time_chunk) %>%
      mutate(index = row_number()) %>%
      filter(index %in% c(1, tc)) %>%
      mutate(
        ratio_value = value / lag(value),
        one_over_change_year = 1 / (year - lag(year)),
        compound_gr = ratio_value^(one_over_change_year) - 1
      ) %>%
      ungroup()
  })

  df_calcs <- reactive({

    a <- input$alpha

    b <- 1 - a


    df_time_chunks() %>%
      filter(
        indicator %in% c("Human Capital Stock (HK)", "GDP (volume)", "Total Hours", "Capital Stock (K)", "Labour Productivity"),
        !is.na(compound_gr)
      ) %>%
      select(indicator, year, compound_gr) %>%
      pivot_wider(names_from = indicator, values_from = compound_gr) %>%
      janitor::clean_names() %>%
      mutate(
        capital_contribution = a * capital_stock_k,
        labour_contribution = b * total_hours,
        contribution_of_human_capital = human_capital_stock_hk * b,
        k_per_worker_growth = capital_stock_k - total_hours,
        contribution_of_capital_deepening = k_per_worker_growth * a,
        crude_tfp_via_labour_productivity = labour_productivity - contribution_of_capital_deepening,
        crude_tfp_via_gdp = gdp_volume - capital_contribution - labour_contribution,
        labour_augmented_tfp_via_gdp = crude_tfp_via_gdp - contribution_of_human_capital,
        labour_augmented_tfp_via_labour_productivity = crude_tfp_via_labour_productivity - contribution_of_human_capital
      ) %>%
      pivot_longer(-year)
  })

  df_capital <- reactive({
    tc <- input$time_chunk

    df %>%
      filter(country == input$country) %>%
      group_by(indicator) %>%
      mutate(time_chunk = year - year %% tc) %>%
      ungroup() %>%
      group_by(indicator, time_chunk) %>%
      mutate(mean_in_tc = mean(value, na.rm = T)) %>%
      ungroup() %>%
      filter(
        str_detect(indicator, "ICT Capital"),
        !is.na(mean_in_tc)
      ) %>%
      distinct(time_chunk, mean_in_tc, .keep_all = T) %>%
      select(indicator, time_chunk, mean_in_tc) %>%
      inner_join(df_time_chunks() %>%
        filter(
          !is.na(compound_gr),
          str_detect(indicator, "ICT Capital")
        ) %>%
        select(indicator, time_chunk, compound_gr),
      by = c("indicator", "time_chunk")
      ) %>%
      mutate(compound_gr = case_when(
        str_detect(indicator, "Volume") ~ compound_gr * 100,
        TRUE ~ compound_gr
      )) %>%
      mutate(mean_in_tc = case_when(
        str_detect(indicator, "Share") ~ mean_in_tc / 100,
        TRUE ~ mean_in_tc
      )) %>%
      mutate(value = case_when(
        str_detect(indicator, "Share") ~ mean_in_tc,
        TRUE ~ compound_gr
      )) %>%
      select(indicator, time_chunk, value) %>%
      pivot_wider(names_from = indicator) %>%
      janitor::clean_names()
  })


  df_ranges <- reactive({

    a <- input$alpha

    b <- 1 - a

    df_range_1 <- df %>%
      filter(
        country == input$country,
        year %in% c(input$range_1[1], input$range_1[2])
      ) %>%
      group_by(indicator) %>%
      mutate(
        ratio_value = value / lag(value),
        one_over_change_year = 1 / (year - lag(year)),
        compound_gr = (ratio_value^(one_over_change_year) - 1) * 100,
        range = paste0(
          input$range_1[1],
          "-",
          input$range_1[2]
        )
      ) %>%
      ungroup() %>%
      select(indicator, year, value, country, compound_gr, range) %>%
      filter(!is.na(compound_gr)) %>%
      select(indicator, range, compound_gr) %>%
      pivot_wider(names_from = indicator, values_from = compound_gr) %>%
      janitor::clean_names() %>%
      mutate(
        capital_contribution = a * capital_stock_k,
        labour_contribution = b * total_hours,
        contribution_of_human_capital = human_capital_stock_hk * b,
        k_per_worker_growth = capital_stock_k - total_hours,
        contribution_of_capital_deepening = k_per_worker_growth * a,
        crude_tfp_via_labour_productivity = labour_productivity - contribution_of_capital_deepening,
        crude_tfp_via_gdp = gdp_volume - capital_contribution - labour_contribution,
        labour_augmented_tfp_via_gdp = crude_tfp_via_gdp - contribution_of_human_capital,
        labour_augmented_tfp_via_labour_productivity = crude_tfp_via_labour_productivity - contribution_of_human_capital) %>%
      pivot_longer(-range)

    df_range_2 <- df %>%
      filter(
        country == input$country,
        year %in% c(input$range_2[1], input$range_2[2])
      ) %>%
      group_by(indicator) %>%
      mutate(
        ratio_value = value / lag(value),
        one_over_change_year = 1 / (year - lag(year)),
        compound_gr = (ratio_value^(one_over_change_year) - 1) * 100,
        range = paste0(
          input$range_2[1],
          "-",
          input$range_2[2]
        )
      ) %>%
      ungroup() %>%
      select(indicator, year, value, country, compound_gr, range) %>%
      filter(!is.na(compound_gr)) %>%
      select(indicator, range, compound_gr) %>%
      pivot_wider(names_from = indicator, values_from = compound_gr) %>%
      janitor::clean_names() %>%
      mutate(
        capital_contribution = a * capital_stock_k,
        labour_contribution = b * total_hours,
        contribution_of_human_capital = human_capital_stock_hk * b,
        k_per_worker_growth = capital_stock_k - total_hours,
        contribution_of_capital_deepening = k_per_worker_growth * a,
        crude_tfp_via_labour_productivity = labour_productivity - contribution_of_capital_deepening,
        crude_tfp_via_gdp = gdp_volume - capital_contribution - labour_contribution,
        labour_augmented_tfp_via_gdp = crude_tfp_via_gdp - contribution_of_human_capital,
        labour_augmented_tfp_via_labour_productivity = crude_tfp_via_labour_productivity - contribution_of_human_capital) %>%
      pivot_longer(-range)

    df_range_3 <- df %>%
      filter(
        country == input$country,
        year %in% c(input$range_3[1], input$range_3[2])
      ) %>%
      group_by(indicator) %>%
      mutate(
        ratio_value = value / lag(value),
        one_over_change_year = 1 / (year - lag(year)),
        compound_gr = (ratio_value^(one_over_change_year) - 1) * 100,
        range = paste0(
          input$range_3[1],
          "-",
          input$range_3[2]
        )
      ) %>%
      ungroup() %>%
      select(indicator, year, value, country, compound_gr, range) %>%
      filter(!is.na(compound_gr)) %>%
      select(indicator, range, compound_gr) %>%
      pivot_wider(names_from = indicator, values_from = compound_gr) %>%
      janitor::clean_names() %>%
      mutate(
        capital_contribution = a * capital_stock_k,
        labour_contribution = b * total_hours,
        contribution_of_human_capital = human_capital_stock_hk * b,
        k_per_worker_growth = capital_stock_k - total_hours,
        contribution_of_capital_deepening = k_per_worker_growth * a,
        crude_tfp_via_labour_productivity = labour_productivity - contribution_of_capital_deepening,
        crude_tfp_via_gdp = gdp_volume - capital_contribution - labour_contribution,
        labour_augmented_tfp_via_gdp = crude_tfp_via_gdp - contribution_of_human_capital,
        labour_augmented_tfp_via_labour_productivity = crude_tfp_via_labour_productivity - contribution_of_human_capital) %>%
      pivot_longer(-range)

    df_range_1 %>%
      bind_rows(df_range_2) %>%
      bind_rows(df_range_3) %>%
      mutate(value = round(value, 2))

  })

}

# Run the application
shinyApp(ui = ui, server = server)
