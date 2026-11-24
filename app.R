library(shiny)
library(tidyverse)
library(fpp3)
library(plotly)
library(urca)

aus_wine <- read_csv("data/AustralianWines.csv", na = "*",
                     col_types = cols(Rose = col_number()),
                     show_col_types = FALSE) |>
    fill(Rose, .direction = "down") |>
    mutate(Month = mdy(str_replace(Month, '-', '-01-')) |> yearmonth())

wines_ts <- aus_wine |>
    pivot_longer(cols = -Month, names_to = "Varietal", values_to = "Sales") |>
    mutate(Varietal = str_trim(Varietal)) |>
    as_tsibble(index = Month, key = Varietal)

ui <- fluidPage(
    titlePanel("Australian Wine Sales Forecasts"),
    sidebarLayout(
        sidebarPanel(
            selectInput("varietal", "Varietal(s)", choices = unique(wines_ts$Varietal),
                        selected = c("Fortified", "Red"), multiple = TRUE),
            sliderInput("dates", "Date range",
                        min = as.Date("1980-01-01"), max = as.Date("1994-12-01"),
                        value = c(as.Date("1980-01-01"), as.Date("1994-12-01")),
                        timeFormat = "%b %Y"),
            numericInput("val", "Validation months", 12, min = 6, max = 36),
            numericInput("h", "Forecast horizon (months)", 12, min = 1, max = 36)
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Overview", plotlyOutput("overview")),
                tabPanel("Model Specs", tableOutput("specs")),
                tabPanel("Accuracy", tableOutput("acc")),
                tabPanel("Forecasts", plotOutput("fc_plot", height = 700))
            )
        )
    )
)

server <- function(input, output) {
    
    wine_sel <- reactive({
        req(input$varietal)
        wines_ts |>
            filter(Varietal %in% input$varietal) |>
            filter_index(as.character(yearmonth(input$dates[1])) ~
                             as.character(yearmonth(input$dates[2])))
    })
    
    wine_trn <- reactive({
        wine_sel() |> filter(Month <= max(Month) - input$val)
    })
    
    fit <- reactive({
        wine_trn() |>
            model(
                tslm  = TSLM(Sales ~ trend() + season()),
                ets   = ETS(Sales),
                arima = ARIMA(Sales)
            )
    })
    
    fc <- reactive({
        fit() |> forecast(h = max(input$h, input$val))
    })
    
    output$overview <- renderPlotly({
        p <- wine_sel() |>
            autoplot(Sales) +
            labs(title = "Monthly Wine Sales", y = "Sales (thousands of liters)")
        ggplotly(p)
    })
    
    output$specs <- renderTable({
        fit() |>
            as_tibble() |>
            mutate(across(c(tslm, ets, arima), ~ str_remove_all(format(.x), "[<>]")))
    })
    
    output$acc <- renderTable({
        bind_rows(
            accuracy(fit()),
            accuracy(fc(), wine_sel())
        ) |>
            select(Varietal, .model, .type, RMSE, MAE, MAPE) |>
            arrange(Varietal, .type, RMSE)
    })
    
    output$fc_plot <- renderPlot({
        fc() |>
            autoplot(wine_sel() |> filter(Month > max(Month) - 48), level = c(80, 95)) +
            facet_grid(Varietal ~ .model, scales = "free_y") +
            labs(title = "Forecasts vs Actual", y = "Sales (thousands of liters)") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
}

shinyApp(ui, server)