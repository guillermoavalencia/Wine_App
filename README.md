# Australian Wine Sales Forecasts

Shiny app comparing TSLM, ETS, and ARIMA forecasts for monthly Australian wine sales (1980–1994).

**Live app:** https://guillermovalencia-wine-app.share.connect.posit.cloud

## Features
- Select wine types, date range, validation window, and forecast horizon
- Model specifications for each wine
- Training and validation accuracy (RMSE, MAE, MAPE)
- Forecasts with 80% and 95% prediction intervals

## Run locally
Place `AustralianWines.csv` in a `data/` folder next to `app.R`, then run `shiny::runApp()`.

Packages: shiny, tidyverse, fpp3, plotly, urca
