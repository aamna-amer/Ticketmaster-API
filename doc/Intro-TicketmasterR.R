## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(TicketmasterR)

## -----------------------------------------------------------------------------
api_key <- get_ticketmaster_api_key()

## -----------------------------------------------------------------------------
events_df <- get_full_ticketmaster_data()
head(events_df)

## -----------------------------------------------------------------------------
print_ticketmaster_analysis(events_df)

## ----fig.width=8, fig.height=6------------------------------------------------
event_class_plot(events_df)

## ----fig.width=8, fig.height=6------------------------------------------------
avg_price_class_plot(events_df)

## ----fig.width=8, fig.height=6------------------------------------------------
event_price_count_plot(events_df)

## ----fig.width=8, fig.height=6------------------------------------------------
avg_event_price_line_plot(events_df)

## ----fig.width=8, fig.height=6------------------------------------------------
event_hourly_distribution_plot(events_df)

## ----fig.width=8, fig.height=6------------------------------------------------
event_day_distribution_plot(events_df)

