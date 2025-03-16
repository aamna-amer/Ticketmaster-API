#' TicketmasterR: An R Wrapper for the Ticketmaster API
#'
#' The `TicketmasterR` package provides functions to interact with the 
#' Ticketmaster API, allowing users to fetch and analyze event data, 
#' retrieve ticket prices, and visualize event trends.
#'
#' ## Main Functions:
#' - `get_full_ticketmaster_data()`: Retrieves and processes event data from the Ticketmaster API.
#' - `print_ticketmaster_analysis()`: Displays a summary of Ticketmaster event data, including venue and ticket price statistics.
#' - `ticketmaster_analysis()`: Analyzes event data from Ticketmaster, including highest and lowest ticket prices and venue statistics.
#' - `get_venue_info()`: Extracts venue information and counts the number of events per venue.
#' - `get_highest_ticket_price()`: Identifies the event with the highest ticket price from the given event data.
#' - `get_lowest_ticket_price()`: Identifies the event with the lowest ticket price from the given event data.
#' - `event_class_plot()`: Generates a bar chart showing the number of events available per genre.
#' - `avg_price_class_plot()`: Generates a bar chart displaying the average ticket price for events, grouped by genre.
#' - `event_price_count_plot()`: Generates a histogram showing the distribution of average ticket prices.
#' - `avg_event_price_line_plot()`: Generates a line plot showing the average ticket price trend for the last 200 events.
#' - `event_hourly_distribution_plot()`: Generates a bar chart displaying the distribution of event counts across different hours of the day.
#' - `event_day_distribution_plot()`: Generates a bar chart displaying the number of events occurring on each day of the week.
#'
#' ## Getting Started:
#' To start using the package, set your API key:
#' ```
#' sys.setenv(TICKETMASTER_API_KEY = "your_api_key_here")
#' ```
#' Then, fetch event data:
#' ```
#' events_df <- get_full_ticketmaster_data()
#' ```
#'
#' @keywords internal
"_PACKAGE"
#' @name TicketmasterR
#' @aliases TicketmasterR