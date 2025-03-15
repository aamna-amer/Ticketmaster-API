#' TicketmasterR: An R Wrapper for the Ticketmaster API
#'
#' The `TicketmasterR` package provides functions to interact with the 
#' Ticketmaster API, allowing users to fetch and analyze event data, 
#' retrieve ticket prices, and visualize event trends.
#'
#' ## Main Functions:
#' - `get_full_ticketmaster_data()`: Fetch event data from Ticketmaster.
#' - `ticketmaster_analysis()`: Analyze event trends, including highest and lowest ticket prices.
#'
#' ## Getting Started:
#' To start using the package, set your API key:
#' ```
#' api_key <- "YOUR API KEY"
#' ```
#' Then, fetch event data:
#' ```
#' events_df <- get_full_ticketmaster_data(api_key)
#' head(events_df)
#' ```
#'
#' @keywords internal
"_PACKAGE"
#' @name TicketmasterR
#' @aliases TicketmasterR