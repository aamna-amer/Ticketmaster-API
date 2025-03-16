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