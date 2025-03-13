library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Makes API request
data_request <- function(endpoint, params, api_key) {
  url <- paste0("https://app.ticketmaster.com/discovery/v2/", endpoint)
  params$apikey <- api_key
  response <- GET(url, query = params)
  if (status_code(response) != 200) {
    stop(paste("Error fetching data:", status_code(response)))
  }
  content(response, "parsed", type = "application/json")
}

# Extracts venue information and counts
get_venue_info <- function(data) {
  venues <- sapply(data$`_embedded`$events, function(event) event$`_embedded`$venues[[1]]$name)
  venue_counts <- table(venues)
  most_events_venue <- names(sort(venue_counts, decreasing = TRUE))[1]
  least_events_venue <- names(sort(venue_counts, decreasing = FALSE))[1]
  list(most_events_venue = most_events_venue, least_events_venue = least_events_venue)
}

# Isolates event with highest ticket price
get_highest_ticket_price <- function(events) {
  highest_price <- 0
  highest_price_event <- NULL
  
  for (event in events) {
    price_ranges <- event$priceRanges
    if (!is.null(price_ranges)) {
      max_price <- max(sapply(price_ranges, function(x) x$max), na.rm = TRUE)
      if (max_price > highest_price) {
        highest_price <- max_price
        highest_price_event <- event$name
      }
    }
  }
  list(event = highest_price_event, price = highest_price)
}

# Isolates event with lowest ticket price
get_lowest_ticket_price <- function(events) {
  lowest_price <- Inf
  lowest_price_event <- NULL
  
  for (event in events) {
    price_ranges <- event$priceRanges
    if (!is.null(price_ranges)) {
      min_price <- min(sapply(price_ranges, function(x) x$min), na.rm = TRUE)
      if (min_price < lowest_price) {
        lowest_price <- min_price
        lowest_price_event <- event$name
      }
    }
  }
  list(event = lowest_price_event, price = lowest_price)
}

# Function to run Ticketmaster analysis
ticketmaster_analysis <- function(city, classification_name, api_key) {
  params <- list(city = city, classificationName = classification_name)
  
  # Isolates data
  data <- data_request('events.json', params, api_key)
  
  # Extracts venue information
  venue_info <- get_venue_info(data)
  
  # Isolates highest and lowest ticket prices
  highest_price_data <- get_highest_ticket_price(data$`_embedded`$events)
  lowest_price_data <- get_lowest_ticket_price(data$`_embedded`$events)
  
  list(
    most_events_venue = venue_info$most_events_venue,
    least_events_venue = venue_info$least_events_venue,
    highest_ticket_price_event = highest_price_data$event,
    highest_ticket_price = highest_price_data$price,
    lowest_ticket_price_event = lowest_price_data$event,
    lowest_ticket_price = lowest_price_data$price
  )
}

api_key <- "INPUT API"
city <- "New York"
classification_name <- "music"

result <- ticketmaster_analysis(city, classification_name, api_key)
cat("Venue with the most events:", result$most_events_venue, "\n")
cat("Venue with the least events:", result$least_events_venue, "\n")
cat("Event with the highest ticket price:", result$highest_ticket_price_event, "at $", result$highest_ticket_price, "\n")
cat("Event with the lowest ticket price:", result$lowest_ticket_price_event, "at $", result$lowest_ticket_price, "\n")


