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
get_venue_info <- function(events_df) {
  venue_counts <- table(events_df$Venue)
  most_events_venue <- names(sort(venue_counts, decreasing = TRUE))[1]
  least_events_venue <- names(sort(venue_counts, decreasing = FALSE))[1]
  list(most_events_venue = most_events_venue, least_events_venue = least_events_venue)
}

# Isolates event with highest ticket price
get_highest_ticket_price <- function(events_df) {
  if (all(is.na(events_df$Max_Price))) return(list(event = NA, price = NA))
  
  highest_price_index <- which.max(events_df$Max_Price)
  highest_price_event <- events_df$Event_Name[highest_price_index]
  highest_price <- events_df$Max_Price[highest_price_index]
  
  list(event = highest_price_event, price = highest_price)
}


# Isolates event with lowest ticket price
get_lowest_ticket_price <- function(events_df) {
  if (all(is.na(events_df$Min_Price))) return(list(event = NA, price = NA))
  
  lowest_price_index <- which.min(events_df$Min_Price)
  lowest_price_event <- events_df$Event_Name[lowest_price_index]
  lowest_price <- events_df$Min_Price[lowest_price_index]
  
  list(event = lowest_price_event, price = lowest_price)
}


# Function to run Ticketmaster analysis
ticketmaster_analysis <- function(events_df) {
  if (nrow(events_df) == 0) {
    stop("No events available for analysis.")
  }
  
  venue_info <- get_venue_info(events_df)
  highest_price_data <- get_highest_ticket_price(events_df)
  lowest_price_data <- get_lowest_ticket_price(events_df)
  
  list(
    most_events_venue = venue_info$most_events_venue,
    least_events_venue = venue_info$least_events_venue,
    highest_ticket_price_event = highest_price_data$event,
    highest_ticket_price = highest_price_data$price,
    lowest_ticket_price_event = lowest_price_data$event,
    lowest_ticket_price = lowest_price_data$price
  )
}

# Function to convert API response into a clean dataframe
get_full_ticketmaster_data <- function(api_key, city = NULL, classification_name = NULL, sort_by = NULL) {
  params <- list()
  
  # Only include parameters if they are specified
  if (!is.null(city)) {
    params$city <- city
  }
  if (!is.null(classification_name)) {
    params$classificationName <- classification_name
  }
  
  # Fetch data from API
  data <- data_request("events.json", params, api_key)

  # Check if events exist
  if (is.null(data$`_embedded`$events)) {
    stop("No events found for the given parameters.")
  }

  events <- data$`_embedded`$events

  # Extract key details into a dataframe
  events_df <- data.frame(
    Event_Name = sapply(events, function(e) e$name),
    Event_ID = sapply(events, function(e) e$id),
    Date = sapply(events, function(e) e$dates$start$localDate),
    Time = sapply(events, function(e) if (!is.null(e$dates$start$localTime)) e$dates$start$localTime else NA),
    Venue = sapply(events, function(e) e$`_embedded`$venues[[1]]$name),
    City = sapply(events, function(e) e$`_embedded`$venues[[1]]$city$name),
    State = sapply(events, function(e) if (!is.null(e$`_embedded`$venues[[1]]$state)) e$`_embedded`$venues[[1]]$state$name else NA),
    Country = sapply(events, function(e) e$`_embedded`$venues[[1]]$country$name),
    Min_Price = sapply(events, function(e) {
      if (!is.null(e$priceRanges)) e$priceRanges[[1]]$min else NA
    }),
    Max_Price = sapply(events, function(e) {
      if (!is.null(e$priceRanges)) e$priceRanges[[1]]$max else NA
    }),
    Ticket_Url = sapply(events, function(e) e$url),
    Genre = sapply(events, function(e) {
      if (!is.null(e$classifications)) e$classifications[[1]]$genre$name else NA
    }),
    Segment = sapply(events, function(e) {
      if (!is.null(e$classifications)) e$classifications[[1]]$segment$name else NA
    }),
    stringsAsFactors = FALSE
  )  
  # Convert columns to appropriate data types
  events_df <- events_df %>%
    mutate(
      Date = as.Date(Date, format = "%Y-%m-%d"),
      Min_Price = as.numeric(Min_Price),
      Max_Price = as.numeric(Max_Price)
    )
  
  # Apply sorting based on user input
  if (!is.null(sort_by)) {
    if (sort_by == "date_asc") {
      events_df <- events_df %>% arrange(Date)
    } else if (sort_by == "date_desc") {
      events_df <- events_df %>% arrange(desc(Date))
    } else if (sort_by == "min_price_asc") {
      events_df <- events_df %>% arrange(Min_Price)
    } else if (sort_by == "min_price_desc") {
      events_df <- events_df %>% arrange(desc(Min_Price))
    } else if (sort_by == "max_price_asc") {
      events_df <- events_df %>% arrange(Max_Price)
    } else if (sort_by == "max_price_desc") {
      events_df <- events_df %>% arrange(desc(Max_Price))
    } else {
      stop("Invalid sort_by parameter. Use 'date_asc', 'date_desc', 'min_price_asc', 'min_price_desc', 'max_price_asc', or 'max_price_desc'.")
    }
  }
  return(events_df)
}

# function to print results
print_ticketmaster_analysis <- function(events_df) {
  result <- ticketmaster_analysis(events_df)
  cat("Venue with the most events:", result$most_events_venue, "\n")
  cat("Venue with the least events:", result$least_events_venue, "\n")
  cat("Event with the highest ticket price:", result$highest_ticket_price_event, "at $", result$highest_ticket_price, "\n")
  cat("Event with the lowest ticket price:", result$lowest_ticket_price_event, "at $", result$lowest_ticket_price, "\n")
}

api_key <- "INSERT API KEY"
city <- "New York"
classification_name <- "music"

events_df = get_full_ticketmaster_data(api_key)
head(events_df)
print_ticketmaster_analysis(events_df)