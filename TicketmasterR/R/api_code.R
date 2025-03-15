library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(plotly)

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
get_full_ticketmaster_data <- function(api_key, city = NULL, classification_name = NULL, sort_by = NULL, size = 200) {
  params <- list()

  # Only include parameters if they are specified
  if(size <= 200 && size >= 1){
    params$size <- as.character(size) # API only accepts size as string
  } else {
    stop("Size must be between 200 and 1 inclusive.")
  }
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

api_key <- Sys.getenv("API_KEY", unset = "INSERT API KEY")
city <- "New York"
classification_name <- "music"

events_df = get_full_ticketmaster_data(api_key)
head(events_df)
print_ticketmaster_analysis(events_df)

# Plots counts of events by type for 200 events
event_class_plot <- function(events_df) {
  event_counts <- events_df %>%
    group_by(Genre) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count))

  ggplot(event_counts, aes(x = reorder(Genre, -Count), y = Count)) +
    geom_bar(stat = "identity", fill = "#d0006f", color = "#009cde", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Events Available by Event Classification",
      x = "Event Type",
      y = "Number of Events"
    ) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 1, color = "#414141", size = 9),
      axis.text.y = element_text(color = "#414141", size = 9),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#414141"),
      axis.title.x = element_text(size = 12, color = "#414141"),
      axis.title.y = element_text(size = 12, color = "#414141"),
      plot.background = element_rect(fill = "#ffffff"),
      panel.background = element_rect(fill = "#ffffff"),
      panel.grid.major = element_line(color = "#b7c9d3", linetype = "dotted"),
      panel.grid.minor = element_line(color = "#009cde", linetype = "dotted"),
      panel.border = element_blank()
    )

}

# Plots average event price by type for 200 events
avg_price_class_plot <- function(events_df) {
  avg_price <- events_df %>%
    filter(!is.na(Min_Price) & !is.na(Max_Price) & Min_Price >= 0 & Max_Price >= 0) %>%
    group_by(Genre) %>%
    summarise(Avg_Price = mean(c(Min_Price, Max_Price), na.rm = TRUE)) %>%
    filter(!is.na(Avg_Price) & Avg_Price >= 0) %>%
    arrange(desc(Avg_Price))

  ggplot(avg_price, aes(x = reorder(Genre, -Avg_Price), y = Avg_Price)) +
    geom_bar(stat = "identity", fill = "#d0006f", color = "#009cde", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Average Ticket Price by Event Classification",
      x = "Event Type",
      y = "Average Ticket Price (USD)"
    ) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 1, color = "#414141", size = 9),
      axis.text.y = element_text(color = "#414141", size = 9),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#414141"),
      axis.title.x = element_text(size = 12, color = "#414141"),
      axis.title.y = element_text(size = 12, color = "#414141"),
      plot.background = element_rect(fill = "#ffffff"),
      panel.background = element_rect(fill = "#ffffff"),
      panel.grid.major = element_line(color = "#b7c9d3", linetype = "dotted"),
      panel.grid.minor = element_line(color = "#009cde", linetype = "dotted"),
      panel.border = element_blank()
    )
}

# Plot count of events by average ticket price for last 200 events
event_price_count_plot <- function(events_df) {
  avg_price_events <- events_df %>%
    filter(!is.na(Min_Price) & !is.na(Max_Price) & Min_Price >= 0 & Max_Price >= 0) %>%
    mutate(Avg_Price = (Min_Price + Max_Price) / 2) %>%
    filter(!is.na(Avg_Price) & Avg_Price >= 0 & Avg_Price < 1001)

  ggplot(avg_price_events, aes(x = Avg_Price)) +
           geom_histogram(binwidth = 8, fill = '#009cde') +
           labs(
             title = "Event Count by Average Ticket Price Under $1000 for Last 200 Events",
             x = "Average Ticket Price (USD)",
             y = "Count"
           ) +
           theme(
             axis.text.x = element_text(angle = 0, hjust = 1, color = "#414141", size = 9),
             axis.text.y = element_text(color = "#414141", size = 9),
             plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#414141"),
             axis.title.x = element_text(size = 12, color = "#414141"),
             axis.title.y = element_text(size = 12, color = "#414141"),
             plot.background = element_rect(fill = "#ffffff"),
             panel.background = element_rect(fill = "#ffffff"),
             panel.grid.major = element_line(color = "#b7c9d3", linetype = "dotted"),
             panel.grid.minor = element_line(color = "#009cde", linetype = "dotted"),
             panel.border = element_blank()
           )
}

# Plot average ticket price for last 200 events
avg_event_price_line_plot <- function(events_df) {
  events <- events_df %>%
    filter(!is.na(Min_Price) & !is.na(Max_Price) & Min_Price >= 0 & Max_Price >= 0) %>%
    mutate(Avg_Price = (Min_Price + Max_Price) / 2) %>%
    filter(!is.na(Avg_Price) & Avg_Price >= 0) %>%
    mutate(Event_Number = 1:n())

  p <- ggplot(events, aes(x = Event_Number, y = Avg_Price)) +
    geom_line(color = "#d0006f", size = 0.5) +
    geom_point(aes(text = paste("Event: ", Event_Name, "<br>Event Type: ", Segment)),
               color = '#009cde', size = 0.5) +
    labs(
      title = "Average Ticket Price for Last 200 Events",
      x = "Event",
      y = "Average Ticket Price (USD)"
    ) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 1, color = "#414141", size = 9),
      axis.text.y = element_text(color = "#414141", size = 9),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#414141"),
      axis.title.x = element_text(size = 12, color = "#414141"),
      axis.title.y = element_text(size = 12, color = "#414141"),
      plot.background = element_rect(fill = "#ffffff"),
      panel.background = element_rect(fill = "#ffffff"),
      panel.grid.major = element_line(color = "#b7c9d3", linetype = "dotted"),
      panel.grid.minor = element_line(color = "#009cde", linetype = "dotted"),
      panel.border = element_blank()
    )
  ggplotly(p)
}

# Function to plot event count distribution by hour of the day
event_hourly_distribution_plot <- function(events_df) {
  events_df <- events_df %>%
    filter(!is.na(Time)) %>%
    mutate(Hour = as.numeric(substr(Time, 1, 2)))  # Extract hour from Time
  
  event_counts <- events_df %>%
    group_by(Hour) %>%
    summarise(Count = n()) %>%
    arrange(Hour)
  
  ggplot(event_counts, aes(x = Hour, y = Count)) +
    geom_bar(stat = "identity", fill = "#d0006f", color = "#009cde", alpha = 0.8) +
    scale_x_continuous(breaks = 0:23) +  # Ensure all hours are shown
    labs(
      title = "Event Count Distribution by Hour of the Day",
      x = "Hour of the Day (24-Hour Format)",
      y = "Number of Events"
    ) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 1, color = "#414141", size = 9),
      axis.text.y = element_text(color = "#414141", size = 9),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#414141"),
      axis.title.x = element_text(size = 12, color = "#414141"),
      axis.title.y = element_text(size = 12, color = "#414141"),
      plot.background = element_rect(fill = "#ffffff"),
      panel.background = element_rect(fill = "#ffffff"),
      panel.grid.major = element_line(color = "#b7c9d3", linetype = "dotted"),
      panel.grid.minor = element_line(color = "#009cde", linetype = "dotted"),
      panel.border = element_blank()
    )
}

# Function to plot event count distribution by day of the week
event_day_distribution_plot <- function(events_df) {
  events_df <- events_df %>%
    filter(!is.na(Date)) %>%
    mutate(Weekday = weekdays(Date))  # Extract day of the week
  
  event_counts <- events_df %>%
    group_by(Weekday) %>%
    summarise(Count = n()) %>%
    mutate(Weekday = factor(Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
    arrange(Weekday)
  
  ggplot(event_counts, aes(x = Weekday, y = Count)) +
    geom_bar(stat = "identity", fill = "#d0006f", color = "#009cde", alpha = 0.8) +
    labs(
      title = "Event Count Distribution by Day of the Week",
      x = "Day of the Week",
      y = "Number of Events"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "#414141", size = 9),
      axis.text.y = element_text(color = "#414141", size = 9),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#414141"),
      axis.title.x = element_text(size = 12, color = "#414141"),
      axis.title.y = element_text(size = 12, color = "#414141"),
      plot.background = element_rect(fill = "#ffffff"),
      panel.background = element_rect(fill = "#ffffff"),
      panel.grid.major = element_line(color = "#b7c9d3", linetype = "dotted"),
      panel.grid.minor = element_line(color = "#009cde", linetype = "dotted"),
      panel.border = element_blank()
    )
}


event_class_plot(events_df)
avg_price_class_plot(events_df)

event_price_count_plot(events_df)
avg_event_price_line_plot(events_df)

event_hourly_distribution_plot(events_df)
event_day_distribution_plot(events_df)
