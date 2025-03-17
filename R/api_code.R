utils::globalVariables(c(
  "Avg_Price", "Count", "Date", "Event_Name", "Event_Number",
  "Genre", "Hour", "Max_Price", "Min_Price", "Segment",
  "Time", "Weekday", "reorder"
))
#' @title Set Up API Key for Ticketmaster
#' @description To use this package, you need to set your Ticketmaster API key as an environment variable.
#' @details
#' The API key must be stored as an environment variable to be used automatically in functions that require it.
#'
#' @examples
#' \dontrun{
#' # Set your API key (only needs to be done once per session)
#' Sys.setenv(TICKETMASTER_API_KEY = "your_api_key_here")
#'
#' # Retrieve the API key (example usage)
#' api_key <- get_ticketmaster_api_key()
#' print(api_key)
#'
#' # Now you can call functions without passing the API key each time
#' # events_df <- get_full_ticketmaster_data()
#' }
#'
#' @export
setup_ticketmaster_api_key <- function() {
  message("This function does nothing. It exists to document how to set your API key.")
}

#' @title Retrieve Ticketmaster API Key
#' @description Fetches the API key from environment variables.
#' @return A character string containing the API key, or stops execution if not found.
#' @examples
#' \dontrun{
#' api_key <- get_ticketmaster_api_key()
#' print(api_key)
#' }
#' @export
get_ticketmaster_api_key <- function() {
  api_key <- Sys.getenv("TICKETMASTER_API_KEY")
  if (api_key == "") {
    stop("API key not found. Please set it using: Sys.setenv(TICKETMASTER_API_KEY = 'your_api_key_here')")
  }
  return(api_key)
}

#' @title API Request Function
#' @description Fetches data from the Ticketmaster API using the given endpoint and parameters.
#' @param endpoint A character string specifying the API endpoint (e.g., "events.json").
#' @param params A list of query parameters for the API request.
#' @param api_key A character string containing your Ticketmaster API key.
#' @return A parsed JSON object containing the API response.
#' @importFrom httr GET content status_code
#' @examples
#' \dontrun{
#' params <- list(city = "New York")
#' response <- data_request("events.json", params, api_key)
#' }
#' @export
data_request <- function(endpoint, params, api_key) {
  api_key <- get_ticketmaster_api_key()
  url <- paste0("https://app.ticketmaster.com/discovery/v2/", endpoint)
  params$apikey <- api_key
  response <- httr::GET(url, query = params)
  if (httr::status_code(response) != 200) {
    stop(paste("Error fetching data:", status_code(response)))
  }
  httr::content(response, "parsed", type = "application/json")
}

#' @title Get Venue Information
#' @description Extracts venue information and counts the number of events per venue.
#' @param events_df A data frame containing event data.
#' @return A list with the venue having the most and least events.
#' @importFrom dplyr summarise group_by
#' @examples
#' \dontrun{
#' venue_info <- get_venue_info(events_df)
#' print(venue_info)
#' }
#' @export
get_venue_info <- function(events_df) {
  # Check if input is NULL or not a data frame
  if (is.null(events_df) || !is.data.frame(events_df)) {
    stop("Error: Input must be a non-null data frame.")
  }

  # Check if 'Venue' column exists
  if (!"Venue" %in% colnames(events_df)) {
    stop("Error: Data frame must contain a 'Venue' column.")
  }

  venue_counts <- table(events_df$Venue)
  most_events_venue <- names(sort(venue_counts, decreasing = TRUE))[1]
  least_events_venue <- names(sort(venue_counts, decreasing = FALSE))[1]
  list(most_events_venue = most_events_venue, least_events_venue = least_events_venue)
}

#' @title Get Event with Highest Ticket Price
#' @description Identifies the event with the highest ticket price from the given event data.
#' @param events_df A data frame containing event data, including a `Max_Price` column.
#' @return A list containing:
#' - `event`: The name of the event with the highest ticket price.
#' - `price`: The highest ticket price.
#' If no valid ticket prices are available, returns `NA` for both values.
#' @importFrom dplyr filter
#' @examples
#' \dontrun{
#' highest_price_info <- get_highest_ticket_price(events_df)
#' print(highest_price_info)
#' }
#' @export
get_highest_ticket_price <- function(events_df) {
  # Check if input is NULL or not a data frame
  if (is.null(events_df) || !is.data.frame(events_df)) {
    stop("Error: Input must be a non-null data frame.")
  }

  # Check if required columns exist
  required_columns <- c("Max_Price", "Event_Name")
  missing_columns <- setdiff(required_columns, colnames(events_df))
  if (length(missing_columns) > 0) {
    stop(paste("Error: Data frame is missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Check if 'Max_Price' column is numeric
  if (!is.numeric(events_df$Max_Price)) {
    stop("Error: 'Max_Price' column must be numeric.")
  }

  if (all(is.na(events_df$Max_Price))) return(list(event = NA, price = NA))

  highest_price_index <- which.max(events_df$Max_Price)
  highest_price_event <- events_df$Event_Name[highest_price_index]
  highest_price <- events_df$Max_Price[highest_price_index]

  list(event = highest_price_event, price = highest_price)
}

#' @title Get Event with Lowest Ticket Price
#' @description Identifies the event with the lowest ticket price from the given event data.
#' @param events_df A data frame containing event data, including a `Min_Price` column.
#' @return A list containing:
#' - `event`: The name of the event with the lowest ticket price.
#' - `price`: The lowest ticket price.
#' If no valid ticket prices are available, returns `NA` for both values.
#' @importFrom dplyr filter
#' @examples
#' \dontrun{
#' lowest_price_info <- get_lowest_ticket_price(events_df)
#' print(lowest_price_info)
#' }
#' @export
get_lowest_ticket_price <- function(events_df) {
  # Check if input is NULL or not a data frame
  if (is.null(events_df) || !is.data.frame(events_df)) {
    stop("Error: Input must be a non-null data frame.")
  }

  # Check if required columns exist
  required_columns <- c("Min_Price", "Event_Name")
  missing_columns <- setdiff(required_columns, colnames(events_df))
  if (length(missing_columns) > 0) {
    stop(paste("Error: Data frame is missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Check if 'Min_Price' column is numeric
  if (!is.numeric(events_df$Min_Price)) {
    stop("Error: 'Min_Price' column must be numeric.")
  }

  if (all(is.na(events_df$Min_Price))) return(list(event = NA, price = NA))

  lowest_price_index <- which.min(events_df$Min_Price)
  lowest_price_event <- events_df$Event_Name[lowest_price_index]
  lowest_price <- events_df$Min_Price[lowest_price_index]

  list(event = lowest_price_event, price = lowest_price)
}

#' @title Ticketmaster Event Analysis
#' @description Analyzes event data from Ticketmaster, including highest and lowest ticket prices and venue statistics.
#' @param events_df A data frame containing event data retrieved using `get_full_ticketmaster_data()`.
#' @return A list containing:
#' - `most_events_venue`: The venue with the most events.
#' - `least_events_venue`: The venue with the least events.
#' - `highest_ticket_price_event`: The event with the highest ticket price.
#' - `highest_ticket_price`: The highest ticket price.
#' - `lowest_ticket_price_event`: The event with the lowest ticket price.
#' - `lowest_ticket_price`: The lowest ticket price.
#' @importFrom dplyr filter mutate summarise group_by
#' @examples
#' \dontrun{
#' result <- ticketmaster_analysis(events_df)
#' print(result)
#' }
#' @export
ticketmaster_analysis <- function(events_df) {
  # Check if input is NULL or not a data frame
  if (is.null(events_df) || !is.data.frame(events_df)) {
    stop("Error: Input must be a non-null data frame.")
  }

  # Check if required columns exist
  required_columns <- c("Venue", "Max_Price", "Min_Price", "Event_Name")
  missing_columns <- setdiff(required_columns, colnames(events_df))
  if (length(missing_columns) > 0) {
    stop(paste("Error: Data frame is missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Check if the data frame is empty
  if (nrow(events_df) == 0 || all(is.na(events_df))) {
    stop("Error: No events available for analysis.")
  }

  # Check if ticket price columns are numeric
  if (!is.numeric(events_df$Max_Price) || !is.numeric(events_df$Min_Price)) {
    stop("Error: 'Max_Price' and 'Min_Price' must be numeric values.")
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

#' @title Fetch and Process Ticketmaster Event Data
#' @description Retrieves and processes event data from the Ticketmaster API.
#' @param city (Optional) A character string specifying a city to filter events.
#' @param classification_name (Optional) A character string to filter by event classification (e.g., "music").
#' @param sort_by (Optional) A character string specifying sorting order (e.g., "date_asc").
#' @param size (Optional) The number of results to return (default = 200).
#' @return A cleaned data frame containing event details.
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate arrange filter group_by summarise desc
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' events_df <- get_full_ticketmaster_data()
#' head(events_df)
#' }
#' @export
get_full_ticketmaster_data <- function(city = NULL, classification_name = NULL, sort_by = NULL, size = 200) {

  params <- list()

  # Validate API key
  if (is.null(api_key) || api_key == "") {
    stop("Error: API key is missing or invalid.")
  }

  # Validate size parameter
  if(size <= 200 && size >= 1){
    params$size <- as.character(size) # API only accepts size as string
  } else {
    stop("Size must be between 200 and 1 inclusive.")
  }

  api_key <- get_ticketmaster_api_key()

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

#' @title Print Ticketmaster Event Analysis
#' @description Displays a summary of Ticketmaster event data, including venue and ticket price statistics.
#' @param events_df A data frame containing event data retrieved using `get_full_ticketmaster_data()`.
#' @return Prints the following summary to the console:
#' - Venue with the most events.
#' - Venue with the least events.
#' - Event with the highest ticket price.
#' - Event with the lowest ticket price.
#' @importFrom dplyr filter mutate summarise group_by
#' @examples
#' \dontrun{
#' print_ticketmaster_analysis(events_df)
#' }
#' @export
print_ticketmaster_analysis <- function(events_df) {
  # Ensure input is valid
  if (is.null(events_df) || !is.data.frame(events_df) || nrow(events_df) == 0) {
    stop("Error: events_df must be a non-null data frame with at least one event.")
  }

  # Safely call ticketmaster_analysis() and catch errors
  result <- tryCatch({
    ticketmaster_analysis(events_df)
  }, error = function(e) {
    cat("Error: Unable to perform analysis. Reason:", e$message, "\n")
    return(NULL)
  })

  result <- ticketmaster_analysis(events_df)
  cat("Venue with the most events:", result$most_events_venue, "\n")
  cat("Venue with the least events:", result$least_events_venue, "\n")
  cat("Event with the highest ticket price:", result$highest_ticket_price_event, "at $", result$highest_ticket_price, "\n")
  cat("Event with the lowest ticket price:", result$lowest_ticket_price_event, "at $", result$lowest_ticket_price, "\n")
}

#' @title Plot Events by Genre
#' @description Generates a bar chart showing the number of events available per genre.
#' @param events_df A data frame containing event data retrieved using `get_full_ticketmaster_data()`.
#' @return A ggplot object displaying the number of events by genre.
#' @import ggplot2
#' @import dplyr
#' @importFrom stats reorder
#' @examples
#' \dontrun{
#' event_class_plot(events_df)
#' }
#' @export
event_class_plot <- function(events_df) {
  # Validate the input data frame
  if (is.null(events_df) || !is.data.frame(events_df) || nrow(events_df) == 0) {
    stop("Error: events_df must be a non-null data frame with at least one event.")
  }

  # Ensure the "Genre" column exists
  if (!"Genre" %in% colnames(events_df)) {
    stop("Error: The data frame must contain a 'Genre' column.")
  }

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

#' @title Plot Average Ticket Price by Event Type for the Last 200 Events
#' @description Generates a bar chart displaying the average ticket price for events, grouped by genre.
#' @param events_df A data frame containing event data, including `Min_Price`, `Max_Price`, and `Genre` columns.
#' @return A ggplot object showing the average ticket price for each event genre.
#' @import ggplot2
#' @import dplyr
#' @importFrom stats reorder
#' @examples
#' \dontrun{
#' avg_price_class_plot(events_df)
#' }
#' @export
avg_price_class_plot <- function(events_df) {
  # Validate input data
  if (is.null(events_df) || !is.data.frame(events_df) || nrow(events_df) == 0) {
    stop("Error: events_df must be a non-null data frame with at least one event.")
  }

  # Ensure required columns exist
  required_columns <- c("Genre", "Min_Price", "Max_Price")
  missing_columns <- setdiff(required_columns, colnames(events_df))
  if (length(missing_columns) > 0) {
    stop(paste("Error: Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Ensure numeric values for price columns
  if (!is.numeric(events_df$Min_Price) || !is.numeric(events_df$Max_Price)) {
    stop("Error: 'Min_Price' and 'Max_Price' must be numeric values.")
  }

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

#' @title Histogram of Event Prices
#' @description Generates a histogram showing the distribution of average ticket prices.
#' @param events_df A data frame containing event data.
#' @return A ggplot histogram of ticket prices.
#' @import ggplot2
#' @import dplyr
#' @examples
#' \dontrun{
#' event_price_count_plot(events_df)
#' }
#' @export
event_price_count_plot <- function(events_df) {
  # Validate input data
  if (is.null(events_df) || !is.data.frame(events_df) || nrow(events_df) == 0) {
    stop("Error: events_df must be a non-null data frame with at least one event.")
  }

  # Ensure required columns exist
  required_columns <- c("Min_Price", "Max_Price")
  missing_columns <- setdiff(required_columns, colnames(events_df))
  if (length(missing_columns) > 0) {
    stop(paste("Error: Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Ensure Min_Price and Max_Price are numeric
  if (!is.numeric(events_df$Min_Price) || !is.numeric(events_df$Max_Price)) {
    stop("Error: 'Min_Price' and 'Max_Price' must be numeric values.")
  }

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

#' @title Plot Average Ticket Price Over Last 200 Events
#' @description Generates a line plot showing the average ticket price trend for the last 200 events.
#' @param events_df A data frame containing event data, including `Min_Price`, `Max_Price`, and `Event_Name` columns.
#' @return An interactive ggplotly object displaying the average ticket price trend over the last 200 events.
#' @import ggplot2
#' @import dplyr
#' @importFrom plotly ggplotly
#' @examples
#' \dontrun{
#' avg_event_price_line_plot(events_df)
#' }
#' @export
avg_event_price_line_plot <- function(events_df) {
  # Validate input data
  if (is.null(events_df) || !is.data.frame(events_df) || nrow(events_df) == 0) {
    stop("Error: events_df must be a non-null data frame with at least one event.")
  }

  # Ensure required columns exist
  required_columns <- c("Event_Name", "Segment", "Min_Price", "Max_Price")
  missing_columns <- setdiff(required_columns, colnames(events_df))
  if (length(missing_columns) > 0) {
    stop(paste("Error: Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Ensure Min_Price and Max_Price are numeric
  if (!is.numeric(events_df$Min_Price) || !is.numeric(events_df$Max_Price)) {
    stop("Error: 'Min_Price' and 'Max_Price' must be numeric values.")
  }

  events <- events_df %>%
    filter(!is.na(Min_Price) & !is.na(Max_Price) & Min_Price >= 0 & Max_Price >= 0) %>%
    mutate(Avg_Price = (Min_Price + Max_Price) / 2) %>%
    filter(!is.na(Avg_Price) & Avg_Price >= 0) %>%
    mutate(Event_Number = 1:n())

  p <- ggplot(events, aes(x = Event_Number, y = Avg_Price)) +
    geom_line(color = "#d0006f", linewidth = 0.5) +
    geom_point(color = '#009cde', size = 0.5) +
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
  ggplotly(p, tooltip = "text") %>%
    plotly::add_trace(
      x = events$Event_Number,
      y = events$Avg_Price,
      type = 'scatter',
      mode = 'markers',
      text = paste("Event: ", events$Event_Name,
                   "<br>Avg Price: $", round(events$Avg_Price, 2),
                   "<br>Event Type: ", events$Segment)
    )
}

#' @title Plot Event Count Distribution by Hour
#' @description Generates a bar chart displaying the distribution of event counts across different hours of the day.
#' @param events_df A data frame containing event data, including a `Time` column in HH:MM:SS format.
#' @return A ggplot object showing the number of events occurring at each hour of the day.
#' @import ggplot2
#' @import dplyr
#' @examples
#' \dontrun{
#' event_hourly_distribution_plot(events_df)
#' }
#' @export
event_hourly_distribution_plot <- function(events_df) {
  # Validate input data
  if (is.null(events_df) || !is.data.frame(events_df) || nrow(events_df) == 0) {
    stop("Error: events_df must be a non-null data frame with at least one event.")
  }

  # Ensure "Time" column exists
  if (!"Time" %in% colnames(events_df)) {
    stop("Error: The data frame must contain a 'Time' column.")
  }

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

#' @title Plot Event Count Distribution by Day of the Week
#' @description Generates a bar chart displaying the number of events occurring on each day of the week.
#' @param events_df A data frame containing event data, including a `Date` column in YYYY-MM-DD format.
#' @return A ggplot object showing the number of events for each weekday.
#' @import ggplot2
#' @import dplyr
#' @examples
#' \dontrun{
#' event_day_distribution_plot(events_df)
#' }
#' @export
event_day_distribution_plot <- function(events_df) {
  # Validate input data
  if (is.null(events_df) || !is.data.frame(events_df) || nrow(events_df) == 0) {
    stop("Error: events_df must be a non-null data frame with at least one event.")
  }

  # Ensure "Date" column exists
  if (!"Date" %in% colnames(events_df)) {
    stop("Error: The data frame must contain a 'Date' column.")
  }

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
