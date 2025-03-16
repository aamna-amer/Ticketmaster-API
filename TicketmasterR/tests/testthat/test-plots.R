# Sample dataset for testing
sample_df <- data.frame(
  Event_Name = c("Event A", "Event B", "Event C"),
  Event_ID = c("ca1", "cb2", "cc3"),
  Date = as.Date(c("2024-04-01", "2024-04-02", "2024-04-03")),
  Time = c("19:00:00", "20:30:00", "21:00:00"),
  Venue = c("Stadium", "Hall", "Field"),
  City = c("New York", "Sacramento", "San Francisco"),
  State = c("New York", "California", "California"),
  Country = c("United States Of America", "United States Of America", "United States Of America"),
  Min_Price = c(50, 30, 20),
  Max_Price = c(200, 100, 80),
  Ticket_URL = c("www.google.com", "www.google.com", "www.google.com"),
  Genre = c("Basketball", "Standup", "Music"),
  Segment = c("Sports", "Comedy", "Concert")
)

# Test 1: event_class_plot() creates a ggplot object
test_that("event_class_plot() creates a ggplot object", {
  p <- event_class_plot(sample_df)
  expect_s3_class(p, "ggplot")
})

# Test 2: avg_price_class_plot() creates a ggplot object
test_that("avg_price_class_plot() creates a ggplot object", {
  p <- avg_price_class_plot(sample_df)
  expect_s3_class(p, "ggplot")
})

# Test 3: event_price_count_plot() creates a ggplot object
test_that("event_price_count_plot() creates a ggplot object", {
  p <- event_price_count_plot(sample_df)
  expect_s3_class(p, "ggplot")
})

# Test 4: avg_event_price_line_plot() creates a ggplot object
test_that("avg_event_price_line_plot() creates a ggplot object", {
  p <- avg_event_price_line_plot(sample_df)
  expect_s3_class(p, "plotly")
})

# Test 5: event_hourly_distribution_plot() creates a ggplot object
test_that("event_hourly_distribution_plot() creates a ggplot object", {
  p <- event_hourly_distribution_plot(sample_df)
  expect_s3_class(p, "ggplot")
})

# Test 6: event_day_distribution_plot() creates a ggplot object
test_that("event_day_distribution_plot() creates a ggplot object", {
  p <- event_day_distribution_plot(sample_df)
  expect_s3_class(p, "ggplot")
})