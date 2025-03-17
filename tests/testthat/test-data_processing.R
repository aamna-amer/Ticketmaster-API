test_that("get_full_ticketmaster_data() returns a valid dataframe", {
  skip_if_not(Sys.getenv("TICKETMASTER_API_KEY") != "", "API key not set, skipping test.")
  api_key <- get_ticketmaster_api_key()
  print(paste("API Key in test:", api_key))
  events_df <- get_full_ticketmaster_data()
  expect_s3_class(events_df, "data.frame")  # Must return a dataframe
  expect_true(nrow(events_df) > 0)  # Should have events
  expect_named(events_df, c("Event_Name", "Event_ID", "Date", "Time", "Venue", "City", "State", "Country", "Min_Price", "Max_Price", "Ticket_Url", "Genre", "Segment"), ignore.order = TRUE)
})
