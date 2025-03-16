test_that("get_full_ticketmaster_data() returns a valid dataframe", {
  api_key <- Sys.getenv("TICKETMASTER_API_KEY", unset = NA)
  skip_if(is.na(api_key), "API key not set")
  
  events_df <- get_full_ticketmaster_data(api_key)
  
  expect_s3_class(events_df, "data.frame")  # Must return a dataframe
  expect_true(nrow(events_df) > 0)  # Should have events
  expect_named(events_df, c("Event_Name", "Event_ID", "Date", "Time", "Venue", "City", "State", "Country", "Min_Price", "Max_Price", "Ticket_Url", "Genre", "Segment"), ignore.order = TRUE)
})