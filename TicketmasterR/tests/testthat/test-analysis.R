test_that("ticketmaster_analysis() returns correct structure", {
  sample_df <- data.frame(
    Event_Name = c("Event A", "Event B"),
    Max_Price = c(100, 200),
    Min_Price = c(50, 80),
    Venue = c("Venue X", "Venue Y")
  )
  
  result <- ticketmaster_analysis(sample_df)
  
  expect_type(result, "list")
  expect_named(result, c("most_events_venue", "least_events_venue", 
                         "highest_ticket_price_event", "highest_ticket_price",
                         "lowest_ticket_price_event", "lowest_ticket_price"))
})
