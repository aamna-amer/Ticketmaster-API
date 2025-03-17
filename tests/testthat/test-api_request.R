test_that("data_request() fetches valid API response", {
  skip_if_not(Sys.getenv("TICKETMASTER_API_KEY") != "", "API key not set, skipping test.")
  
  params <- list(city = "New York")
  response <- data_request("events.json", params)
  
  expect_type(response, "list")  # API should return a list
  expect_true(!is.null(response$`_embedded`$events))  # Events should exist
})

test_that("data_request() handles errors correctly", {
  skip_if_not(Sys.getenv("TICKETMASTER_API_KEY") != "", "API key not set, skipping test.")
  expect_error(data_request("invalid_endpoint", list()), "Error fetching data")
})