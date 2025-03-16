test_that("data_request() fetches valid API response", {
  api_key <- Sys.getenv("TICKETMASTER_API_KEY", unset = NA)
  skip_if(is.na(api_key), "API key not set")
  
  params <- list(city = "New York")
  response <- data_request("events.json", params, api_key)
  
  expect_type(response, "list")  # API should return a list
  expect_true(!is.null(response$`_embedded`$events))  # Events should exist
})

test_that("data_request() handles errors correctly", {
  expect_error(data_request("invalid_endpoint", list(), "INVALID_KEY"), "Error fetching data")
})
