test_that("data_request() fetches valid API response", {
  params <- list(city = "New York")
  response <- data_request("events.json", params)
  
  expect_type(response, "list")  # API should return a list
  expect_true(!is.null(response$`_embedded`$events))  # Events should exist
})

test_that("data_request() handles errors correctly", {
  expect_error(data_request("invalid_endpoint", list()), "Error fetching data")
})