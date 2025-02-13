record_test("path/to/your/app")

test_that("Server logic works", {
  input <- list(file1 = list(datapath = "path/to/test.csv"), header = TRUE)
  output <- list()
  session <- NULL
  
  server(input, output, session)
  
  expect_true(is.data.frame(output$contents()))
})