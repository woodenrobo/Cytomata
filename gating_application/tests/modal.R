library(shinytest2)
library(testthat)

Sys.setenv(CHROMOTE_CHROME = "/mnt/c/Program Files/Google/Chrome/Application/chrome.exe")

test_that("Modal focus and Enter key functionality works", {
  app <- AppDriver$new("../")
  
  # Open the modal
  app$click("show_modal")
  app$wait_for_idle()

  # Test that misc.js is loaded
  expect_true(app$get_js("return $('script[src=\"misc.js\"]').length > 0"))

  # Test that the input field is focused
  expect_equal(
    app$get_js("return document.activeElement.id"),
    "gate_name"
  )

  # Enter a gate name
  app$set_inputs(gate_name = "test_gate")

  # Simulate pressing Enter
  app$run_js("
    var e = $.Event('keypress');
    e.which = 13;
    $('#gate_name').trigger(e);
  ")
  app$wait_for_idle()

  # Check if the modal was closed (indicating the OK button was clicked)
  expect_false(app$get_js("return $('.modal').is(':visible')"))
})


test_that("Modal opens correctly", {
  app <- AppDriver$new("../")
  app$click("show_modal")
  app$wait_for_idle()
  expect_true(app$get_js("return $('.modal').is(':visible')"))
})

test_that("Cancel button closes modal", {
  app <- AppDriver$new("../")
  app$click("show_modal")
  app$wait_for_idle()
  app$click("cancel")
  app$wait_for_idle()
  expect_false(app$get_js("return $('.modal').is(':visible')"))
})


