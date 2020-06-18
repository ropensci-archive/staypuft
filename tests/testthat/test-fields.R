context("fields")

test_that("fields", {
  expect_is(fields, "list")
  expect_is(fields[[1]], "function")
  expect_named(fields)
  expect_equal(fields$field(), Field$new())
})
