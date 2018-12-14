context("puft_fields")

test_that("puft_fields", {
  expect_is(puft_fields, "list")
  expect_is(puft_fields[[1]], "function")
  expect_named(puft_fields)
  expect_equal(puft_fields$field(), Field$new())
})
