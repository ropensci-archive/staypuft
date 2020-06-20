context("Field")
test_that("Field", {
  expect_is(Field, "R6ClassGenerator")
  expect_is(Field$new(), "Field")
})
