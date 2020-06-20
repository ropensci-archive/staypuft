context("fields")

test_that("fields", {
  expect_is(fields, "list")
  expect_is(fields[[1]], "function")
  expect_named(fields)
  expect_equal(fields$field(), Field$new())
  for (i in seq_along(fields)) expect_is(fields[[i]], "function")
  for (i in seq_along(fields)) {
    if (!names(fields)[i] %in% c("nested", "named_list")) {
      expect_is(fields[[i]](), "R6")
    }
  }
  # an unknown field
  expect_null(fields$foobar)
})
