sch <- Schema$new("FooBar",
  name = puft_fields$character(),
  title = puft_fields$character()
)

context("Schema: structure")
test_that("Schema", {
  expect_is(Schema, "R6ClassGenerator")
  expect_is(Schema$new("foobar"), "Schema")
})

test_that("structure", {
  expect_is(sch, "Schema")
  expect_equal(sch$schema_name, "FooBar")
  expect_is(sch$fields, "list")
  expect_named(sch$fields, c("name","title"))
  expect_is(sch$fields[[1]], "Field")
  expect_is(sch$load, "function")
  expect_is(sch$load_json, "function")
  expect_is(sch$dump, "function")
  expect_is(sch$dump_json, "function")
})

context("Schema: lists")
test_that("Schema: list", {
  # dump/dump_json
  ## list
  x <- list(name = "Jane Doe", title = "Howdy doody")
  expect_is(sch$dump(x), "list")
  expect_is(sch$dump_json(x), "json")
  ## S3
  x <- list(name = "Jane Doe", title = "Howdy doody")
  class(x) <- "mars"
  expect_is(sch$dump(x), "list")
  expect_is(sch$dump_json(x), "json")
  ## R6
  jenny <- R6::R6Class("Numbers",
    public = list(name = "Jenny", title = "hello"))
  x <- jenny$new()
  expect_is(sch$dump(x), "list")
  expect_is(sch$dump_json(x), "json")

  # expect_is(sch$dump_json(x, auto_unbox=TRUE), "json")
})
test_that("Schema: list within list", {
 x <- list(
    list(name = "Jane Doe", title = "hello world"),
    list(name = "Alice Water", title = "bye mars")
  )
  expect_is(sch$load(x, many = TRUE), "list")
})

context("Schema: unknown fields")
test_that("Schema: unknown fields", {
  x <- list(name = "Jane Doe", my_title = "Howdy doody")
  expect_error(sch$dump(x), "named element not in allowed set")
  expect_error(sch$dump_json(x), "named element not in allowed set")
})

test_that("Schema: as_df works", {
  # single list
  x <- list(name = "Jane Doe", title = "hello world")
  res <- sch$load(x, as_df = TRUE)
  expect_is(res, "tbl_df")
  expect_equal(NROW(res), 1)

  # list within list
  x <- list(
    list(name = "Jane Doe", title = "hello world"),
    list(name = "Alice Water", title = "bye mars")
  )
  ers <- sch$load(x, many = TRUE, as_df = TRUE)
  expect_is(ers, "tbl_df")
  expect_equal(NROW(ers), 2)
})

test_that("Schema fails well", {
  expect_error(sch$load(5), "is not TRUE")
})
