context("fields: Field")
test_that("Field", {
  expect_is(Field, "R6ClassGenerator")
  expect_is(Field$new(), "Field")
})

context("fields: Any")
test_that("Any", {
  expect_is(Any, "R6ClassGenerator")
  expect_is(Any$new(), "Any")
  z <- Schema$new("MySchema",
    name = fields$any()
  )
  expect_identical(
    z$load(list(name = 4)),
    list(name = 4)
  )
  expect_identical(
    z$load(list(name = mtcars)),
    list(name = mtcars)
  )
  expect_identical(
    z$load(list(name = c(TRUE, FALSE, FALSE, 5))),
    list(name = c(1, 0, 0, 5))
  )
})
