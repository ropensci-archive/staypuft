context("field classes: Any")
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

context("field classes: Missing")
test_that("Missing", {
  expect_is(Missing, "R6ClassGenerator")
  expect_is(Missing$new(), "Missing")
  z <- Schema$new("MySchema",
    name = fields$missing()
  )
  expect_named(z$fields, "name")
  expect_is(z$fields$name, "Missing")
  expect_equal(Missing$new(), miss_ing)
})

context("field classes: Character")
test_that("Character", {
  expect_is(Character, "R6ClassGenerator")
  expect_is(Character$new(), "Character")
  z <- Schema$new("MySchema",
    name = fields$character()
  )
  expect_named(z$fields, "name")
  expect_is(z$fields$name, "Character")
  # FIXME: this should error
  expect_identical(
    z$load(list(name = 4)),
    list(name = "4")
  )
})

context("field classes: UUID")
test_that("UUID", {
  expect_is(UUID, "R6ClassGenerator")
  expect_is(UUID$new(), "UUID")
  z <- Schema$new("MySchema",
    x = fields$uuid()
  )
  expect_named(z$fields, "x")
  expect_is(z$fields$x, "UUID")
  expect_is(z$load(list(x = "9a5f6bba-4101-48e9-a7e3-b5ac456a04b5")), "list")
  expect_error(z$load(list(x = 4)), "ValidationError")
})
