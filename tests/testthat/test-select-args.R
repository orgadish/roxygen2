test_that("warns on invalid input", {
  topic <- RoxyTopic$new()
  topic$add(rd_section("name", "test"))

  expect_snapshot({
    select_args_text(sum, "-xlab:", topic)
    select_args_text(sum, '"a"', topic)
    select_args_text(function(x, y, z) {}, "-x:z", topic)
  })
})

test_that("positive initial values starts from nothing", {
  f <- function(x, y, z) {}

  expect_equal(select_args_text(f, "x y"), c("x", "y"))
})

test_that("negative initial starts from everything", {
  f <- function(x, y, z) {}

  expect_equal(select_args_text(f, "-z"), c("x", "y"))
})

test_that("can alternative exclusion and inclusion", {
  f <- function(x, y, z) {}

  expect_equal(select_args_text(f, "-z z"), c("x", "y", "z"))
  expect_equal(select_args_text(f, "z -z"), character())
})

test_that("select_args includes formals from S3 methods not on generic", {
  # roclet_output.roclet_rd has an is_first argument specific not on the generic.
  expect_all_false(names(formals(roclet_output)) == "is_first")
  expect_in("is_first", names(formals(roclet_output.roclet_rd)))

  expect_contains(select_args(roclet_output), "is_first")
})
