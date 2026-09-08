# Tests for setlevels() — fast factor level recoding
# Migrated from tests/test_kit.R (custom check() suite) to testthat 3e — see issue #54.
# Each legacy check("id", actual, expected) maps to expect_identical();
# 31 tolerant cases (identical FALSE but all.equal+typeof TRUE) use expect_kit_equal();
# check(..., error=) maps to expect_error(regexp=, fixed=TRUE).
# Legacy IDs are preserved as test_that() labels (prefixed by function for uniqueness).
# Setup and expectations stay interleaved in legacy order: later sections may
# redefine x/y/out_vec/... so setup must not be hoisted above earlier tests.

set.seed(123)


test_that("setlevels-0007.01", {
  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "B", "C"), c("X", "Y", "Z")), factor(c("X", "X", "Y", "Y", "Y", "Z")))
})


test_that("setlevels-0007.02", {
  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "B"), c("X", "Y", "Z")), regexp = "'old' and 'new' are not the same length.", fixed = TRUE)
})


test_that("setlevels-0007.03", {
  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "B", "B"), c("X", "Y", "Z")), regexp = "'old' has duplicated value. Please make sure no duplicated values are introduced.", fixed = TRUE)
})


test_that("setlevels-0007.04", {
  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "B", "C"), c("X", "X", "Z")), regexp = "'new' has duplicated value. Please make sure no duplicated values are introduced.", fixed = TRUE)
})


test_that("setlevels-0007.05", {
  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A"), c("X")), factor(c("X", "X", "B", "B", "B", "C"), levels = c("X", "B", "C")))
})


test_that("setlevels-0007.06", {
  expect_identical(setlevels(factor(c(1, 1, 2, 2, 2, 3)), c("1","2","3"), c("X","Y","Z")), factor(c("X", "X", "Y", "Y", "Y", "Z")))
})


test_that("setlevels-0007.07", {
  expect_error(setlevels(factor(c(1, 1, 2, 2, 2, 3)), 1:3, c("X","Y","Z")), regexp = "Type of 'old' must be character.", fixed = TRUE)
})


test_that("setlevels-0007.08", {
  expect_error(setlevels(factor(c(1, 1, 2, 2, 2, 3)), c("1","2","3"), 1:3), regexp = "Type of 'new' must be character.", fixed = TRUE)
})


test_that("setlevels-0007.09", {
  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C")), new = c("X", "Y", "Z")), factor(c("X", "X", "Y", "Y", "Y", "Z")))
})


test_that("setlevels-0007.10", {
  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "B", "D"), c("X", "Y", "Z")), regexp = "Element 'D' of 'old' does not exist in 'x'.", fixed = TRUE)
})


test_that("setlevels-0007.11", {
  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C"))), regexp = "argument \"new\" is missing, with no default", fixed = TRUE)
})


test_that("setlevels-0007.12", {
  expect_error(setlevels(c("A", "A", "B", "B", "B", "C"), c("A", "B", "C"), c("X", "Y", "Z")), regexp = "'setlevels' must be passed a factor.", fixed = TRUE)
})


test_that("setlevels-0007.13", {
  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "A","B", "B"), c("X", "X","Y", "Z")), regexp = "'old' has duplicated value. Please make sure no duplicated values are introduced.", fixed = TRUE)
})


test_that("setlevels-0007.14", {
  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C"), levels = c("C","A","B")), c("C", "A", "B"), c("Z", "X", "Y")), factor(c("X", "X", "Y", "Y", "Y", "Z"), levels = c("Z", "X", "Y")))
})


test_that("setlevels-0007.15", {
  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C"), levels = c("C","A","B","D")), c("A", "B", "C"), c("X", "Y", "Z")), factor(c("X", "X", "Y", "Y", "Y", "Z"), levels = c("Z", "X", "Y","D")))
})


test_that("setlevels-0007.16", {
  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B","D")), c("A", "B", "C"), c("X", "Y", "Z")), factor(c("X", "X", "Y", "Y", "Y", "Z",NA,NA), levels = c("Z", "X", "Y","D")))
})


test_that("setlevels-0007.17", {
  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B",NA), exclude = NULL), c("A", "B", "C"), c("X", "Y", "Z")), factor(c("X", "X", "Y", "Y", "Y", "Z",NA,NA), levels = c("Z", "X", "Y",NA), exclude=NULL))
})


test_that("setlevels-0007.18", {
  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C"), levels = c("C","A","B",NA), exclude = NULL), c("A", "B", "C"), c("X", "Y", "Z")), factor(c("X", "X", "Y", "Y", "Y", "Z"), levels = c("Z", "X", "Y",NA), exclude=NULL))
})


test_that("setlevels-0007.19", {
  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B",NA), exclude = NULL), c("A", NA), c("X","D")), factor(c("X", "X", "B", "B", "B", "C","D","D"), levels = c("C", "X", "B","D")))
})


test_that("setlevels-0007.20", {
  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B",NA), exclude = NULL), as.character(NA), "D"), factor(c("A", "A", "B", "B", "B", "C","D","D"), levels = c("C", "A", "B","D")))
})


test_that("setlevels-0007.21", {
  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B",NA), exclude = NULL), NA_character_, "D"), factor(c("A", "A", "B", "B", "B", "C","D","D"), levels = c("C", "A", "B","D")))
})


test_that("setlevels-0007.22", {
  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B",NA), exclude = NULL), NA, "D"), regexp = "Type of 'old' must be character.", fixed = TRUE)
})


test_that("setlevels-0007.23", {
  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B",NA), exclude = NULL), "A", NA), regexp = "Type of 'new' must be character.", fixed = TRUE)
})


test_that("setlevels-0007.24", {
  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C"), levels = c("A","B","C")), c("A", "B", "C"), c("X", "Y", "Z"), c(FALSE,TRUE)), regexp = "Argument 'skip_absent' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("setlevels-0007.25", {
  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "B", "D"), c("X", "Y", "Z"), TRUE), factor(c("X", "X", "Y", "Y", "Y", "C"), levels = c("X","Y","C")))
})

