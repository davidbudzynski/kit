# Tests for charToFact() — character to factor
# Migrated from tests/test_kit.R (custom check() suite) to testthat 3e — see issue #54.
# Each legacy check("id", actual, expected) maps to expect_identical();
# 31 tolerant cases (identical FALSE but all.equal+typeof TRUE) use expect_kit_equal();
# check(..., error=) maps to expect_error(regexp=, fixed=TRUE).
# Legacy IDs are preserved as test_that() labels (prefixed by function for uniqueness).
# Setup and expectations stay interleaved in legacy order: later sections may
# redefine x/y/out_vec/... so setup must not be hoisted above earlier tests.

set.seed(123)

x1 = sample(c(letters,LETTERS),1e2,TRUE)

test_that("charToFact-0021.001", {
  expect_identical(charToFact(c("a","b")), as.factor(c("a","b")))
})


test_that("charToFact-0021.002", {
  expect_identical(charToFact(c("a","b","a")), as.factor(c("a","b","a")))
})


test_that("charToFact-0021.003", {
  expect_identical(charToFact(x1), as.factor(x1))
})


test_that("charToFact-0021.004", {
  expect_error(charToFact(c("2L","1L","3L"),nThread=1), regexp = "Argument 'nThread' (double) must be of type integer.", fixed = TRUE)
})


test_that("charToFact-0021.005", {
  expect_error(charToFact(1L), regexp = "Argument 'x' must be of type character.", fixed = TRUE)
})


test_that("charToFact-0021.006", {
  expect_identical(charToFact(c("a","b",NA,"a")), addNA(as.factor(c("a","b",NA,"a"))))
})


test_that("levels-0021.007", {
  expect_identical(levels(charToFact(x1,decreasing = TRUE)), sort(levels(as.factor(x1)),decreasing = TRUE))
})


test_that("charToFact-0021.008", {
  expect_error(charToFact(c("a","b"),addNA=NA), regexp = "Argument 'addNA' must be TRUE or FALSE.", fixed = TRUE)
})


test_that("charToFact-0021.009", {
  expect_identical(charToFact(c("a","b",NA,"a"), addNA=FALSE), as.factor(c("a","b",NA,"a")))
})


test_that("charToFact-0021.010", {
  expect_identical(charToFact(c("a","b",NA,"c")), addNA(as.factor(c("a","b",NA,"c"))))
})


test_that("charToFact-0021.011", {
  expect_identical(charToFact(c("a","b",NA,"c"),addNA=FALSE), as.factor(c("a","b",NA,"c")))
})


test_that("charToFact-0021.012", {
  expect_identical(charToFact(c("a",NA,"b")), addNA(as.factor(c("a",NA,"b"))))
})


test_that("charToFact-0021.013", {
  expect_identical(charToFact(c("a",NA,"a","b")), addNA(as.factor(c("a",NA,"a","b"))))
})


test_that("charToFact-0021.014", {
  expect_identical(charToFact(c("a",NA,"aa","b")), addNA(as.factor(c("a",NA,"aa","b"))))
})


test_that("charToFact-0021.015", {
  expect_identical(charToFact(c("a",NA,"aa")), addNA(as.factor(c("a",NA,"aa"))))
})

