# Tests for psort() — parallel string sort
# Migrated from tests/test_kit.R (custom check() suite) to testthat 3e — see issue #54.
# Each legacy check("id", actual, expected) maps to expect_identical();
# 31 tolerant cases (identical FALSE but all.equal+typeof TRUE) use expect_kit_equal();
# check(..., error=) maps to expect_error(regexp=, fixed=TRUE).
# Legacy IDs are preserved as test_that() labels (prefixed by function for uniqueness).
# Setup and expectations stay interleaved in legacy order: later sections may
# redefine x/y/out_vec/... so setup must not be hoisted above earlier tests.

set.seed(123)

x1 = c("a","ab","c","b","a","c")
x2 = c("aaaba","dfjasdlifjai","jiifjeogiejogp","aabaaaa","gsgj","gerph","aaaaaaa","htjltjlrth",
       "joasdjfisdjfdo","hthe","aaaaaba","j","a","jrykpjl","hkoptjltp","aaaaaa","lprrjt")
x3 = sample(c(letters,LETTERS),1e4,TRUE)
x4 = c("a","ab","c","b","a",NA,"c")
x5 = c("a","ab","c","b"," ","a",NA,"c")
x6 = c("a","ab","c","b"," "," ","a",NA," d","c")
x7 = c("a","ab","c","b"," ","a","",NA," ","c")
x8 = c("b","a","d","c",NA,"")
x9 = sample(c("a","ab","c","b"," ","","a",NA," d","c"), 1e4, TRUE)
x10 = c("b","a","A","B","\xe4","d","c",NA)
Encoding(x10) = "UTF-8"
Encoding(x10[5]) = "latin1"
x11 = rep(x10,3L)

test_that("psort-0020.001", {
  expect_warning(res <- psort(c(2L,1L,3L),c.locale = FALSE), regexp = "Function 'psort' was only implemented for character vectors. Defaulting to base::sort.", fixed = TRUE)
  expect_identical(res, sort(c(2L,1L,3L)))
})


test_that("psort-0020.002", {
  expect_identical(psort(x1), sort(x1))
})


test_that("psort-0020.003", {
  expect_identical(psort(x1,decreasing = TRUE), sort(x1,decreasing = TRUE))
})


test_that("psort-0020.004", {
  expect_identical(psort(x2), sort(x2))
})


test_that("psort-0020.005", {
  expect_identical(psort(x2,decreasing = TRUE), sort(x2,decreasing = TRUE))
})


test_that("psort-0020.006", {
  expect_identical(psort(x3,c.locale = FALSE), sort(x3))
})


test_that("psort-0020.007", {
  expect_identical(psort(x3,decreasing = TRUE,c.locale = FALSE), sort(x3,decreasing = TRUE))
})


test_that("psort-0020.008", {
  expect_identical(psort(x4,na.last = TRUE), sort(x4,na.last = TRUE))
})


test_that("psort-0020.009", {
  expect_identical(psort(x4,na.last = FALSE), sort(x4,na.last = FALSE))
})


test_that("psort-0020.010", {
  expect_identical(psort(x5,na.last = TRUE), sort(x5,na.last = TRUE))
})


test_that("psort-0020.011", {
  expect_identical(psort(x5,na.last = FALSE), sort(x5,na.last = FALSE))
})


test_that("psort-0020.012", {
  expect_identical(psort(x6,na.last = TRUE), sort(x6,na.last = TRUE))
})


test_that("psort-0020.013", {
  expect_identical(psort(x6,na.last = FALSE), sort(x6,na.last = FALSE))
})


test_that("psort-0020.014", {
  expect_identical(psort(x7,na.last = TRUE), sort(x7,na.last = TRUE))
})


test_that("psort-0020.015", {
  expect_identical(psort(x7,na.last = FALSE), sort(x7,na.last = FALSE))
})


test_that("psort-0020.016", {
  expect_identical(psort(x4,na.last = TRUE,decreasing = TRUE), sort(x4,na.last = TRUE,decreasing = TRUE))
})


test_that("psort-0020.017", {
  expect_identical(psort(x4,na.last = FALSE,decreasing = TRUE), sort(x4,na.last = FALSE,decreasing = TRUE))
})


test_that("psort-0020.018", {
  expect_identical(psort(x5,na.last = TRUE,decreasing = TRUE), sort(x5,na.last = TRUE,decreasing = TRUE))
})


test_that("psort-0020.019", {
  expect_identical(psort(x5,na.last = FALSE,decreasing = TRUE), sort(x5,na.last = FALSE,decreasing = TRUE))
})


test_that("psort-0020.020", {
  expect_identical(psort(x6,na.last = TRUE,decreasing = TRUE), sort(x6,na.last = TRUE,decreasing = TRUE))
})


test_that("psort-0020.021", {
  expect_identical(psort(x6,na.last = FALSE,decreasing = TRUE), sort(x6,na.last = FALSE,decreasing = TRUE))
})


test_that("psort-0020.022", {
  expect_identical(psort(x7,na.last = TRUE,decreasing = TRUE), sort(x7,na.last = TRUE,decreasing = TRUE))
})


test_that("psort-0020.023", {
  expect_identical(psort(x7,na.last = FALSE,decreasing = TRUE), sort(x7,na.last = FALSE,decreasing = TRUE))
})


test_that("psort-0020.024", {
  expect_identical(psort(x4,na.last = NA), sort(x4,na.last = NA))
})


test_that("psort-0020.025", {
  expect_identical(psort(x5,na.last = NA), sort(x5,na.last = NA))
})


test_that("psort-0020.026", {
  expect_identical(psort(x6,na.last = NA), sort(x6,na.last = NA))
})


test_that("psort-0020.027", {
  expect_identical(psort(x7,na.last = NA), sort(x7,na.last = NA))
})


test_that("psort-0020.028", {
  expect_identical(psort(x4,na.last = NA,decreasing = TRUE), sort(x4,na.last = NA,decreasing = TRUE))
})


test_that("psort-0020.029", {
  expect_identical(psort(x5,na.last = NA,decreasing = TRUE), sort(x5,na.last = NA,decreasing = TRUE))
})


test_that("psort-0020.030", {
  expect_identical(psort(x6,na.last = NA,decreasing = TRUE), sort(x6,na.last = NA,decreasing = TRUE))
})


test_that("psort-0020.031", {
  expect_identical(psort(x7,na.last = NA,decreasing = TRUE), sort(x7,na.last = NA,decreasing = TRUE))
})


test_that("psort-0020.032", {
  expect_identical(psort(x8,na.last = TRUE), sort(x8,na.last = TRUE))
})


test_that("psort-0020.033", {
  expect_identical(psort(x8,na.last = FALSE), sort(x8,na.last = FALSE))
})


test_that("psort-0020.034", {
  expect_identical(psort(x8,na.last = TRUE,decreasing = TRUE), sort(x8,na.last = TRUE,decreasing = TRUE))
})


test_that("psort-0020.035", {
  expect_identical(psort(x8,na.last = FALSE,decreasing = TRUE), sort(x8,na.last = FALSE,decreasing = TRUE))
})


test_that("psort-0020.036", {
  expect_identical(psort(x8,na.last = NA), sort(x8,na.last = NA))
})


test_that("psort-0020.037", {
  expect_identical(psort(x8,na.last = NA,decreasing = TRUE), sort(x8,na.last = NA,decreasing = TRUE))
})


test_that("psort-0020.050", {
  expect_identical(psort(x10,c.locale = FALSE), sort(x10))
})


test_that("psort-0020.051", {
  expect_identical(psort(x10,decreasing = TRUE,c.locale = FALSE), sort(x10,decreasing = TRUE))
})


test_that("psort-0020.052", {
  expect_error(psort(x1,na.last = 2), regexp = "Argument 'na.last' must be TRUE, FALSE or NA.", fixed = TRUE)
})


test_that("psort-0020.053", {
  expect_error(psort(x1,decreasing = 2), regexp = "Argument 'decreasing' must be TRUE or FALSE.", fixed = TRUE)
})


test_that("psort-0020.056", {
  expect_error(psort(c("2L","1L","3L"),nThread=1), regexp = "Argument 'nThread' (double) must be of type integer.", fixed = TRUE)
})


test_that("psort-0020.058", {
  expect_error(psort(x1,c.locale = NA), regexp = "Argument 'c.locale' must be TRUE or FALSE.", fixed = TRUE)
})


test_that("psort-0020.059", {
  expect_identical(psort(x1,c.locale = TRUE), sort(x1,method="radix"))
})


test_that("psort-0020.060", {
  expect_identical(psort(x1,decreasing = TRUE,c.locale = TRUE), sort(x1,decreasing = TRUE,method="radix"))
})


test_that("psort-0020.061", {
  expect_identical(psort(x2,c.locale = TRUE), sort(x2,method="radix"))
})


test_that("psort-0020.062", {
  expect_identical(psort(x2,decreasing = TRUE,c.locale = TRUE), sort(x2,decreasing = TRUE,method="radix"))
})


test_that("psort-0020.063", {
  expect_identical(psort(x6,c.locale = TRUE), sort(x6,method="radix"))
})


test_that("psort-0020.064", {
  expect_identical(psort(x6,decreasing = TRUE,c.locale = TRUE), sort(x6,decreasing = TRUE,method="radix"))
})


test_that("psort-0020.065", {
  expect_identical(psort(x6,c.locale = TRUE,na.last = TRUE), sort(x6,method="radix",na.last = TRUE))
})


test_that("psort-0020.066", {
  expect_identical(psort(x6,decreasing = TRUE,c.locale = TRUE,na.last = TRUE), sort(x6,decreasing = TRUE,method="radix",na.last = TRUE))
})


test_that("psort-0020.067", {
  expect_identical(psort(x6,c.locale = TRUE,na.last = FALSE), sort(x6,method="radix",na.last = FALSE))
})


test_that("psort-0020.068", {
  expect_identical(psort(x6,decreasing = TRUE,c.locale = TRUE,na.last = FALSE), sort(x6,decreasing = TRUE,method="radix",na.last = FALSE))
})


test_that("psort-0020.069", {
  expect_identical(psort(x10,c.locale = TRUE), sort(x10,method="radix"))
})


test_that("psort-0020.070", {
  expect_identical(psort(x10,decreasing = TRUE,c.locale = TRUE), sort(x10,decreasing = TRUE,method="radix"))
})


test_that("psort-0020.071", {
  expect_identical(psort(x11,c.locale = TRUE), sort(x11,method="radix"))
})


test_that("psort-0020.072", {
  expect_identical(psort(x11,decreasing = TRUE,c.locale = TRUE), sort(x11,decreasing = TRUE,method="radix"))
})


test_that("psort-0020.073", {
  expect_identical(psort(x11,c.locale = FALSE), sort(x11))
})


test_that("psort-0020.074", {
  expect_identical(psort(x11,decreasing = TRUE,c.locale = FALSE), sort(x11,decreasing = TRUE))
})

