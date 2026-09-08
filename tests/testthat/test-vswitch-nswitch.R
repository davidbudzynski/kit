# Tests for vswitch() and nswitch() — vectorised switch
# Migrated from tests/test_kit.R (custom check() suite) to testthat 3e — see issue #54.
# Each legacy check("id", actual, expected) maps to expect_identical();
# 31 tolerant cases (identical FALSE but all.equal+typeof TRUE) use expect_kit_equal();
# check(..., error=) maps to expect_error(regexp=, fixed=TRUE).
# Legacy IDs are preserved as test_that() labels (prefixed by function for uniqueness).
# Setup and expectations stay interleaved in legacy order: later sections may
# redefine x/y/out_vec/... so setup must not be hoisted above earlier tests.

set.seed(123)

x0 = c(1L, 0L, 0L, 1L, NA_integer_, 1L, NA_integer_)
x1 = c(NA_integer_, 1L, 0L, 1L, NA_integer_, 1L, NA_integer_)
values0 = c(0L, 1L)
outputs0 = list(0L, 1L)
outputs1 = list(11:17, 21:27)
outputs0l = list(FALSE, TRUE)
outputs1l = list(as.logical(11:17), as.logical( 21:27))
outputs0n = list(0, 1)
outputs1n = list(as.numeric(11:17), as.numeric( 21:27))
outputs0c = list(0+0i, 1+0i)
outputs1c = list(as.complex(11:17), as.complex( 21:27))
outputs0s = list("0", "1")
outputs1s = list(as.character(11:17), as.character(21:27))
outputs0v = list(as.list(0L), as.list(1L))
outputs1v = list(as.list(11:17), as.list(21:27))
na0 = NA_integer_
na1 = 1:7
out11 = c(NA_integer_, 22L, 13L, 24L, NA_integer_, 26L, NA_integer_)
out12 = c(1L, 22L, 13L, 24L, 5L, 26L, 7L)
x0l = list(1L, 0L, 0L, 1L, NULL, 1L, NULL)
x1l = list(NULL, 1L, 0L, 1L, NULL, 1L, NULL)
out11l = list(NULL, 22L, 13L, 24L, NULL, 26L, NULL)
out12l = list(1L, 22L, 13L, 24L, 5L, 26L, 7L)
na0l = list(NULL)
class2133 = setClass("class2133", slots=list(x="numeric"))
s1 = class2133(x=20191231)
s2 = class2133(x=20191230)
enc1 = "fa\xE7ile"
Encoding(enc1) = "latin1"
enc2 = enc2utf8(enc1)

test_that("vswitch-0008.001", {
  expect_identical(vswitch(x0, values0, outputs0), x0)
})


test_that("vswitch-0008.002", {
  expect_identical(vswitch(x0, values0, outputs0, na0), x0)
})


test_that("vswitch-0008.003", {
  expect_identical(vswitch(x1, values0, outputs1), out11)
})


test_that("vswitch-0008.004", {
  expect_identical(vswitch(x1, values0, outputs1, na1), out12)
})


test_that("vswitch-0008.005", {
  expect_error(vswitch(s1, values0, outputs0), regexp = "S4 class objects for argument 'x' are not supported.", fixed = TRUE)
})


test_that("vswitch-0008.006", {
  expect_error(vswitch(x0, s1, outputs0), regexp = "S4 class objects for argument 'values' are not supported.", fixed = TRUE)
})


test_that("vswitch-0008.007", {
  expect_error(vswitch(x0, values0, outputs0, s1), regexp = "S4 class objects for argument 'na' are not supported.", fixed = TRUE)
})


test_that("vswitch-0008.008", {
  expect_error(vswitch(x0, values0[1L], outputs0), regexp = "Length of 'values' and 'outputs' are different. Please make sure they are the same.", fixed = TRUE)
})


test_that("vswitch-0008.009", {
  expect_error(vswitch(x0, as.logical(values0), outputs0), regexp = "Type of 'x' and 'values' are different. Please make sure they are the same.", fixed = TRUE)
})


test_that("vswitch-0008.010", {
  expect_error(vswitch(x0, values0, outputs0, NA), regexp = "Type of 'na' and 'outputs' are different. Please make sure they are the same.", fixed = TRUE)
})


test_that("vswitch-0008.011", {
  expect_error(vswitch(x0, values0, outputs0, 1:2), regexp = "Length of 'na'  is different than 1 and length of 'x'. Please make length of 'na' is 1 or length of 'x'.", fixed = TRUE)
})


test_that("vswitch-0008.012", {
  expect_error(vswitch(1, c(as.Date("2020-04-14")), list(1L)), regexp = "Argument 'x' and 'values' must have same class.", fixed = TRUE)
})


test_that("vswitch-0008.013", {
  expect_identical(vswitch(1, 1, 1L), 1L)
})


test_that("vswitch-0008.014", {
  expect_error(vswitch(factor("a"), factor("b"), list(1L)), regexp = "Argument 'x' and 'values' are both factor but their levels are different.", fixed = TRUE)
})


test_that("vswitch-0008.015", {
  expect_error(vswitch(x0, values0[1], list(as.Date("2020-04-14")), 2), regexp = "Argument 'na' and items of 'outputs' must have same class.", fixed = TRUE)
})


test_that("vswitch-0008.016", {
  expect_error(vswitch(x0, values0, list(factor(c("a","b")),factor(c("a","b"))), factor("c")), regexp = "Argument 'na' and items of 'outputs' are both factor but their levels are different.", fixed = TRUE)
})


test_that("vswitch-0008.017", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), outputs0), x0)
})


test_that("vswitch-0008.018", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), outputs0, na0), x0)
})


test_that("vswitch-0008.019", {
  expect_identical(vswitch(as.numeric(x1), as.numeric(values0), outputs1), out11)
})


test_that("vswitch-0008.020", {
  expect_identical(vswitch(as.numeric(x1), as.numeric(values0), outputs1, na1), out12)
})


test_that("vswitch-0008.021", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), outputs0), x0)
})


test_that("vswitch-0008.022", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), outputs0, na0), x0)
})


test_that("vswitch-0008.023", {
  expect_identical(vswitch(as.complex(x1), as.complex(values0), outputs1), out11)
})


test_that("vswitch-0008.024", {
  expect_identical(vswitch(as.complex(x1), as.complex(values0), outputs1, na1), out12)
})


test_that("vswitch-0008.025", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), outputs0), x0)
})


test_that("vswitch-0008.026", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), outputs0, na0), x0)
})


test_that("vswitch-0008.027", {
  expect_identical(vswitch(as.logical(x1), as.logical(values0), outputs1), out11)
})


test_that("vswitch-0008.028", {
  expect_identical(vswitch(as.logical(x1), as.logical(values0), outputs1, na1), out12)
})


test_that("vswitch-0008.029", {
  expect_identical(vswitch(as.character(x0), as.character(values0), outputs0), x0)
})


test_that("vswitch-0008.030", {
  expect_identical(vswitch(as.character(x0), as.character(values0), outputs0, na0), x0)
})


test_that("vswitch-0008.031", {
  expect_identical(vswitch(as.character(x1), as.character(values0), outputs1), out11)
})


test_that("vswitch-0008.032", {
  expect_identical(vswitch(as.character(x1), as.character(values0), outputs1, na1), out12)
})


test_that("vswitch-0008.033", {
  expect_identical(vswitch(as.list(x0), as.list(values0), outputs0), x0)
})


test_that("vswitch-0008.034", {
  expect_identical(vswitch(as.list(x0), as.list(values0), outputs0, na0), x0)
})


test_that("vswitch-0008.035", {
  expect_identical(vswitch(as.list(x1), as.list(values0), outputs1), out11)
})


test_that("vswitch-0008.036", {
  expect_identical(vswitch(as.list(x1), as.list(values0), outputs1, na1), out12)
})


test_that("vswitch-0008.037", {
  expect_identical(vswitch(x0, values0, outputs0l), as.logical(x0))
})


test_that("vswitch-0008.038", {
  expect_identical(vswitch(x0, values0, outputs0l, as.logical(na0)), as.logical(x0))
})


test_that("vswitch-0008.039", {
  expect_identical(vswitch(x1, values0, outputs1l), as.logical(out11))
})


test_that("vswitch-0008.040", {
  expect_identical(vswitch(x1, values0, outputs1l, as.logical(na1)), as.logical(out12))
})


test_that("vswitch-0008.041", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), outputs0l), as.logical(x0))
})


test_that("vswitch-0008.042", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), outputs0l, as.logical(na0)), as.logical(x0))
})


test_that("vswitch-0008.043", {
  expect_identical(vswitch(as.numeric(x1), as.numeric(values0), outputs1l), as.logical(out11))
})


test_that("vswitch-0008.044", {
  expect_identical(vswitch(as.numeric(x1), as.numeric(values0), outputs1l, as.logical(na1)), as.logical(out12))
})


test_that("vswitch-0008.045", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), outputs0l), as.logical(x0))
})


test_that("vswitch-0008.046", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), outputs0l, as.logical(na0)), as.logical(x0))
})


test_that("vswitch-0008.047", {
  expect_identical(vswitch(as.complex(x1), as.complex(values0), outputs1l), as.logical(out11))
})


test_that("vswitch-0008.048", {
  expect_identical(vswitch(as.complex(x1), as.complex(values0), outputs1l, as.logical(na1)), as.logical(out12))
})


test_that("vswitch-0008.049", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), outputs0l), as.logical(x0))
})


test_that("vswitch-0008.050", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), outputs0l, as.logical(na0)), as.logical(x0))
})


test_that("vswitch-0008.051", {
  expect_identical(vswitch(as.logical(x1), as.logical(values0), outputs1l), as.logical(out11))
})


test_that("vswitch-0008.052", {
  expect_identical(vswitch(as.logical(x1), as.logical(values0), outputs1l, as.logical(na1)), as.logical(out12))
})


test_that("vswitch-0008.053", {
  expect_identical(vswitch(as.character(x0), as.character(values0), outputs0l), as.logical(x0))
})


test_that("vswitch-0008.054", {
  expect_identical(vswitch(as.character(x0), as.character(values0), outputs0l, as.logical(na0)), as.logical(x0))
})


test_that("vswitch-0008.055", {
  expect_identical(vswitch(as.character(x1), as.character(values0), outputs1l), as.logical(out11))
})


test_that("vswitch-0008.056", {
  expect_identical(vswitch(as.character(x1), as.character(values0), outputs1l, as.logical(na1)), as.logical(out12))
})


test_that("vswitch-0008.057", {
  expect_identical(vswitch(as.list(x0), as.list(values0), outputs0l), as.logical(x0))
})


test_that("vswitch-0008.058", {
  expect_identical(vswitch(as.list(x0), as.list(values0), outputs0l, as.logical(na0)), as.logical(x0))
})


test_that("vswitch-0008.059", {
  expect_identical(vswitch(as.list(x1), as.list(values0), outputs1l), as.logical(out11))
})


test_that("vswitch-0008.060", {
  expect_identical(vswitch(as.list(x1), as.list(values0), outputs1l, as.logical(na1)), as.logical(out12))
})


test_that("vswitch-0008.061", {
  expect_identical(vswitch(x0, values0, outputs0n), as.numeric(x0))
})


test_that("vswitch-0008.062", {
  expect_identical(vswitch(x0, values0, outputs0n, as.numeric(na0)), as.numeric(x0))
})


test_that("vswitch-0008.063", {
  expect_identical(vswitch(x1, values0, outputs1n), as.numeric(out11))
})


test_that("vswitch-0008.064", {
  expect_identical(vswitch(x1, values0, outputs1n, as.numeric(na1)), as.numeric(out12))
})


test_that("vswitch-0008.065", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), outputs0n), as.numeric(x0))
})


test_that("vswitch-0008.066", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), outputs0n, as.numeric(na0)), as.numeric(x0))
})


test_that("vswitch-0008.067", {
  expect_identical(vswitch(as.numeric(x1), as.numeric(values0), outputs1n), as.numeric(out11))
})


test_that("vswitch-0008.068", {
  expect_identical(vswitch(as.numeric(x1), as.numeric(values0), outputs1n, as.numeric(na1)), as.numeric(out12))
})


test_that("vswitch-0008.069", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), outputs0n), as.numeric(x0))
})


test_that("vswitch-0008.070", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), outputs0n, as.numeric(na0)), as.numeric(x0))
})


test_that("vswitch-0008.071", {
  expect_identical(vswitch(as.complex(x1), as.complex(values0), outputs1n), as.numeric(out11))
})


test_that("vswitch-0008.072", {
  expect_identical(vswitch(as.complex(x1), as.complex(values0), outputs1n, as.numeric(na1)), as.numeric(out12))
})


test_that("vswitch-0008.073", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), outputs0n), as.numeric(x0))
})


test_that("vswitch-0008.074", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), outputs0n, as.numeric(na0)), as.numeric(x0))
})


test_that("vswitch-0008.075", {
  expect_identical(vswitch(as.logical(x1), as.logical(values0), outputs1n), as.numeric(out11))
})


test_that("vswitch-0008.076", {
  expect_identical(vswitch(as.logical(x1), as.logical(values0), outputs1n, as.numeric(na1)), as.numeric(out12))
})


test_that("vswitch-0008.077", {
  expect_identical(vswitch(as.character(x0), as.character(values0), outputs0n), as.numeric(x0))
})


test_that("vswitch-0008.078", {
  expect_identical(vswitch(as.character(x0), as.character(values0), outputs0n, as.numeric(na0)), as.numeric(x0))
})


test_that("vswitch-0008.079", {
  expect_identical(vswitch(as.character(x1), as.character(values0), outputs1n), as.numeric(out11))
})


test_that("vswitch-0008.080", {
  expect_identical(vswitch(as.character(x1), as.character(values0), outputs1n, as.numeric(na1)), as.numeric(out12))
})


test_that("vswitch-0008.081", {
  expect_identical(vswitch(as.list(x0), as.list(values0), outputs0n), as.numeric(x0))
})


test_that("vswitch-0008.082", {
  expect_identical(vswitch(as.list(x0), as.list(values0), outputs0n, as.numeric(na0)), as.numeric(x0))
})


test_that("vswitch-0008.083", {
  expect_identical(vswitch(as.list(x1), as.list(values0), outputs1n), as.numeric(out11))
})


test_that("vswitch-0008.084", {
  expect_identical(vswitch(as.list(x1), as.list(values0), outputs1n, as.numeric(na1)), as.numeric(out12))
})


test_that("vswitch-0008.085", {
  expect_identical(vswitch(x0, values0, outputs0s), as.character(x0))
})


test_that("vswitch-0008.086", {
  expect_identical(vswitch(x0, values0, outputs0s, as.character(na0)), as.character(x0))
})


test_that("vswitch-0008.087", {
  expect_identical(vswitch(x1, values0, outputs1s), as.character(out11))
})


test_that("vswitch-0008.088", {
  expect_identical(vswitch(x1, values0, outputs1s, as.character(na1)), as.character(out12))
})


test_that("vswitch-0008.089", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), outputs0s), as.character(x0))
})


test_that("vswitch-0008.090", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), outputs0s, as.character(na0)), as.character(x0))
})


test_that("vswitch-0008.091", {
  expect_identical(vswitch(as.numeric(x1), as.numeric(values0), outputs1s), as.character(out11))
})


test_that("vswitch-0008.092", {
  expect_identical(vswitch(as.numeric(x1), as.numeric(values0), outputs1s, as.character(na1)), as.character(out12))
})


test_that("vswitch-0008.093", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), outputs0s), as.character(x0))
})


test_that("vswitch-0008.094", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), outputs0s, as.character(na0)), as.character(x0))
})


test_that("vswitch-0008.095", {
  expect_identical(vswitch(as.complex(x1), as.complex(values0), outputs1s), as.character(out11))
})


test_that("vswitch-0008.096", {
  expect_identical(vswitch(as.complex(x1), as.complex(values0), outputs1s, as.character(na1)), as.character(out12))
})


test_that("vswitch-0008.097", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), outputs0s), as.character(x0))
})


test_that("vswitch-0008.098", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), outputs0s, as.character(na0)), as.character(x0))
})


test_that("vswitch-0008.099", {
  expect_identical(vswitch(as.logical(x1), as.logical(values0), outputs1s), as.character(out11))
})


test_that("vswitch-0008.100", {
  expect_identical(vswitch(as.logical(x1), as.logical(values0), outputs1s, as.character(na1)), as.character(out12))
})


test_that("vswitch-0008.101", {
  expect_identical(vswitch(as.character(x0), as.character(values0), outputs0s), as.character(x0))
})


test_that("vswitch-0008.102", {
  expect_identical(vswitch(as.character(x0), as.character(values0), outputs0s, as.character(na0)), as.character(x0))
})


test_that("vswitch-0008.103", {
  expect_identical(vswitch(as.character(x1), as.character(values0), outputs1s), as.character(out11))
})


test_that("vswitch-0008.104", {
  expect_identical(vswitch(as.character(x1), as.character(values0), outputs1s, as.character(na1)), as.character(out12))
})


test_that("vswitch-0008.105", {
  expect_identical(vswitch(as.list(x0), as.list(values0), outputs0s), as.character(x0))
})


test_that("vswitch-0008.106", {
  expect_identical(vswitch(as.list(x0), as.list(values0), outputs0s, as.character(na0)), as.character(x0))
})


test_that("vswitch-0008.107", {
  expect_identical(vswitch(as.list(x1), as.list(values0), outputs1s), as.character(out11))
})


test_that("vswitch-0008.108", {
  expect_identical(vswitch(as.list(x1), as.list(values0), outputs1s, as.character(na1)), as.character(out12))
})


test_that("vswitch-0008.109", {
  expect_identical(vswitch(x0, values0, outputs0v), x0l)
})


test_that("vswitch-0008.110", {
  expect_identical(vswitch(x0, values0, outputs0v, na0l), x0l)
})


test_that("vswitch-0008.111", {
  expect_identical(vswitch(x1, values0, outputs1v), out11l)
})


test_that("vswitch-0008.112", {
  expect_identical(vswitch(x1, values0, outputs1v, as.list(na1)), out12l)
})


test_that("vswitch-0008.113", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), outputs0v), x0l)
})


test_that("vswitch-0008.114", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), outputs0v, na0l), x0l)
})


test_that("vswitch-0008.115", {
  expect_identical(vswitch(as.numeric(x1), as.numeric(values0), outputs1v), out11l)
})


test_that("vswitch-0008.116", {
  expect_identical(vswitch(as.numeric(x1), as.numeric(values0), outputs1v, as.list(na1)), out12l)
})


test_that("vswitch-0008.117", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), outputs0v), x0l)
})


test_that("vswitch-0008.118", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), outputs0v, na0l), x0l)
})


test_that("vswitch-0008.119", {
  expect_identical(vswitch(as.complex(x1), as.complex(values0), outputs1v), out11l)
})


test_that("vswitch-0008.120", {
  expect_identical(vswitch(as.complex(x1), as.complex(values0), outputs1v, as.list(na1)), out12l)
})


test_that("vswitch-0008.121", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), outputs0v), x0l)
})


test_that("vswitch-0008.122", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), outputs0v, na0l), x0l)
})


test_that("vswitch-0008.123", {
  expect_identical(vswitch(as.logical(x1), as.logical(values0), outputs1v), out11l)
})


test_that("vswitch-0008.124", {
  expect_identical(vswitch(as.logical(x1), as.logical(values0), outputs1v, as.list(na1)), out12l)
})


test_that("vswitch-0008.125", {
  expect_identical(vswitch(as.character(x0), as.character(values0), outputs0v), x0l)
})


test_that("vswitch-0008.126", {
  expect_identical(vswitch(as.character(x0), as.character(values0), outputs0v, na0l), x0l)
})


test_that("vswitch-0008.127", {
  expect_identical(vswitch(as.character(x1), as.character(values0), outputs1v), out11l)
})


test_that("vswitch-0008.128", {
  expect_identical(vswitch(as.character(x1), as.character(values0), outputs1v, as.list(na1)), out12l)
})


test_that("vswitch-0008.129", {
  expect_identical(vswitch(as.list(x0), as.list(values0), outputs0v), x0l)
})


test_that("vswitch-0008.130", {
  expect_identical(vswitch(as.list(x0), as.list(values0), outputs0v, na0l), x0l)
})


test_that("vswitch-0008.131", {
  expect_identical(vswitch(as.list(x1), as.list(values0), outputs1v), out11l)
})


test_that("vswitch-0008.132", {
  expect_identical(vswitch(as.list(x1), as.list(values0), outputs1v, as.list(na1)), out12l)
})


test_that("vswitch-0008.133", {
  expect_error(vswitch(x0, values0, list(as.raw(0),as.raw(1))), regexp = "Type raw is not supported for argument 'outputs'", fixed = TRUE)
})


test_that("vswitch-0008.134", {
  expect_error(vswitch(as.raw(rep(0,7L)), c(as.raw(0), as.raw(1)), outputs0), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("vswitch-0008.135", {
  expect_error(vswitch(as.raw(rep(0,7L)), c(as.raw(0), as.raw(1)), outputs0l), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("vswitch-0008.136", {
  expect_error(vswitch(as.raw(rep(0,7L)), c(as.raw(0), as.raw(1)), outputs0n), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("vswitch-0008.137", {
  expect_error(vswitch(as.raw(rep(0,7L)), c(as.raw(0), as.raw(1)), outputs0s), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("vswitch-0008.138", {
  expect_error(vswitch(as.raw(rep(0,7L)), c(as.raw(0), as.raw(1)), outputs0c), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("vswitch-0008.139", {
  expect_error(vswitch(as.raw(rep(0,7L)), c(as.raw(0), as.raw(1)), outputs0v), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("vswitch-0008.140", {
  expect_identical(vswitch(factor(c("a","b"), levels=letters[1:2]), factor("a", levels=letters[1:2]), list(1L)), c(1L, NA_integer_))
})


test_that("vswitch-0008.141", {
  expect_identical(vswitch(x0, values0, list(factor("a", levels=letters[1:2]),factor("a", levels=letters[1:2])), factor("b", levels=letters[1:2])), factor(c("a","a","a","a","b","a","b"), levels=letters[1:2]))
})


test_that("vswitch-0008.142", {
  expect_error(vswitch(x0, values0, list(1L,s2)), regexp = "S4 class objects for argument 'outputs' (item 2) are not supported.", fixed = TRUE)
})


test_that("vswitch-0008.143", {
  expect_identical(vswitch(x0, values0, list(as.Date("2020-04-14"),as.Date("2020-04-15"))), c(as.Date("2020-04-15"),as.Date("2020-04-14"),as.Date("2020-04-14"),as.Date("2020-04-15"),NA,as.Date("2020-04-15"),NA))
})


test_that("vswitch-0008.144", {
  expect_error(vswitch(x0, values0, list(1:2,3)), regexp = "Length of item 1 of 'output' is different than 1 and length of 'x'. Please make sure that all items of 'output' have length 1 or length of 'x'(7).", fixed = TRUE)
})


test_that("vswitch-0008.145", {
  expect_kit_equal(vswitch(x0, values0, outputs0c), as.complex(x0))
})


test_that("vswitch-0008.146", {
  expect_identical(vswitch(x0, values0, outputs0c, as.complex(na0)), as.complex(x0))
})


test_that("vswitch-0008.147", {
  expect_kit_equal(vswitch(x1, values0, outputs1c), as.complex(out11))
})


test_that("vswitch-0008.148", {
  expect_identical(vswitch(x1, values0, outputs1c, as.complex(na1)), as.complex(out12))
})


test_that("vswitch-0008.149", {
  expect_kit_equal(vswitch(as.numeric(x0), as.numeric(values0), outputs0c), as.complex(x0))
})


test_that("vswitch-0008.150", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), outputs0c, as.complex(na0)), as.complex(x0))
})


test_that("vswitch-0008.151", {
  expect_kit_equal(vswitch(as.numeric(x1), as.numeric(values0), outputs1c), as.complex(out11))
})


test_that("vswitch-0008.152", {
  expect_identical(vswitch(as.numeric(x1), as.numeric(values0), outputs1c, as.complex(na1)), as.complex(out12))
})


test_that("vswitch-0008.153", {
  expect_kit_equal(vswitch(as.complex(x0), as.complex(values0), outputs0c), as.complex(x0))
})


test_that("vswitch-0008.154", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), outputs0c, as.complex(na0)), as.complex(x0))
})


test_that("vswitch-0008.155", {
  expect_kit_equal(vswitch(as.complex(x1), as.complex(values0), outputs1c), as.complex(out11))
})


test_that("vswitch-0008.156", {
  expect_identical(vswitch(as.complex(x1), as.complex(values0), outputs1c, as.complex(na1)), as.complex(out12))
})


test_that("vswitch-0008.157", {
  expect_kit_equal(vswitch(as.logical(x0), as.logical(values0), outputs0c), as.complex(x0))
})


test_that("vswitch-0008.158", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), outputs0c, as.complex(na0)), as.complex(x0))
})


test_that("vswitch-0008.159", {
  expect_kit_equal(vswitch(as.logical(x1), as.logical(values0), outputs1c), as.complex(out11))
})


test_that("vswitch-0008.160", {
  expect_identical(vswitch(as.logical(x1), as.logical(values0), outputs1c, as.complex(na1)), as.complex(out12))
})


test_that("vswitch-0008.161", {
  expect_kit_equal(vswitch(as.character(x0), as.character(values0), outputs0c), as.complex(x0))
})


test_that("vswitch-0008.162", {
  expect_identical(vswitch(as.character(x0), as.character(values0), outputs0c, as.complex(na0)), as.complex(x0))
})


test_that("vswitch-0008.163", {
  expect_kit_equal(vswitch(as.character(x1), as.character(values0), outputs1c), as.complex(out11))
})


test_that("vswitch-0008.164", {
  expect_identical(vswitch(as.character(x1), as.character(values0), outputs1c, as.complex(na1)), as.complex(out12))
})


test_that("vswitch-0008.165", {
  expect_kit_equal(vswitch(as.list(x0), as.list(values0), outputs0c), as.complex(x0))
})


test_that("vswitch-0008.166", {
  expect_identical(vswitch(as.list(x0), as.list(values0), outputs0c, as.complex(na0)), as.complex(x0))
})


test_that("vswitch-0008.167", {
  expect_kit_equal(vswitch(as.list(x1), as.list(values0), outputs1c), as.complex(out11))
})


test_that("vswitch-0008.168", {
  expect_identical(vswitch(as.list(x1), as.list(values0), outputs1c, as.complex(na1)), as.complex(out12))
})


test_that("vswitch-0008.169", {
  expect_error(vswitch(x0, values0, list(as.Date("2020-04-14"),10)), regexp = "Items 1 and  2 of 'outputs' must have same class.", fixed = TRUE)
})


test_that("vswitch-0008.170", {
  expect_error(vswitch(x0, values0, list(factor("a", levels = letters[1:2]),factor("c", levels = letters[1:3]))), regexp = "Items 1 and  2 of 'outputs' are both factor but their levels are different.", fixed = TRUE)
})


test_that("vswitch-0008.171", {
  expect_identical(vswitch(x0, values0, as.integer(outputs0)), x0)
})


test_that("vswitch-0008.172", {
  expect_identical(vswitch(x0, values0, as.numeric(outputs0)), as.numeric(x0))
})


test_that("vswitch-0008.173", {
  expect_identical(vswitch(x0, values0, as.logical(outputs0)), as.logical(x0))
})


test_that("vswitch-0008.174", {
  expect_identical(vswitch(x0, values0, as.character(outputs0)), as.character(x0))
})


test_that("vswitch-0008.175", {
  expect_kit_equal(vswitch(x0, values0, as.complex(outputs0)), as.complex(x0))
})


test_that("vswitch-0008.176", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), as.integer(outputs0)), x0)
})


test_that("vswitch-0008.177", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), as.numeric(outputs0)), as.numeric(x0))
})


test_that("vswitch-0008.178", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), as.logical(outputs0)), as.logical(x0))
})


test_that("vswitch-0008.179", {
  expect_identical(vswitch(as.numeric(x0), as.numeric(values0), as.character(outputs0)), as.character(x0))
})


test_that("vswitch-0008.180", {
  expect_kit_equal(vswitch(as.numeric(x0), as.numeric(values0), as.complex(outputs0)), as.complex(x0))
})


test_that("vswitch-0008.181", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), as.integer(outputs0)), x0)
})


test_that("vswitch-0008.182", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), as.numeric(outputs0)), as.numeric(x0))
})


test_that("vswitch-0008.183", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), as.logical(outputs0)), as.logical(x0))
})


test_that("vswitch-0008.184", {
  expect_identical(vswitch(as.logical(x0), as.logical(values0), as.character(outputs0)), as.character(x0))
})


test_that("vswitch-0008.185", {
  expect_kit_equal(vswitch(as.logical(x0), as.logical(values0), as.complex(outputs0)), as.complex(x0))
})


test_that("vswitch-0008.186", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), as.integer(outputs0)), x0)
})


test_that("vswitch-0008.187", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), as.numeric(outputs0)), as.numeric(x0))
})


test_that("vswitch-0008.188", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), as.logical(outputs0)), as.logical(x0))
})


test_that("vswitch-0008.189", {
  expect_identical(vswitch(as.complex(x0), as.complex(values0), as.character(outputs0)), as.character(x0))
})


test_that("vswitch-0008.190", {
  expect_kit_equal(vswitch(as.complex(x0), as.complex(values0), as.complex(outputs0)), as.complex(x0))
})


test_that("vswitch-0008.191", {
  expect_identical(vswitch(as.character(x0), as.character(values0), as.integer(outputs0)), x0)
})


test_that("vswitch-0008.192", {
  expect_identical(vswitch(as.character(x0), as.character(values0), as.numeric(outputs0)), as.numeric(x0))
})


test_that("vswitch-0008.193", {
  expect_identical(vswitch(as.character(x0), as.character(values0), as.logical(outputs0)), as.logical(x0))
})


test_that("vswitch-0008.194", {
  expect_identical(vswitch(as.character(x0), as.character(values0), as.character(outputs0)), as.character(x0))
})


test_that("vswitch-0008.195", {
  expect_kit_equal(vswitch(as.character(x0), as.character(values0), as.complex(outputs0)), as.complex(x0))
})


test_that("vswitch-0008.196", {
  expect_identical(vswitch(as.list(x0), as.list(values0), as.integer(outputs0)), x0)
})


test_that("vswitch-0008.197", {
  expect_identical(vswitch(as.list(x0), as.list(values0), as.numeric(outputs0)), as.numeric(x0))
})


test_that("vswitch-0008.198", {
  expect_identical(vswitch(as.list(x0), as.list(values0), as.logical(outputs0)), as.logical(x0))
})


test_that("vswitch-0008.199", {
  expect_identical(vswitch(as.list(x0), as.list(values0), as.character(outputs0)), as.character(x0))
})


test_that("vswitch-0008.200", {
  expect_kit_equal(vswitch(as.list(x0), as.list(values0), as.complex(outputs0)), as.complex(x0))
})


test_that("vswitch-0008.201", {
  expect_error(vswitch(x0, values0, as.integer(outputs0), NA_character_), regexp = "Type of 'na' and 'outputs' are different. Please make sure they are the same.", fixed = TRUE)
})


test_that("vswitch-0008.202", {
  expect_error(vswitch(x0, values0, as.raw(c(0,1))), regexp = "Type raw is not supported for argument 'outputs'", fixed = TRUE)
})


test_that("vswitch-0008.203", {
  expect_error(vswitch(x0, values0[1], as.Date("2020-04-14"), 2), regexp = "Argument 'na' and 'outputs' must have same class.", fixed = TRUE)
})


test_that("vswitch-0008.204", {
  expect_error(vswitch(x0, values0, factor(c("a","b")), factor("c")), regexp = "Argument 'na' and 'outputs' are both factor but their levels are different.", fixed = TRUE)
})


test_that("vswitch-0008.205", {
  expect_identical(vswitch(x0, values0[1], factor(c("a"),levels=c("a","b")), factor(("b"),levels = c("a","b"))), factor(c("b","a","a","b","b","b","b"),levels=c("a","b")))
})


test_that("vswitch-0008.206", {
  expect_error(vswitch(as.raw(rep(0,7L)), c(as.raw(0), as.raw(1)), as.integer(outputs0)), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("vswitch-0008.207", {
  expect_error(vswitch(as.raw(rep(0,7L)), c(as.raw(0), as.raw(1)), as.numeric(outputs0)), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("vswitch-0008.208", {
  expect_error(vswitch(as.raw(rep(0,7L)), c(as.raw(0), as.raw(1)), as.logical(outputs0)), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("vswitch-0008.209", {
  expect_error(vswitch(as.raw(rep(0,7L)), c(as.raw(0), as.raw(1)), as.character(outputs0)), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("vswitch-0008.210", {
  expect_error(vswitch(as.raw(rep(0,7L)), c(as.raw(0), as.raw(1)), as.complex(outputs0)), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("vswitch-0008.211", {
  expect_identical(vswitch(c(enc1,enc2),enc1,1), c(1,1))
})


test_that("vswitch-0008.212", {
  expect_identical(vswitch(c(enc1,enc1),enc1,1), c(1,1))
})


test_that("vswitch-0008.213", {
  expect_identical(vswitch(c(enc2,enc2),enc2,1), c(1,1))
})


test_that("vswitch-0008.214", {
  expect_identical(vswitch(c(enc1,enc2),enc2,1), c(1,1))
})


test_that("vswitch-0008.215", {
  expect_error(vswitch("a",character(),1), regexp = "Argument'values' cannot be zero-length vector.", fixed = TRUE)
})


test_that("vswitch-0008.216", {
  expect_error(vswitch("a","b",1,checkEnc = NA), regexp = "Argument 'checkEnc' must be TRUE or FALSE and length 1.", fixed = TRUE)
})

x1 = c(0L, 0L, 1L, 2L, 3L, 2L)
x2 = as.logical(x1)
x3 = as.numeric(x1)
x4 = as.complex(x1)
x5 = as.character(x1)
x6 = as.list(x1)
class2133 = setClass("class2133", slots=list(x="numeric"))
s1 = class2133(x=20191231)
s2 = class2133(x=20191230)
enc1 = "fa\xE7ile"
Encoding(enc1) = "latin1"
enc2 = enc2utf8(enc1)

test_that("nswitch-0019.001", {
  expect_identical(nswitch(x1, 0L, FALSE, 1L, TRUE, 2L, TRUE, default = FALSE), c(FALSE,FALSE,TRUE,TRUE,FALSE,TRUE))
})


test_that("nswitch-0019.002", {
  expect_identical(nswitch(x2, FALSE, FALSE, TRUE, TRUE, default = FALSE), c(FALSE,FALSE,TRUE,TRUE,TRUE,TRUE))
})


test_that("nswitch-0019.003", {
  expect_identical(nswitch(x3, 0, FALSE, 1, TRUE, 2, TRUE, default = FALSE), c(FALSE,FALSE,TRUE,TRUE,FALSE,TRUE))
})


test_that("nswitch-0019.004", {
  expect_identical(nswitch(x4, 0+0i, FALSE, 1+0i, TRUE, 2+0i, TRUE, default = FALSE), c(FALSE,FALSE,TRUE,TRUE,FALSE,TRUE))
})


test_that("nswitch-0019.005", {
  expect_identical(nswitch(x5, "0", FALSE, "1", TRUE, "2", TRUE, default = FALSE), c(FALSE,FALSE,TRUE,TRUE,FALSE,TRUE))
})


test_that("nswitch-0019.006", {
  expect_identical(nswitch(x1, 0L, 1L, 1L, 2L, 2L, 3L, default = 4L), c(1L,1L,2L,3L,4L,3L))
})


test_that("nswitch-0019.007", {
  expect_identical(nswitch(x2, TRUE, 1L, FALSE, 2L, default = 0L), c(2L,2L,1L,1L,1L,1L))
})


test_that("nswitch-0019.008", {
  expect_identical(nswitch(x3, 0, 1L, 1, 2L, 2, 3L, default = 4L), c(1L,1L,2L,3L,4L,3L))
})


test_that("nswitch-0019.009", {
  expect_identical(nswitch(x4, 0+0i, 0L, 1+0i, 1L, 2+0i, 2L, default = 3L), c(0L,0L,1L,2L,3L,2L))
})


test_that("nswitch-0019.010", {
  expect_identical(nswitch(x5, "0", 0L, "1", 1L, "2", 2L, default = 3L), c(0L,0L,1L,2L,3L,2L))
})


test_that("nswitch-0019.011", {
  expect_identical(nswitch(x1, 0L, 1, 1L, 2, 2L, 3, default = 4), c(1,1,2,3,4,3))
})


test_that("nswitch-0019.012", {
  expect_identical(nswitch(x2, TRUE, 1, FALSE, 2, default = 0), c(2,2,1,1,1,1))
})


test_that("nswitch-0019.013", {
  expect_identical(nswitch(x3, 0, 1, 1, 2, 2, 3, default = 4), c(1,1,2,3,4,3))
})


test_that("nswitch-0019.014", {
  expect_identical(nswitch(x4, 0+0i, 0, 1+0i, 1, 2+0i, 2, default = 3), c(0,0,1,2,3,2))
})


test_that("nswitch-0019.015", {
  expect_identical(nswitch(x5, "0", 0, "1", 1, "2", 2, default = 3), c(0,0,1,2,3,2))
})


test_that("nswitch-0019.016", {
  expect_identical(nswitch(x1, 0L, 1+0i, 1L, 2+0i, 2L, 3+0i, default = 4+0i), c(1+0i,1+0i,2+0i,3+0i,4+0i,3+0i))
})


test_that("nswitch-0019.017", {
  expect_identical(nswitch(x2, TRUE, 1+0i, FALSE, 2+0i, default = 0+0i), c(2+0i,2+0i,1+0i,1+0i,1+0i,1+0i))
})


test_that("nswitch-0019.018", {
  expect_identical(nswitch(x3, 0, 1+0i, 1, 2+0i, 2, 3+0i, default = 4+0i), c(1+0i,1+0i,2+0i,3+0i,4+0i,3+0i))
})


test_that("nswitch-0019.019", {
  expect_identical(nswitch(x4, 0+0i, 0+0i, 1+0i, 1+0i, 2+0i, 2+0i, default = 3+0i), c(0+0i,0+0i,1+0i,2+0i,3+0i,2+0i))
})


test_that("nswitch-0019.020", {
  expect_identical(nswitch(x5, "0", 0+0i, "1", 1+0i, "2", 2+0i, default = 3+0i), c(0+0i,0+0i,1+0i,2+0i,3+0i,2+0i))
})


test_that("nswitch-0019.021", {
  expect_identical(nswitch(x1, 0L, "1+0i", 1L, "2+0i", 2L, "3+0i", default = "4+0i"), c("1+0i","1+0i","2+0i","3+0i","4+0i","3+0i"))
})


test_that("nswitch-0019.022", {
  expect_identical(nswitch(x2, TRUE, "1+0i", FALSE, "2+0i", default = "0+0i"), c("2+0i","2+0i","1+0i","1+0i","1+0i","1+0i"))
})


test_that("nswitch-0019.023", {
  expect_identical(nswitch(x3, 0, "1+0i", 1, "2+0i", 2, "3+0i", default = "4+0i"), c("1+0i","1+0i","2+0i","3+0i","4+0i","3+0i"))
})


test_that("nswitch-0019.024", {
  expect_identical(nswitch(x4, 0+0i, "0+0i", 1+0i, "1+0i", 2+0i, "2+0i", default = "3+0i"), c("0+0i","0+0i","1+0i","2+0i","3+0i","2+0i"))
})


test_that("nswitch-0019.025", {
  expect_identical(nswitch(x5, "0", "0+0i", "1", "1+0i", "2", "2+0i", default = "3+0i"), c("0+0i","0+0i","1+0i","2+0i","3+0i","2+0i"))
})


test_that("nswitch-0019.026", {
  expect_error(nswitch(x1, 0L, FALSE, 1L, TRUE, 2L, TRUE, checkEnc = 2), regexp = "Argument 'checkEnc' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("nswitch-0019.027", {
  expect_error(nswitch(x1, 0L, FALSE, 1L, TRUE, 2L), regexp = "Received 5 inputs; please supply an even number of arguments in ... consisting of target value, resulting output pairs (in that order). Note that argument 'default' must be named explicitly (e.g.: default=0)", fixed = TRUE)
})


test_that("nswitch-0019.028", {
  expect_error(nswitch(x1, 0L, FALSE, 1L, TRUE, 2L, TRUE, default = s1), regexp = "S4 class objects for argument 'na' are not supported.", fixed = TRUE)
})


test_that("nswitch-0019.029", {
  expect_error(nswitch(x1, 0L, FALSE, 1L, TRUE, 2L, TRUE, default = c(0L,1L)), regexp = "Length of 'default' must either be 1 or length of 'x'.", fixed = TRUE)
})


test_that("nswitch-0019.030", {
  expect_error(nswitch(x1, 0L, FALSE, 1L, TRUE, 2L, TRUE, default = 1), regexp = "Resulting value is of type logical but 'default' is of type double. Please make sure that both arguments have the same type.", fixed = TRUE)
})


test_that("nswitch-0019.031", {
  expect_error(nswitch(s1, 0L, FALSE, 1L, TRUE, 2L, TRUE), regexp = "S4 class objects for argument 'x' are not supported.", fixed = TRUE)
})


test_that("nswitch-0019.032", {
  expect_identical(nswitch(x6, list(0L), FALSE, list(1L), TRUE, list(2L), TRUE, default = FALSE), c(FALSE,FALSE,TRUE,TRUE,FALSE,TRUE))
})


test_that("nswitch-0019.033", {
  expect_identical(nswitch(x6, list(0L), 0L, list(1L), 1L, list(2L), 2L, default = 3L), c(0L,0L,1L,2L,3L,2L))
})


test_that("nswitch-0019.034", {
  expect_identical(nswitch(x6, list(0L), 0, list(1L), 1, list(2L), 2, default = 3), c(0,0,1,2,3,2))
})


test_that("nswitch-0019.035", {
  expect_identical(nswitch(x6, list(0L), 0+0i, list(1L), 1+0i, list(2L), 2+0i, default = 3+0i), c(0+0i,0+0i,1+0i,2+0i,3+0i,2+0i))
})


test_that("nswitch-0019.036", {
  expect_identical(nswitch(x6, list(0L), "0", list(1L), "1", list(2L), "2", default = "3"), c("0","0","1","2","3","2"))
})


test_that("nswitch-0019.037", {
  expect_error(nswitch(x6, 0L, list("0"), 1L, list("1"), 2L, list("2"), default = list("3")), regexp = "Type of 'x' and 'values' are different. Please make sure they are the same.", fixed = TRUE)
})


test_that("nswitch-0019.038", {
  expect_identical(nswitch(x6, list(0L), list("0"), list(1L), list("1"), list(2L), list("2"), default = list("3")), list("0","0","1","2","3","2"))
})


test_that("nswitch-0019.039", {
  expect_identical(nswitch(x1, 0L, list("0"), 1L, list("1"), 2L, list("2"), default = list("3")), list("0","0","1","2","3","2"))
})


test_that("nswitch-0019.040", {
  expect_identical(nswitch(x2, TRUE, list("0"), FALSE, list("1"), default = list("3")), list("1","1","0","0","0","0"))
})


test_that("nswitch-0019.041", {
  expect_identical(nswitch(x3, 0, list("0"), 1, list("1"), 2, list("2"), default = list("3")), list("0","0","1","2","3","2"))
})


test_that("nswitch-0019.042", {
  expect_identical(nswitch(x4, 0+0i, list("0"), 1+0i, list("1"), 2+0i, list("2"), default = list("3")), list("0","0","1","2","3","2"))
})


test_that("nswitch-0019.043", {
  expect_identical(nswitch(x5, "0", list("0"), "1", list("1"), "2", list("2"), default = list("3")), list("0","0","1","2","3","2"))
})


test_that("nswitch-0019.044", {
  expect_error(nswitch(x1, 0L, as.raw("00"), 1L, as.raw("01"), 2L, as.raw("02"), default = as.raw("03")), regexp = "Type raw is not supported for argument 'outputs'", fixed = TRUE)
})


test_that("nswitch-0019.045", {
  expect_error(nswitch(x1, 0L, 1, 1L, 2, 2L, 3, default = as.Date("2020-01-01")), regexp = "Resulting value has different class than 'default'. Please make sure that both arguments have the same class.", fixed = TRUE)
})


test_that("nswitch-0019.046", {
  expect_error(nswitch(as.raw(x1), as.raw(0L), 1, as.raw(1L), 2, as.raw(2L), 3, default = 4), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("nswitch-0019.047", {
  expect_error(nswitch(as.raw(x1), as.raw(0L), 1L, as.raw(1L), 2L, as.raw(2L), 3L, default = 4L), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("nswitch-0019.048", {
  expect_error(nswitch(as.raw(x1), as.raw(0L), 1+0i, as.raw(1L), 2+0i, as.raw(2L), 3+0i, default = 4+0i), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("nswitch-0019.049", {
  expect_error(nswitch(as.raw(x1), as.raw(0L), "1", as.raw(1L), "2", as.raw(2L), "3", default = "4"), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("nswitch-0019.050", {
  expect_error(nswitch(as.raw(x1), as.raw(0L), list(1), as.raw(1L), list(2), as.raw(2L), list(3), default = list(4)), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("nswitch-0019.051", {
  expect_error(nswitch(as.raw(x1), as.raw(0L), TRUE, as.raw(1L), FALSE), regexp = "Type raw is not supported for argument 'x'.", fixed = TRUE)
})


test_that("nswitch-0019.052", {
  expect_error(nswitch(x1, 0L, as.factor(1L), default = as.factor(4L)), regexp = "Resulting value and 'default' are both type factor but their levels are different.", fixed = TRUE)
})


test_that("nswitch-0019.053", {
  expect_error(nswitch(x1, 0L, as.factor(1L),1L, as.factor(2L), default = as.factor(1L)), regexp = "Items 2 and  4 of '...' are both factor but their levels are different.", fixed = TRUE)
})


test_that("nswitch-0019.054", {
  expect_error(nswitch(x1, 0L, 1, 1L, as.Date("2020-01-01"), default = 2), regexp = "Items 2 and  4 of '...' must have same class.", fixed = TRUE)
})


test_that("nswitch-0019.055", {
  expect_error(nswitch(x1, 0L, 1L, 1L, s1), regexp = "S4 class objects for argument '...' (item 2) are not supported.", fixed = TRUE)
})


test_that("nswitch-0019.056", {
  expect_error(nswitch(x1, 0L, 1L, 1L, c(2L,3L)), regexp = "Length of item 4 of '...' is different than 1 and length of 'x'. Please make sure that all items of 'output' have length 1 or length of 'x'(6).", fixed = TRUE)
})


test_that("nswitch-0019.057", {
  expect_error(nswitch(x1, 0L, 1L, 1L, 2, 2L, 3L), regexp = "Item 2 and 4 of '...' are not of the same type.", fixed = TRUE)
})


test_that("nswitch-0019.058", {
  expect_error(nswitch(x1, 0L, 1L, 1, 2L, 2L, 3L), regexp = "Item 1 and 3 of '...' are not of the same type.", fixed = TRUE)
})


test_that("nswitch-0019.059", {
  expect_error(nswitch(x1, c(0L,1L), 1L, 1L, 2L, 2L, 3L), regexp = "Length of item 1 of '...' is different than 1. Please make sure it has length 1.", fixed = TRUE)
})


test_that("nswitch-0019.060", {
  expect_identical(nswitch(c(enc1,enc2),enc1,1), c(1,1))
})


test_that("nswitch-0019.061", {
  expect_identical(nswitch(c(enc1,enc1),enc1,1), c(1,1))
})


test_that("nswitch-0019.062", {
  expect_identical(nswitch(c(enc2,enc2),enc2,1), c(1,1))
})


test_that("nswitch-0019.063", {
  expect_identical(nswitch(c(enc1,enc2),enc2,1), c(1,1))
})


test_that("nswitch-0019.064", {
  expect_identical(nswitch(c(enc1,enc1),enc2,1), c(1,1))
})


test_that("nswitch-0019.065", {
  expect_identical(nswitch(rep(1:4, each = 2), 1L, 1:8, 2L, 11:18, 3L, 21:28, 4L, 31:38), vswitch(x = rep(1:4, each = 2), values = c(1L,2L,3L,4L), outputs = list(1:8,11:18,21:28,31:38)))
})


test_that("nswitch-0019.066", {
  expect_identical(nswitch(rep(1:4, each = 2), 1L, as.numeric(1:8), 2L, as.numeric(11:18), 3L, as.numeric(21:28), 4L, as.numeric(31:38)), vswitch(x = rep(1:4, each = 2), values = c(1L,2L,3L,4L), outputs = list(as.numeric(1:8),as.numeric(11:18),as.numeric(21:28),as.numeric(31:38))))
})


test_that("nswitch-0019.067", {
  expect_identical(nswitch(rep(1:4, each = 2), 1L, as.character(1:8), 2L, as.character(11:18), 3L, as.character(21:28), 4L, as.character(31:38)), vswitch(x = rep(1:4, each = 2), values = c(1L,2L,3L,4L), outputs = list(as.character(1:8),as.character(11:18),as.character(21:28),as.character(31:38))))
})


test_that("nswitch-0019.068", {
  expect_identical(nswitch(rep(as.numeric(1:4), each = 2), 1, 1:8, 2, 11:18, 3, 21:28, 4, 31:38), vswitch(x = rep(as.numeric(1:4), each = 2), values = c(1,2,3,4), outputs = list(1:8,11:18,21:28,31:38)))
})


test_that("nswitch-0019.069", {
  expect_identical(nswitch(rep(as.numeric(1:4), each = 2), 1, as.numeric(1:8), 2, as.numeric(11:18), 3, as.numeric(21:28), 4, as.numeric(31:38)), vswitch(x = rep(as.numeric(1:4), each = 2), values = c(1,2,3,4), outputs = list(as.numeric(1:8),as.numeric(11:18),as.numeric(21:28),as.numeric(31:38))))
})


test_that("nswitch-0019.070", {
  expect_identical(nswitch(rep(as.numeric(1:4), each = 2), 1, as.character(1:8), 2, as.character(11:18), 3, as.character(21:28), 4, as.character(31:38)), vswitch(x = rep(as.numeric(1:4), each = 2), values = c(1,2,3,4), outputs = list(as.character(1:8),as.character(11:18),as.character(21:28),as.character(31:38))))
})

