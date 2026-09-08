# Tests for iif() and nif() — vectorised if / nested if
# Migrated from tests/test_kit.R (custom check() suite) to testthat 3e — see issue #54.
# Each legacy check("id", actual, expected) maps to expect_identical();
# 31 tolerant cases (identical FALSE but all.equal+typeof TRUE) use expect_kit_equal();
# check(..., error=) maps to expect_error(regexp=, fixed=TRUE).
# Legacy IDs are preserved as test_that() labels (prefixed by function for uniqueness).
# Setup and expectations stay interleaved in legacy order: later sections may
# redefine x/y/out_vec/... so setup must not be hoisted above earlier tests.

set.seed(123)

class2132 = setClass("class2132", slots=list(x="numeric"))
s1 = class2132(x=20191231)
s2 = class2132(x=20191230)
test_vec = -5L:5L < 0L
test_vec_na = c(test_vec, NA)
out_vec = rep(1:0, 5:6)
out_vec_na = c(out_vec, NA_integer_)
date_vec = as.Date(14975:14979, origin = '1970-01-01')

test_that("iif-0002.001", {
  expect_identical(iif(test_vec, 1L, 0L), out_vec)
})


test_that("iif-0002.002", {
  expect_identical(iif(test_vec, 1, 0), as.numeric(out_vec))
})


test_that("iif-0002.003", {
  expect_identical(iif(test_vec, TRUE, FALSE), as.logical(out_vec))
})


test_that("iif-0002.004", {
  expect_identical(iif(test_vec, "1", "0"), as.character(out_vec))
})


test_that("iif-0002.005", {
  expect_identical(iif(test_vec_na, TRUE, NA), c(rep(TRUE,5L), rep(NA,7L)))
})


test_that("iif-0002.006", {
  expect_identical(iif(test_vec, rep(1L,11L), rep(0L,11L)), out_vec)
})


test_that("iif-0002.007", {
  expect_identical(iif(test_vec, rep(1L,11L), 0L), out_vec)
})


test_that("iif-0002.008", {
  expect_identical(iif(test_vec, 1L, rep(0L,11L)), out_vec)
})


test_that("iif-0002.009", {
  expect_error(iif(test_vec, rep(1L,11L), rep(0L,10L)), regexp = "Length of 'no' is 10 but must be 1 or length of 'test' (11).", fixed = TRUE)
})


test_that("iif-0002.010", {
  expect_error(iif(test_vec, rep(1,10L), rep(0,11L)), regexp = "Length of 'yes' is 10 but must be 1 or length of 'test' (11).", fixed = TRUE)
})


test_that("iif-0002.011", {
  expect_error(iif(test_vec, rep(TRUE,10L), rep(FALSE,10L)), regexp = "Length of 'yes' is 10 but must be 1 or length of 'test' (11).", fixed = TRUE)
})


test_that("iif-0002.012", {
  expect_error(iif(0:1, rep(TRUE,2L), rep(FALSE,2L)), regexp = "Argument 'test' must be logical.", fixed = TRUE)
})


test_that("iif-0002.013", {
  expect_error(iif(test_vec, TRUE, "FALSE"), regexp = "'yes' is of type logical but 'no' is of type character. Please", fixed = TRUE)
})


test_that("iif-0002.014", {
  expect_error(iif(test_vec, list(1),list(2,4)), regexp = "Length of 'no' is 2 but must be 1 or length of 'test' (11).", fixed = TRUE)
})


test_that("iif-0002.015", {
  expect_error(iif(test_vec, list(1,3),list(2,4)), regexp = "Length of 'yes' is 2 but must be 1 or length of 'test' (11).", fixed = TRUE)
})


test_that("iif-0002.016", {
  expect_identical(iif(test_vec, list(1), list(0)), as.list(as.numeric(out_vec)))
})


test_that("iif-0002.017", {
  expect_identical(iif(test_vec, list(1), list(0)), as.list(as.numeric(out_vec)))
})


test_that("iif-0002.018", {
  expect_identical(iif(date_vec == "2011-01-01", date_vec - 1L, date_vec), c(date_vec[1L] - 1L, date_vec[2:5]))
})


test_that("iif-0002.019", {
  expect_identical(iif(c(TRUE,FALSE,TRUE,TRUE,FALSE), factor(letters[1:5]), factor("a", levels=letters[1:5])), factor(c("a","a","c","d","a"), levels=letters[1:5]))
})


test_that("iif-0002.020", {
  expect_identical(iif(test_vec_na, 1L, 0L), out_vec_na)
})


test_that("iif-0002.021", {
  expect_identical(iif(test_vec_na, rep(1L,12L), 0L), out_vec_na)
})


test_that("iif-0002.022", {
  expect_identical(iif(test_vec_na, rep(1L,12L), rep(0L,12L)), out_vec_na)
})


test_that("iif-0002.023", {
  expect_identical(iif(test_vec_na, 1L, rep(0L,12L)), out_vec_na)
})


test_that("iif-0002.024", {
  expect_identical(iif(test_vec_na, 1, 0), as.numeric(out_vec_na))
})


test_that("iif-0002.025", {
  expect_identical(iif(test_vec_na, rep(1,12L), 0), as.numeric(out_vec_na))
})


test_that("iif-0002.026", {
  expect_identical(iif(test_vec_na, rep(1,12L), rep(0,12L)), as.numeric(out_vec_na))
})


test_that("iif-0002.027", {
  expect_identical(iif(test_vec_na, 1, rep(0,12L)), as.numeric(out_vec_na))
})


test_that("iif-0002.028", {
  expect_identical(iif(test_vec_na, TRUE, rep(FALSE,12L)), as.logical(out_vec_na))
})


test_that("iif-0002.029", {
  expect_identical(iif(test_vec_na, rep(TRUE,12L), FALSE), as.logical(out_vec_na))
})


test_that("iif-0002.030", {
  expect_identical(iif(test_vec_na, rep(TRUE,12L), rep(FALSE,12L)), as.logical(out_vec_na))
})


test_that("iif-0002.031", {
  expect_identical(iif(test_vec_na, "1", rep("0",12L)), as.character(out_vec_na))
})


test_that("iif-0002.032", {
  expect_identical(iif(test_vec_na, rep("1",12L), "0"), as.character(out_vec_na))
})


test_that("iif-0002.033", {
  expect_identical(iif(test_vec_na, rep("1",12L), rep("0",12L)), as.character(out_vec_na))
})


test_that("iif-0002.034", {
  expect_identical(iif(test_vec_na, "1", "0"), as.character(out_vec_na))
})


test_that("iif-0002.035", {
  expect_error(iif(test_vec, as.Date("2011-01-01"), FALSE), regexp = "'yes' is of type double but 'no' is of type logical. Please", fixed = TRUE)
})


test_that("iif-0002.036", {
  expect_kit_equal(iif(test_vec_na, 1+0i, 0+0i), as.complex(out_vec_na))
})


test_that("iif-0002.037", {
  expect_kit_equal(iif(test_vec_na, rep(1+0i,12L), 0+0i), as.complex(out_vec_na))
})


test_that("iif-0002.038", {
  expect_kit_equal(iif(test_vec_na, rep(1+0i,12L), rep(0+0i,12L)), as.complex(out_vec_na))
})


test_that("iif-0002.039", {
  expect_kit_equal(iif(test_vec_na, 1+0i, rep(0+0i,12L)), as.complex(out_vec_na))
})


test_that("iif-0002.040", {
  expect_error(iif(test_vec, as.raw(0), as.raw(1)), regexp = "Type raw is not supported.", fixed = TRUE)
})


test_that("iif-0002.041", {
  expect_error(iif(TRUE,1,as.Date("2019-07-07")), regexp = "'yes' has different class than 'no'. Please", fixed = TRUE)
})


test_that("iif-0002.042", {
  expect_error(iif(TRUE,1L,factor(letters[1])), regexp = "'yes' has different class than 'no'. Please", fixed = TRUE)
})


test_that("iif-0002.043", {
  expect_identical(iif(TRUE, list(1:5), list(5:1)), list(1:5))
})


test_that("iif-0002.044", {
  expect_identical(iif(as.logical(NA), list(1:5), list(5:1)), list(NULL))
})


test_that("iif-0002.045", {
  expect_identical(iif(FALSE, list(1:5), list(5:1)), list(5:1))
})


test_that("iif-0002.048", {
  expect_identical(iif(TRUE, list(data.frame(1:5)), list(data.frame(5:1))), list(data.frame(1:5)))
})


test_that("iif-0002.049", {
  expect_identical(iif(FALSE, list(data.frame(1:5)), list(data.frame(5:1))), list(data.frame(5:1)))
})


test_that("iif-0002.050", {
  expect_identical(iif(c(TRUE,FALSE), list(1:5,6:10), list(10:6,5:1)), list(1:5,5:1))
})


test_that("iif-0002.051", {
  expect_identical(iif(c(NA,TRUE), list(1:5,6:10), list(10:6,5:1)), list(NULL,6:10))
})


test_that("iif-0002.052", {
  expect_identical(iif(c(FALSE,TRUE), list(1:5,6:10), list(10:6,5:1)), list(10:6,6:10))
})


test_that("iif-0002.053", {
  expect_identical(iif(c(NA,TRUE), list(1:5), list(10:6,5:1)), list(NULL,1:5))
})


test_that("iif-0002.054", {
  expect_identical(iif(c(NA,TRUE), list(1:5,6:10), list(5:1)), list(NULL,6:10))
})


test_that("iif-0002.055", {
  expect_identical(iif(c(FALSE,TRUE), list(TRUE), list(10:6,5:1)), list(10:6,TRUE))
})


test_that("iif-0002.056", {
  expect_identical(iif(c(FALSE,TRUE), list(as.Date("2019-07-07")), list(10:6,5:1)), list(10:6,as.Date("2019-07-07")))
})


test_that("iif-0002.057", {
  expect_identical(iif(c(FALSE,TRUE), list(factor(letters[1:5])), list(10:6,5:1)), list(10:6,factor(letters[1:5])))
})


test_that("iif-0002.058", {
  expect_identical(iif(c(NA,FALSE), list(1:5), list(10:6,5:1)), list(NULL,5:1))
})


test_that("iif-0002.059", {
  expect_identical(iif(c(NA,FALSE), list(1:5,6:10), list(5:1)), list(NULL,5:1))
})


test_that("iif-0002.060", {
  expect_identical(iif(c(NA,FALSE), list(1:5), list(5:1)), list(NULL,5:1))
})


test_that("iif-0002.061", {
  expect_identical(iif(c(TRUE,FALSE), list(1L), list(0L)), list(1L,0L))
})


test_that("iif-0002.062", {
  expect_identical(iif(c(TRUE,FALSE), list(1L), list(0L)), list(1L,0L))
})


test_that("iif-0002.063", {
  expect_error(iif(c(TRUE,FALSE), factor(c("a","b")), factor(c("a","c"))), regexp = "'yes' and 'no' are both type factor but their levels are different", fixed = TRUE)
})


test_that("iif-0002.064", {
  expect_identical(iif(c(TRUE, TRUE, TRUE, FALSE, FALSE), factor(NA, levels=letters[1:5]), factor(letters[1:5])), factor(c(NA,NA,NA,"d","e"),levels=letters[1:5]))
})


test_that("iif-0002.065", {
  expect_identical(iif(c(TRUE, TRUE, TRUE, FALSE, NA, FALSE), factor(NA, levels=letters[1:6]), factor(letters[1:6])), factor(c(NA,NA,NA,"d",NA,"f"),levels=letters[1:6]))
})


test_that("iif-0002.066", {
  expect_identical(iif(c(TRUE, TRUE, TRUE, FALSE, NA, FALSE), factor(letters[1:6]), factor(NA, levels=letters[1:6])), factor(c("a","b","c",NA,NA,NA), levels=letters[1:6]))
})


test_that("iif-0002.067", {
  expect_identical(iif(c(TRUE, NA, TRUE, FALSE, FALSE, FALSE), factor(NA), factor(NA)), factor(c(NA,NA,NA,NA,NA,NA)))
})


test_that("iif-0002.068", {
  expect_identical(iif(c(a=TRUE,b=FALSE), list(m=1,n=2), list(x=11,y=12)), list(a=1, b=12))
})


test_that("iif-0002.069", {
  expect_identical(iif(c(a=TRUE,b=FALSE), c(m=1,n=2), c(x=11,y=12)), c(a=1, b=12))
})


test_that("ifelse-0002.070", {
  expect_identical(ifelse(c(a=TRUE,b=FALSE), c(1,2), c(11,12)), c(a=1, b=12))
})


test_that("iif-0002.071", {
  expect_error(iif(TRUE, s1, s2), regexp = "S4 class objects are not supported.", fixed = TRUE)
})


test_that("iif-0002.072", {
  expect_error(iif(TRUE, 1, s2), regexp = "S4 class objects are not supported.", fixed = TRUE)
})


test_that("iif-0002.073", {
  expect_identical(iif(test_vec, 1L, 0,tprom = TRUE), as.numeric(out_vec))
})


test_that("iif-0002.074", {
  expect_error(iif(test_vec, 1L, raw(0),tprom = TRUE), regexp = "Type raw (argument 'no') is not supported.", fixed = TRUE)
})


test_that("iif-0002.075", {
  expect_error(iif(test_vec, raw(1), 0,tprom = TRUE), regexp = "Type raw (argument 'yes') is not supported.", fixed = TRUE)
})


test_that("iif-0002.076", {
  expect_error(iif(test_vec, 1L, 0, "NA", TRUE), regexp = "Type of 'na' (character) is higher than double (highest type of 'yes' and 'no'). Please make sure that it is at lower or the same.", fixed = TRUE)
})


test_that("iif-0002.077", {
  expect_identical(iif(test_vec_na, 1, 0, NA, TRUE), as.numeric(out_vec_na))
})


test_that("iif-0002.078", {
  expect_identical(iif(test_vec_na, rep(1L, 12L), rep(0, 12L), rep(NA, 12L), TRUE), as.numeric(out_vec_na))
})


test_that("iif-0002.079", {
  expect_identical(iif(test_vec_na, rep(1, 12L), rep(0L, 12L), rep(NA, 12L), TRUE), as.numeric(out_vec_na))
})


test_that("iif-0002.080", {
  expect_identical(iif(test_vec_na, 1, rep(0L, 12L), rep(NA, 12L), TRUE), as.numeric(out_vec_na))
})


test_that("iif-0002.081", {
  expect_identical(iif(test_vec_na, 1, rep(0L, 12L), NA, TRUE), as.numeric(out_vec_na))
})


test_that("iif-0002.082", {
  expect_identical(iif(test_vec_na, rep(1, 12L), 0L, rep(NA, 12L), TRUE), as.numeric(out_vec_na))
})


test_that("iif-0002.083", {
  expect_identical(iif(test_vec_na, rep(1L, 12L), rep(0, 12L), rep(NA, 12L), TRUE), as.numeric(out_vec_na))
})


test_that("iif-0002.084", {
  expect_identical(iif(c(TRUE, TRUE, TRUE, FALSE, NA, FALSE), factor(letters[1:6]), factor("a", levels=letters[1:6]), tprom = TRUE), factor(c("a","b","c","a",NA,"a"), levels=letters[1:6]))
})


test_that("iif-0002.085", {
  expect_identical(iif(test_vec_na, 1, 0, rep(NA, 12L), TRUE), as.numeric(out_vec_na))
})


test_that("iif-0002.086", {
  expect_identical(iif(c(date_vec == "2011-01-01", NA), c(date_vec - 1L, date_vec[1L]), c(date_vec, date_vec[1L]), NA, TRUE), c(date_vec[1L] - 1L, date_vec[2:5], NA))
})


test_that("iif-0002.087", {
  expect_error(iif(c(date_vec == "2011-01-01", NA), c(date_vec - 1L, date_vec[1L]), c(date_vec, date_vec[1L]), NA, FALSE), regexp = "'yes' is of type double but 'na' is of type logical. Please make sure that both arguments have the same type.", fixed = TRUE)
})


test_that("iif-0002.088", {
  expect_identical(iif(c(date_vec == "2011-01-01", NA), c(date_vec - 1L, date_vec[1L]), c(date_vec, date_vec[1L]), as.Date("2020-05-05"), TRUE), c(date_vec[1L] - 1L, date_vec[2:5], as.Date("2020-05-05")))
})


test_that("iif-0002.089", {
  expect_identical(iif(c(date_vec == "2011-01-01", NA), c(date_vec - 1L, date_vec[1L]), c(date_vec, date_vec[1L]), 3L, TRUE), c(date_vec[1L] - 1L, date_vec[2:5], as.Date(3L,origin = "1970-01-01")))
})


test_that("iif-0002.090", {
  expect_identical(iif(c(date_vec == "2011-01-01", NA), c(date_vec - 1L, date_vec[1L]), c(date_vec, date_vec[1L]), 3, TRUE), c(date_vec[1L] - 1L, date_vec[2:5], as.Date(3,origin = "1970-01-01")))
})


test_that("iif-0002.091", {
  expect_identical(iif(c(date_vec == "2011-01-01", NA), c(date_vec - 1L, date_vec[1L]), c(date_vec, date_vec[1L]), 1L, TRUE), c(date_vec[1L] - 1L, date_vec[2:5], as.Date(1,origin = "1970-01-01")))
})


test_that("iif-0002.092", {
  expect_error(iif(test_vec_na, 1, 0, rep(NA, 12L), c(TRUE,FALSE)), regexp = "Argument 'tprom' must be either FALSE or TRUE and length 1.", fixed = TRUE)
})


test_that("iif-0002.093", {
  expect_error(iif(test_vec_na, 1, 0, rep(NA, 11L), TRUE), regexp = "Length of 'na' is 11 but must be 1 or length of 'test' (12).", fixed = TRUE)
})


test_that("iif-0002.094", {
  expect_error(iif(TRUE, as.Date("2020-04-14"), as.Date("2020-04-12"), 2), regexp = "'yes' has different class than 'na'. Please make sure that both arguments have the same class.", fixed = TRUE)
})


test_that("iif-0002.095", {
  expect_error(iif(TRUE, factor(c("a"),levels=c("a","b")), factor(c("b"),levels=c("a","b")), factor(c("c"),levels=c("a","c"))), regexp = "'yes' and 'na' are both type factor but their levels are different.", fixed = TRUE)
})


test_that("iif-0002.096", {
  expect_identical(iif(c(TRUE, NA), list(1,2), list(3,4),list(5,6)), list(1,6))
})


test_that("iif-0002.097", {
  expect_identical(iif(c(TRUE, NA, FALSE), as.Date("2020-04-14"), 18368, as.Date("2020-04-15"), TRUE), c(as.Date("2020-04-14"), as.Date("2020-04-15"), as.Date("2020-04-16")))
})


test_that("iif-0002.098", {
  expect_identical(iif(c(TRUE, NA, FALSE), 18366, as.Date("2020-04-16"), as.Date("2020-04-15"), TRUE), c(18366, 18367, 18368))
})


test_that("iif-0002.099", {
  expect_identical(iif(c(TRUE, NA, FALSE), as.Date("2020-04-14"), as.Date("2020-04-16"), 18367, TRUE), c(as.Date("2020-04-14"), as.Date("2020-04-15"), as.Date("2020-04-16")))
})


test_that("iif-0002.100", {
  expect_identical(iif(c(TRUE, NA, FALSE), TRUE, FALSE, NA), c(TRUE, NA, FALSE))
})


test_that("iif-0002.101", {
  expect_identical(iif(c(TRUE, NA, FALSE), rep(TRUE,3L), FALSE, NA), c(TRUE, NA, FALSE))
})


test_that("iif-0002.101-2", {
  expect_identical(iif(c(TRUE, NA, FALSE), rep(TRUE,3L), rep(FALSE,3L), NA), c(TRUE, NA, FALSE))
})


test_that("iif-0002.102", {
  expect_identical(iif(c(TRUE, NA, FALSE), TRUE, rep(FALSE,3L), NA), c(TRUE, NA, FALSE))
})


test_that("iif-0002.103", {
  expect_identical(iif(c(TRUE, NA, FALSE), 1L, 0L, NA_integer_), c(1L, NA_integer_, 0L))
})


test_that("iif-0002.104", {
  expect_identical(iif(c(TRUE, NA, FALSE), rep(1L,3L), 0L, NA_integer_), c(1L, NA_integer_, 0L))
})


test_that("iif-0002.105", {
  expect_identical(iif(c(TRUE, NA, FALSE), rep(1L,3L), rep(0L,3L), NA_integer_), c(1L, NA_integer_, 0L))
})


test_that("iif-0002.106", {
  expect_identical(iif(c(TRUE, NA, FALSE), 1L, rep(0L,3L), NA_integer_), c(1L, NA_integer_, 0L))
})


test_that("iif-0002.107", {
  expect_identical(iif(c(TRUE, NA, FALSE), 1+0i, 0+0i, NA_complex_), c(1+0i, NA_complex_, 0+0i))
})


test_that("iif-0002.108", {
  expect_identical(iif(c(TRUE, NA, FALSE), rep(1+0i,3L), 0+0i, NA_complex_), c(1+0i, NA_complex_, 0+0i))
})


test_that("iif-0002.109", {
  expect_identical(iif(c(TRUE, NA, FALSE), rep(1+0i,3L), rep(0+0i,3L), NA_complex_), c(1+0i, NA_complex_, 0+0i))
})


test_that("iif-0002.110", {
  expect_identical(iif(c(TRUE, NA, FALSE), 1+0i, rep(0+0i,3L), NA_complex_), c(1+0i, NA_complex_, 0+0i))
})

test_vec1 = -5L:5L < 0L
test_vec2 = -5L:5L > 0L
test_vec3 = -5L:5L < 5L
test_vec_na1 = c(test_vec1, NA)
test_vec_na2 = c(test_vec2, NA)
out_vec = c(1,1,1,1,1,NA,0,0,0,0,0)
out_vec_def = c(1,1,1,1,1,2,0,0,0,0,0)
out_vec_na= c(1,1,1,1,1,NA,0,0,0,0,0,NA)
out_vec_oc= c(1,1,1,1,1,NA,NA,NA,NA,NA,NA)
class2132 = setClass("class2132", slots=list(x="numeric"))
s1 = class2132(x=20191231)
s2 = class2132(x=20191230)
V1 = rnorm(1000000L)
V2 = rnorm(1000000L)
V3 = rnorm(1000000L)
V0 = nif(
  V1 > 0 & V2 <= 1 & V3 > 1, V2 * 100L,
  V1 > 1 & V2 <= 0 & V3 > 0, V3 * 100L,
  V1 > -1 & V2 <= 2 & V3 > 1, V1 * 100L,
  V1 > 1 & V2 <= 0 & V3 > 2, 300,
  V1 > 0 & V2 <= 1 & V3 > 1, 100,
  V1 > -1 & V2 <= 0 & V3 > -1, V1 * 100L,
  default = 0
)
V4 = iif(V1 > 0 & V2 <= 1 & V3 > 1, V2 * 100L,
         iif(V1 > 1 & V2 <= 0 & V3 > 0, V3 * 100L,
             iif(V1 > -1 & V2 <= 2 & V3 > 1, V1 * 100L,
                 iif(V1 > 1 & V2 <= 0 & V3 > 2, 300,
                     iif(V1 > 0 & V2 <= 1 & V3 > 1, 100,
                         iif(V1 > -1 & V2 <= 0 & V3 > -1, V1 * 100L, 0)
                     )
                 )
             )
         )
)
n = 1e7
x = structure(rnorm(n), class = 'abc')

test_that("nif-0003.001", {
  expect_identical(nif(test_vec1, 1L, test_vec2, 0L), as.integer(out_vec))
})


test_that("nif-0003.002", {
  expect_identical(nif(test_vec1, 1, test_vec2, 0), out_vec)
})


test_that("nif-0003.003", {
  expect_identical(nif(test_vec1, "1", test_vec2, "0"), as.character(out_vec))
})


test_that("nif-0003.004", {
  expect_identical(nif(test_vec1, TRUE, test_vec2, FALSE), as.logical(out_vec))
})


test_that("nif-0003.005", {
  expect_kit_equal(nif(test_vec1, 1+0i, test_vec2, 0+0i), as.complex(out_vec))
})


test_that("nif-0003.006", {
  expect_identical(nif(test_vec1, list(1), test_vec2, list(0)), list(1,1,1,1,1, NULL, 0, 0, 0, 0, 0))
})


test_that("nif-0003.007", {
  expect_identical(nif(test_vec1, as.Date("2019-10-11"), test_vec2, as.Date("2019-10-14")), c(rep(as.Date("2019-10-11"),5),NA,rep(as.Date("2019-10-14"),5)))
})


test_that("nif-0003.008", {
  expect_identical(nif(test_vec1, factor("a", levels=letters[1:3]), test_vec2, factor("b", levels=letters[1:3])), factor(c(rep("a",5),NA,rep("b",5)), levels=letters[1:3]))
})


test_that("nif-0003.009", {
  expect_identical(nif(test_vec1, 1L, test_vec2, 0L, default=2L), as.integer(out_vec_def))
})


test_that("nif-0003.010", {
  expect_identical(nif(test_vec1, 1, test_vec2, 0,default=2), out_vec_def)
})


test_that("nif-0003.011", {
  expect_identical(nif(test_vec1, "1", test_vec2, "0", default ="2"), as.character(out_vec_def))
})


test_that("nif-0003.012", {
  expect_identical(nif(test_vec1, TRUE, test_vec2, FALSE, default=TRUE), as.logical(out_vec_def))
})


test_that("nif-0003.013", {
  expect_identical(nif(test_vec1, 1+0i, test_vec2, 0+0i, default=2+0i), as.complex(out_vec_def))
})


test_that("nif-0003.014", {
  expect_identical(nif(test_vec1, list(1), test_vec2, list(0),default=list(2)), list(1,1,1,1,1, 2, 0, 0, 0, 0, 0))
})


test_that("nif-0003.015", {
  expect_identical(nif(test_vec1, as.Date("2019-10-11"), test_vec2, as.Date("2019-10-14"),default=as.Date("2019-10-15")), c(rep(as.Date("2019-10-11"),5),as.Date("2019-10-15"),rep(as.Date("2019-10-14"),5)))
})


test_that("nif-0003.016", {
  expect_identical(nif(test_vec1, factor("a", levels=letters[1:3]), test_vec2, factor("b", levels=letters[1:3]),default=factor("c", levels=letters[1:3])), factor(c(rep("a",5),"c",rep("b",5)), levels=letters[1:3]))
})


test_that("nif-0003.017", {
  expect_error(nif(test_vec1, as.raw(1), test_vec2, as.raw(0)), regexp = "Type raw is not supported.", fixed = TRUE)
})


test_that("nif-0003.018", {
  expect_error(nif(test_vec1, factor("a", levels=letters[1]), test_vec2, factor("b", levels=letters[1:3])), regexp = "Argument #2 and argument #4 are both factor but their levels are different.", fixed = TRUE)
})


test_that("nif-0003.019", {
  expect_error(nif(test_vec1, factor("a", levels=letters[1:2]), test_vec2, factor("b", levels=letters[1:2]),default=factor("c", levels=letters[1:3])), regexp = "Resulting value and 'default' are both type factor but their levels are different.", fixed = TRUE)
})


test_that("nif-0003.020", {
  expect_error(nif(test_vec1, 1L:10L, test_vec2, 3L:12L, test_vec2), regexp = "Received 5 inputs; please supply an even number of arguments in ... consisting of logical condition, resulting value pairs (in that order). Note that argument 'default' must be named explicitly (e.g.: default=0)", fixed = TRUE)
})


test_that("nif-0003.021", {
  expect_error(nif(test_vec1, 1L, test_vec2, 3), regexp = "Argument #4 is of type double, however argument #2 is of type integer. Please make sure all output values have the same type.", fixed = TRUE)
})


test_that("nif-0003.022", {
  expect_error(nif(test_vec1, "FALSE", test_vec2, TRUE), regexp = "Argument #4 is of type logical, however argument #2 is of type character. Please make sure all output values have the same type.", fixed = TRUE)
})


test_that("nif-0003.023", {
  expect_error(nif(test_vec1, "FALSE", test_vec2, 5L), regexp = "Argument #4 is of type integer, however argument #2 is of type character. Please make sure all output values have the same type.", fixed = TRUE)
})


test_that("nif-0003.024", {
  expect_error(nif(test_vec1, as.Date("2019-10-11"), test_vec2, as.Date("2019-10-14"),default="2019-10-15"), regexp = "Resulting value is of type double but 'default' is of type character. Please make sure that both arguments have the same type.", fixed = TRUE)
})


test_that("nif-0003.025", {
  expect_error(nif(test_vec1, as.Date("2019-10-11"), test_vec2, as.Date("2019-10-14"),default=123), regexp = "Resulting value has different class than 'default'. Please make sure that both arguments have the same class.", fixed = TRUE)
})


test_that("nif-0003.026", {
  expect_identical(nif(test_vec1, 1L, test_vec2, 0L, default=rep(2L, 11)), as.integer(out_vec_def))
})


test_that("nif-0003.027", {
  expect_identical(nif(test_vec1, 1L, test_vec2, rep(0L, 11), default=rep(2L, 11)), as.integer(out_vec_def))
})


test_that("nif-0003.028", {
  expect_identical(nif(test_vec1, rep(1L,11L), test_vec2, rep(0L,11L)), as.integer(out_vec))
})


test_that("nif-0003.029", {
  expect_identical(nif(test_vec1, rep(1,11L), test_vec2, rep(0,11L)), out_vec)
})


test_that("nif-0003.030", {
  expect_identical(nif(test_vec1, rep("1",11L), test_vec2, rep("0",11L)), as.character(out_vec))
})


test_that("nif-0003.031", {
  expect_identical(nif(test_vec1, rep(TRUE,11L), test_vec2, rep(FALSE,11L)), as.logical(out_vec))
})


test_that("nif-0003.032", {
  expect_kit_equal(nif(test_vec1, rep(1+0i,11L), test_vec2, rep(0+0i,11L)), as.complex(out_vec))
})


test_that("nif-0003.033", {
  expect_identical(nif(test_vec1, rep(list(1),11L), test_vec2, rep(list(0),11L)), list(1,1,1,1,1, NULL, 0, 0, 0, 0, 0))
})


test_that("nif-0003.034", {
  expect_identical(nif(test_vec1, rep(as.Date("2019-10-11"),11L), test_vec2, rep(as.Date("2019-10-14"),11L)), c(rep(as.Date("2019-10-11"),5),NA,rep(as.Date("2019-10-14"),5)))
})


test_that("nif-0003.035", {
  expect_identical(nif(test_vec1, rep(factor("a", levels=letters[1:3]),11L), test_vec2, rep(factor("b", levels=letters[1:3]),11L)), factor(c(rep("a",5),NA,rep("b",5)), levels=letters[1:3]))
})


test_that("nif-0003.036", {
  expect_identical(nif(test_vec_na1, 1L, test_vec_na2, 0L), as.integer(out_vec_na))
})


test_that("nif-0003.037", {
  expect_identical(nif(test_vec_na1, 1, test_vec_na2, 0), out_vec_na)
})


test_that("nif-0003.038", {
  expect_identical(nif(test_vec_na1, "1", test_vec_na2, "0"), as.character(out_vec_na))
})


test_that("nif-0003.039", {
  expect_identical(nif(test_vec_na1, TRUE, test_vec_na2, FALSE), as.logical(out_vec_na))
})


test_that("nif-0003.040", {
  expect_kit_equal(nif(test_vec_na1, 1+0i, test_vec_na2, 0+0i), as.complex(out_vec_na))
})


test_that("nif-0003.041", {
  expect_identical(nif(test_vec_na1, list(1), test_vec_na2, list(0)), list(1,1,1,1,1, NULL, 0, 0, 0, 0, 0,NULL))
})


test_that("nif-0003.042", {
  expect_identical(nif(c(TRUE,TRUE,TRUE,FALSE,FALSE),factor(NA,levels=letters[1:5]),c(FALSE,FALSE,FALSE,TRUE,TRUE),factor(letters[1:5])), factor(c(NA,NA,NA,"d","e"),levels=letters[1:5]))
})


test_that("nif-0003.043", {
  expect_identical(nif(c(TRUE,TRUE,TRUE,FALSE,NA,FALSE),factor(NA,levels=letters[1:6]),c(FALSE,FALSE,FALSE,TRUE,NA,TRUE),factor(letters[1:6])), factor(c(NA,NA,NA,"d",NA,"f"),levels=letters[1:6]))
})


test_that("nif-0003.044", {
  expect_identical(nif(c(TRUE,TRUE,TRUE,FALSE,NA,FALSE),factor(letters[1:6]),c(FALSE,FALSE,FALSE,TRUE,NA,TRUE),factor(NA,levels = letters[1:6])), factor(c("a","b","c",NA,NA,NA),levels=letters[1:6]))
})


test_that("nif-0003.045", {
  expect_identical(nif(c(TRUE,NA,TRUE,FALSE,FALSE,FALSE),factor(NA),c(TRUE,TRUE,TRUE,FALSE,NA,FALSE),factor(NA)), factor(c(NA,NA,NA,NA,NA,NA)))
})


test_that("nif-0003.046", {
  expect_error(nif(test_vec1, 1L, test_vec2, 0L, default=NA), regexp = "Resulting value is of type integer but 'default' is of type logical. Please make sure that both arguments have the same type.", fixed = TRUE)
})


test_that("nif-0003.047", {
  expect_error(nif(test_vec1, 1L, test_vec2, rep(0L, 11), default=NA), regexp = "Resulting value is of type integer but 'default' is of type logical. Please make sure that both arguments have the same type.", fixed = TRUE)
})


test_that("nif-0003.048", {
  expect_identical(nif(TRUE, list(data.frame(1:5)), FALSE, list(data.frame(5:1))), list(data.frame(1:5)))
})


test_that("nif-0003.049", {
  expect_identical(nif(FALSE, list(data.frame(1:5)), TRUE, list(data.frame(5:1))), list(data.frame(5:1)))
})


test_that("nif-0003.050", {
  expect_error(nif(1L,1L,TRUE,0L), regexp = "Argument #1 must be logical.", fixed = TRUE)
})


test_that("nif-0003.051", {
  expect_identical(nif(TRUE,1L,5L,0L), 1L)
})


test_that("nif-0003.052", {
  expect_identical(nif(test_vec1, 1L, test_vec2, 0L, test_vec3, 2L), as.integer(out_vec_def))
})


test_that("nif-0003.053", {
  expect_identical(nif(test_vec1, 1, test_vec2, 0, test_vec3, 2), out_vec_def)
})


test_that("nif-0003.054", {
  expect_identical(nif(test_vec1, "1", test_vec2, "0", test_vec3, "2"), as.character(out_vec_def))
})


test_that("nif-0003.055", {
  expect_identical(nif(test_vec1, TRUE, test_vec2, FALSE, test_vec3, TRUE), as.logical(out_vec_def))
})


test_that("nif-0003.056", {
  expect_identical(nif(test_vec1, 1+0i, test_vec2, 0+0i, test_vec3, 2+0i), as.complex(out_vec_def))
})


test_that("nif-0003.057", {
  expect_identical(nif(test_vec1, list(1), test_vec2, list(0), test_vec3, list(2)), list(1,1,1,1,1, 2, 0, 0, 0, 0, 0))
})


test_that("nif-0003.058", {
  expect_identical(nif(test_vec1, as.Date("2019-10-11"), test_vec2, as.Date("2019-10-14"), test_vec3, as.Date("2019-10-15")), c(rep(as.Date("2019-10-11"),5),as.Date("2019-10-15"),rep(as.Date("2019-10-14"),5)))
})


test_that("nif-0003.059", {
  expect_identical(nif(test_vec1, factor("a", levels=letters[1:3]), test_vec2, factor("b", levels=letters[1:3]), test_vec3, factor("c", levels=letters[1:3])), factor(c(rep("a",5),"c",rep("b",5)), levels=letters[1:3]))
})


test_that("nif-0003.060", {
  expect_identical(nif(test_vec1, 1L), as.integer(out_vec_oc))
})


test_that("nif-0003.061", {
  expect_identical(nif(test_vec1, 1), out_vec_oc)
})


test_that("nif-0003.062", {
  expect_identical(nif(test_vec1, "1"), as.character(out_vec_oc))
})


test_that("nif-0003.063", {
  expect_identical(nif(test_vec1, TRUE), as.logical(out_vec_oc))
})


test_that("nif-0003.064", {
  expect_kit_equal(nif(test_vec1, 1+0i), as.complex(out_vec_oc))
})


test_that("nif-0003.065", {
  expect_identical(nif(test_vec1, list(1)), list(1,1,1,1,1, NULL, NULL, NULL, NULL, NULL, NULL))
})


test_that("nif-0003.066", {
  expect_identical(nif(test_vec1, as.Date("2019-10-11")), c(rep(as.Date("2019-10-11"),5),rep(NA,6)))
})


test_that("nif-0003.067", {
  expect_identical(nif(test_vec1, factor("a", levels=letters[1:3])), factor(c(rep("a",5),rep("NA",6)), levels=letters[1:3]))
})


test_that("nif-0003.068", {
  expect_error(nif(test_vec1, 1L, default = 1:2), regexp = "Length of 'default' must either be 1 or length of logical condition.", fixed = TRUE)
})


test_that("nif-0003.069", {
  expect_error(nif(test_vec1, 1L, test_vec_na1, 2L), regexp = "Argument #3 has a different length than argument #1. Please make sure all logical conditions have the same length.", fixed = TRUE)
})


test_that("nif-0003.070", {
  expect_error(nif(test_vec1, as.Date("2019-10-11"), test_vec2, 2), regexp = "Argument #4 has different class than argument #2, Please make sure all output values have the same class.", fixed = TRUE)
})


test_that("nif-0003.071", {
  expect_error(nif(test_vec1, 1L, test_vec2, 2:3), regexp = "Length of output value #4 must either be 1 or length of logical condition.", fixed = TRUE)
})


test_that("nif-0003.072", {
  expect_identical(nif(TRUE, 1L, FALSE, stop("bang!")), 1L)
})


test_that("nif-0003.073", {
  expect_identical(nif(test_vec1, 1L, test_vec2, 0:10), as.integer(c( 1, 1, 1, 1, 1, NA, 6, 7, 8, 9, 10)))
})


test_that("nif-0003.074", {
  expect_identical(nif(test_vec1, 0:10, test_vec2, 0L), as.integer(c( 0, 1, 2, 3, 4, NA, 0, 0, 0, 0, 0)))
})


test_that("nif-0003.075", {
  expect_identical(nif(test_vec1, 1, test_vec2, as.numeric(0:10)), as.numeric(c( 1, 1, 1, 1, 1, NA, 6, 7, 8, 9, 10)))
})


test_that("nif-0003.076", {
  expect_identical(nif(test_vec1, as.numeric(0:10), test_vec2, 0), as.numeric(c( 0, 1, 2, 3, 4, NA, 0, 0, 0, 0, 0)))
})


test_that("nif-0003.077", {
  expect_identical(nif(test_vec1, "1", test_vec2, as.character(0:10)), as.character(c( 1, 1, 1, 1, 1, NA, 6, 7, 8, 9, 10)))
})


test_that("nif-0003.078", {
  expect_identical(nif(test_vec1, as.character(0:10), test_vec2, "0"), as.character(c( 0, 1, 2, 3, 4, NA, 0, 0, 0, 0, 0)))
})


test_that("nif-0003.079", {
  expect_identical(nif(test_vec1, TRUE, test_vec2, rep(FALSE, 11L)), as.logical(out_vec))
})


test_that("nif-0003.080", {
  expect_identical(nif(test_vec1, rep(TRUE, 11L), test_vec2, FALSE), as.logical(out_vec))
})


test_that("nif-0003.081", {
  expect_kit_equal(nif(test_vec1, 1+0i, test_vec2, rep(0+0i, 11L)), as.complex(out_vec))
})


test_that("nif-0003.082", {
  expect_kit_equal(nif(test_vec1, rep(1+0i, 11L), test_vec2, 0+0i), as.complex(out_vec))
})


test_that("nif-0003.083", {
  expect_identical(nif(test_vec1, list(rep(1, 11L)), test_vec2, list(0)), list(rep(1, 11L),rep(1, 11L),rep(1, 11L),rep(1, 11L),rep(1, 11L), NULL, 0, 0, 0, 0, 0))
})


test_that("nif-0003.084", {
  expect_identical(nif(test_vec1, list(1), test_vec2, list(rep(0,11L))), list(1,1,1,1,1, NULL, rep(0,11L), rep(0,11L), rep(0,11L), rep(0,11L), rep(0,11L)))
})


test_that("nif-0003.085", {
  expect_identical(nif(test_vec1, list(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), test_vec2, list(0)), list(1,1,1,1,1, NULL, 0, 0, 0, 0, 0))
})


test_that("nif-0003.086", {
  expect_identical(nif(test_vec1, list(1), test_vec2, list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), list(1,1,1,1,1, NULL, 0, 0, 0, 0, 0))
})


test_that("nif-0003.087", {
  expect_error(nif(TRUE, s1, FALSE, s2), regexp = "S4 class objects are not supported.", fixed = TRUE)
})


test_that("nif-0003.088", {
  expect_identical(nif(test_vec1, rep(1L, 11), test_vec2, 0L, default=rep(2L, 11)), as.integer(out_vec_def))
})


test_that("nif-0003.089", {
  expect_identical(nif(test_vec1, 1, test_vec2, 0, default=rep(2, 11)), as.numeric(out_vec_def))
})


test_that("nif-0003.090", {
  expect_identical(nif(test_vec1, rep(1, 11), test_vec2, rep(0, 11), default=rep(2, 11)), as.numeric(out_vec_def))
})


test_that("nif-0003.091", {
  expect_identical(nif(test_vec1, 1, test_vec2, rep(0, 11), default=rep(2, 11)), as.numeric(out_vec_def))
})


test_that("nif-0003.092", {
  expect_identical(nif(test_vec1, rep(1, 11), test_vec2, 0, default=rep(2, 11)), as.numeric(out_vec_def))
})


test_that("nif-0003.093", {
  expect_identical(nif(test_vec1, 1+0i, test_vec2, 0+0i, default=rep(2+0i, 11)), as.complex(out_vec_def))
})


test_that("nif-0003.094", {
  expect_identical(nif(test_vec1, rep(1+0i, 11), test_vec2, rep(0+0i, 11), default=rep(2+0i, 11)), as.complex(out_vec_def))
})


test_that("nif-0003.095", {
  expect_identical(nif(test_vec1, 1+0i, test_vec2, rep(0+0i, 11), default=rep(2+0i, 11)), as.complex(out_vec_def))
})


test_that("nif-0003.096", {
  expect_identical(nif(test_vec1, rep(1+0i, 11), test_vec2, 0+0i, default=rep(2+0i, 11)), as.complex(out_vec_def))
})


test_that("nif-0003.097", {
  expect_identical(nif(test_vec1, "1", test_vec2, "0", default=rep("2", 11)), as.character(out_vec_def))
})


test_that("nif-0003.098", {
  expect_identical(nif(test_vec1, rep("1", 11), test_vec2, rep("0", 11), default=rep("2", 11)), as.character(out_vec_def))
})


test_that("nif-0003.099", {
  expect_identical(nif(test_vec1, "1", test_vec2, rep("0", 11), default=rep("2", 11)), as.character(out_vec_def))
})


test_that("nif-0003.100", {
  expect_identical(nif(test_vec1, rep("1", 11), test_vec2, "0", default=rep("2", 11)), as.character(out_vec_def))
})


test_that("nif-0003.101", {
  expect_identical(nif(test_vec1, TRUE, test_vec2, FALSE, default=rep(TRUE, 11)), as.logical(out_vec_def))
})


test_that("nif-0003.102", {
  expect_identical(nif(test_vec1, rep(TRUE, 11), test_vec2, rep(FALSE, 11), default=rep(TRUE, 11)), as.logical(out_vec_def))
})


test_that("nif-0003.103", {
  expect_identical(nif(test_vec1, TRUE, test_vec2, rep(FALSE, 11), default=rep(TRUE, 11)), as.logical(out_vec_def))
})


test_that("nif-0003.104", {
  expect_identical(nif(test_vec1, rep(TRUE, 11), test_vec2, FALSE, default=rep(TRUE, 11)), as.logical(out_vec_def))
})


test_that("nif-0003.105", {
  expect_identical(nif(test_vec1, list(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), test_vec2, list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),default=list(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)), list(1,1,1,1,1, 2, 0, 0, 0, 0, 0))
})


test_that("nif-0003.106", {
  expect_identical(nif(test_vec1, list(1), test_vec2, list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),default=list(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)), list(1,1,1,1,1, 2, 0, 0, 0, 0, 0))
})


test_that("nif-0003.107", {
  expect_identical(nif(test_vec1, list(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), test_vec2, list(0),default=list(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)), list(1,1,1,1,1, 2, 0, 0, 0, 0, 0))
})


test_that("nif-0003.108", {
  expect_identical(nif(test_vec1, list(1), test_vec2, list(0),default=list(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)), list(1,1,1,1,1, 2, 0, 0, 0, 0, 0))
})


test_that("0003.109", {
  expect_identical(V0, V4)
})


test_that("nif-0003.110", {
  expect_identical(nif(x <= -100, structure(x * 1.0, class = 'abc'),
          x <= -10, structure(x * 1.0, class = 'abc'),
          x <=  0,  structure(x * 1.0, class = 'abc'),
          x <=  100, structure(x * 1.0, class = 'abc'), 
          x <=  1000, structure(x * 1.0, class = 'abc'),
          x >=  1000, structure(x * 1.0, class = 'abc')), structure(x, class = 'abc'))
})


test_that("nif-0003.111", {
  expect_error(nif(c(TRUE,FALSE), 1, c(FALSE,TRUE), s2), regexp = "S4 class objects are not supported.", fixed = TRUE)
})

