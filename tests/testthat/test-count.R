# Tests for counting — countNA, count, pcount, pcountNA, pallNA, pallv, panyv, panyNA
# Migrated from tests/test_kit.R (custom check() suite) to testthat 3e — see issue #54.
# Each legacy check("id", actual, expected) maps to expect_identical();
# 31 tolerant cases (identical FALSE but all.equal+typeof TRUE) use expect_kit_equal();
# check(..., error=) maps to expect_error(regexp=, fixed=TRUE).
# Legacy IDs are preserved as test_that() labels (prefixed by function for uniqueness).
# Setup and expectations stay interleaved in legacy order: later sections may
# redefine x/y/out_vec/... so setup must not be hoisted above earlier tests.

set.seed(123)

x = c(0L,1L,2L,NA_integer_)

test_that("countNA-0012.001", {
  expect_identical(countNA(x), sum(is.na(x)))
})


test_that("countNA-0012.002", {
  expect_identical(countNA(as.logical(x)), sum(is.na(as.logical(x))))
})


test_that("countNA-0012.003", {
  expect_identical(countNA(as.numeric(x)), sum(is.na(as.numeric(x))))
})


test_that("countNA-0012.004", {
  expect_identical(countNA(as.complex(x)), sum(is.na(as.complex(x))))
})


test_that("countNA-0012.005", {
  expect_identical(countNA(as.character(x)), sum(is.na(as.character(x))))
})


test_that("countNA-0012.006", {
  expect_identical(countNA(as.list(x)), list(0L,0L,0L,1L))
})


test_that("countNA-0012.007", {
  expect_identical(countNA(NaN), sum(is.na(NaN)))
})


test_that("countNA-0012.008", {
  expect_error(countNA(as.raw("0")), regexp = "Type raw is not supported.", fixed = TRUE)
})


test_that("countNA-0012.009", {
  expect_identical(countNA(NULL), 0L)
})


test_that("countNA-0012.010", {
  expect_identical(countNA(list(c(0L,1L,2L,NA_integer_),NULL)), list(1L,0L))
})


test_that("countNA-0012.011", {
  expect_identical(countNA(list(x,list(x,1),1)), list(1L,list(1L,0L),0L))
})

x = c(0L,1L,2L,NA_integer_)

test_that("count-0013.001", {
  expect_identical(count(x, 1L), sum(x == 1L,na.rm = TRUE))
})


test_that("count-0013.002", {
  expect_identical(count(as.logical(x), TRUE), sum(as.logical(x) == TRUE,na.rm = TRUE))
})


test_that("count-0013.003", {
  expect_identical(count(as.numeric(x), 1), sum(as.numeric(x) == 1,na.rm = TRUE))
})


test_that("count-0013.004", {
  expect_identical(count(as.complex(x), 1+0i), sum(as.complex(x) == 1+0i,na.rm = TRUE))
})


test_that("count-0013.005", {
  expect_identical(count(as.character(x), "2"), sum(as.character(x) == "2",na.rm = TRUE))
})


test_that("count-0013.006", {
  expect_error(count(NULL,NA), regexp = "Type of 'value' (logical) is different than type of 'x' (NULL). Please make sure both have the same type.", fixed = TRUE)
})


test_that("count-0013.007", {
  expect_identical(count(NaN, NA_real_), 0L)
})


test_that("count-0013.008", {
  expect_error(count(as.raw("00"),as.raw("01")), regexp = "Type raw is not supported.", fixed = TRUE)
})


test_that("count-0013.009", {
  expect_error(count(NULL,NULL), regexp = "Argument 'value' must be non NULL and length 1.", fixed = TRUE)
})


test_that("count-0013.010", {
  expect_identical(count(c(as.Date("2020-06-20"),as.Date("2020-06-21")), as.Date("2020-06-20")), 1L)
})


test_that("count-0013.011", {
  expect_error(count(c(as.Date("2020-06-20"),as.Date("2020-06-21")), 0), regexp = "'x' has different class than 'y'. Please make sure that both arguments have the same class.", fixed = TRUE)
})


test_that("count-0013.012", {
  expect_error(count(iris$Species, factor("setosa","setosa")), regexp = "'x' and 'y' are both type factor but their levels are different.", fixed = TRUE)
})


test_that("count-0013.013", {
  expect_identical(count(iris$Species, iris$Species[1]), 50L)
})

x = c(1, 3, NA, 5)
y = c(2, NA, 4, 1)
z = c(3, 4, 4, 1)
d1 = c(as.Date("2020-06-20"),as.Date("2020-06-21"),as.Date("2020-06-20"),as.Date("2020-06-21"))
d2 = c(as.Date("2020-06-22"),as.Date("2020-06-23"),as.Date("2020-06-22"),as.Date("2020-06-23"))
f1 = factor(c("a","b","c","d"), c("a","b","c","d"))
f2 = factor(c("a","a","c","a"), c("a","b","c","d"))

test_that("pcount-0014.001", {
  expect_identical(pcount(x, value = 3), sapply(1:4, function(i) count(x[i], 3)))
})


test_that("pcount-0014.002", {
  expect_identical(pcount(as.integer(x), value = 3L), sapply(1:4, function(i) count(as.integer(x[i]), 3L)))
})


test_that("pcount-0014.003", {
  expect_identical(pcount(as.character(x), value = "3"), sapply(1:4, function(i) count(as.character(x[i]), "3")))
})


test_that("pcount-0014.004", {
  expect_identical(pcount(as.complex(x), value = 3+0i), sapply(1:4, function(i) count(as.complex(x[i]), 3+0i)))
})


test_that("pcount-0014.005", {
  expect_identical(pcount(as.logical(x), value = TRUE), sapply(1:4, function(i) count(as.logical(x[i]), TRUE)))
})


test_that("pcount-0014.006", {
  expect_error(pcount(as.logical(x), value = NULL), regexp = "argument is of length zero", fixed = TRUE)
})


test_that("pcount-0014.007", {
  expect_identical(pcount(x, value = NA_real_), c(0L,0L,1L,0L))
})


test_that("pcount-0014.008", {
  expect_error(pcount(value = TRUE), regexp = "Please supply at least 1 argument. (0 argument supplied)", fixed = TRUE)
})


test_that("pcount-0014.009", {
  expect_identical(pcount(x,y,z,value = 3), c(1L,1L,0L,0L))
})


test_that("pcount-0014.010", {
  expect_identical(pcount(x,y,z,value = 4), c(0L,1L,2L,0L))
})


test_that("pcount-0014.011", {
  expect_error(pcount(x,y,z[1:3],value = 4), regexp = "Argument 3 is of length 3 but argument 1 is of length 4. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("pcount-0014.012", {
  expect_error(pcount(x,y,as.logical(z),value = 4), regexp = "Type of argument 3 is logical but argument 1 is of type double. Please make sure both have the same type.", fixed = TRUE)
})


test_that("pcount-0014.013", {
  expect_error(pcount(x,y,z,value = 4L), regexp = "Type of 'value' (integer) is different than type of Argument 1 (double). Please make sure both have the same type.", fixed = TRUE)
})


test_that("pcount-0014.014", {
  expect_error(pcount(list(x),y,z,value = 4), regexp = "Argument 1 is of type list. Only logical, integer, double, complex and character types are supported.", fixed = TRUE)
})


test_that("pcount-0014.015", {
  expect_identical(pcount(d1, d2, value = as.Date("2020-06-20")), c(1L, 0L, 1L, 0L))
})


test_that("pcount-0014.016", {
  expect_error(pcount(d1, d2, value = 5), regexp = "Class of 'value' is different than class of Argument 1. Please make sure both have the same class.", fixed = TRUE)
})


test_that("pcount-0014.017", {
  expect_error(pcount(d1, c(1,2,3,4), value = as.Date("2020-06-20")), regexp = "Class of 'value' is different than class of Argument 2. Please make sure both have the same class.", fixed = TRUE)
})


test_that("pcount-0014.018", {
  expect_identical(pcount(f1, f2, value = factor("a", c("a","b","c","d"))), c(2L, 1L, 0L, 1L))
})


test_that("pcount-0014.019", {
  expect_error(pcount(f1, f2, value = factor("a", c("a","b","c"))), regexp = "Levels of 'value' are different than levels of Argument 1. Please make sure both have the same levels.", fixed = TRUE)
})


test_that("pcount-0014.020", {
  expect_error(pcount(f1, factor("a", c("a","b","c")), value = factor("a", c("a","b","c","d"))), regexp = "Levels of 'value' are different than levels of Argument 2. Please make sure both have the same levels.", fixed = TRUE)
})

v = c("hello",NA,"bye","john")
w = c(NA_integer_,2L,8L,9L)
x = c(1, 3, NA, 5)
y = c(2, NA, 4, 1)
z = c(3, 4, 4, 1)
d1 = c(as.Date("2020-06-22"),as.Date("2020-06-23"),as.Date("2020-06-22"),NA)
f1 = factor(c("a","b","c",NA), c("a","b","c",NA))

test_that("pcountNA-0023.001", {
  expect_identical(pcountNA(x), c(0L,0L,1L,0L))
})


test_that("pcountNA-0023.002", {
  expect_identical(pcountNA(x, y), c(0L,1L,1L,0L))
})


test_that("pcountNA-0023.003", {
  expect_identical(pcountNA(x, y, z), c(0L,1L,1L,0L))
})


test_that("pcountNA-0023.004", {
  expect_identical(pcountNA(x, y, z, v), c(0L,2L,1L,0L))
})


test_that("pcountNA-0023.005", {
  expect_identical(pcountNA(x, y, z, v, w), c(1L,2L,1L,0L))
})


test_that("pcountNA-0023.006", {
  expect_identical(pcountNA(data.frame(x, y, z, v, w, f1, d1)), c(1L,2L,1L,2L))
})


test_that("pallNA-0024.001", {
  expect_identical(pallNA(data.frame(x, y, z, v, w, f1, d1)), c(FALSE, FALSE, FALSE, FALSE))
})


test_that("pallNA-0024.002", {
  expect_identical(pallNA(data.frame(x, x, x, x)), c(FALSE, FALSE, TRUE, FALSE))
})


test_that("pallNA-0024.003", {
  expect_identical(pallNA(v), c(FALSE, TRUE, FALSE, FALSE))
})


test_that("pallNA-0024.004", {
  expect_identical(pallNA(w), c(TRUE, FALSE, FALSE, FALSE))
})


test_that("pallNA-0024.005", {
  expect_identical(pallNA(x), c(FALSE, FALSE, TRUE, FALSE))
})


test_that("pallv-0025.001", {
  expect_identical(pallv(data.frame(a=c(1,1,2,2),b=c(1,2,1,2)),value=1), c(TRUE, FALSE, FALSE, FALSE))
})


test_that("pallv-0025.002", {
  expect_identical(pallv(v, value = "bye"), c(FALSE, FALSE, TRUE, FALSE))
})


test_that("pallv-0025.003", {
  expect_identical(pallv(x, x, value = 3), c(FALSE, TRUE, FALSE, FALSE))
})


test_that("pallv-0025.004", {
  expect_identical(pallv(w, value = 8L), c(FALSE, FALSE, TRUE, FALSE))
})


test_that("panyv-0026.001", {
  expect_identical(panyv(data.frame(a=c(1,1,2,2),b=c(1,2,1,2)),value=1), c(TRUE, TRUE, TRUE, FALSE))
})


test_that("panyv-0026.002", {
  expect_identical(panyv(v, value = "bye"), c(FALSE, FALSE, TRUE, FALSE))
})


test_that("panyv-0026.003", {
  expect_identical(panyv(x, y, value = 1), c(TRUE, FALSE, FALSE, TRUE))
})


test_that("panyv-0026.004", {
  expect_identical(panyv(w, value = 8L), c(FALSE, FALSE, TRUE, FALSE))
})


test_that("panyNA-0027.001", {
  expect_identical(panyNA(data.frame(x, y, z, v, w, f1, d1)), c(TRUE, TRUE, TRUE, TRUE))
})


test_that("panyNA-0027.002", {
  expect_identical(panyNA(data.frame(y, z, v, w, f1, d1)), c(TRUE, TRUE, FALSE, TRUE))
})


test_that("panyNA-0027.003", {
  expect_identical(panyNA(v), c(FALSE, TRUE, FALSE, FALSE))
})


test_that("panyNA-0027.004", {
  expect_identical(panyNA(w), c(TRUE, FALSE, FALSE, FALSE))
})


test_that("panyNA-0027.005", {
  expect_identical(panyNA(x), c(FALSE, FALSE, TRUE, FALSE))
})

