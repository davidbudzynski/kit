# Tests for parallel reductions — psum, pprod, fpmin, fpmax, prange, pall, pany, pmean
# Migrated from tests/test_kit.R (custom check() suite) to testthat 3e — see issue #54.
# Each legacy check("id", actual, expected) maps to expect_identical();
# 31 tolerant cases (identical FALSE but all.equal+typeof TRUE) use expect_kit_equal();
# check(..., error=) maps to expect_error(regexp=, fixed=TRUE).
# Legacy IDs are preserved as test_that() labels (prefixed by function for uniqueness).
# Setup and expectations stay interleaved in legacy order: later sections may
# redefine x/y/out_vec/... so setup must not be hoisted above earlier tests.

set.seed(123)

x = c(1, 3, NA, 5)
y = c(2, NA, 4, 1)
z = c(3, 4, 4, 1)
x0 = rnorm(1000L)
y0 = rnorm(1000L)
z0 = rnorm(1000L)

test_that("psum-0005.001", {
  expect_identical(psum(x, y, z, na.rm = FALSE), c(6, NA, NA, 7))
})


test_that("psum-0005.002", {
  expect_identical(psum(x, y, z, na.rm = TRUE), c(6, 7, 8, 7))
})


test_that("psum-0005.003", {
  expect_identical(psum(as.integer(x), as.integer(y), as.integer(z), na.rm = FALSE), c(6L, NA_integer_, NA_integer_, 7L))
})


test_that("psum-0005.004", {
  expect_identical(psum(as.integer(x), as.integer(y), as.integer(z), na.rm = TRUE), c(6L, 7L, 8L, 7L))
})


test_that("psum-0005.005", {
  expect_error(psum(as.raw(z), y, na.rm = TRUE), regexp = "Argument 1 is of type raw. Only integer/logical, double and complex types are supported. A data.frame (of the previous types) is also supported as a single input.", fixed = TRUE)
})


test_that("psum-0005.006", {
  expect_error(psum(x, y, 1:2, na.rm = FALSE), regexp = "Argument 3 is of length 2 but argument 1 is of length 4. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("psum-0005.007", {
  expect_error(psum(1:10, 1:5, na.rm = FALSE), regexp = "Argument 2 is of length 5 but argument 1 is of length 10. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("psum-0005.008", {
  expect_error(psum(x, as.raw(z), y, na.rm = TRUE), regexp = "Argument 2 is of type raw. Only integer/logical, double and complex types are supported.", fixed = TRUE)
})


test_that("psum-0005.009", {
  expect_identical(psum(1:10, 1:10, 21:30), 1:10 + 1:10 + 21:30)
})


test_that("psum-0005.010", {
  expect_error(psum(x, y, z, na.rm = NA), regexp = "Argument 'na.rm' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("psum-0005.011", {
  expect_identical(psum(x, na.rm = FALSE), x)
})


test_that("psum-0005.012", {
  expect_identical(psum(as.integer(x), y, z, na.rm = TRUE), c(6, 7, 8, 7))
})


test_that("psum-0005.013", {
  expect_identical(psum(c(1,3,NA,5,NA), c(2,NA,4,1,NA), na.rm = TRUE), c(3, 3, 4, 6, 0))
})


test_that("psum-0005.014", {
  expect_identical(psum(x, y, as.integer(z), na.rm = FALSE), c(6, NA, NA, 7))
})


test_that("psum-0005.015", {
  expect_error(psum(na.rm = FALSE), regexp = "Please supply at least 1 argument. (0 argument supplied)", fixed = TRUE)
})


test_that("psum-0005.016", {
  expect_identical(psum(x0, y0, z0), x0+y0+z0)
})


test_that("psum-0005.017", {
  expect_identical(psum(as.complex(x0), as.complex(y0), as.complex(z0)), as.complex(x0)+as.complex(y0)+as.complex(z0))
})


test_that("psum-0005.018", {
  expect_identical(psum(as.complex(x0), as.complex(y0), z0), as.complex(x0)+as.complex(y0)+z0)
})


test_that("psum-0005.019", {
  expect_identical(psum(as.complex(x), as.complex(y), as.complex(z), na.rm = FALSE), as.complex(c(6, NA, NA, 7)))
})


test_that("psum-0005.020", {
  expect_identical(psum(as.complex(x), as.complex(y), as.complex(z), na.rm = TRUE), as.complex(c(6, 7, 8, 7)))
})


test_that("psum-0005.021", {
  expect_identical(psum(x, y, z, rep(Inf,4L), na.rm = FALSE), x+y+z+Inf)
})


test_that("psum-0005.022", {
  expect_identical(psum(x, y, z, rep(Inf,4L), na.rm = TRUE), rep(Inf, 4L))
})


test_that("psum-0005.023", {
  expect_identical(psum(NA_integer_, na.rm = TRUE), 0L)
})


test_that("psum-0005.024", {
  expect_identical(psum(NA_real_, na.rm = TRUE), 0)
})


test_that("psum-0005.025", {
  expect_identical(psum(NA_complex_, na.rm = TRUE), 0+0i)
})


test_that("psum-0005.026", {
  expect_identical(psum(iris[,1:2]), rowSums(iris[,1:2]))
})


test_that("psum-0005.027", {
  expect_error(psum(iris[,1:2],iris[,1:2]), regexp = "Argument 1 is of type list. Only integer/logical, double and complex types are supported. A data.frame (of the previous types) is also supported as a single input.", fixed = TRUE)
})


test_that("psum-0005.028", {
  expect_error(psum(1:150,iris$Species, na.rm = FALSE), regexp = "Function 'psum' is not meaningful for factors.", fixed = TRUE)
})


test_that("psum-0005.029", {
  expect_identical(psum(unclass(mtcars)), psum(mtcars))
})


test_that("pprod-0006.001", {
  expect_identical(pprod(x, y, z, na.rm = FALSE), c(6, NA, NA, 5))
})


test_that("pprod-0006.002", {
  expect_identical(pprod(x, y, z, na.rm = TRUE), c(6, 12, 16, 5))
})


test_that("pprod-0006.003", {
  expect_identical(pprod(as.integer(x), as.integer(y), as.integer(z), na.rm = FALSE), c(6, NA_real_, NA_real_, 5))
})


test_that("pprod-0006.004", {
  expect_identical(pprod(as.integer(x), as.integer(y), as.integer(z), na.rm = TRUE), c(6, 12, 16, 5))
})


test_that("pprod-0006.005", {
  expect_error(pprod(as.raw(z), y, na.rm = TRUE), regexp = "Argument 1 is of type raw. Only integer/logical, double and complex types are supported. A data.frame (of the previous types) is also supported as a single input.", fixed = TRUE)
})


test_that("pprod-0006.006", {
  expect_error(pprod(x, y, 1:2, na.rm = FALSE), regexp = "Argument 3 is of length 2 but argument 1 is of length 4. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("pprod-0006.007", {
  expect_error(pprod(1:10, 1:5, na.rm = FALSE), regexp = "Argument 2 is of length 5 but argument 1 is of length 10. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("pprod-0006.008", {
  expect_error(pprod(x, as.raw(z), y, na.rm = TRUE), regexp = "Argument 2 is of type raw. Only integer/logical, double and complex types are supported.", fixed = TRUE)
})


test_that("pprod-0006.009", {
  expect_identical(pprod(1:10, 1:10, 21:30), as.double(1:10 * 1:10 * 21:30))
})


test_that("pprod-0006.010", {
  expect_error(pprod(x, y, z, na.rm = NA), regexp = "Argument 'na.rm' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("pprod-0006.011", {
  expect_identical(pprod(x, na.rm = FALSE), x)
})


test_that("pprod-0006.012", {
  expect_identical(pprod(as.integer(x), y, z, na.rm = TRUE), c(6, 12, 16, 5))
})


test_that("pprod-0006.013", {
  expect_identical(pprod(c(1,3,NA,5,NA), c(2,NA,4,1,NA), na.rm = TRUE), c(2, 3, 4, 5, 1))
})


test_that("pprod-0006.014", {
  expect_identical(pprod(x, y, as.integer(z), na.rm = FALSE), c(6, NA, NA, 5))
})


test_that("pprod-0006.015", {
  expect_error(pprod(na.rm = FALSE), regexp = "Please supply at least 1 argument. (0 argument supplied)", fixed = TRUE)
})


test_that("pprod-0006.016", {
  expect_identical(pprod(x0, y0, z0), x0*y0*z0)
})


test_that("pprod-0006.017", {
  expect_identical(pprod(as.complex(x0), as.complex(y0), as.complex(z0)), as.complex(x0)*as.complex(y0)*as.complex(z0))
})


test_that("pprod-0006.018", {
  expect_identical(pprod(as.complex(x0), as.complex(y0), z0), as.complex(x0)*as.complex(y0)*z0)
})


test_that("pprod-0006.019", {
  expect_kit_equal(pprod(as.complex(x), as.complex(y), as.complex(z), na.rm = FALSE), as.complex(c(6, NA, NA, 5)))
})


test_that("pprod-0006.020", {
  expect_identical(pprod(as.complex(x), as.complex(y), as.complex(z), na.rm = TRUE), as.complex(c(6, 12, 16, 5)))
})


test_that("pprod-0006.021", {
  expect_identical(pprod(x, y, z, rep(Inf, 4L), na.rm = FALSE), x*y*z*Inf)
})


test_that("pprod-0006.022", {
  expect_identical(pprod(x, y, z, rep(Inf, 4L), na.rm = TRUE), rep(Inf, 4L))
})


test_that("pprod-0006.023", {
  expect_identical(pprod(NA_integer_, na.rm = TRUE), 1)
})


test_that("pprod-0006.024", {
  expect_identical(pprod(NA_real_, na.rm = TRUE), 1)
})


test_that("pprod-0006.025", {
  expect_identical(pprod(NA_complex_, na.rm = TRUE), 1+0i)
})


test_that("pprod-0006.026", {
  expect_identical(pprod(iris[,1:2]), iris$Sepal.Length*iris$Sepal.Width)
})


test_that("pprod-0006.027", {
  expect_error(pprod(iris[,1:2],iris[,1:2]), regexp = "Argument 1 is of type list. Only integer/logical, double and complex types are supported. A data.frame (of the previous types) is also supported as a single input.", fixed = TRUE)
})


test_that("pprod-0006.028", {
  expect_error(pprod(1:150,iris$Species, na.rm = FALSE), regexp = "Function 'pprod' is not meaningful for factors.", fixed = TRUE)
})


test_that("pprod-0006.029", {
  expect_identical(pprod(unclass(mtcars)), pprod(mtcars))
})

x = c(1, 3, NA, 5)
y = c(2, NA, 4, 1)
z = c(3, 4, 4, 1)
x0 = rnorm(1000L)
y0 = rnorm(1000L)
z0 = rnorm(1000L)

test_that("fpmin-0008.001", {
  expect_identical(fpmin(x, y, z, na.rm = FALSE), c(1, NA, NA, 1))
})


test_that("fpmin-0008.002", {
  expect_identical(fpmin(x, y, z, na.rm = TRUE), c(1, 3, 4, 1))
})


test_that("fpmin-0008.003", {
  expect_identical(fpmin(as.integer(x), as.integer(y), as.integer(z), na.rm = FALSE), c(1L, NA_integer_, NA_integer_, 1L))
})


test_that("fpmin-0008.004", {
  expect_identical(fpmin(as.integer(x), as.integer(y), as.integer(z), na.rm = TRUE), c(1L, 3L, 4L, 1L))
})


test_that("fpmin-0008.005", {
  expect_identical(fpmin(c(TRUE, FALSE, NA), c(FALSE, TRUE, FALSE), na.rm = FALSE), c(FALSE, FALSE, NA))
})


test_that("fpmin-0008.006", {
  expect_identical(fpmin(c(TRUE, FALSE, NA), c(FALSE, TRUE, FALSE), na.rm = TRUE), c(FALSE, FALSE, FALSE))
})


test_that("fpmin-0008.007", {
  expect_error(fpmin(as.raw(z), y, na.rm = TRUE), regexp = "Argument 1 is of type raw. Only integer/logical and double types are supported. A data.frame (of the previous types) is also supported as a single input.", fixed = TRUE)
})


test_that("fpmin-0008.008", {
  expect_error(fpmin(x, y, 1:2, na.rm = FALSE), regexp = "Argument 3 is of length 2 but argument 1 is of length 4. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("fpmin-0008.009", {
  expect_error(fpmin(x, y, z, na.rm = NA), regexp = "Argument 'na.rm' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("fpmin-0008.010", {
  expect_identical(fpmin(x, na.rm = FALSE), x)
})


test_that("fpmin-0008.011", {
  expect_identical(fpmin(as.integer(x), y, z, na.rm = TRUE), c(1, 3, 4, 1))
})


test_that("fpmin-0008.012", {
  expect_identical(fpmin(x, y, as.integer(z), na.rm = FALSE), c(1, NA, NA, 1))
})


test_that("fpmin-0008.013", {
  expect_error(fpmin(na.rm = FALSE), regexp = "Please supply at least 1 argument. (0 argument supplied)", fixed = TRUE)
})


test_that("fpmin-0008.014", {
  expect_identical(fpmin(x0, y0, z0), pmin(x0, y0, z0))
})


test_that("fpmin-0008.015", {
  expect_identical(fpmin(c(1,3,NA,5,NA), c(2,NA,4,1,NA), na.rm = TRUE), c(1, 3, 4, 1, NA))
})


test_that("fpmin-0008.016", {
  expect_identical(fpmin(NA_integer_, na.rm = TRUE), NA_integer_)
})


test_that("fpmin-0008.017", {
  expect_identical(fpmin(NA_real_, na.rm = TRUE), NA_real_)
})


test_that("fpmin-0008.018", {
  expect_identical(fpmin(iris[,1:2]), pmin(iris$Sepal.Length, iris$Sepal.Width))
})


test_that("fpmin-0008.019", {
  expect_error(fpmin(1:150,iris$Species, na.rm = FALSE), regexp = "Function 'fpmin' is not meaningful for factors.", fixed = TRUE)
})


test_that("fpmax-0009.001", {
  expect_identical(fpmax(x, y, z, na.rm = FALSE), c(3, NA, NA, 5))
})


test_that("fpmax-0009.002", {
  expect_identical(fpmax(x, y, z, na.rm = TRUE), c(3, 4, 4, 5))
})


test_that("fpmax-0009.003", {
  expect_identical(fpmax(as.integer(x), as.integer(y), as.integer(z), na.rm = FALSE), c(3L, NA_integer_, NA_integer_, 5L))
})


test_that("fpmax-0009.004", {
  expect_identical(fpmax(as.integer(x), as.integer(y), as.integer(z), na.rm = TRUE), c(3L, 4L, 4L, 5L))
})


test_that("fpmax-0009.005", {
  expect_identical(fpmax(c(TRUE, FALSE, NA), c(FALSE, TRUE, FALSE), na.rm = FALSE), c(TRUE, TRUE, NA))
})


test_that("fpmax-0009.006", {
  expect_identical(fpmax(c(TRUE, FALSE, NA), c(FALSE, TRUE, FALSE), na.rm = TRUE), c(TRUE, TRUE, FALSE))
})


test_that("fpmax-0009.007", {
  expect_error(fpmax(as.raw(z), y, na.rm = TRUE), regexp = "Argument 1 is of type raw. Only integer/logical and double types are supported. A data.frame (of the previous types) is also supported as a single input.", fixed = TRUE)
})


test_that("fpmax-0009.008", {
  expect_error(fpmax(x, y, 1:2, na.rm = FALSE), regexp = "Argument 3 is of length 2 but argument 1 is of length 4. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("fpmax-0009.009", {
  expect_error(fpmax(x, y, z, na.rm = NA), regexp = "Argument 'na.rm' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("fpmax-0009.010", {
  expect_identical(fpmax(x, na.rm = FALSE), x)
})


test_that("fpmax-0009.011", {
  expect_identical(fpmax(as.integer(x), y, z, na.rm = TRUE), c(3, 4, 4, 5))
})


test_that("fpmax-0009.012", {
  expect_identical(fpmax(x, y, as.integer(z), na.rm = FALSE), c(3, NA, NA, 5))
})


test_that("fpmax-0009.013", {
  expect_error(fpmax(na.rm = FALSE), regexp = "Please supply at least 1 argument. (0 argument supplied)", fixed = TRUE)
})


test_that("fpmax-0009.014", {
  expect_identical(fpmax(x0, y0, z0), pmax(x0, y0, z0))
})


test_that("fpmax-0009.015", {
  expect_identical(fpmax(c(1,3,NA,5,NA), c(2,NA,4,1,NA), na.rm = TRUE), c(2, 3, 4, 5, NA))
})


test_that("fpmax-0009.016", {
  expect_identical(fpmax(NA_integer_, na.rm = TRUE), NA_integer_)
})


test_that("fpmax-0009.017", {
  expect_identical(fpmax(NA_real_, na.rm = TRUE), NA_real_)
})


test_that("fpmax-0009.018", {
  expect_identical(fpmax(iris[,1:2]), pmax(iris$Sepal.Length, iris$Sepal.Width))
})


test_that("fpmax-0009.019", {
  expect_error(fpmax(1:150,iris$Species, na.rm = FALSE), regexp = "Function 'fpmax' is not meaningful for factors.", fixed = TRUE)
})


test_that("prange-0010.001", {
  expect_identical(prange(x, y, z, na.rm = FALSE), c(2, NA, NA, 4))
})


test_that("prange-0010.002", {
  expect_identical(prange(x, y, z, na.rm = TRUE), c(2, 1, 0, 4))
})


test_that("prange-0010.003", {
  expect_identical(prange(as.integer(x), as.integer(y), as.integer(z), na.rm = FALSE), c(2, NA, NA, 4))
})


test_that("prange-0010.004", {
  expect_identical(prange(as.integer(x), as.integer(y), as.integer(z), na.rm = TRUE), c(2, 1, 0, 4))
})


test_that("prange-0010.005", {
  expect_identical(prange(c(TRUE, FALSE, NA), c(FALSE, TRUE, FALSE), na.rm = FALSE), c(1, 1, NA))
})


test_that("prange-0010.006", {
  expect_identical(prange(c(TRUE, FALSE, NA), c(FALSE, TRUE, FALSE), na.rm = TRUE), c(1, 1, 0))
})


test_that("prange-0010.007", {
  expect_error(prange(as.raw(z), y, na.rm = TRUE), regexp = "Argument 1 is of type raw. Only integer/logical and double types are supported. A data.frame (of the previous types) is also supported as a single input.", fixed = TRUE)
})


test_that("prange-0010.008", {
  expect_error(prange(x, y, 1:2, na.rm = FALSE), regexp = "Argument 3 is of length 2 but argument 1 is of length 4. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("prange-0010.009", {
  expect_error(prange(x, y, z, na.rm = NA), regexp = "Argument 'na.rm' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("prange-0010.010", {
  expect_identical(prange(x, na.rm = FALSE), c(0, 0, NA, 0))
})


test_that("prange-0010.011", {
  expect_identical(prange(as.integer(x), y, z, na.rm = TRUE), c(2, 1, 0, 4))
})


test_that("prange-0010.012", {
  expect_identical(prange(x, y, as.integer(z), na.rm = FALSE), c(2, NA, NA, 4))
})


test_that("prange-0010.013", {
  expect_error(prange(na.rm = FALSE), regexp = "Please supply at least 1 argument. (0 argument supplied)", fixed = TRUE)
})


test_that("prange-0010.014", {
  expect_identical(prange(x0, y0, z0), pmax(x0, y0, z0) - pmin(x0, y0, z0))
})


test_that("prange-0010.015", {
  expect_identical(prange(c(1,3,NA,5,NA), c(2,NA,4,1,NA), na.rm = TRUE), c(1, 0, 0, 4, NA))
})


test_that("prange-0010.016", {
  expect_identical(prange(NA_integer_, na.rm = TRUE), NA_real_)
})


test_that("prange-0010.017", {
  expect_identical(prange(NA_real_, na.rm = TRUE), NA_real_)
})


test_that("prange-0010.018", {
  expect_identical(prange(iris[,1:2]), pmax(iris$Sepal.Length, iris$Sepal.Width) - pmin(iris$Sepal.Length, iris$Sepal.Width))
})


test_that("prange-0010.019", {
  expect_error(prange(1:150,iris$Species, na.rm = FALSE), regexp = "Function 'prange' is not meaningful for factors.", fixed = TRUE)
})

x = c(TRUE, FALSE, NA, FALSE)
y = c(TRUE, NA, TRUE, TRUE)
z = c(TRUE, TRUE, FALSE, NA)
x0 = sample(c(TRUE, FALSE, NA),1e3,TRUE)
y0 = sample(c(TRUE, FALSE, NA),1e3,TRUE)
z0 = sample(c(TRUE, FALSE, NA),1e3,TRUE)

test_that("pall-0009.001", {
  expect_identical(pall(x, y, z, na.rm = FALSE), sapply(1:4, function(i) all(x[i],y[i],z[i],na.rm=FALSE)))
})


test_that("pall-0009.002", {
  expect_identical(pall(x, y, z, na.rm = TRUE), sapply(1:4, function(i) all(x[i],y[i],z[i],na.rm=TRUE)))
})


test_that("pall-0009.003", {
  expect_error(pall(x, y, TRUE, na.rm = FALSE), regexp = "Argument 3 is of length 1 but argument 1 is of length 4. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("pall-0009.004", {
  expect_error(pall(c(TRUE,FALSE,NA), c(TRUE,FALSE), na.rm = FALSE), regexp = "Argument 2 is of length 2 but argument 1 is of length 3. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("pall-0009.005", {
  expect_error(pall(x, y, z, na.rm = NA), regexp = "Argument 'na.rm' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("pall-0009.006", {
  expect_identical(pall(x, na.rm = FALSE), x)
})


test_that("pall-0009.007", {
  expect_error(pall(na.rm = FALSE), regexp = "Please supply at least 1 argument. (0 argument supplied)", fixed = TRUE)
})


test_that("pall-0009.008", {
  expect_error(pall(x, as.integer(z), y, na.rm = TRUE), regexp = "Argument 2 is of type integer. Only logical type is supported.", fixed = TRUE)
})


test_that("pall-0009.009", {
  expect_error(pall(as.double(z), y, na.rm = TRUE), regexp = "Argument 1 is of type double. Only logical type is supported.Data.frame (of logical vectors) is also supported as a single input.", fixed = TRUE)
})


test_that("pall-0009.010", {
  expect_identical(pall(NA, na.rm = TRUE), TRUE)
})


test_that("pall-0009.011", {
  expect_identical(pall(NA, na.rm = FALSE), NA)
})


test_that("pall-0009.012", {
  expect_identical(pall(x0, y0, z0, na.rm = FALSE), sapply(1:1e3, function(i) all(x0[i],y0[i],z0[i],na.rm=FALSE)))
})


test_that("pall-0009.013", {
  expect_identical(pall(x0, y0, z0, na.rm = TRUE), sapply(1:1e3, function(i) all(x0[i],y0[i],z0[i],na.rm=TRUE)))
})


test_that("pall-0009.014", {
  expect_identical(pall(data.frame(x,y), na.rm = FALSE), pall(x,y, na.rm = FALSE))
})


test_that("pany-0010.001", {
  expect_identical(pany(x, y, z, na.rm = FALSE), sapply(1:4, function(i) any(x[i],y[i],z[i],na.rm=FALSE)))
})


test_that("pany-0010.002", {
  expect_identical(pany(x, y, z, na.rm = TRUE), sapply(1:4, function(i) any(x[i],y[i],z[i],na.rm=TRUE)))
})


test_that("pany-0010.003", {
  expect_error(pany(x, y, TRUE, na.rm = FALSE), regexp = "Argument 3 is of length 1 but argument 1 is of length 4. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("pany-0010.004", {
  expect_error(pany(c(TRUE,FALSE,NA), c(TRUE,FALSE), na.rm = FALSE), regexp = "Argument 2 is of length 2 but argument 1 is of length 3. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("pany-0010.005", {
  expect_error(pany(x, y, z, na.rm = NA), regexp = "Argument 'na.rm' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("pany-0010.006", {
  expect_identical(pany(x, na.rm = FALSE), x)
})


test_that("pany-0010.007", {
  expect_error(pany(na.rm = FALSE), regexp = "Please supply at least 1 argument. (0 argument supplied)", fixed = TRUE)
})


test_that("pany-0010.008", {
  expect_error(pany(x, as.integer(z), y, na.rm = TRUE), regexp = "Argument 2 is of type integer. Only logical type is supported.", fixed = TRUE)
})


test_that("pany-0010.009", {
  expect_error(pany(as.double(z), y, na.rm = TRUE), regexp = "Argument 1 is of type double. Only logical type is supported.Data.frame (of logical vectors) is also supported as a single input.", fixed = TRUE)
})


test_that("pany-0010.010", {
  expect_identical(pany(NA, na.rm = TRUE), TRUE)
})


test_that("pany-0010.011", {
  expect_identical(pany(NA, na.rm = FALSE), NA)
})


test_that("pany-0010.012", {
  expect_identical(pany(x0, y0, z0, na.rm = FALSE), sapply(1:1e3, function(i) any(x0[i],y0[i],z0[i],na.rm=FALSE)))
})


test_that("pany-0010.013", {
  expect_identical(pany(x0, y0, z0, na.rm = TRUE), sapply(1:1e3, function(i) any(x0[i],y0[i],z0[i],na.rm=TRUE)))
})


test_that("pany-0010.014", {
  expect_identical(pany(data.frame(x,y), na.rm = FALSE), pany(x,y, na.rm = FALSE))
})

x = c(1, 3, NA, 5)
y = c(2, NA, 4, 1)
z = c(3, 4, 4, 1)
x0 = rnorm(100L)
y0 = rnorm(100L)
z0 = rnorm(100L)
x1 = sample(c(1,2,NA),1e2,TRUE)
y1 = sample(c(1,2,NA),1e2,TRUE)
z1 = sample(c(1,2,NA),1e2,TRUE)

test_that("pmean-0011.001", {
  expect_identical(pmean(x, y, z, na.rm = FALSE), sapply(1:4, function(i) mean(c(x[i], y[i], z[i]), na.rm = FALSE)))
})


test_that("pmean-0011.002", {
  expect_identical(pmean(x, y, z, na.rm = TRUE), sapply(1:4, function(i) mean(c(x[i], y[i], z[i]), na.rm = TRUE)))
})


test_that("pmean-0011.003", {
  expect_error(pmean(as.raw(z), y, na.rm = TRUE), regexp = "Argument 1 is of type raw. Only integer/logical and double types are supported. A data.frame (of the previous types) is also supported as a single input.", fixed = TRUE)
})


test_that("pmean-0011.004", {
  expect_error(pmean(x, y, 1:2, na.rm = FALSE), regexp = "Argument 3 is of length 2 but argument 1 is of length 4. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("pmean-0011.005", {
  expect_error(pmean(1:10, 1:5, na.rm = FALSE), regexp = "Argument 2 is of length 5 but argument 1 is of length 10. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE)
})


test_that("pmean-0011.006", {
  expect_error(pmean(x, as.raw(z), y, na.rm = TRUE), regexp = "Argument 2 is of type raw. Only integer/logical and double types are supported.", fixed = TRUE)
})


test_that("pmean-0011.007", {
  expect_error(pmean(x, y, z, na.rm = NA), regexp = "Argument 'na.rm' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("pmean-0011.008", {
  expect_identical(pmean(x, na.rm = FALSE), sapply(1:4, function(i) mean(c(x[i]), na.rm = FALSE)))
})


test_that("pmean-0011.009", {
  expect_identical(pmean(c(1,3,NA,5,NA), c(2,NA,4,1,NA), na.rm = TRUE), sapply(1:5, function(i) mean(c(c(1,3,NA,5,NA)[i], c(2,NA,4,1,NA)[i]), na.rm = TRUE)))
})


test_that("pmean-0011.010", {
  expect_error(pmean(na.rm = FALSE), regexp = "Please supply at least 1 argument. (0 argument supplied)", fixed = TRUE)
})


test_that("pmean-0011.011", {
  expect_kit_equal(pmean(x0, y0, z0), sapply(1:100, function(i) mean(c(x0[i], y0[i], z0[i]), na.rm = FALSE)))
})


test_that("pmean-0011.012", {
  expect_identical(pmean(x, y, z, rep(Inf,4L), na.rm = FALSE), sapply(1:4, function(i) mean(c(x[i], y[i], z[i],rep(Inf,4L)[i]), na.rm = FALSE)))
})


test_that("pmean-0011.013", {
  expect_identical(pmean(x, y, z, rep(Inf,4L), na.rm = TRUE), sapply(1:4, function(i) mean(c(x[i], y[i], z[i],rep(Inf,4L)[i]), na.rm = TRUE)))
})


test_that("pmean-0011.014", {
  expect_identical(pmean(as.integer(x), as.integer(y), as.integer(z), na.rm = FALSE), sapply(1:4, function(i) mean(c(as.integer(x[i]), as.integer(y[i]), as.integer(z[i])), na.rm = FALSE)))
})


test_that("pmean-0011.015", {
  expect_identical(pmean(as.integer(x), as.integer(y), as.integer(z), na.rm = TRUE), sapply(1:4, function(i) mean(c(as.integer(x[i]), as.integer(y[i]), as.integer(z[i])), na.rm = TRUE)))
})


test_that("pmean-0011.016", {
  expect_identical(pmean(as.integer(x), y, z, na.rm = TRUE), sapply(1:4, function(i) mean(c(as.integer(x[i]), y[i], z[i]), na.rm = TRUE)))
})


test_that("pmean-0011.017", {
  expect_identical(pmean(x, y, as.integer(z), na.rm = FALSE), sapply(1:4, function(i) mean(c(x[i], y[i], as.integer(z[i])), na.rm = FALSE)))
})


test_that("pmean-0011.018", {
  expect_identical(pmean(NA_integer_, na.rm = FALSE), mean(NA_integer_,na.rm = FALSE))
})


test_that("pmean-0011.019", {
  expect_identical(pmean(NA_real_, na.rm = FALSE), mean(NA_real_,na.rm = FALSE))
})


test_that("pmean-0011.020", {
  expect_kit_equal(pmean(x0, y0, z0, na.rm = TRUE), sapply(1:100, function(i) mean(c(x0[i], y0[i], z0[i]), na.rm = TRUE)))
})


test_that("pmean-0011.021", {
  expect_identical(pmean(x1, y1, z1, na.rm = FALSE), sapply(1:100, function(i) mean(c(x1[i], y1[i], z1[i]), na.rm = FALSE)))
})


test_that("pmean-0011.022", {
  expect_identical(pmean(x1, y1, z1, na.rm = TRUE), sapply(1:100, function(i) mean(c(x1[i], y1[i], z1[i]), na.rm = TRUE)))
})


test_that("pmean-0011.023", {
  expect_identical(pmean(NA_integer_, na.rm = TRUE), mean(NA_integer_,na.rm = TRUE))
})


test_that("pmean-0011.024", {
  expect_identical(pmean(NA_real_, na.rm = TRUE), mean(NA_real_,na.rm = TRUE))
})


test_that("pmean-0011.025", {
  expect_identical(pmean(data.frame(x,y,z), na.rm = TRUE), pmean(x,y,z,na.rm = TRUE))
})


test_that("pmean-0011.026", {
  expect_error(pmean(1:150,iris$Species, na.rm = FALSE), regexp = "Function 'pmean' is not meaningful for factors.", fixed = TRUE)
})


test_that("pmean-0011.027", {
  expect_identical(pmean(unclass(mtcars)), pmean(mtcars))
})

