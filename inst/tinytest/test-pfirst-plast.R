# Tests for pfirst()/plast() — first/last non-missing values — tinytest (see issue #54).
# Migrated from testthat 3e to tinytest to keep kit lean (zero test dependencies).
# Each legacy check("id", actual, expected) maps to expect_identical(..., info="id");
# 31 tolerant cases (identical FALSE but all.equal+typeof TRUE) use expect_kit_equal(..., info="id");
# check(..., error=) maps to expect_error(pattern=, fixed=TRUE, info="id").
# Legacy IDs are preserved as info= labels (prefixed by function for uniqueness).
# Setup and expectations stay interleaved in legacy order: later sections may
# redefine x/y/out_vec/... so setup must not be hoisted above earlier tests.

sys.source("helper-kit.R", envir = environment())
set.seed(123)

x = c(1, 3, NA, 5)
y = c(2, NA, 4, 1)
z = c(3, 4, 4, NA)
x1 = sample(c("a","b",NA),1e2,TRUE)
y1 = sample(c("c","d",NA),1e2,TRUE)
z1 = sample(c("e","f",NA),1e2,TRUE)
base_pfirst <- function(...) {
  x = if(...length() == 1L && is.list(..1)) unclass(..1) else list(...)
  res = x[[1]]
  if(length(x) == 1L) return(res)
  for(i in 2:length(x)) {
    miss <- is.na(res)
    res[miss] <- x[[i]][miss]
  }
  res
}
base_plast <- function(...) {
  x = if(...length() == 1L && is.list(..1)) unclass(..1) else list(...)
  n = length(x)
  res = x[[n]]
  if(n == 1L) return(res)
  for(i in (n-1):1) {
    miss <- is.na(res)
    res[miss] <- x[[i]][miss]
  }
  res
}

  expect_identical(pfirst(x, y, z), base_pfirst(x, y, z), info="pfirst-0028.001")


  expect_identical(plast(x, y, z), base_plast(x, y, z), info="plast-0028.002")


  expect_identical(pfirst(y, z, x), base_pfirst(y, z, x), info="pfirst-0028.003")


  expect_identical(plast(y, z, x), base_plast(y, z, x), info="plast-0028.004")


  expect_identical(pfirst(x1, y1, z1), base_pfirst(x1, y1, z1), info="pfirst-0028.005")


  expect_identical(plast(x1, y1, z1), base_plast(x1, y1, z1), info="plast-0028.006")


  expect_identical(pfirst(y1, z1, x1), base_pfirst(y1, z1, x1), info="pfirst-0028.007")


  expect_identical(plast(y1, z1, x1), base_plast(y1, z1, x1), info="plast-0028.008")


  expect_identical(pfirst(list(x1, y1, z1)), base_pfirst(list(x1, y1, z1)), info="pfirst-0028.009")


  expect_identical(plast(list(x1, y1, z1)), base_plast(list(x1, y1, z1)), info="plast-0028.010")


  expect_identical(pfirst(data.frame(y1, z1, x1)), base_pfirst(data.frame(y1, z1, x1)), info="pfirst-0028.011")


  expect_identical(plast(data.frame(y1, z1, x1)), base_plast(data.frame(y1, z1, x1)), info="plast-0028.012")


  expect_identical(pfirst(list(1, NULL), list(NULL, 2)), list(1, 2), info="pfirst-0028.013")


  expect_identical(plast(list(1, NULL), list(NULL, 2)), list(1, 2), info="plast-0028.014")


  expect_error(pfirst(as.character(z), y), pattern = "All arguments need to have the same data type, except for numeric and logical types", fixed = TRUE, info="pfirst-0028.015")


  expect_error(pfirst(x, y, 1:2), pattern = "Argument 3 is of length 2 but argument 1 is of length 4. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE, info="pfirst-0028.016")


  expect_error(pfirst(1:10, 1:5), pattern = "Argument 2 is of length 5 but argument 1 is of length 10. If you wish to 'recycle' your argument, please use rep() to make this intent clear to the readers of your code.", fixed = TRUE, info="pfirst-0028.017")


  expect_error(pfirst(x, as.list(z), y), pattern = "All arguments need to have the same data type, except for numeric and logical types", fixed = TRUE, info="pfirst-0028.018")


  expect_identical(typeof(pfirst(1:4, x)), "double", info="typeof-0028.019")


  expect_identical(typeof(pfirst(x, 1:4)), "double", info="typeof-0028.020")


  expect_error(pfirst(as.factor(x), x), pattern = "If one argument is a factor, all arguments need to be factors", fixed = TRUE, info="pfirst-0028.021")


  expect_error(pfirst(x, as.factor(x)), pattern = "If one argument is a factor, all arguments need to be factors", fixed = TRUE, info="pfirst-0028.022")


  expect_error(pfirst(as.factor(x), as.factor(y)), pattern = "All factors need to have identical levels", fixed = TRUE, info="pfirst-0028.023")


  expect_identical(class(pfirst(as.factor(x1), as.factor(x1))), "factor", info="class-0028.024")
