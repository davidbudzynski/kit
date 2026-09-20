# Tests for setlevels() — fast factor level recoding — tinytest (see issue #54).
# Migrated from testthat 3e to tinytest to keep kit lean (zero test dependencies).
# Each legacy check("id", actual, expected) maps to expect_identical(..., info="id");
# 31 tolerant cases (identical FALSE but all.equal+typeof TRUE) use expect_kit_equal(..., info="id");
# check(..., error=) maps to expect_error(pattern=, fixed=TRUE, info="id").
# Legacy IDs are preserved as info= labels (prefixed by function for uniqueness).
# Setup and expectations stay interleaved in legacy order: later sections may
# redefine x/y/out_vec/... so setup must not be hoisted above earlier tests.

sys.source("helper-kit.R", envir = environment())
set.seed(123)


  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "B", "C"), c("X", "Y", "Z")), factor(c("X", "X", "Y", "Y", "Y", "Z")), info="setlevels-0007.01")


  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "B"), c("X", "Y", "Z")), pattern = "'old' and 'new' are not the same length.", fixed = TRUE, info="setlevels-0007.02")


  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "B", "B"), c("X", "Y", "Z")), pattern = "'old' has duplicated value. Please make sure no duplicated values are introduced.", fixed = TRUE, info="setlevels-0007.03")


  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "B", "C"), c("X", "X", "Z")), pattern = "'new' has duplicated value. Please make sure no duplicated values are introduced.", fixed = TRUE, info="setlevels-0007.04")


  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A"), c("X")), factor(c("X", "X", "B", "B", "B", "C"), levels = c("X", "B", "C")), info="setlevels-0007.05")


  expect_identical(setlevels(factor(c(1, 1, 2, 2, 2, 3)), c("1","2","3"), c("X","Y","Z")), factor(c("X", "X", "Y", "Y", "Y", "Z")), info="setlevels-0007.06")


  expect_error(setlevels(factor(c(1, 1, 2, 2, 2, 3)), 1:3, c("X","Y","Z")), pattern = "Type of 'old' must be character.", fixed = TRUE, info="setlevels-0007.07")


  expect_error(setlevels(factor(c(1, 1, 2, 2, 2, 3)), c("1","2","3"), 1:3), pattern = "Type of 'new' must be character.", fixed = TRUE, info="setlevels-0007.08")


  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C")), new = c("X", "Y", "Z")), factor(c("X", "X", "Y", "Y", "Y", "Z")), info="setlevels-0007.09")


  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "B", "D"), c("X", "Y", "Z")), pattern = "Element 'D' of 'old' does not exist in 'x'.", fixed = TRUE, info="setlevels-0007.10")


  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C"))), pattern = "argument \"new\" is missing, with no default", fixed = TRUE, info="setlevels-0007.11")


  expect_error(setlevels(c("A", "A", "B", "B", "B", "C"), c("A", "B", "C"), c("X", "Y", "Z")), pattern = "'setlevels' must be passed a factor.", fixed = TRUE, info="setlevels-0007.12")


  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "A","B", "B"), c("X", "X","Y", "Z")), pattern = "'old' has duplicated value. Please make sure no duplicated values are introduced.", fixed = TRUE, info="setlevels-0007.13")


  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C"), levels = c("C","A","B")), c("C", "A", "B"), c("Z", "X", "Y")), factor(c("X", "X", "Y", "Y", "Y", "Z"), levels = c("Z", "X", "Y")), info="setlevels-0007.14")


  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C"), levels = c("C","A","B","D")), c("A", "B", "C"), c("X", "Y", "Z")), factor(c("X", "X", "Y", "Y", "Y", "Z"), levels = c("Z", "X", "Y","D")), info="setlevels-0007.15")


  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B","D")), c("A", "B", "C"), c("X", "Y", "Z")), factor(c("X", "X", "Y", "Y", "Y", "Z",NA,NA), levels = c("Z", "X", "Y","D")), info="setlevels-0007.16")


  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B",NA), exclude = NULL), c("A", "B", "C"), c("X", "Y", "Z")), factor(c("X", "X", "Y", "Y", "Y", "Z",NA,NA), levels = c("Z", "X", "Y",NA), exclude=NULL), info="setlevels-0007.17")


  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C"), levels = c("C","A","B",NA), exclude = NULL), c("A", "B", "C"), c("X", "Y", "Z")), factor(c("X", "X", "Y", "Y", "Y", "Z"), levels = c("Z", "X", "Y",NA), exclude=NULL), info="setlevels-0007.18")


  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B",NA), exclude = NULL), c("A", NA), c("X","D")), factor(c("X", "X", "B", "B", "B", "C","D","D"), levels = c("C", "X", "B","D")), info="setlevels-0007.19")


  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B",NA), exclude = NULL), as.character(NA), "D"), factor(c("A", "A", "B", "B", "B", "C","D","D"), levels = c("C", "A", "B","D")), info="setlevels-0007.20")


  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B",NA), exclude = NULL), NA_character_, "D"), factor(c("A", "A", "B", "B", "B", "C","D","D"), levels = c("C", "A", "B","D")), info="setlevels-0007.21")


  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B",NA), exclude = NULL), NA, "D"), pattern = "Type of 'old' must be character.", fixed = TRUE, info="setlevels-0007.22")


  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C", NA, NA), levels = c("C","A","B",NA), exclude = NULL), "A", NA), pattern = "Type of 'new' must be character.", fixed = TRUE, info="setlevels-0007.23")


  expect_error(setlevels(factor(c("A", "A", "B", "B", "B", "C"), levels = c("A","B","C")), c("A", "B", "C"), c("X", "Y", "Z"), c(FALSE,TRUE)), pattern = "Argument 'skip_absent' must be TRUE or FALSE and length 1.", fixed = TRUE, info="setlevels-0007.24")


  expect_identical(setlevels(factor(c("A", "A", "B", "B", "B", "C")), c("A", "B", "D"), c("X", "Y", "Z"), TRUE), factor(c("X", "X", "Y", "Y", "Y", "C"), levels = c("X","Y","C")), info="setlevels-0007.25")

