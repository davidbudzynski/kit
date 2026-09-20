# Helper for legacy tolerant comparisons (issue #54)
# Mirrors the old tests/test_kit.R check() value logic:
#   pass if identical() OR (atomic + all.equal() + same typeof())
# Needed for 31 cases where identical() is FALSE but all.equal() is TRUE,
# e.g. complex NA imaginary part (NA vs 0) and C-vs-R floating-point summation
# order differences in pmean().
# NOTE: this file is loaded via sys.source("helper-kit.R", envir = environment())
# from each test file, so bare expect_true() below resolves to the runner's
# recording version. Do NOT use tinytest::expect_true() here and do NOT load
# this file with plain source() (that evaluates in globalenv()).
expect_kit_equal <- function(current, target, info = NA_character_) {
  ok <- identical(current, target) ||
    (is.atomic(current) && is.atomic(target) &&
      isTRUE(all.equal(current, target, check.names = !isTRUE(target))) &&
      typeof(current) == typeof(target))
  expect_true(ok, info = info)
}
