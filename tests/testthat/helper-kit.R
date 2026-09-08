# Helper for legacy tolerant comparisons (issue #54)
# Mirrors the old tests/test_kit.R check() value logic:
#   pass if identical() OR (atomic + all.equal() + same typeof())
# Needed for 31 cases where identical() is FALSE but all.equal() is TRUE,
# e.g. complex NA imaginary part (NA vs 0) and C-vs-R floating-point summation
# order differences in pmean(). testthat 3e expect_equal() (waldo) is strict
# about these and would fail, while the legacy suite passed.
expect_kit_equal <- function(object, expected) {
  ok <- identical(object, expected) ||
    (is.atomic(object) && is.atomic(expected) &&
      isTRUE(all.equal(object, expected, check.names = !isTRUE(expected))) &&
      typeof(object) == typeof(expected))
  testthat::expect(
    ok,
    failure_message = "kit legacy tolerant check failed (identical() FALSE and all.equal()+typeof() FALSE)"
  )
  invisible(NULL)
}
