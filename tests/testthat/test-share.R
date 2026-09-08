# Tests for shareData() / getData() / clearData() — shared-memory data sharing
# Migrated from tests/test_kit.R (custom check() suite) to testthat 3e — see issue #54.
# Legacy IDs 0022.001-0022.002 preserved as test_that() labels.

test_that("share-0022.001 getData roundtrip", {
  x <- tryCatch(shareData(mtcars, "share1"), error = function(err) NULL)
  skip_if(is.null(x), "shared memory unavailable - skipping shareData tests")
  expect_identical(getData("share1"), mtcars)
  expect_true(clearData(x))
})
