# Tests for shareData() / getData() / clearData() — shared-memory data sharing
# tinytest version (see issue #54).
# Legacy IDs 0022.001-0022.002 preserved as info= labels.

x <- tryCatch(shareData(mtcars, "share1"), error = function(err) NULL)
if (is.null(x)) exit_file("shared memory unavailable - skipping shareData tests")
expect_identical(getData("share1"), mtcars, info = "share-0022.001 getData roundtrip")
expect_true(clearData(x), info = "share-0022.002 clearData")
