# Tests for shareData() / getData() / clearData() — shared-memory data sharing
# tinytest version (see issue #54).
# Legacy IDs 0022.001-0022.002 preserved as info= labels.

# --- R-side validation (issue #59): no shm needed, runs even without shm ---
expect_error(shareData(mtcars, ""), pattern = "Argument 'map_name' must be a single non-empty, non-missing string.", fixed = TRUE, info = "share-0059.001 empty map_name")
expect_error(shareData(mtcars, NA_character_), pattern = "Argument 'map_name' must be a single non-empty, non-missing string.", fixed = TRUE, info = "share-0059.002 NA map_name")
expect_error(shareData(mtcars, character(0)), pattern = "Argument 'map_name' must be a single non-empty, non-missing string.", fixed = TRUE, info = "share-0059.003 length-0 map_name")
expect_error(shareData(mtcars, 123), pattern = "Argument 'map_name' must be a single non-empty, non-missing string.", fixed = TRUE, info = "share-0059.004 non-character map_name")
expect_error(shareData(mtcars, "share59", verbose = NA), pattern = "Argument 'verbose' must be TRUE or FALSE and length 1.", fixed = TRUE, info = "share-0059.005 NA verbose")
expect_error(shareData(mtcars, "share59", verbose = "yes"), pattern = "Argument 'verbose' must be TRUE or FALSE and length 1.", fixed = TRUE, info = "share-0059.006 non-logical verbose")
expect_error(getData(""), pattern = "Argument 'map_name' must be a single non-empty, non-missing string.", fixed = TRUE, info = "share-0059.007 getData empty map_name")
expect_error(getData(NA_character_), pattern = "Argument 'map_name' must be a single non-empty, non-missing string.", fixed = TRUE, info = "share-0059.008 getData NA map_name")
expect_error(getData("share59", verbose = NA), pattern = "Argument 'verbose' must be TRUE or FALSE and length 1.", fixed = TRUE, info = "share-0059.009 getData NA verbose")

x <- tryCatch(shareData(mtcars, "share1"), error = function(err) NULL)
if (is.null(x)) exit_file("shared memory unavailable - skipping shareData tests")
expect_identical(getData("share1"), mtcars, info = "share-0022.001 getData roundtrip")
expect_true(clearData(x), info = "share-0022.002 clearData")
