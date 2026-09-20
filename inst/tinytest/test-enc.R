# Mixed-encoding (checkEnc), attributes, Date/factor preservation and
# empty inputs — tinytest (see issue #63).
# Complements test-vswitch-nswitch.R (enc1/enc2 basics) and test-unique.R
# (label attrs): locks checkEnc TRUE vs FALSE, data.table/tibble attrs,
# Date/factor classes and safe length-0 paths.
# Tests-only: no C changes.
# NOTE: vswitch()/nswitch() on length-0 *character* input currently segfaults
# (see follow-up to be filed); those inputs are deliberately NOT exercised here.
# Only safe empties (integer-0 switch, funique/psort/charToFact empties) are tested.

sys.source("helper-kit.R", envir = environment())
set.seed(123)

enc1 <- "fa\xE7ile"
Encoding(enc1) <- "latin1"
enc2 <- enc2utf8(enc1)

# --- checkEnc: mixed latin1/UTF-8 matches with TRUE, misses with FALSE ---
expect_identical(Encoding(enc1), "latin1", info="enc-001")
expect_identical(Encoding(enc2), "UTF-8", info="enc-002")
expect_identical(vswitch(c(enc1, enc2), enc1, 1), c(1, 1), info="enc-003")
expect_identical(vswitch(c(enc1, enc2), enc2, 1), c(1, 1), info="enc-004")
expect_identical(vswitch(c(enc1, enc2), enc1, 1, checkEnc = TRUE), c(1, 1), info="enc-005")
expect_identical(vswitch(c(enc1, enc2), enc1, 1, checkEnc = FALSE), c(1, NA), info="enc-006")
expect_identical(vswitch(c(enc1, enc1), enc1, 1, checkEnc = FALSE), c(1, 1), info="enc-007")
expect_identical(nswitch(c(enc1, enc2), enc1, 1, default = 0), c(1, 1), info="enc-008")
expect_identical(nswitch(c(enc1, enc2), enc2, 1, default = 0), c(1, 1), info="enc-009")
expect_identical(nswitch(c(enc1, enc2), enc1, 1, default = 0, checkEnc = TRUE), c(1, 1), info="enc-010")

# --- checkEnc argument validation ---
expect_error(vswitch("a", "b", 1, checkEnc = NA), pattern = "Argument 'checkEnc' must be TRUE or FALSE and length 1.", fixed = TRUE, info="enc-011")
expect_error(vswitch("a", "b", 1, checkEnc = 2), pattern = "Argument 'checkEnc' must be TRUE or FALSE and length 1.", fixed = TRUE, info="enc-012")
expect_error(nswitch(1L, 1L, "a", checkEnc = 2), pattern = "Argument 'checkEnc' must be TRUE or FALSE and length 1.", fixed = TRUE, info="enc-013")
expect_error(nswitch(1L, 1L, "a", checkEnc = NA), pattern = "Argument 'checkEnc' must be TRUE or FALSE and length 1.", fixed = TRUE, info="enc-014")

# --- funique preserves Date class ---
d <- as.Date(c("2020-01-01", "2020-01-01", "2020-01-02"))
expect_identical(funique(d), as.Date(c("2020-01-01", "2020-01-02")), info="enc-015")
expect_identical(class(funique(d)), "Date", info="enc-016")
expect_identical(
  funique(data.frame(a = c(as.Date("2020-05-01"), as.Date("2020-05-01")))),
  data.frame(a = as.Date("2020-05-01")),
  info="enc-017"
)

# --- funique preserves factor levels ---
f <- factor(c("a", "b", "a"))
expect_identical(funique(f), factor(c("a", "b")), info="enc-018")
expect_identical(levels(funique(f)), c("a", "b"), info="enc-019")
expect_identical(funique(f, fromLast = TRUE), factor(c("b", "a")), info="enc-020")

# --- funique preserves data.table / tibble attributes (needs Suggests) ---
if (requireNamespace("data.table", quietly = TRUE)) {
  dt <- data.table::data.table(a = c(1, 1, 2), b = c("x", "x", "y"))
  fu_dt <- funique(dt)
  expect_true("data.table" %in% class(fu_dt), info="enc-021")
  expect_identical(nrow(fu_dt), 2L, info="enc-022")
  expect_identical(fu_dt$a, c(1, 2), info="enc-023")
} else {
  expect_true(TRUE, info="enc-021-skipped-no-data.table")
}
if (requireNamespace("tibble", quietly = TRUE)) {
  tb <- tibble::tibble(a = c(1, 1, 2), b = c("x", "x", "y"))
  fu_tb <- funique(tb)
  expect_true("tbl_df" %in% class(fu_tb), info="enc-024")
  expect_identical(nrow(fu_tb), 2L, info="enc-025")
  expect_identical(fu_tb$a, c(1, 2), info="enc-026")
} else {
  expect_true(TRUE, info="enc-024-skipped-no-tibble")
}

# --- funique rejects lists with a clean error ---
expect_error(funique(list()), pattern = "Type list is not supported.", fixed = TRUE, info="enc-027")

# --- Safe empty / length-0 inputs (character-0 switch omitted: segfault, see NOTE) ---
expect_identical(funique(character(0)), character(0), info="enc-028")
expect_identical(funique(integer(0)), integer(0), info="enc-029")
expect_identical(funique(as.Date(character(0))), as.Date(character(0)), info="enc-030")
expect_identical(class(funique(as.Date(character(0)))), "Date", info="enc-031")
expect_identical(funique(factor(character(0))), factor(character(0)), info="enc-032")
expect_identical(nrow(funique(data.frame(a = numeric(0), b = character(0)))), 0L, info="enc-033")
expect_identical(vswitch(integer(0), 1L, 1L), integer(0), info="enc-034")
expect_identical(iif(logical(0), 1L, 0L), integer(0), info="enc-035")
expect_identical(psort(character(0)), character(0), info="enc-036")
expect_identical(charToFact(character(0)), factor(character(0)), info="enc-037")
