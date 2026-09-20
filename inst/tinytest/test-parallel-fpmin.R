# Tests for fpmin()/fpmax()/prange() gaps — NA/NaN, promotion, prange double-only,
# single-arg list/data.frame path — tinytest (see issue #63).
# Complements test-parallel.R (fpmin-0008/fpmax-0009/prange-0010) which covers
# basic na.rm/integer/logical cases. IDs here use the -gap- prefix for uniqueness.
# Tests-only: locks current C behaviour in src/psum.c fpminR/fpmaxR/prangeR.

sys.source("helper-kit.R", envir = environment())
set.seed(123)

# --- NA / NaN are both missing (ISNAN at C level), na.rm skips them ---
x <- c(1, NA, NaN, 2)
y <- c(2, 1, 1, NA)

expect_identical(fpmin(x, y, na.rm = FALSE), c(1, NA, NA, NA), info="fpmin-gap-001")
expect_identical(fpmin(x, y, na.rm = TRUE), c(1, 1, 1, 2), info="fpmin-gap-002")
expect_identical(fpmax(x, y, na.rm = FALSE), c(2, NA, NA, NA), info="fpmax-gap-001")
expect_identical(fpmax(x, y, na.rm = TRUE), c(2, 1, 1, 2), info="fpmax-gap-002")
expect_identical(prange(x, y, na.rm = FALSE), c(1, NA, NA, NA), info="prange-gap-001")
expect_identical(prange(x, y, na.rm = TRUE), c(1, 0, 0, 0), info="prange-gap-002")

# NaN and NA_real_ are handled identically
expect_identical(
  fpmin(c(NaN, 1), c(1, NaN), na.rm = TRUE),
  fpmin(c(NA_real_, 1), c(1, NA_real_), na.rm = TRUE),
  info="fpmin-gap-003"
)
expect_identical(
  fpmax(c(NaN, 1), c(1, NaN), na.rm = FALSE),
  fpmax(c(NA_real_, 1), c(1, NA_real_), na.rm = FALSE),
  info="fpmax-gap-003"
)
expect_identical(
  prange(c(NaN, 1), c(1, NaN), na.rm = TRUE),
  prange(c(NA_real_, 1), c(1, NA_real_), na.rm = TRUE),
  info="prange-gap-003"
)

# All-NA rows stay NA even with na.rm = TRUE (nothing to reduce)
expect_identical(fpmin(c(NA, NA), c(NA, NA), na.rm = TRUE), c(NA, NA), info="fpmin-gap-004")
expect_identical(fpmax(c(NA, NA), c(NA, NA), na.rm = TRUE), c(NA, NA), info="fpmax-gap-004")
expect_identical(prange(c(NA, NA), c(NA, NA), na.rm = TRUE), c(NA_real_, NA_real_), info="prange-gap-004")
expect_identical(fpmin(NA_integer_, na.rm = TRUE), NA_integer_, info="fpmin-gap-005")
expect_identical(fpmax(NA_integer_, na.rm = TRUE), NA_integer_, info="fpmax-gap-005")
expect_identical(prange(NA_integer_, na.rm = TRUE), NA_real_, info="prange-gap-005")

# --- Inf / -Inf propagate like base pmin/pmax ---
xi <- c(1, Inf, -Inf, NA)
yi <- c(2, 1, 1, 1)
zi <- c(0, 5, 5, 5)

expect_identical(fpmin(xi, yi, zi, na.rm = FALSE), c(0, 1, -Inf, NA), info="fpmin-gap-006")
expect_identical(fpmin(xi, yi, zi, na.rm = TRUE), c(0, 1, -Inf, 1), info="fpmin-gap-007")
expect_identical(fpmax(xi, yi, zi, na.rm = FALSE), c(2, Inf, 5, NA), info="fpmax-gap-006")
expect_identical(fpmax(xi, yi, zi, na.rm = TRUE), c(2, Inf, 5, 5), info="fpmax-gap-007")
expect_identical(prange(xi, yi, zi, na.rm = FALSE), c(2, Inf, Inf, NA), info="prange-gap-006")
expect_identical(prange(xi, yi, zi, na.rm = TRUE), c(2, Inf, Inf, 4), info="prange-gap-007")

# --- Type promotion: logical < integer < double; prange always double ---
expect_identical(typeof(fpmin(c(TRUE, FALSE), 1L:2L)), "integer", info="fpmin-gap-008")
expect_identical(fpmin(c(TRUE, FALSE), 1L:2L), c(1L, 0L), info="fpmin-gap-009")
expect_identical(typeof(fpmin(1L:2L, c(1.5, 2.5))), "double", info="fpmin-gap-010")
expect_identical(fpmin(1L:2L, c(1.5, 2.5)), c(1, 2), info="fpmin-gap-011")
expect_identical(typeof(fpmin(c(TRUE, FALSE), c(1.5, 2.5))), "double", info="fpmin-gap-012")
expect_identical(typeof(fpmax(c(TRUE, FALSE), 1L:2L)), "integer", info="fpmax-gap-008")
expect_identical(typeof(fpmax(1L:2L, c(1.5, 2.5))), "double", info="fpmax-gap-009")
# All-logical inputs stay logical
expect_identical(typeof(fpmin(c(TRUE, FALSE), c(FALSE, TRUE))), "logical", info="fpmin-gap-013")
expect_identical(fpmin(c(TRUE, FALSE), c(FALSE, TRUE)), c(FALSE, FALSE), info="fpmin-gap-014")
expect_identical(typeof(fpmax(c(TRUE, FALSE), c(FALSE, TRUE))), "logical", info="fpmax-gap-010")
expect_identical(fpmax(c(TRUE, FALSE), c(FALSE, TRUE)), c(TRUE, TRUE), info="fpmax-gap-011")
# prange double-only rule, even for integer/logical inputs
expect_identical(typeof(prange(1L:3L, 3L:1L)), "double", info="prange-gap-008")
expect_identical(prange(1L:3L, 3L:1L), c(2, 0, 2), info="prange-gap-009")
expect_identical(typeof(prange(c(TRUE, FALSE), c(FALSE, TRUE))), "double", info="prange-gap-010")
expect_identical(prange(c(TRUE, FALSE), c(FALSE, TRUE)), c(1, 1), info="prange-gap-011")
# NA_integer_ promoted against double
expect_identical(fpmin(c(NA_integer_, 1L), c(1.5, 2.5), na.rm = TRUE), c(1.5, 1), info="fpmin-gap-015")
expect_identical(typeof(fpmin(c(NA_integer_, 1L), c(1.5, 2.5), na.rm = TRUE)), "double", info="fpmin-gap-016")

# --- Single list / data.frame path (R/call.R ...length() == 1 && is.list(..1)) ---
a <- c(1, 3, NA, 5)
b <- c(2, NA, 4, 1)
cc <- c(3, 4, 4, 1)
df <- data.frame(a, b, cc)

expect_identical(fpmin(df), fpmin(a, b, cc), info="fpmin-gap-017")
expect_identical(fpmax(df), fpmax(a, b, cc), info="fpmax-gap-012")
expect_identical(prange(df), prange(a, b, cc), info="prange-gap-012")
expect_identical(fpmin(list(a, b, cc)), fpmin(a, b, cc), info="fpmin-gap-018")
expect_identical(fpmax(list(a, b, cc)), fpmax(a, b, cc), info="fpmax-gap-013")
expect_identical(prange(list(a, b, cc)), prange(a, b, cc), info="prange-gap-013")
# Mixed logical < integer < double data.frame promotes to double
dfm <- data.frame(l = c(TRUE, FALSE, NA), i = 1L:3L, d = c(1.5, 2.5, 3.5))
expect_identical(typeof(fpmin(dfm)), "double", info="fpmin-gap-019")
expect_identical(fpmin(dfm), c(1, 0, NA), info="fpmin-gap-020")
expect_identical(fpmin(dfm), fpmin(dfm$l, dfm$i, dfm$d), info="fpmin-gap-021")
expect_identical(typeof(prange(dfm)), "double", info="prange-gap-014")
# data.frame equivalent to unclassed list
expect_identical(fpmin(unclass(mtcars)), fpmin(mtcars), info="fpmin-gap-022")
expect_identical(fpmax(unclass(mtcars)), fpmax(mtcars), info="fpmax-gap-014")
expect_identical(prange(unclass(mtcars)), prange(mtcars), info="prange-gap-015")

# --- Single-arg identity and length-0 inputs ---
expect_identical(fpmin(a, na.rm = FALSE), a, info="fpmin-gap-023")
expect_identical(fpmax(a, na.rm = FALSE), a, info="fpmax-gap-015")
expect_identical(prange(a, na.rm = FALSE), c(0, 0, NA, 0), info="prange-gap-016")
expect_identical(prange(a, na.rm = TRUE), c(0, 0, NA, 0), info="prange-gap-017")
expect_identical(fpmin(numeric(0), numeric(0)), numeric(0), info="fpmin-gap-024")
expect_identical(typeof(fpmin(numeric(0), numeric(0))), "double", info="fpmin-gap-025")
expect_identical(fpmin(integer(0), integer(0)), integer(0), info="fpmin-gap-026")
expect_identical(prange(numeric(0), numeric(0)), numeric(0), info="prange-gap-018")
expect_identical(typeof(prange(integer(0), integer(0))), "double", info="prange-gap-019")

# --- Fuzz against base pmin/pmax ---
set.seed(42)
x0 <- rnorm(1000L)
y0 <- rnorm(1000L)
z0 <- rnorm(1000L)

expect_identical(fpmin(x0, y0, z0), pmin(x0, y0, z0), info="fpmin-gap-027")
expect_identical(fpmax(x0, y0, z0), pmax(x0, y0, z0), info="fpmax-gap-016")
expect_identical(prange(x0, y0, z0), pmax(x0, y0, z0) - pmin(x0, y0, z0), info="prange-gap-020")

# --- Double answer with integer/logical later args (C REALSXP branches) ---
# Covers src/psum.c fpminR/fpmaxR integer-arg loops for both na.rm settings,
# including the all-NA first-arg (found[j] == 0) path.
expect_identical(fpmin(c(NA_real_, 2), c(1L, 0L), na.rm = TRUE), c(1, 0), info="fpmin-gap-032")
expect_identical(typeof(fpmin(c(NA_real_, 2), c(1L, 0L), na.rm = TRUE)), "double", info="fpmin-gap-033")
expect_identical(fpmax(c(NA_real_, 2), c(1L, 5L), na.rm = TRUE), c(1, 5), info="fpmax-gap-020")
expect_identical(fpmin(c(5, 5), c(1L, 2L), na.rm = FALSE), c(1, 2), info="fpmin-gap-034")
expect_identical(fpmax(c(1, 1), c(3L, 0L), na.rm = FALSE), c(3, 1), info="fpmax-gap-021")

# --- Errors: types, lengths, na.rm, factors (complex/raw unsupported here) ---
expect_error(fpmin(as.raw(1:3), 1:3), pattern = "Only integer/logical and double types are supported", fixed = TRUE, info="fpmin-gap-028")
expect_error(fpmax(1 + 0i, 2 + 0i), pattern = "Only integer/logical and double types are supported", fixed = TRUE, info="fpmax-gap-017")
expect_error(prange(1 + 0i, 2 + 0i), pattern = "Only integer/logical and double types are supported", fixed = TRUE, info="prange-gap-021")
expect_error(fpmin(1:3, 1:2), pattern = "If you wish to 'recycle' your argument", fixed = TRUE, info="fpmin-gap-029")
expect_error(fpmax(1:3, 1:2), pattern = "If you wish to 'recycle' your argument", fixed = TRUE, info="fpmax-gap-018")
expect_error(prange(1:3, 1:2), pattern = "If you wish to 'recycle' your argument", fixed = TRUE, info="prange-gap-022")
expect_error(fpmin(1:3, na.rm = NA), pattern = "Argument 'na.rm' must be TRUE or FALSE and length 1.", fixed = TRUE, info="fpmin-gap-030")
expect_error(prange(na.rm = FALSE), pattern = "Please supply at least 1 argument. (0 argument supplied)", fixed = TRUE, info="prange-gap-023")
expect_error(fpmin(factor(c("a", "b"))), pattern = "not meaningful for factors", fixed = TRUE, info="fpmin-gap-031")
expect_error(fpmax(1:3, factor(c("a", "b", "c"))), pattern = "not meaningful for factors", fixed = TRUE, info="fpmax-gap-019")
expect_error(prange(data.frame(x = 1:3, f = factor(c("a", "b", "c")))), pattern = "not meaningful for factors", fixed = TRUE, info="prange-gap-024")

# Non-first arguments must also have supported types (C arg i+1 error path)
expect_error(fpmin(c(1, 2, 3, 4), c(2, 3, 4, 5), as.raw(c(3L, 4L, 4L, 1L))), pattern = "Argument 3 is of type raw. Only integer/logical and double types are supported.", fixed = TRUE, info="fpmin-gap-035")
expect_error(fpmax(c(1, 2, 3, 4), as.raw(c(3L, 4L, 4L, 1L))), pattern = "Argument 2 is of type raw. Only integer/logical and double types are supported.", fixed = TRUE, info="fpmax-gap-022")
expect_error(prange(c(1, 2, 3, 4), as.raw(c(3L, 4L, 4L, 1L))), pattern = "Argument 2 is of type raw. Only integer/logical and double types are supported.", fixed = TRUE, info="prange-gap-025")
