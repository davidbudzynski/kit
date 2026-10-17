# Long-vector and ALTREP boundary tests for the XLENGTH/R_xlen_t audit (issue #61).
#
# The checks at the top are cheap: lengths are validated before any input is
# materialized or read, so nothing large is ever allocated.
#
# The >INT_MAX round trips at the bottom need tens of GB of RAM, so they only
# run when KIT_TEST_LONG_VECTOR=true (e.g. KIT_TEST_LONG_VECTOR=true R CMD check
# on a big-memory machine). Topn's case is the heaviest: order() on a long
# vector returns a double index vector of the full input length.

# --- cheap: long lengths are reported / refused without allocating ---

# psum compares lengths with xlength() before reading data: the >INT_MAX length
# must appear in full, not truncated to int.
expect_error(
  psum(1:3e9, 1:10, na.rm = FALSE),
  pattern = "Argument 2 is of length 10 but argument 1 is of length 3000000000",
  fixed = TRUE,
  info = "longv-0001.001"
)

# funique/fduplicated/uniqLen refuse long vectors before materializing the ALTREP sequence
expect_error(funique(1:3e9), pattern = "Long vector not supported yet", fixed = TRUE, info = "longv-0001.002")
expect_error(fduplicated(1:3e9), pattern = "Long vector not supported yet", fixed = TRUE, info = "longv-0001.003")
expect_error(uniqLen(1:3e9), pattern = "Long vector not supported yet", fixed = TRUE, info = "longv-0001.004")

# --- ALTREP (compact integer sequence) inputs on normal-size paths ---

expect_identical(psum(seq_len(10000L), seq_len(10000L), na.rm = TRUE), 2L * seq_len(10000L), info = "longv-0001.005")
expect_identical(funique(seq_len(10000L)), seq_len(10000L), info = "longv-0001.006")
expect_identical(uniqLen(seq_len(10000L)), 10000L, info = "longv-0001.007")
expect_identical(topn(seq_len(10000L), 5L, decreasing = FALSE, hasna = FALSE), 1:5, info = "longv-0001.008")

# a data.frame with > INT_MAX cells is refused up front. All columns share one
# small logical vector, so building it is cheap and the error fires before
# dfToMatrix allocates the matrix.
col <- rep(TRUE, 50000L)
bigdf <- structure(rep(list(col), 50000L), names = paste0("V", seq_len(50000L)),
                   class = "data.frame", row.names = c(NA_integer_, -50000L))
expect_error(funique(bigdf), pattern = "Length of 'x' is too large. (Long vector not supported yet)", fixed = TRUE, info = "longv-0001.009")
expect_error(uniqLen(bigdf), pattern = "Length of 'x' is too large. (Long vector not supported yet)", fixed = TRUE, info = "longv-0001.010")
rm(col, bigdf)

# --- opt-in: >INT_MAX round trips ---
if (!identical(Sys.getenv("KIT_TEST_LONG_VECTOR"), "true")) {
  exit_file("KIT_TEST_LONG_VECTOR != true: skipping >INT_MAX round-trip tests (needs tens of GB of RAM)")
}

n <- 2^31 + 1000000 # double, as length() returns for long vectors

# psum: integer path on a long vector (x is shared between the two arguments)
x <- rep(1L, n)
r <- psum(x, x, na.rm = TRUE)
expect_identical(length(r), n, info = "longv-0001.101")
expect_identical(r[c(1, n)], c(2L, 2L), info = "longv-0001.102")
rm(x, r)
invisible(gc())

# funique/uniqLen: long factor keeps distinct factor values in both directions
f <- structure(rep(1L, n), levels = c("a", "b"), class = "factor")
expect_identical(funique(f), f[1L], info = "longv-0001.103")
expect_identical(funique(f, fromLast = TRUE), f[1L], info = "longv-0001.104")
expect_identical(uniqLen(f), 1L, info = "longv-0001.105")
rm(f)
invisible(gc())

# funique/uniqLen: long logical
lg <- rep(TRUE, n)
expect_identical(funique(lg), TRUE, info = "longv-0001.106")
expect_identical(uniqLen(lg), 1L, info = "longv-0001.107")
rm(lg)
invisible(gc())

# psort: long character vectors are refused before being read
s <- rep("a", n)
expect_error(psort(s), pattern = "Long vector not supported yet", fixed = TRUE, info = "longv-0001.108")
rm(s)
invisible(gc())

# topn: long input takes the order() path and returns double indices like base::order()
v <- seq_len(n)
r <- topn(v, 5L, decreasing = FALSE, hasna = FALSE)
expect_identical(typeof(r), "double", info = "longv-0001.109")
expect_identical(as.integer(r), 1:5, info = "longv-0001.110")
rm(v, r)
invisible(gc())
