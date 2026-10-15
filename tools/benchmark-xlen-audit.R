args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 2L) stop("Usage: Rscript tools/benchmark-xlen-audit.R [library] [label]")
if (length(args) >= 1L && nzchar(args[[1]])) .libPaths(c(args[[1]], .libPaths()))
label <- if (length(args) >= 2L) args[[2]] else "current"
library(kit)
options(kit.nThread = 1L)

bench <- function(f, reps = 15L, calls = 3L) {
  invisible(f())
  times <- numeric(reps)
  for (i in seq_len(reps)) {
    invisible(gc())
    start <- proc.time()[["elapsed"]]
    for (j in seq_len(calls)) f()
    times[[i]] <- (proc.time()[["elapsed"]] - start) / calls
  }
  unname(times)
}

summarize <- function(name, f, reps = 15L, calls = 3L) {
  times <- bench(f, reps = reps, calls = calls)
  quartiles <- unname(quantile(times, probs = c(0.25, 0.5, 0.75), names = FALSE))
  cat(sprintf("%s\t%s\tmedian\t%.6f\tq1\t%.6f\tq3\t%.6f\tmin\t%.6f\n", label, name, quartiles[[2]], quartiles[[1]], quartiles[[3]], min(times)))
}

# Hot paths touched by the long-vector audit (#61): funique factor/logical
# counters, topn small-n entry, psort character entry guard.
set.seed(61L)
n <- 10000000L
xi <- sample.int(1000000L, n, TRUE)
fs <- factor(sample.int(100L, n, TRUE))
lg <- sample(c(TRUE, FALSE, NA), n, TRUE)
sc <- sprintf("k%07d", sample.int(n, n))
stopifnot(length(funique(fs)) == length(unique(fs)))

cat(sprintf("# R\t%s\tplatform\t%s\tpackage\t%s\tlabel\t%s\tn\t%d\n", R.version.string, R.version$platform, as.character(packageVersion("kit")), label, n))
summarize("funique_factor", function() funique(fs))
summarize("funique_logical", function() funique(lg))
summarize("funique_int_hash", function() funique(xi))
summarize("uniqLen_factor", function() uniqLen(fs))
summarize("topn_int_small_dcr", function() topn(xi, 5L, TRUE, FALSE))
summarize("topn_int_small_asc", function() topn(xi, 5L, FALSE, FALSE))
summarize("topn_dbl_small_dcr", function() topn(as.numeric(xi), 5L, TRUE, FALSE))
summarize("psort_char", function() psort(sc), reps = 7L, calls = 2L)
