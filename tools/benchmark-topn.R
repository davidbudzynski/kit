args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 3L) stop("Usage: Rscript tools/benchmark-topn.R [library] [label] [n]")
if (length(args) >= 1L && nzchar(args[[1]])) .libPaths(c(args[[1]], .libPaths()))
label <- if (length(args) >= 2L) args[[2]] else "current"
n <- if (length(args) >= 3L) as.numeric(args[[3]]) else 20000000L
library(kit)
options(kit.nThread = 1L)

bench <- function(f, reps = 7L) {
  invisible(f())
  times <- numeric(reps)
  for (i in seq_len(reps)) {
    invisible(gc())
    start <- proc.time()[["elapsed"]]
    f()
    times[[i]] <- proc.time()[["elapsed"]] - start
  }
  unname(times)
}

summarize <- function(name, f, reps = 7L) {
  times <- bench(f, reps)
  q <- unname(quantile(times, probs = c(0.25, 0.5, 0.75), names = FALSE))
  cat(sprintf("%s\t%s\tmedian\t%.6f\tq1\t%.6f\tq3\t%.6f\tmin\t%.6f\n",
              label, name, q[[2]], q[[1]], q[[3]], min(times)))
}

asc <- seq_len(n)
desc <- rev(asc)
ascd <- as.numeric(asc)
set.seed(74L)
rnd <- sample.int(n)

cat(sprintf("# R\t%s\tplatform\t%s\tpackage\t%s\tlabel\t%s\tn\t%.0f\n",
            R.version.string, R.version$platform,
            as.character(packageVersion("kit")), label, n))

# Ordered opposite to the requested direction: the former O(n * len0) case.
summarize("asc_n6", function() topn(asc, 6L, TRUE, FALSE), reps = 5L)
summarize("asc_n1500", function() topn(asc, 1500L, TRUE, FALSE), reps = 3L)
summarize("asc_n2001", function() topn(asc, 2001L, TRUE, FALSE), reps = 3L)
summarize("desc_n2000", function() topn(desc, 2000L, FALSE, FALSE), reps = 3L)
summarize("asc_dbl_n1500", function() topn(ascd, 1500L, TRUE, TRUE), reps = 3L)
summarize("desc_dbl_n2000", function() topn(rev(ascd), 2000L, FALSE, TRUE), reps = 3L)

# Random input: must stay on the linear fast path.
summarize("random_n6", function() topn(rnd, 6L, TRUE, FALSE), reps = 5L)
summarize("random_n1500", function() topn(rnd, 1500L, TRUE, FALSE), reps = 5L)
summarize("random_n2000", function() topn(rnd, 2000L, TRUE, FALSE), reps = 5L)
