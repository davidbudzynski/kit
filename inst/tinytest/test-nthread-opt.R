# kit.nThread option robustness (.onLoad/.onAttach) — tinytest (see issue #56).
# Runs even with OpenMP disabled (unlike test-nthread.R): direct .onLoad/.onAttach
# calls, no detach/reattach so the suite session stays attached.

sys.source("helper-kit.R", envir = environment())

old_nthread <- getOption("kit.nThread", NULL)
has_old <- "kit.nThread" %in% names(options())
on.exit(if (has_old) options(kit.nThread = old_nthread) else options(kit.nThread = NULL), add = TRUE)

no_error <- function(expr) {
  !inherits(tryCatch({ expr; NULL }, error = function(e) e), "error")
}

# --- .onLoad restores default when missing, preserves existing ---
options(kit.nThread = NULL)
kit:::.onLoad(NULL, "kit")
expect_identical(getOption("kit.nThread"), 1L, info="nthnull-load-001")
options(kit.nThread = 2L)
kit:::.onLoad(NULL, "kit")
expect_identical(getOption("kit.nThread"), 2L, info="nthnull-load-002")

# --- .onAttach never errors on NULL/NA/0/"2"/length>1 (issue #56) ---
options(kit.nThread = NULL)
expect_true(no_error(kit:::.onAttach(NULL, "kit")), info="nthnull-attach-001")
options(kit.nThread = NA_integer_)
expect_true(no_error(kit:::.onAttach(NULL, "kit")), info="nthnull-attach-002")
options(kit.nThread = NA)
expect_true(no_error(kit:::.onAttach(NULL, "kit")), info="nthnull-attach-003")
options(kit.nThread = 0L)
expect_true(no_error(kit:::.onAttach(NULL, "kit")), info="nthnull-attach-004")
options(kit.nThread = "2")
expect_true(no_error(kit:::.onAttach(NULL, "kit")), info="nthnull-attach-005")
options(kit.nThread = c(1L, 2L))
expect_true(no_error(kit:::.onAttach(NULL, "kit")), info="nthnull-attach-006")

# --- functions honour NULL default without error ---
options(kit.nThread = NULL)
expect_identical(iif(c(TRUE, FALSE), 1L, 0L), c(1L, 0L), info="nthnull-iif-001")
expect_identical(vswitch(c("a", "b"), "a", 1L), c(1L, NA_integer_), info="nthnull-vswitch-001")
