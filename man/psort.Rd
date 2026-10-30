\name{psort}
\alias{psort}
\title{Parallel Sort}
\description{
  Similar to \code{base::sort}, but only for character vectors and partly
  using parallelism. It is experimental and might change in the future.
  Use with caution.
}
\usage{
psort(x, decreasing=FALSE, na.last=NA,
      nThread=getOption("kit.nThread"),c.locale=TRUE)
}
\arguments{
  \item{x}{A character vector. For other input, \code{psort()} warns and
  calls \code{base::sort()}.}
  \item{na.last}{Controls the treatment of \code{NA}s. If \code{TRUE},
  missing values are put last; if \code{FALSE}, they are put first; if
  \code{NA}, they are removed.}
  \item{decreasing}{Logical. If \code{TRUE}, sort in decreasing order.
  Default is \code{FALSE}.}
  \item{nThread}{Integer. Number of threads to use. Default is
  \code{getOption("kit.nThread")}.}
  \item{c.locale}{Logical. If \code{TRUE} (default), sort byte by byte in the
  C locale. The result is the same on every operating system and matches
  \code{sort(x, method = "radix")}. If \code{FALSE}, use the session locale
  and \code{sort(x, method = "quick")}; the order of accented letters and
  other non-ASCII text can differ between operating systems and locales.}
}
\details{
  For non-character \code{x}, \code{psort()} warns and calls
  \code{base::sort()} with \code{method = "radix"} when
  \code{c.locale = TRUE}, and \code{method = "quick"} otherwise.
}
\value{
  A character vector with the values of \code{x} in sorted order.
}
\author{Morgan Jacob}
\examples{
x = c("b","A","B","a","\xe4")
Encoding(x) = "latin1"
identical(psort(x, c.locale=FALSE), sort(x))
identical(psort(x, c.locale=TRUE), sort(x, method="radix"))

# Benchmarks
# ----------
# strings = as.character(as.hexmode(1:1000))
# x = sample(strings, 1e8, replace=TRUE)
# system.time({kit::psort(x, na.last = TRUE, nThread = 1L)})
#   user  system elapsed
#  2.833   0.434   3.277
# system.time({sort(x,method="radix",na.last = TRUE)})
#   user  system elapsed
#  5.597   0.559   6.176
# system.time({x[order(x,method="radix",na.last = TRUE)]})
#   user  system elapsed
#  5.561   0.563   6.143
}
