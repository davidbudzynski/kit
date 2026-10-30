\name{charToFact}
\alias{charToFact}
\title{Convert Character Vector to Factor}
\description{
  Similar to \code{base::as.factor}, but much faster and only for converting
  a character vector to a factor.
}
\usage{
charToFact(x, decreasing=FALSE, addNA=TRUE,
           nThread=getOption("kit.nThread"))
}
\arguments{
  \item{x}{A character vector.}
  \item{decreasing}{Logical. If \code{TRUE}, order levels in decreasing order.
  Default is \code{FALSE}.}
  \item{addNA}{Logical. If \code{TRUE} (default), include \code{NA} in the
  levels of the output.}
  \item{nThread}{Integer. Number of threads to use. Default is
  \code{getOption("kit.nThread")}.}
}
\details{
  Levels are sorted in the session locale. For non-ASCII text, the level order
  can therefore differ between operating systems and locales. If you need a
  fixed order, set the levels yourself with \code{factor(x, levels = ...)}.

  With \code{addNA = TRUE} (default), \code{NA} is a level, unlike
  \code{as.factor()}, which drops \code{NA} levels.
}
\value{
  A factor with the same length as \code{x}.
}
\examples{
x = c("b","A","B","a","\xe4","a")
Encoding(x) = "latin1"
identical(charToFact(x), as.factor(x))
identical(charToFact(c("a","b",NA,"a")), addNA(as.factor(c("a","b",NA,"a"))))
identical(charToFact(c("a","b",NA,"a"), addNA=FALSE), as.factor(c("a","b",NA,"a")))

# Benchmarks
# ----------
# x = sample(letters,3e7,TRUE)
# microbenchmark::microbenchmark(
#   kit=kit::charToFact(x,nThread = 1L),
#   base=as.factor(x),
#   times = 5L
# )
# Unit: milliseconds
# expr  min   lq   mean  median   uq  max neval
# kit   188  190    196     194  200  208     5
# base 1402 1403   1455    1414 1420 1637     5
}
