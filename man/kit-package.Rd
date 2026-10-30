\name{kit-package}
\alias{kit-package}
\alias{kit}
\docType{package}
\title{kit: Fast Data Manipulation Functions in C}
\description{
  \pkg{kit} provides fast, vectorised functions for data manipulation.
  The compiled code is written in C. The package has no runtime dependencies.
}
\details{
  \strong{Threads.}
  Five functions can use OpenMP: \code{\link{iif}}, \code{\link{nswitch}},
  \code{\link{vswitch}}, \code{\link{psort}}, and \code{\link{charToFact}}.
  The other functions use one thread.

  Set the number of threads with \code{options(kit.nThread = 4L)}.
  The default is \code{1}. If you set a value that is larger than the number
  of available processors, \pkg{kit} uses the number of available processors.
  If \pkg{kit} was built without OpenMP, all functions use one thread and
  this option has no effect.

  \strong{Platforms.}
  \pkg{kit} is tested on Linux, macOS, and Windows. OpenMP is optional.
  On macOS, OpenMP requires \code{libomp} from Homebrew. On Windows, OpenMP
  comes with Rtools. See the README for installation details.

  \strong{Encodings.}
  \code{\link{psort}} sorts text in the C locale by default. Set
  \code{c.locale = FALSE} to use the session locale.
  \code{\link{charToFact}} uses the session locale. Factor levels and sort
  order can thus differ between operating systems for non-ASCII text.
  \code{\link{vswitch}} and \code{\link{nswitch}} convert mixed encodings to
  UTF-8 by default.
}
\seealso{
  \code{\link{psum}}, \code{\link{funique}}, \code{\link{topn}},
  \code{\link{shareData}}
}
\keyword{package}
