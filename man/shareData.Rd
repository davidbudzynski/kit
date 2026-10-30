\name{shareData/getData/clearData}
\alias{shareData}
\alias{getData}
\alias{clearData}
\title{ Share Data between R Sessions}
\description{
Share one \R object between two \R sessions on the same machine.
}
\usage{
shareData(data, map_name, verbose=FALSE)
getData(map_name, verbose=FALSE)
clearData(x, verbose=FALSE)
}
\arguments{
  \item{data}{ An \R object, for example a vector or a \code{data.frame}.}
  \item{map_name}{ A character string. The name of the shared memory object. The function adds a leading \code{/} if it is missing. Use a simple name. Do not use a path or a Windows \code{Global} or \code{Local} prefix. A name can describe only one object: calling \code{shareData()} again under a name that is in use overwrites that object (or fails if the new object is larger), and the older pointer's \code{clearData()} then leaves the newer object in place.}
  \item{x}{ An external pointer returned by \code{shareData}.}
  \item{verbose}{ A logical value. Use \code{TRUE} to print progress information, or \code{FALSE} to suppress it.}
}
\details{
\strong{This is a copy, not a live pointer.}
\code{shareData()} serializes \code{data} to a raw vector and copies it into shared memory. \code{getData()} copies the raw vector back and unserializes it. The object is copied twice. Large objects thus take time and memory. A plain pointer into the R heap of another session cannot work, because R objects carry process-private metadata (garbage collection, string cache, ALTREP, ...). Issue #43 (\url{https://github.com/fastverse/kit/issues/43}) tracks the request for a lightweight pointer-based alternative.

\strong{Lifetime and platform differences.}
\describe{
  \item{POSIX (Linux, macOS, and other Unix-like systems)}{
    \code{getData()} unlinks the shared memory object before it calls \code{unserialize()}. A second call with the same \code{map_name} thus fails. The object stays alive while the pointer returned by \code{shareData()} exists. \code{clearData()} unmaps and unlinks the object it created, unless another session has since reused the name; then it only cleans up its own view. The same cleanup occurs when the pointer is garbage collected or the session exits. If the sharing session is killed, the object can stay in \code{/dev/shm} on Linux. It stays there until you remove it or the machine restarts.
  }
  \item{Windows}{
    \code{getData()} closes its handles but does not delete the mapping. A second call thus succeeds while the sharing session is alive. \code{clearData()} removes the mapping when the last handle is closed. If the sharing session is killed, Windows closes its handles and the mapping disappears.
  }
}

\strong{Scope.}
Sharing works only on one machine and for the same user. It does not send data over a network. Use the same \R version and the same version of \pkg{kit} when possible. External pointers and connections do not survive. Environments are copied, not shared.

\strong{Memory limits.}
The operating system limits shared memory. On Linux, \code{/dev/shm} is often smaller than RAM, especially in containers. A large object can thus fail even when the machine has enough memory.

The functions are currently tested on POSIX platforms (with \code{shm_open}) and Windows (with named file mappings). Other platforms are untested.
}
\value{
\code{shareData} returns an external pointer.
\code{getData} returns the \R object stored at \code{map_name}.
\code{clearData} returns \code{TRUE} if it released the pointer's mapping, or \code{FALSE} if the pointer was already cleared.
}
\author{Morgan Jacob}
\examples{
\dontrun{
# In R session 1: share data in memory
x = shareData(mtcars, "share1")

# In R session 2: get data from session 1
getData("share1")

# In R session 1: clear data in memory
clearData(x)
}
}
