\name{find_elements}
\alias{find_elements}
\title{Find Boundary Positions in a Sorted Vector}
\usage{
find_elements(v, a, b)
}
\arguments{
  \item{v}{A nondecreasing numeric vector.}
  \item{a}{Lower boundary.}
  \item{b}{Upper boundary.}
}
\value{
A named integer vector containing the largest one-based position whose value
is less than or equal to \code{a}, and the smallest position whose value is
greater than or equal to \code{b}. A missing upper boundary is returned as
\code{NA_integer_}.
}
\description{
Finds sample-size boundary positions used by the split-search algorithm.
}
\examples{
find_elements(c(2, 4, 6, 8), 4, 7)
}

