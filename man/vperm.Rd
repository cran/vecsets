\name{vperm}
\alias{vperm}
\title{
Calculate all permutations of all combinations of a specified size from a data object.
}
\description{
This function first uses \code{combn} to generate combinations of the desired size, then calculates all permutations of all said combinations. 
}
\usage{
vperm(x, m, FUN = NULL, ...)
}
\arguments{
  \item{x}{
vector source for combinations, or integer n for x <- seq_len(n)
}
  \item{m}{
number of elements to choose in making the combinations
}
  \item{FUN}{
function to be applied to each combination; default NULL means the identity, i.e., to return the combination (vector of length m) 
}
  \item{...}{ 
Additional arguments, if any, required for the function \code{FUN} . See Details.
}
}
\value{
An array within which each row contains one of the permutations. 
}

\details{
NA values are considered as valid elements and will be processed just as they are in \code{combn}
The input arguments are passed directly to \code{combn} but with one important exception.  \code{combn}'s argument "simplify" is forced to "TRUE" inside this function so as to allow the permutations to be more easily generated.  If the user includes \code{simplify = FALSE} in the \code{...} input, it will be overwritten. 

}
\author{
Carl Witthoft, with some code taken from Sven Hohenstein via Stack Overflow

}

\seealso{
 \code{\link{intersect}}, the CRAN package \code{sets}, \code{\link[pracma]{perms}}
}
\examples{
x <- c(1:5,3,3,3,2,NA,NA)
xp <- vperm(x,4) #large array
}
