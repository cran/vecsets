\name{vunion}
\alias{vunion}
\title{
Returns the union of its inputs including repeated elements.
}
\description{
The \code{base::union} function removes duplicates per algebraic set theory. \code{vunion} does not, and so returns as many duplicate elements as are in either input vector (not the sum of their inputs.)  In short, \code{vunion} is the same as \code{vintersect(x,y) + vsetdiff(x,y) + vsetdiff(y,x)}.
}
\usage{
vunion(x, y, multiple = TRUE)
}
\arguments{
  \item{x}{
A vector or an object which can be coerced to a vector
}
  \item{y}{
A vector or an object which can be coerced to a vector
}
  \item{multiple}{
Should repeated "multiple" items be returned? Default is \code{TRUE}; if set to \code{FALSE}, \code{vunion} acts like the \code{base::vunion} function.
}
}

\value{
A vector of the union of the two input vectors. If \code{multiple} is set to \code{FALSE} then the value returned is the same as \code{base::union}.
}

\author{
Carl Witthoft
}

\seealso{
 \code{\link{union}}, the CRAN package \code{sets} 
}
\examples{
x <- c(1:5,3,3,3,2,NA,NA)
y<- c(2:5,4,3,NA)
vunion(x,y)
vunion(x,y,multiple=FALSE) 
union(x,y) #same as previous line
}
