\name{vsetdiff}
\alias{vsetdiff}
\title{
Find all elements in first argument which are not in second argument.
}
\description{
Finds all elements in first argument which are not in the second argument.  Unlike the \code{base::setdiff} function, if the vectors have repeated elements in common, only the "excess" number of a given element are returned. 
}
\usage{
vsetdiff(x, y, multiple = TRUE)
}
\arguments{
  \item{x}{
A vector or an object which can be coerced to a vector
}
  \item{y}{
A vector or an object which can be coerced to a vector
}
  \item{multiple}{
Should repeated "multiple" items be returned? Default is \code{TRUE}; if set to \code{FALSE}, \code{vintersect} acts like the \code{base::intersect} function.
}
}

\value{
A vector of all elements in \code{x} which are not in \code{y}.   If \code{multiple=FALSE} is set, only unique values are returned.
}


\author{
Carl Witthoft
}

\seealso{
\code{\link{setdiff}}, the CRAN package \code{sets}
}

\examples{
x <- c(1:5,3,3,3,2,NA,NA)
y<- c(2:5,4,3,NA)
vsetdiff(x,y)
vsetdiff(x,y,multiple=FALSE)
setdiff(x,y) # same as previous line
vsetdiff(y,x) #note the asymmetry
}
