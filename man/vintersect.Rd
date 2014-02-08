\name{vintersect}
\alias{vintersect}
\title{
Perform intersection of two vectors, including counting repeated elements.
}
\description{
Unlike the \code{base::intersect} function, if the vectors have repeated elements in common, the intersection returns as many of these elements as are in whichever vector has fewer of them.
}
\usage{
vintersect(x, y, multiple = TRUE)
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
A vector of the elements in the intersection of the two vectors. If \code{multiple=FALSE} is set, only unique values are returned.
}

\author{
Carl Witthoft, with some code taken from Sven Hohenstein via Stack Overflow

}

\seealso{
 \code{\link{intersect}}, the CRAN package \code{sets} 
}
\examples{
x <- c(1:5,3,3,3,2,NA,NA)
y<- c(2:5,4,3,NA)
vintersect(x,y)
vintersect(x,y,multiple=FALSE) 
intersect(x,y) #same as previous line
}
