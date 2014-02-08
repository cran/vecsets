\name{vsetequal}
\alias{vsetequal}
\title{
Check whether two vectors contain exactly the same collection of elements.
}
\description{
Unlike the \code{base::setequal} function, if the vectors have repeated elements in common, the count of these elements is checked. As a result, vectors of different lengths will never be "equal." 
}

\usage{ vsetequal(x, y, multiple = TRUE)
}

\arguments{
k  \item{x}{
A vector or an object which can be coerced to a vector
}
  \item{y}{
A vector or an object which can be coerced to a vector
}
  \item{multiple}{
Should repeated "multiple" items be returned? Default is \code{TRUE}; if set to \code{FALSE}, \code{vsetequal} acts like the \code{base::intersect} function.
}
}

\value{
A logical value indicating equality or inequality. If \code{multiple=FALSE} is set, both input vectors are reduced to unique values before checking for equality.
}

\author{
Carl Witthoft
}

\seealso{
 \code{\link{setequal}}, the CRAN package \code{sets} 
}
\examples{
x <- c(1:5,3,3,3,2,NA,NA)
y<- c(1:5,4,3,NA)
vsetequal(x,y)
vsetequal(x,y,multiple=FALSE) 
setequal(x,y) #same as previous line
}

