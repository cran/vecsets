\name{vecsets-package}
\alias{vecsets-package}
\alias{vecsets}
\docType{package}
\title{An extension of the \code{base} "sets" tools which does not reduce to unique elements}
\description{
The \code{base} "sets" tools follow the algebraic definition that each element of a set must be unique.  Since it's often helpful to compare all elements of two vectors, this toolset treats every element as unique for counting purposes.
For ease of use, all functions in \code{vecsets} have an argument \code{multiple} which, when set to \code{FALSE} , reverts them to the \code{base} set tools functionality.
}
\details{
\tabular{ll}{
Package: \tab vecsets\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-02-08\cr
License: \tab GPL-3\cr
}

}
\author{
Carl Witthoft, with some code taken from Sven Hohenstein via Stack Overflow

Maintainer: Carl Witthoft carl@witthoft.com
}
