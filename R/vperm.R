#  vperm for vecsets
# hey hey hey: a way to enhance pracma::perms!  
# 
# disallow 'simplify = FALSE' 
vperm <- function(x, m = if(length(x) == 1) x else length(x), FUN=NULL, ...) {
	#where m is number of elt's to take at a time 
elist <- list(...)
	# force safety internal data type
elist$simplify = TRUE  # i.e. if some joker puts 'simplify' into ellipsis
# combn and pracma:combs return transforms of each other...
# if FUN returns a single value, I get a 1XN output, unlike when arrays are returned. 
# How to discern that from , e.g.  combn(1:5,1) and combn(1:5,5) ? 
# combn returns all combinations in columns.  The problem is that when I apply a FUN, the result is a vector, 
# not a  1XN array, and t(vector) turns out to be 1XN, not what I want. 

thecomb <- combn(x, m , FUN, ...)
# combn may return an array, albeit of one dimension.  however, ncol(thecomb) will be NA
# the problem is that certain classes of "x" and certain values of "m" can return different 
# dimensional results
if (!is.na(ncol(thecomb)) ) {
	thecomb <- t(thecomb)
	} else {
		m = 1 # because no longer have multiple elements 
		dim(thecomb) <- c(dim(thecomb), 1) #force it to have a column
		}
facm <- factorial(m)
# build size of theperm
theperm <- matrix(0,nrow= nrow(thecomb) *facm, ncol = m)
for(jrow in 1: nrow(thecomb)) {
	startrow <- 1 + (jrow-1) * facm
	endrow <- startrow - 1 + facm
	theperm[startrow:endrow,] <- pracma::perms(thecomb[jrow,] )
	}
return(invisible(theperm))
}
