vunion <- function (x, y,  multiple=TRUE) {
# 'multiple' = FALSE is normal union
#  view of data would mean intersect(x,y) is always empty
# just call union when !multiple 
if (multiple) {
# is calling 3 things faster than calculating max(each unique element in x,y)  ??
# The latter (max()) is 4X faster
#	trueun <- c(vintersect(x,y), vsetdiff(x,y), vsetdiff(y,x) )
# but have to remove NA values to avoid disaster
	x <- as.vector(x)
	y <- as.vector(y)
	xx <- x[!is.na(x)]
	xn <- length(x) - length(xx) #x[is.na(x)]
	yy <- y[!is.na(y)]
	yn <- length(y) - length(yy) # y[is.na(y)]
	# here I want the max of how many NA there are
	ndif <- max(xn,yn) 
	uniqs <- sort(unique(c(xx,yy))) #makes output pretty
	trueun <- vector()
	for (ju in 1:length(uniqs)) {
		trueun <- c(trueun, rep(uniqs[ju],times = max(sum(xx==uniqs[ju]), sum(yy==uniqs[ju]) ) ) )
		}
# now put NAs back in
	trueun <- c(trueun,rep(NA,ndif))
	return(trueun)
	} else return(union(x,y))
}
