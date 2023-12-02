# rev 1.3: fixed crash on zero intersection, thanks to nathan.cornwell@optum.com
# Rev 1.2: added checks for empty-set inputs
#Jan2021: clean up & speed up


vintersect <- function(x, y, multiple=TRUE){
# base::intersect does as.vector internally
	 # really can start out with this if() and just return vector() {logical(0)}
xtype <- typeof(x)
 if(!length(x) | !length(y)) return( vector(mode = xtype) )
 # make output look just like base::intersect 
# is there something in the intersection?
if (!length(intersect(x,y)))   return( vector(mode = xtype) )
if(multiple) {
	x <- as.vector(x)
	y <- as.vector(y)
	xx <- x[!is.na(x)]
	xn <- length(x) - length(xx) #x[is.na(x)]
	yy <- y[!is.na(y)]
	yn <- length(y) - length(yy) # y[is.na(y)]
	#unlike vdiff, here I want the difference in how many NA there are
	ndif <- min(xn,yn)  #length(xn), length(yn))
	intout <- vector()
# but since base::intersect includes "NA", have to jigger it here
	trueint = intersect(xx,yy)
	for(jj in 1: length(trueint) ) {
		# use sum instead of length(which), is 10% faster at least for big vectors 
	#intout <- c(intout, rep(trueint[jj], min(length(which(trueint[jj]==xx)), length(which(trueint[jj]==yy) ) ) ) )
		intout <- c(intout, rep(trueint[jj], min(sum(trueint[jj]==xx),sum(trueint[jj]==yy) ) ) )
		}
	trueint<-c( intout, rep(NA,ndif))
	return(trueint)
	} else return(intersect(x,y))
}
