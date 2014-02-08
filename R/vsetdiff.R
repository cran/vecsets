vsetdiff <-
function (x, y, multiple=TRUE) 
{
    x <- as.vector(x)
    y <- as.vector(y)
    xx <- x[!is.na(x)]
    xn <- x[is.na(x)]
    yy <- y[!is.na(y)]
    yn <- y[is.na(y)]
    if (length(x) || length(y)) {
        if (!multiple) {
             difout <- unique( x[match(x, y, 0L) == 0L])   #original code plus output obj
#this which fails on NA, so have to rip things apart..
              }else {
              	difout<- xx[-unlist( tapply(yy, yy, function(yyy) head(which(xx == yyy[1]), length(yyy))) ) ]  
              	ndif <- max(0,length(xn)-length(yn) )
              	difout<- c(difout, rep(NA,ndif) )
              }
        } else  difout <- x
     return(difout)
}
