# Rev 1.2: added checks for empty-set inputs 
# Jan 2021 minor speed cleanups
vsetdiff <-  function (x, y, multiple=TRUE) {
x <- as.vector(x)
y <- as.vector(y)
# new code to check for empty sets  here
if(!multiple) return ( setdiff(x,y))
if(!length(x)) return(NULL)
if(!length(y)) return(x)
 
xx <- x[!is.na(x)]
xn <- length(x) - length(xx) #x[is.na(x)]
yy <- y[!is.na(y)]
yn <- length(y) - length(yy) # y[is.na(y)]

# if the output of unlist() is length 0
# then difout <- xx  (foo[-0]  does naughty things)
tapout <- unlist( tapply(yy, yy, function(yyy) head(which(xx == yyy[1]), length(yyy) )   )  ) 
if(length(tapout)) difout<-xx[-tapout] else difout<- xx
ndif <- max(0,length(xn)-length(yn) )
difout<- c(difout, rep(NA,ndif) )

return(difout)
}
