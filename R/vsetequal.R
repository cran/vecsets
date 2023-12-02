vsetequal <- function (x, y, multiple=TRUE) {
x <- as.vector(x)
y <- as.vector(y)
#?? is all(....) any different from base::setequal? 
if(!multiple) {
return(	setequal(x,y) )
#	all(c(match(x, y, 0L) > 0L, match(y, x, 0L) > 0L))
} else {
# Can get away with this 'cause set theory doesn't "allow" floats
# works fine when both x and y are empty
# == takes precedence over && ; parentheses added for clarity
return (	(length(x) == length(y)) && identical(sort(x), sort(y)) )
}
}
