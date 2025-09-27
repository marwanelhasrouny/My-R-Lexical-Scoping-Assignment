makeCacheVector <- function(x = numeric()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setMean <- function(mean_value) m <<- mean_value
  
  getMean <- function() m
  
  list(set = set, get = get,
       setMean = setMean,
       getMean = getMean)
}

cacheMean <- function(x, ...) {
  m <- x$getMean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setMean(m)
  m
}

my_vector <- makeCacheVector(c(1, 2, 3, 4, 5))

cacheMean(my_vector)

cacheMean(my_vector)

my_vector$set(c(10, 20, 30))
cacheMean(my_vector)
