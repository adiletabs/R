?runif

NA.position <- function(x){
  res <- which(is.na(x))
  return(res)
}
t = c(1, 2, 3, NA, NA, 6)
NA.position(t)
?which

NA.counter <- function(x){
  # put your code here  
 return(sum(is.na(x))) 
}
NA.counter(t)

x <- runif(100)
q <- quantile(x, probs = c(0.25, 0.75))
IQR(x)

outliers.rm <- function(x){
  q <- quantile(x, probs = c(0.25, 0.75))
  i <- IQR(x) * 1.5
  res <- x[which(abs(x - q[1]) <= i & abs(x - q[2]) <= i)]
  return(res)
}
outliers.rm(x)
