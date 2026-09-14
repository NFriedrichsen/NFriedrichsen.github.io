exampleFunction1=function(x){
  return(mean(x**3-12))
}
exampleFunction2=function(x){
  return(sum(x-mean(x)^2))
}
Week2functionsTest<-c(1,2,5,6,9,10)
my_fun <- function(x = 99){
  d2 <- (mean(x, na.rm = TRUE) - median(x, na.rm = TRUE))^2
  return(d2)
}