int2ftM <- function(i) {
  if(!is.numeric(i)||any(i<=0)||!is.vector(i))
    stop("'i' must be a positive-valued numeric vector")
  ## Solve the quadratic equation
  ## a*n1^2 + b*n1 + c = 0
  ## with b<0 and a*c<0. It has a positive solution
  ## n1 = (-b+sqrt(b*b-4*a*c))/(2*a)
  ## Here: a=1/2, b=-1/2, c=-i+1  
  n1 <- floor(0.5+sqrt(0.25+2*(i-1)))
  n2 <- i - n1*(n1-1)/2
  ## +1 since we start counting at 1            
  return(cbind(n1=n1+1, n2))
}

ftM2int <-function(ft) {
  if(!is.numeric(ft)||any(ft<=0))
    stop("'ft' must contain positive numbers.")
  if(!is.matrix(ft) || !any(dim(ft)==2))
    stop("'ft' must be a 2xn or nx2 matrix")
  if(ncol(ft)!=2)
        ft <- t(ft)
  return(ft[,2] + (ft[,1]-1)*(ft[,1]-2)/2)
}
