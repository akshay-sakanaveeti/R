#probability
prob = function (n, m =5000){
  count = 0 
  for (i in 1:m){
    x = sample(1:n, size = 2, replace = FALSE)
    x = sort(x)
    if (x[2]%%x[1] == 0){
      count = count+1
        }
    
    }
  return(count/m)
  }

#mle

likelihood = function(data, par ) {
  x = 1
  for (i in data){
    x = 3*dnorm(i, mean = par[1],sd = par[2] )*(pnorm(i, mean = par[1], sd = par[2]))^2
    
  }
  return(x)
}

mle = function(info){
  return(optim(par = c(0,1), fn=likelihood,lower = c(-Inf,0), data = info))
}















#mandelbrot

in_mandelbrot = function(c, upperlimit){
  i = 0
  z = 0
  for (j in 0:upperlimit){
    if (Mod(z) > 2){
      break
    }
    z = z^2+c
    i = j
  }
  return(i == upperlimit )
}

mandelbrot = function(k,x1,x2,y1,y2,m){
  z = vector()
  for (x in seq(x1,x2,length.out= m)){
    for (y in 1i*seq(y1,y2,length.out= m)){
      z = c((x+y),z)
    }
  }
  
  
  z = z[sapply(z, in_mandelbrot, upperlimit=k)]
  plot(z)
}






#results
mandelbrot(200,-2,0.5,-1.25,1.25,150)
