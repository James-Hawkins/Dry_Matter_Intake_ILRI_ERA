threepoint <<- function(x, y, ladder=c(1, 1/2, 1/3, 0, -1/2, -1)) {
  # x and y are length-three samples from a dataset.
  dx <- diff(x)
  f <- function(parms) (diff(diff(box.cox(y, parms)) / dx))^2
  fit <- nlm(f, c(1,0))
  parms <- fit$estimate 
  lambda <- ladder[which.min(abs(parms[1] - ladder))]
  if (lambda==0) offset = 0 else {
    do <- diff(range(y))
    offset <- optimize(function(x) f(c(lambda, x)), 
                       c(max(-min(x), parms[2]-do), parms[2]+do))$minimum    
  }
  c(lambda, offset)
}

optim.tform <<- function( reg.dat , variable, species , ndf.level )  {
  
  # variable <- "feed_intake_g_d" ; ndf.level <- ndf.lev.lo ; species <- 'Sheep'
  
  
  
  y.var <- reg.dat[  reg.dat$ndf.level == ndf.level & reg.dat$Species == species  ,  variable ]
  y.var.log <- log(y.var )
  y.var.sqt <- sqrt(y.var )
  y.var.sqd <- (y.var * y.var)
  
  
  
  var.mm.s <- str_c(variable , suffx.mm.s)
  y.var.mm.s <- reg.dat[  reg.dat$ndf.level == ndf.level & reg.dat$Species == species  ,  var.mm.s  ]
  
  var.mean.s <- str_c(variable , suffx.mean.s)
  y.var.mean.s <- reg.dat[  reg.dat$ndf.level == ndf.level & reg.dat$Species == species  ,    var.mean.s ]
  
  
  transforms <- c('NULL' , 'log' , 'sqt' , 'sqd' , suffx.mm.s , suffx.mean.s )
  
  shap.tests <- c()
  
  shap.tests[1] <- as.numeric(shapiro.test(y.var)[1])
  shap.tests[2] <- as.numeric(shapiro.test(y.var.log)[1])
  shap.tests[3] <- as.numeric(shapiro.test(y.var.sqt)[1])
  shap.tests[4] <- as.numeric(shapiro.test(y.var.sqd)[1])
  
  shap.tests[5] <- as.numeric(shapiro.test(  y.var.mm.s )[1])
  shap.tests[6] <- as.numeric(shapiro.test( y.var.mean.s)[1])
  
  
  best.tform <- which( max(shap.tests) == shap.tests )
  
  
  
  
  if( transforms[best.tform[1]] == transforms[1] ){  var.name <- variable } else{
    var.name <- str_c(variable , '.' , transforms[best.tform])
  }
  
  
  return (list(  transforms[best.tform] , var.name , shap.tests))
  
}
