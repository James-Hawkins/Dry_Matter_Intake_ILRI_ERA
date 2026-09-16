library(mboost)
library(multcomp)
library(partykit)
library(mermboost)
library(lme4)
library(performance)

install.packages("mboost")
install.packages("multcomp")
install.packages("partykit")



reg.vars <- c( 
  'feed_intake_g_d' 
  
  , 'bw_kg.e25'
  , 'bw_kg.sqt'
  , 'bw_kg.e75'
  
  , 'bw_kg' 
  , 'bw_kg.cbd' 
  , 'bw_kg.sqd' 
  , 'bw_kg.log'
  
  
  , 'adg_g_day.e25'
  , 'adg_g_day.sqt'
  , 'adg_g_day.e75'
  , 'adg_g_day'
  , 'adg_g_day.sqd'
  , 'adg_g_day.cbd'
  , 'adg_g_day.log'
  
  
  , 'NDF_nutrition'
  
  ,'CP_nutrition'
  
  
  , "B.Code"
  
)



# Specifies the error distribution and link function. Common options include Gaussian() (default), Binomial(), and Poisson()
#control: Used to set boosting parameters like mstop (the number of boosting iterations). The number of iterations is a tuning parameter that can be selected via cross-validation using cvrisk().
#center: A logical value indicating whether the design matrix should be centered. Centering often leads to quicker convergence

cnd.sp.lo.ndf <- (s.rums.cc$Species == species.sheep & s.rums.cc$ndf.level == ndf.lev.lo)
cnd.gt.lo.ndf <- (s.rums.cc$Species == species.goat & s.rums.cc$ndf.level == ndf.lev.lo)


dat.sp.lo.ndf <- s.rums.cc[ cnd.sp.lo.ndf , reg.vars]


r.test.sp.lo.ndf <- sample( dat.sp.lo.ndf[ , 'B.Code' ] , ( 0.8 ) * length(unique(dat.sp.lo.ndf[ , 'B.Code' ] ))   )





t.dat.sp.lo.ndf <- dat.sp.lo.ndf[ dat.sp.lo.ndf$B.Code %in% r.test.sp.lo.ndf , ]
v.dat.sp.lo.ndf <- dat.sp.lo.ndf[ !(dat.sp.lo.ndf$B.Code %in% r.test.sp.lo.ndf) , ]



t.dat.sp.lo.ndf <- t.dat.sp.lo.ndf[complete.cases(t.dat.sp.lo.ndf),]
v.dat.sp.lo.ndf <- v.dat.sp.lo.ndf[complete.cases(v.dat.sp.lo.ndf),]

t.dat.sp.lo.ndf$B.Code <- as.factor(t.dat.sp.lo.ndf$B.Code)

# Fit a linear model using boosting (parametric components)
model.sp.lo.ndf <- glmboost( 
  
  feed_intake_g_d  ~ 
    
    bw_kg 

  + NDF_nutrition 
  
  
  + bw_kg.e25
  + bw_kg.sqt
  + bw_kg.e75
  
  + bw_kg
  + bw_kg.cbd
  + bw_kg.sqd
  + bw_kg.log
  
  + adg_g_day.e25
  + adg_g_day.sqt 
  + adg_g_day.e25  
  + adg_g_day 
  + adg_g_day.log 
  + adg_g_day.sqd 
  + adg_g_day.cbd 
  
  , data = t.dat.sp.lo.ndf  
  , family = Gaussian() 
  ,  control = boost_control(mstop = 2000)
  , center = FALSE
  )



model.gt.lo.ndf <- glmboost( feed_intake_g_d  ~  bw_kg + adg_g_day + NDF_nutrition +  CP_nutrition, data = s.rums.cc[ cnd.gt.lo.ndf , ], family = Gaussian() ,  control = boost_control(mstop = 2000), center = FALSE)



summary(model.sp.lo.ndf)

coef(model.sp.lo.ndf )
coef(model.gt.lo.ndf )

predicted.sp.lo.ndf <- predict(model.sp.lo.ndf, newdata = v.dat.sp.lo.ndf )
predicted.gt.lo.ndf <- predict(model.gt.lo.ndf, newdata = s.rums.cc[ cnd.gt.lo.ndf , ])


resids.sp.lo.ndf <- v.dat.sp.lo.ndf[,'feed_intake_g_d'] - predicted.sp.lo.ndf 
resids.gt.lo.ndf <- s.rums.cc[cnd.gt.lo.ndf,'feed_intake_g_d'] - predicted.gt.lo.ndf 

sq.resids.sp.lo.ndf <- resids.sp.lo.ndf ^ 2
sq.resids.gt.lo.ndf <- resids.gt.lo.ndf ^ 2

mse.sp.lo.ndf <- mean(sq.resids.sp.lo.ndf)
mse.gt.lo.ndf <- mean(sq.resids.gt.lo.ndf)


rmse.sp.lo.ndf <- sqrt(mse.sp.lo.ndf )
rmse.gt.lo.ndf <- sqrt(mse.gt.lo.ndf )


nrmse.sp.lo.ndf <- rmse.sp.lo.ndf / mean(v.dat.sp.lo.ndf[,'feed_intake_g_d'])
nrmse.gt.lo.ndf <- rmse.gt.lo.ndf / mean(s.rums.cc[cnd.gt.lo.ndf,'feed_intake_g_d'])


rss.sp.lo.ndf <- sum(sq.resids.sp.lo.ndf)
rss.gt.lo.ndf <- sum(sq.resids.gt.lo.ndf)

tss.sp.lo.ndf  <- sum(v.dat.sp.lo.ndf[,'feed_intake_g_d']^2)
tss.gt.lo.ndf  <- sum(s.rums.cc[cnd.gt.lo.ndf,'feed_intake_g_d']^2)


1- rss.sp.lo.ndf / tss.sp.lo.ndf
1- rss.gt.lo.ndf / tss.gt.lo.ndf



plot( v.dat.sp.lo.ndf[,'feed_intake_g_d'] , predicted.sp.lo.ndf)




# Example using a formula with random effects (e.g., (1|Subject))
model <- mermboost(feed_intake_g_d  ~ bw_kg + adg_g_day +  NDF_nutrition + (1 | r.id), data = s.rums.cc.gbm , baselearner = 'bols')



summary(model)


coef(model ) # Extract coefficients

print(model)


s.rums.cc.gbm$r.id <- as.factor( s.rums.cc.gbm$r.id)






# Using brandom() for a 'Subject' grouping variable
model <- mboost(feed_intake_g_d  ~ bw_kg + brandom(r.id), data = s.rums.cc.gbm)


summary(model)

coef(model) # Extract coefficients

print(model)
