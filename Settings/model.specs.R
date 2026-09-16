# Parameters for dataset de-lineation

LR.only <<- TRUE # True if only working on large ruminants 

merge.sexes <<- FALSE # Do you merge both male and female (if yes, set n.sexes to 1)
merge.breeds <<- FALSE # Do you merge both Indigenous (Bos indicus) and exotic (crossbred/purebred Bos taurus)

# Sub-samples
n.species <<- 1 # number of species (i.e. sheep, goat, cattle)
n.ndf <<- 2 # number of NDF levels  (i.e. low, high)
n.sexes <<- 2 # number of sexes (i.e. male, female)
n.breeds <<- 2 # number of breed types 



if (  merge.breeds ) { n.breeds <<- 1 ; breed.indig  <<- c( breed.indig  , breed.cbred )  ;  breeds <<- c(breed.indig )} else { n.breeds <<- 2  ; breeds <- c(breed.indig , breed.cbred)}


if (  merge.sexes  ) { n.sexes <<- 1 ; sex.male <<- c( sex.male , sex.female) ; sexes <- sex.male }  else { n.sexes <<- 2  ; sexes <- c(sex.male, sex.female)}


if (  LR.only  ) { n.species <<- 1 }  



# GLOBAL REGRESSION SETTINGS
# Include a random experiment intercept (yes/no)
random.exp.int <<- FALSE

# Fold by experiment or experimental unit
fold.by.exp <<- TRUE

# age.status <<- 'growing'
age.status <<- NA


# GBR compute SEs
compute.GBR.SEs <<- FALSE ; num.boostraps <<- 25  # Bootstraps must be sufficiently high, e.g. > 20

# GBR report SEs in coefficients table (excel file)
report.GBR.stability <<- FALSE

# Variable used for selecting optimal model
optimization.metric.var.name.1 <<- vn.w.R2.mean

if (optimization.metric.var.name.1 == vn.w.R2.mean){ optim.metric.ucb <<- vn.w.R2.ucb.ur  }
optimization.metric.c.param <<- -1 # note: if the optimization metric is R2, this parameter should be negative to indicate that a higher value is better


# Method used for defining regression weights
# Options: sqr , log , cubed.root

# Regression weighting method
reg.wt.method.null <<- FALSE

reg.wt.method.cubed.root <<- FALSE 
reg.wt.method.quartic.root <<- FALSE 
reg.wt.method.sqrt <<- TRUE
reg.wt.method.log <<- FALSE


include.redf <<- FALSE # Include re-defined variables such as NDF x NDF digestibility



# Regression model parameters
n.folds <<- 3 # number of folds 
min.e.units <<- round ( n.folds * 1.0 , 0 ) # By default, the minimum number of experimental units must be at least as great as number of folds
n.mod.classes <<- 2 # number of model classes (e.g. GBR, mixed effects, ...)
n.mod.form <<- 6 # Number of formulae per model class (note: this should be a multiple of 3, i.e. 3, 6, or 9)

model.classes.all <<- seq( 1 , n.mod.classes )


# Hyper-parameters (gradient boosted regression only)
m.stop.cv <<- FALSE
cv.risk.min.grid <<- 1000
cv.risk.max.grid <<- 1100


n.mod.v.family <<- 1
n.mod.v.boost.control.mstop <<- 1
n.mod.v.boost.control.nu <<- 1

ml.boost.families <<- c( Gaussian() , Laplace() , Huber())


mstop.max <<- 3000; mstop.min <<- 1000 ; m.stop.range <<- (mstop.max - mstop.min)
nu.max <<- 0.2 ; nu.min <<- 0.1 ; nu.range <<- (nu.max - nu.min)


# Ranges of hyperparameters (GBR)
min.nus <<- c( 0.01 , 0.01 , 0.01 , 0.01)
max.nus <<- c( 0.15 , 0.15 ,0.15 ,0.15 )

min.m.stops <<- c( 400 , 400 , 400 , 400)
max.m.stops <<- c( 1500 , 1500 , 1500 , 1500)



# QUALITY CONTROL (outlier control)
remove.outliers <<- TRUE # If true, removes outliers using two methods
remove.outliers.range.method <<- FALSE
remove.outliers.residual.method <<- TRUE


cutoff.ol.s.residual.method <<- 2.5


#' ordering
#'  BW in kg
#''feed_intake_kg_d'
#' 'NDF_nutrition'
#'  'CP_nutrition'
#' 'adg_g_day'


outlier.range.minimum.sr <<- c(
5
  , 0.4
  , 50
  ,80
  , -1.200
)

outlier.range.maximum.sr <<- c(
   100
  ,12
  , 900
  , 300
  , 1.500
  
)

#' ordering





#' 'adg_g_day'
 

# Bovines
outlier.range.minimum.bv <<- c(
  5 #'  BW in kg
  , 1.2 #''feed_intake_kg_d'
  , 50 #' 'NDF_nutrition'
  , 0.0 #'  'ADF_nutrition'
  , 50 #'  'CP_nutrition'
  , 0 #'  ether extract
  , 3 #'  Ash
  , -2.000
  , 0
  , 0
  , 0
  , 0
)

outlier.range.maximum.bv <<- c(
  600
  ,25
  , 900
  ,  1750
  , 350
  , 65
  , 300
  , 2.000
  , 1000
  , 1000
  , 1000
  , 1000
)



# Model stability computation metrics

stabsel.q.value <<- 4 ; stabsel.cutoff.value <<- 0.9

if (n.sexes == 1) {merge.sexes <- TRUE}

