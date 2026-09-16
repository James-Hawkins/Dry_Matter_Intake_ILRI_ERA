# source(str_c(  modules.dir  ,'cv.dat.prep.R')  )

{ 
  
  
  

d.reg.in.min.vars <-  ( !is.na(d.reg.in$feed_intake_g_d) & !is.na(d.reg.in$bw_kg) &  !is.na(d.reg.in$adg_kg_day) &  !is.na(d.reg.in$Sample.size)  )
  
if ( is.na(age.status) ) { d.reg.in.age.subset <- TRUE } else if (age.status == 'growing') { 
    d.reg.in.age.subset <- ( d.reg.in$bw_kg < 20 & !is.na(d.reg.in$bw_kg )) }
  
  
  d.reg.in.dont.exclude <- ( d.reg.in.age.subset &   d.reg.in.min.vars )
  
  # handle outliers
  cnd.sp.lo.ndf.male.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.sheep  & d.reg.in$sex %in% sex.male & d.reg.in$ndf.level == ndf.lev.lo & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  cnd.sp.lo.ndf.female.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.sheep  & d.reg.in$sex %in% sex.female & d.reg.in$ndf.level == ndf.lev.lo & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  
  cnd.sp.hi.ndf.male.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.sheep & d.reg.in$sex %in% sex.male  & d.reg.in$ndf.level == ndf.lev.hi & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  cnd.sp.hi.ndf.female.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.sheep &d.reg.in$sex %in% sex.female & d.reg.in$ndf.level == ndf.lev.hi & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  
  cnd.gt.lo.ndf.male.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.goat & d.reg.in$sex %in% sex.male  & d.reg.in$ndf.level == ndf.lev.lo & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  cnd.gt.lo.ndf.female.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.goat & d.reg.in$sex %in% sex.female & d.reg.in$ndf.level == ndf.lev.lo & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  
  cnd.gt.hi.ndf.male.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.goat & d.reg.in$sex %in% sex.male  & d.reg.in$ndf.level == ndf.lev.hi & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  cnd.gt.hi.ndf.female.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.goat & d.reg.in$sex %in% sex.female & d.reg.in$ndf.level == ndf.lev.hi & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  
  # Bovines
  cnd.bv.lo.ndf.Bind.male.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.cattle & d.reg.in$sex %in% sex.male  & d.reg.in$ndf.level == ndf.lev.lo & d.reg.in$breed.type %in% breed.indig & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  cnd.bv.lo.ndf.Btau.male.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.cattle & d.reg.in$sex %in% sex.male  & d.reg.in$ndf.level == ndf.lev.lo & d.reg.in$breed.type %in% breed.cbred & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  
  cnd.bv.lo.ndf.Bind.female.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.cattle & d.reg.in$sex %in% sex.female & d.reg.in$ndf.level == ndf.lev.lo & d.reg.in$breed.type  %in% breed.indig & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  cnd.bv.lo.ndf.Btau.female.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.cattle & d.reg.in$sex %in% sex.female  & d.reg.in$ndf.level == ndf.lev.lo & d.reg.in$breed.type  %in% breed.cbred & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  
  cnd.bv.hi.ndf.Bind.male.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.cattle & d.reg.in$sex %in% sex.male  & d.reg.in$ndf.level == ndf.lev.hi & d.reg.in$breed.type %in% breed.indig & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  cnd.bv.hi.ndf.Btau.male.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.cattle & d.reg.in$sex %in% sex.male  & d.reg.in$ndf.level == ndf.lev.hi & d.reg.in$breed.type %in% breed.cbred & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  
  cnd.bv.hi.ndf.Bind.female.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.cattle & d.reg.in$sex %in% sex.female  & d.reg.in$ndf.level == ndf.lev.hi & d.reg.in$breed.type %in% breed.indig & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  cnd.bv.hi.ndf.Btau.female.pre.ol <- (d.reg.in.min.vars & d.reg.in$Species == species.cattle & d.reg.in$sex %in% sex.female  & d.reg.in$ndf.level == ndf.lev.hi & d.reg.in$breed.type  %in% breed.cbred & d.reg.in.dont.exclude & !is.na(d.reg.in$ndf.level)) 
  
  



  #  ~ ~ - - - - - - Outlier control --
  d.reg.in[,ol.status.residual.method.var.name] <- FALSE
  d.reg.in[,ol.status.range.method.var.name ] <- FALSE
  
  # Categorize rows according to outlier method -- regression residual method
  if (  remove.outliers & remove.outliers.residual.method  ) { 
    
  d.reg.in <- ols.status.residual.method( cnd.sp.lo.ndf.male.pre.ol )
  d.reg.in <- ols.status.residual.method( cnd.sp.lo.ndf.female.pre.ol )
  
  d.reg.in <- ols.status.residual.method( cnd.sp.hi.ndf.male.pre.ol )
  d.reg.in <- ols.status.residual.method( cnd.sp.hi.ndf.female.pre.ol )
  
  d.reg.in <- ols.status.residual.method( cnd.gt.lo.ndf.male.pre.ol )
  d.reg.in <- ols.status.residual.method( cnd.gt.lo.ndf.female.pre.ol )
  
  d.reg.in <- ols.status.residual.method( cnd.gt.hi.ndf.male.pre.ol )
  d.reg.in <- ols.status.residual.method( cnd.gt.hi.ndf.female.pre.ol )
  
  # Bovines 
  d.reg.in <- ols.status.residual.method( cnd.bv.lo.ndf.Bind.male.pre.ol )
  d.reg.in <- ols.status.residual.method( cnd.bv.lo.ndf.Btau.male.pre.ol )
  
  d.reg.in <- ols.status.residual.method( cnd.bv.lo.ndf.Bind.female.pre.ol )
  d.reg.in <- ols.status.residual.method( cnd.bv.lo.ndf.Btau.female.pre.ol )
  
  d.reg.in <- ols.status.residual.method( cnd.bv.hi.ndf.Bind.male.pre.ol )
  d.reg.in <- ols.status.residual.method( cnd.bv.hi.ndf.Btau.male.pre.ol )
  
  d.reg.in <- ols.status.residual.method( cnd.bv.hi.ndf.Bind.female.pre.ol )
  d.reg.in <- ols.status.residual.method( cnd.bv.hi.ndf.Btau.female.pre.ol )
  
  
  }
  
  # Categorize rows according to outlier method -- absolute variable range method
  
  if (  remove.outliers & remove.outliers.range.method ){ 
    
  d.reg.in <- ols.status.variable.range.method( cnd.sp.lo.ndf.male.pre.ol )
  d.reg.in <- ols.status.variable.range.method( cnd.sp.lo.ndf.female.pre.ol )
  
  d.reg.in <- ols.status.variable.range.method( cnd.sp.hi.ndf.male.pre.ol )
  d.reg.in <- ols.status.variable.range.method( cnd.sp.hi.ndf.female.pre.ol )
  
  d.reg.in <- ols.status.variable.range.method( cnd.gt.lo.ndf.male.pre.ol )
  d.reg.in <- ols.status.variable.range.method( cnd.gt.lo.ndf.female.pre.ol )
  
  d.reg.in <- ols.status.variable.range.method( cnd.gt.hi.ndf.male.pre.ol )
  d.reg.in <- ols.status.variable.range.method( cnd.gt.hi.ndf.female.pre.ol )
  
  # Bovines 
  d.reg.in <- ols.status.variable.range.method( cnd.bv.lo.ndf.Bind.male.pre.ol )
  d.reg.in <- ols.status.variable.range.method( cnd.bv.lo.ndf.Btau.male.pre.ol )
  
  d.reg.in <- ols.status.variable.range.method( cnd.bv.lo.ndf.Bind.female.pre.ol )
  d.reg.in <- ols.status.variable.range.method( cnd.bv.lo.ndf.Btau.female.pre.ol )
  
  d.reg.in <- ols.status.variable.range.method( cnd.bv.hi.ndf.Bind.male.pre.ol )
  d.reg.in <- ols.status.variable.range.method( cnd.bv.hi.ndf.Btau.male.pre.ol )
  
  d.reg.in <- ols.status.variable.range.method( cnd.bv.hi.ndf.Bind.female.pre.ol )
  d.reg.in <- ols.status.variable.range.method( cnd.bv.hi.ndf.Btau.female.pre.ol )
  
  
  }
  
  d.reg.in.variable.redfd <<- ( d.reg.in$NDF_digest.redf | d.reg.in$NDF_nutrition.redf ) 
  
  cnd.sp.lo.ndf.male <- (cnd.sp.lo.ndf.male.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  cnd.sp.lo.ndf.female <- (cnd.sp.lo.ndf.female.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  
  cnd.sp.hi.ndf.male <- (cnd.sp.hi.ndf.male.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  cnd.sp.hi.ndf.female <- (cnd.sp.hi.ndf.female.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  
  cnd.gt.lo.ndf.male <- (cnd.gt.lo.ndf.male.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  cnd.gt.lo.ndf.female <- (cnd.gt.lo.ndf.female.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  
  cnd.gt.hi.ndf.male <- (cnd.gt.hi.ndf.male.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  cnd.gt.hi.ndf.female <- (cnd.gt.hi.ndf.female.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  
  # Bovines
  cnd.bv.lo.ndf.Bind.male <- (cnd.bv.lo.ndf.Bind.male.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  cnd.bv.lo.ndf.Btau.male <- (cnd.bv.lo.ndf.Btau.male.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  
  cnd.bv.lo.ndf.Bind.female <- (cnd.bv.lo.ndf.Bind.female.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  cnd.bv.lo.ndf.Btau.female <- (cnd.bv.lo.ndf.Btau.female.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  
  cnd.bv.hi.ndf.Bind.male <- (cnd.bv.hi.ndf.Bind.male.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  cnd.bv.hi.ndf.Btau.male <- (cnd.bv.hi.ndf.Btau.male.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  
  cnd.bv.hi.ndf.Bind.female <- (cnd.bv.hi.ndf.Bind.female.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  cnd.bv.hi.ndf.Btau.female <- (cnd.bv.hi.ndf.Btau.female.pre.ol &  !(d.reg.in[,ol.status.range.method.var.name]  |  d.reg.in[,ol.status.residual.method.var.name] )  )
  

  
  if ( include.redf ){
    
    cnd.sp.lo.ndf.male <- (cnd.sp.lo.ndf.male  & !d.reg.in.variable.redfd)
    cnd.sp.lo.ndf.female <- (cnd.sp.lo.ndf.female  & !d.reg.in.variable.redfd)
    
    cnd.sp.hi.ndf.male <- (cnd.sp.hi.ndf.male  & !d.reg.in.variable.redfd)
    cnd.sp.hi.ndf.female <- (cnd.sp.hi.ndf.female  & !d.reg.in.variable.redfd)
    
    cnd.gt.lo.ndf.male <- (cnd.gt.lo.ndf.male  & !d.reg.in.variable.redfd)
    cnd.gt.lo.ndf.female <- (cnd.gt.lo.ndf.female  & !d.reg.in.variable.redfd)
    
    cnd.gt.hi.ndf.male <- (cnd.gt.hi.ndf.male  & !d.reg.in.variable.redfd)
    cnd.gt.hi.ndf.female <- (cnd.gt.hi.ndf.female  & !d.reg.in.variable.redfd)
    
    cnd.bv.lo.ndf.Bind.male <- ( cnd.bv.lo.ndf.Bind.male & !d.reg.in.variable.redfd)
    cnd.bv.lo.ndf.Btau.male <- ( cnd.bv.lo.ndf.Btau.male & !d.reg.in.variable.redfd)
    
    cnd.bv.lo.ndf.Bind.female <- ( cnd.bv.lo.ndf.Bind.female & !d.reg.in.variable.redfd)
    cnd.bv.lo.ndf.Btau.female <- ( cnd.bv.lo.ndf.Btau.female & !d.reg.in.variable.redfd)
    
    cnd.bv.hi.ndf.Bind.male <- ( cnd.bv.hi.ndf.Bind.male & !d.reg.in.variable.redfd)
    cnd.bv.hi.ndf.Btau.male <- ( cnd.bv.hi.ndf.Btau.male & !d.reg.in.variable.redfd)
    
    cnd.bv.hi.ndf.Bind.female <- ( cnd.bv.hi.ndf.Bind.female & !d.reg.in.variable.redfd)
    cnd.bv.hi.ndf.Btau.female <- ( cnd.bv.hi.ndf.Btau.female & !d.reg.in.variable.redfd)
    
    
    
  }
  
  
  #summary(d.reg.in[cnd.sp.lo.ndf , 'ol.status'])
  #summary(d.reg.in[cnd.sp.hi.ndf , 'ol.status'])
  #summary(d.reg.in[cnd.gt.lo.ndf , 'ol.status'])
  #summary(d.reg.in[cnd.gt.hi.ndf , 'ol.status'])
  
  
{
    
    
    # Quantity of regression formats (independent of breed/ndf/sex etc samples being tested)
    n.mod.vers <-  n.mod.classes * n.mod.v.family * n.mod.v.boost.control.mstop * n.mod.v.boost.control.nu  
    
    n.mod.v.ids <- c()
    
    # Model class - e.g. mixed effect, linear gradient boosting , etc.
    # Model family - solver used , e.g gausiaan, Laplace, etc.
    # Mboost and nu - hyperparameters (GBR only)
    # Sexes - male or female
    
    count <- 1
    type.count <<- 1
    family.count <<- 1
    mstop.count <<- 1
    nu.count <<- 1
    sex.count <<- 1
    breed.count <<- 1
    
    str.filler <- '.'
    
# Model version ID: class-family-mstop-nu
for (t in 1  : n.mod.classes) { 
for (f in 1  :  n.mod.v.family) { 
for (m in 1  :  n.mod.v.boost.control.mstop) { 
for (n in 1 :   n.mod.v.boost.control.nu) {
for (s in 1 :   n.sexes) {
for (b in 1 :   n.breeds) {


          n.mod.v.ids[ count   ] <- str_c( t ,str.filler , f,  str.filler , m  , str.filler , n , str.filler , s , str.filler , b)
          
          count <- count + 1
          
          breed.count <- breed.count + 1}
          sex.count <- sex.count + 1}
          nu.count <- nu.count + 1}
          mstop.count <- mstop.count + 1}
          family.count <- family.count + 1 }
  
     }
    
  } # Define iterations for hyper parameter tuning
  
  
  
{
  n.rows <- n.folds  * n.ndf * n.sexes * n.breeds * n.mod.vers * n.mod.form  * n.species 
  
  
  col.k <- c(  rep ( seq(1,n.folds,by=1), n.ndf * n.sexes * n.breeds * n.species * n.mod.vers * n.mod.form  ))
  col.ndf <- c(  rep ( c( rep(ndf.levs[1] , n.folds  )  , rep(ndf.levs[2], n.folds  )) , n.species * n.mod.vers  * n.mod.form * n.sexes * n.breeds ))
  
  col.sexes <- rep ( c(rep( sex.levels[1]   , n.folds * n.ndf ) , rep( sex.levels[2]   , n.folds * n.ndf ))  , n.species * n.mod.vers * n.mod.form * n.breeds ) 
  col.breeds <- rep ( c(rep( breed.levels[1]  , n.folds * n.ndf * n.sexes ) , rep( breed.levels[2]   , n.folds * n.ndf * n.sexes  ))  , n.species * n.mod.vers * n.mod.form  ) 
  
  
  # If merging sexes, default to just sex == 1
  if ( merge.sexes ){ col.sexes <-  col.sexes[ which(col.sexes != 2) ]  } 
  


  col.mf <-  rep(1,   n.folds * n.ndf * n.sexes * n.breeds * n.mod.vers  ) 
  
  if (n.mod.form >1){ col.mf <- c(col.mf, rep(2,   n.folds * n.ndf * n.mod.vers  * n.sexes * n.breeds )  ) }
  if (n.mod.form >2){ col.mf <- c(col.mf, rep(3,   n.folds * n.ndf * n.mod.vers * n.sexes * n.breeds )  ) }
  if (n.mod.form >3){ col.mf <- c(col.mf, rep(4,   n.folds * n.ndf * n.mod.vers * n.sexes * n.breeds )  ) }
  if (n.mod.form >4){ col.mf <- c(col.mf, rep(5,   n.folds * n.ndf * n.mod.vers * n.sexes * n.breeds )  ) }
  if (n.mod.form >5){ col.mf <- c(col.mf, rep(6,   n.folds * n.ndf * n.mod.vers * n.sexes * n.breeds )  ) }
  if (n.mod.form >6){ col.mf <- c(col.mf, rep(7,   n.folds * n.ndf * n.mod.vers * n.sexes * n.breeds )  ) }
  if (n.mod.form >7){ col.mf <- c(col.mf, rep(8,   n.folds * n.ndf * n.mod.vers * n.sexes * n.breeds )  ) }
  if (n.mod.form >8){ col.mf <- c(col.mf, rep(9,   n.folds * n.ndf * n.mod.vers * n.sexes * n.breeds )  ) }
  if (n.mod.form >9){ col.mf <- c(col.mf, rep(10,   n.folds * n.ndf * n.mod.vers * n.sexes * n.breeds )  ) }
  
  col.mf <- c( rep(col.mf ,n.species) )
  
  
  if ( !LR.only ){ # If small ruminants
    
    col.species <- c( rep(species.sheep , n.rows/n.species )  )
    
  if (n.species == 2){ col.species <- c( col.species , rep(species.goat, n.rows/n.species) ) } 
  
  } else { # If large ruminants
    
    col.species <- c( rep(species.cattle , n.rows/n.species )  )
    
  }
  
  # Check to confirm that all columns are equal length
  length(  col.k  )
  length(  col.ndf  )
  length(  col.mf  )
  length(  col.species  )
  length(  col.sexes )
  length(  col.breeds )
  
  # Define column of model version IDs
  col.mod.vers <- c()
  seq.start.indx <- 1
  
  for (mv in 1:length(n.mod.v.ids)){
    
    #  mv <- 1
    
    sequence.2.add <- rep(n.mod.v.ids[mv], times = (n.folds * n.ndf ) )
    
    seq.end.indx <-  seq.start.indx + length( sequence.2.add  ) - 1
    
    col.mod.vers[ seq(from = seq.start.indx , to = (seq.end.indx ) )  ] <- sequence.2.add
    
    seq.start.indx <- seq.end.indx +1 
    
  }
  
  col.mod.vers <<- c( rep (col.mod.vers , n.mod.form  * n.species ) )
  col.mod.class <-  as.numeric(substr( col.mod.vers , start = 1, stop = 1))
  
  
  length(col.species)
  length(col.breeds)
  length(col.ndf )
  length(col.k)
  length(col.mod.vers)
  
  
  d.reg.out <- data.frame( matrix(NA, nrow = n.rows, ncol = 1) )
  

  for (r in 1:length( model.variables.all  )){
    
    var.name <- model.variables.all[r]
    
    d.reg.out[  ,  var.name  ] <- NA
    
  }
  
    d.reg.out[ , vn.iter.is.NULL ] <- FALSE
    
    d.reg.out[, vn.species ] <- col.species
    d.reg.out[, vn.breed ] <- col.breeds
    d.reg.out[, vn.ndf ] <- col.ndf
    
    d.reg.out[, vn.mod.vers  ] <- col.mod.vers
    d.reg.out[, vn.mod.class ] <- col.mod.class
    d.reg.out[, vn.mod.form ] <- col.mf
    d.reg.out[, vn.k ] <- col.k
    d.reg.out[, vn.sex ] <- col.sexes
    
    d.reg.out[ ,  vn.is.best.tier ] <- FALSE
    d.reg.out[ ,  vn.best.global ] <- FALSE
    d.reg.out[ ,  vn.best.in.formula] <- FALSE
    d.reg.out[ ,  vn.best.in.class] <- FALSE
    
    
    
  # For model class = 3 --> Mixed effect model, omit duplicates since there are no hyperparameters to tune
  

for (s in c(species.sheep , species.goat , species.cattle)){
    for ( f in seq(1,n.mod.form)  ){
       for (k in 1:n.folds ){
         for (n in c(ndf.lev.lo , ndf.lev.hi) ){
           for (s in c(1 , 2) ){
         
         print(paste(k))
         
         # test: f <- 1; k <- 1; s <- 'Sheep'
      
      cond <- (d.reg.out$mod.class == 3 & d.reg.out$mod.form == f & d.reg.out$fold == k & d.reg.out$species == s & d.reg.out$ndf == n & d.reg.out$sex == s)
      
      d.reg.out <- d.reg.out[!(cond & duplicated(cond)), ]
      
      
    }}}}}
    

    
} # Generate regression out dataframe (for storing regression results)
    
  
  # Define row conditions for regression dataframe
  reg.out.cond.shp.lo.ndf.male <- (d.reg.out$species == species.sheep & d.reg.out$ndf == ndf.lev.lo & d.reg.out$sex %in% sex.male  )
  reg.out.cond.shp.lo.ndf.female <- (d.reg.out$species == species.sheep & d.reg.out$ndf == ndf.lev.lo  & d.reg.out$sex %in% sex.female )
  
  reg.out.cond.shp.hi.ndf.male <- (d.reg.out$species == species.sheep & d.reg.out$ndf == ndf.lev.hi & d.reg.out$sex %in% sex.male  )
  reg.out.cond.shp.hi.ndf.female <- (d.reg.out$species == species.sheep & d.reg.out$ndf == ndf.lev.hi  & d.reg.out$sex %in% sex.female)
  
  reg.out.cond.gt.lo.ndf.male <- (d.reg.out$species == species.goat & d.reg.out$ndf == ndf.lev.lo & d.reg.out$sex %in% sex.male )
  reg.out.cond.gt.lo.ndf.female <- (d.reg.out$species == species.goat & d.reg.out$ndf == ndf.lev.lo  & d.reg.out$sex %in% sex.female)
  
  reg.out.cond.gt.hi.ndf.male <- (d.reg.out$species == species.goat & d.reg.out$ndf == ndf.lev.hi & d.reg.out$sex %in% sex.male )
  reg.out.cond.gt.hi.ndf.female <- (d.reg.out$species == species.goat & d.reg.out$ndf == ndf.lev.hi  & d.reg.out$sex %in% sex.female)
  
  
  # Bovines
  reg.out.cond.bv.lo.ndf.Bind.male <- (d.reg.out$species == species.cattle & d.reg.out$ndf == ndf.lev.lo & d.reg.out$sex %in% sex.male & d.reg.out$breed %in% breed.indig & !is.na(d.reg.out$breed) ) 
  reg.out.cond.bv.lo.ndf.Btau.male <- (d.reg.out$species == species.cattle & d.reg.out$ndf == ndf.lev.lo & d.reg.out$sex %in% sex.male & d.reg.out$breed %in% breed.cbred & !is.na(d.reg.out$breed))
  
  reg.out.cond.bv.lo.ndf.Bind.female <- (d.reg.out$species == species.cattle & d.reg.out$ndf == ndf.lev.lo & d.reg.out$sex %in% sex.female & d.reg.out$breed %in% breed.indig & !is.na(d.reg.out$breed))
  reg.out.cond.bv.lo.ndf.Btau.female <- (d.reg.out$species == species.cattle & d.reg.out$ndf == ndf.lev.lo & d.reg.out$sex %in% sex.female & d.reg.out$breed %in% breed.cbred & !is.na(d.reg.out$breed))
  
  reg.out.cond.bv.hi.ndf.Bind.male <- (d.reg.out$species == species.cattle & d.reg.out$ndf == ndf.lev.hi & d.reg.out$sex %in% sex.male & d.reg.out$breed %in% breed.indig & !is.na(d.reg.out$breed))
  reg.out.cond.bv.hi.ndf.Btau.male <- (d.reg.out$species == species.cattle & d.reg.out$ndf == ndf.lev.hi & d.reg.out$sex %in% sex.male & d.reg.out$breed %in% breed.cbred & !is.na(d.reg.out$breed))
  
  reg.out.cond.bv.hi.ndf.Bind.female <- (d.reg.out$species == species.cattle & d.reg.out$ndf == ndf.lev.hi & d.reg.out$sex %in% sex.female & d.reg.out$breed %in% breed.indig & !is.na(d.reg.out$breed))
  reg.out.cond.bv.hi.ndf.Btau.female <- (d.reg.out$species == species.cattle & d.reg.out$ndf == ndf.lev.hi & d.reg.out$sex %in% sex.female & d.reg.out$breed %in% breed.cbred & !is.na(d.reg.out$breed))
  
  # Reg in conditions

  
  

  
  # Bovines
  reg.in.cond.bv.lo.ndf.Bind.male <<- (d.reg.in$Species ==  species.cattle & d.reg.in$ndf == ndf.lev.lo & d.reg.in$sex %in% sex.male & d.reg.in$breed.type %in% breed.indig & !is.na(d.reg.in$breed.type))
  reg.in.cond.bv.lo.ndf.Btau.male <<- (d.reg.in$Species ==  species.cattle & d.reg.in$ndf == ndf.lev.lo & d.reg.in$sex %in% sex.male & d.reg.in$breed.type %in% breed.cbred & !is.na(d.reg.in$breed.type))
  
  reg.in.cond.bv.lo.ndf.Bind.female <<- (d.reg.in$Species ==  species.cattle & d.reg.in$ndf == ndf.lev.lo & d.reg.in$sex %in% sex.female & d.reg.in$breed.type %in% breed.indig & !is.na(d.reg.in$breed.type))
  reg.in.cond.bv.lo.ndf.Btau.female <<-(d.reg.in$Species ==  species.cattle & d.reg.in$ndf == ndf.lev.lo & d.reg.in$sex %in% sex.female & d.reg.in$breed.type %in% breed.cbred & !is.na(d.reg.in$breed.type))
  
  reg.in.cond.bv.hi.ndf.Bind.male <<- (d.reg.in$Species ==  species.cattle & d.reg.in$ndf == ndf.lev.hi & d.reg.in$sex %in% sex.male & d.reg.in$breed.type %in% breed.indig & !is.na(d.reg.in$breed.type))
  reg.in.cond.bv.hi.ndf.Btau.male <<- (d.reg.in$Species ==  species.cattle & d.reg.in$ndf == ndf.lev.hi & d.reg.in$sex %in% sex.male & d.reg.in$breed.type %in% breed.cbred & !is.na(d.reg.in$breed.type))
  
  reg.in.cond.bv.hi.ndf.Bind.female <<- (d.reg.in$Species ==  species.cattle & d.reg.in$ndf == ndf.lev.hi & d.reg.in$sex %in% sex.female & d.reg.in$breed.type %in% breed.indig & !is.na(d.reg.in$breed.type))
  reg.in.cond.bv.hi.ndf.Btau.female <<- (d.reg.in$Species ==  species.cattle & d.reg.in$ndf == ndf.lev.hi & d.reg.in$sex %in% sex.female & d.reg.in$breed.type %in% breed.cbred & !is.na(d.reg.in$breed.type))
  
  
  # Reg out row conditions
  d.reg.out[ reg.out.cond.shp.lo.ndf.male , 'r.cond'] <- listify( cnd.sp.lo.ndf.male)
  d.reg.out[ reg.out.cond.shp.lo.ndf.female , 'r.cond'] <- listify( cnd.sp.lo.ndf.female)
  
  d.reg.out[ reg.out.cond.shp.hi.ndf.male , 'r.cond'] <- listify( cnd.sp.hi.ndf.male)
  d.reg.out[ reg.out.cond.shp.hi.ndf.female , 'r.cond'] <-listify( cnd.sp.hi.ndf.female)
  
  d.reg.out[ reg.out.cond.gt.lo.ndf.male , 'r.cond'] <- listify( cnd.sp.lo.ndf.male)
  d.reg.out[ reg.out.cond.gt.lo.ndf.female , 'r.cond'] <- listify( cnd.sp.lo.ndf.female)
  
  d.reg.out[ reg.out.cond.gt.hi.ndf.male , 'r.cond'] <- listify( cnd.sp.hi.ndf.male)
  d.reg.out[ reg.out.cond.gt.hi.ndf.female , 'r.cond'] <-listify( cnd.sp.hi.ndf.female)
  
  # Bovines
  d.reg.out[ reg.out.cond.bv.lo.ndf.Bind.male, 'r.cond'] <-  listify( cnd.bv.lo.ndf.Bind.male  )
  d.reg.out[ reg.out.cond.bv.lo.ndf.Btau.male, 'r.cond'] <-  listify( cnd.bv.lo.ndf.Btau.male  )
  
  d.reg.out[ reg.out.cond.bv.lo.ndf.Bind.female, 'r.cond'] <-  listify( cnd.bv.lo.ndf.Bind.female  )
  d.reg.out[ reg.out.cond.bv.lo.ndf.Btau.female, 'r.cond'] <-  listify( cnd.bv.lo.ndf.Btau.female  )
  
  d.reg.out[ reg.out.cond.bv.hi.ndf.Bind.male, 'r.cond'] <-  listify( cnd.bv.hi.ndf.Bind.male  )
  d.reg.out[ reg.out.cond.bv.hi.ndf.Btau.male, 'r.cond'] <-  listify( cnd.bv.hi.ndf.Btau.male  )
  
  d.reg.out[ reg.out.cond.bv.hi.ndf.Bind.female, 'r.cond'] <-  listify( cnd.bv.hi.ndf.Bind.female  )
  d.reg.out[ reg.out.cond.bv.hi.ndf.Btau.female, 'r.cond'] <-  listify( cnd.bv.hi.ndf.Btau.female  )
  
  
  
  ue.ids.sp.lo.ndf.male <- unique(d.reg.in[ cnd.sp.lo.ndf.male , 'ue.id'])
  ue.ids.sp.lo.ndf.female <- unique(d.reg.in[ cnd.sp.lo.ndf.female , 'ue.id'])
  
  ue.ids.sp.hi.ndf.male <- unique(d.reg.in[ cnd.sp.hi.ndf.male , 'ue.id'])
  ue.ids.sp.hi.ndf.female <- unique(d.reg.in[ cnd.sp.hi.ndf.female , 'ue.id'])
  
  ue.ids.gt.lo.ndf.male <- unique(d.reg.in[ cnd.gt.lo.ndf.male , 'ue.id'])
  ue.ids.gt.lo.ndf.female <- unique(d.reg.in[ cnd.gt.lo.ndf.female , 'ue.id'])
  
  ue.ids.gt.hi.ndf.male <- unique(d.reg.in[ cnd.gt.hi.ndf.male , 'ue.id'])
  ue.ids.gt.hi.ndf.female <- unique(d.reg.in[ cnd.gt.hi.ndf.female , 'ue.id'])

  ue.ids.bv.lo.ndf.Bind.male <- unique(d.reg.in[ cnd.bv.lo.ndf.Bind.male , 'ue.id'])
  ue.ids.bv.lo.ndf.Btau.male <- unique(d.reg.in[ cnd.bv.lo.ndf.Btau.male , 'ue.id'])
  
  ue.ids.bv.lo.ndf.Bind.female <- unique(d.reg.in[ cnd.bv.lo.ndf.Bind.female , 'ue.id'])
  ue.ids.bv.lo.ndf.Btau.female <- unique(d.reg.in[ cnd.bv.lo.ndf.Btau.female , 'ue.id'])
  
  ue.ids.bv.hi.ndf.Bind.male <- unique(d.reg.in[ cnd.bv.hi.ndf.Bind.male , 'ue.id'])
  ue.ids.bv.hi.ndf.Btau.male <- unique(d.reg.in[ cnd.bv.hi.ndf.Btau.male , 'ue.id'])
  
  ue.ids.bv.hi.ndf.Bind.female <- unique(d.reg.in[ cnd.bv.hi.ndf.Bind.female , 'ue.id'])
  ue.ids.bv.hi.ndf.Btau.female <- unique(d.reg.in[ cnd.bv.hi.ndf.Btau.female , 'ue.id'])
  

  
  # Treatment Id lists
  d.reg.out[reg.out.cond.shp.lo.ndf.male  , vn.all.treatment.IDs] <- listify(  unique(d.reg.in[ cnd.sp.lo.ndf.male , 'ut.id'])  )
  d.reg.out[reg.out.cond.shp.lo.ndf.female  ,vn.all.treatment.IDs] <- listify( unique(d.reg.in[ cnd.sp.lo.ndf.female , 'ut.id'])  )
  
  d.reg.out[reg.out.cond.shp.hi.ndf.male  , vn.all.treatment.IDs] <- listify( unique(d.reg.in[ cnd.sp.hi.ndf.male , 'ut.id'])  )
  d.reg.out[reg.out.cond.shp.hi.ndf.female  , vn.all.treatment.IDs] <- listify( unique(d.reg.in[ cnd.sp.hi.ndf.female , 'ut.id'])  )
  
  d.reg.out[reg.out.cond.gt.lo.ndf.male  , vn.all.treatment.IDs] <- listify( unique(d.reg.in[ cnd.gt.lo.ndf.male , 'ut.id'])  )
  d.reg.out[reg.out.cond.gt.lo.ndf.female  , vn.all.treatment.IDs] <- listify(  unique(d.reg.in[ cnd.gt.lo.ndf.female , 'ut.id'])  )
  
  d.reg.out[reg.out.cond.gt.hi.ndf.male  , vn.all.treatment.IDs] <- listify(  unique(d.reg.in[ cnd.gt.hi.ndf.male , 'ut.id'])  )
  d.reg.out[reg.out.cond.gt.hi.ndf.female  , vn.all.treatment.IDs] <- listify( unique(d.reg.in[ cnd.gt.hi.ndf.female , 'ut.id'])  )
  
  # Bovines
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male  , vn.all.treatment.IDs] <- listify(  unique(d.reg.in[ cnd.bv.lo.ndf.Bind.male , 'ut.id'])  )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male  , vn.all.treatment.IDs] <- listify(  unique(d.reg.in[ cnd.bv.lo.ndf.Btau.male , 'ut.id'])  )
  
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female  , vn.all.treatment.IDs] <- listify(  unique(d.reg.in[ cnd.bv.lo.ndf.Bind.female , 'ut.id'])  )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female  , vn.all.treatment.IDs] <- listify(  unique(d.reg.in[ cnd.bv.lo.ndf.Btau.female , 'ut.id'])  )
  
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male  , vn.all.treatment.IDs] <- listify(  unique(d.reg.in[ cnd.bv.hi.ndf.Bind.male , 'ut.id'])  )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male  , vn.all.treatment.IDs] <- listify(  unique(d.reg.in[ cnd.bv.hi.ndf.Btau.male , 'ut.id'])  )
  
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female  , vn.all.treatment.IDs] <- listify(  unique(d.reg.in[ cnd.bv.hi.ndf.Bind.female , 'ut.id'])  )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female  , vn.all.treatment.IDs] <- listify(  unique(d.reg.in[ cnd.bv.hi.ndf.Btau.female , 'ut.id'])  )
  

 
  # Experiment Id lists
  d.reg.out[reg.out.cond.shp.lo.ndf.male  , vn.all.experiment.IDs] <- listify(unique(d.reg.in[ cnd.sp.lo.ndf.male , 'ue.id'])  )
  d.reg.out[reg.out.cond.shp.lo.ndf.female  , vn.all.experiment.IDs] <- listify(unique(d.reg.in[ cnd.sp.lo.ndf.female , 'ue.id'])  )
  
  d.reg.out[reg.out.cond.shp.hi.ndf.male  , vn.all.experiment.IDs] <- listify(unique(d.reg.in[ cnd.sp.hi.ndf.male , 'ue.id'])  )
  d.reg.out[reg.out.cond.shp.hi.ndf.female  , vn.all.experiment.IDs] <- listify(unique(d.reg.in[ cnd.sp.hi.ndf.female , 'ue.id'])  )
  
  d.reg.out[reg.out.cond.gt.lo.ndf.male  , vn.all.experiment.IDs] <- listify(unique(d.reg.in[ cnd.gt.lo.ndf.male , 'ue.id'])  )
  d.reg.out[reg.out.cond.gt.lo.ndf.female  , vn.all.experiment.IDs] <- listify(unique(d.reg.in[ cnd.gt.lo.ndf.female , 'ue.id'])  )
  
  d.reg.out[reg.out.cond.gt.hi.ndf.male  , vn.all.experiment.IDs] <- listify(unique(d.reg.in[ cnd.gt.hi.ndf.male , 'ue.id'])  )
  d.reg.out[reg.out.cond.gt.hi.ndf.female  , vn.all.experiment.IDs] <- listify(unique(d.reg.in[ cnd.gt.hi.ndf.female , 'ue.id'])  )
  
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male  , vn.all.experiment.IDs ] <- listify(  unique(d.reg.in[ cnd.bv.lo.ndf.Bind.male , 'ue.id'])  )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male  , vn.all.experiment.IDs ] <- listify(  unique(d.reg.in[ cnd.bv.lo.ndf.Btau.male , 'ue.id'])  )
  
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female  , vn.all.experiment.IDs ] <- listify(  unique(d.reg.in[ cnd.bv.lo.ndf.Bind.female , 'ue.id'])  )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female  , vn.all.experiment.IDs ] <- listify(  unique(d.reg.in[ cnd.bv.lo.ndf.Btau.female , 'ue.id'])  )
  
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male  , vn.all.experiment.IDs ] <- listify(  unique(d.reg.in[ cnd.bv.hi.ndf.Bind.male , 'ue.id'])  )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male  , vn.all.experiment.IDs ] <- listify(  unique(d.reg.in[ cnd.bv.hi.ndf.Btau.male , 'ue.id'])  )
  
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female  , vn.all.experiment.IDs ] <- listify(  unique(d.reg.in[ cnd.bv.hi.ndf.Bind.female , 'ue.id'])  )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female  , vn.all.experiment.IDs ] <- listify(  unique(d.reg.in[ cnd.bv.hi.ndf.Btau.female , 'ue.id'])  )
  
  
  
  # Sample sizes per species-ndf level
  d.reg.out[ reg.out.cond.shp.lo.ndf.male , vn.total.trt.sample.size    ] <- length(unique(d.reg.in[ cnd.sp.lo.ndf.male , 'ut.id']))
  d.reg.out[ reg.out.cond.shp.lo.ndf.female , vn.total.trt.sample.size   ] <- length(unique(d.reg.in[ cnd.sp.lo.ndf.female , 'ut.id']))
  
  d.reg.out[ reg.out.cond.shp.hi.ndf.male , vn.total.trt.sample.size   ] <- length(unique(d.reg.in[ cnd.sp.hi.ndf.male , 'ut.id']))
  d.reg.out[ reg.out.cond.shp.hi.ndf.female , vn.total.trt.sample.size   ] <- length(unique(d.reg.in[ cnd.sp.hi.ndf.female , 'ut.id']))
  
  d.reg.out[ reg.out.cond.gt.lo.ndf.male , vn.total.trt.sample.size   ] <- length(unique(d.reg.in[ cnd.gt.lo.ndf.male , 'ut.id']))
  d.reg.out[ reg.out.cond.gt.lo.ndf.female , vn.total.trt.sample.size   ] <- length(unique(d.reg.in[ cnd.gt.lo.ndf.female , 'ut.id']))
  
  d.reg.out[ reg.out.cond.gt.hi.ndf.male ,vn.total.trt.sample.size   ] <- length(unique(d.reg.in[ cnd.gt.hi.ndf.male , 'ut.id']))
  d.reg.out[ reg.out.cond.gt.hi.ndf.female , vn.total.trt.sample.size   ] <- length(unique(d.reg.in[ cnd.gt.hi.ndf.female , 'ut.id']))
  
  
  # Bovines
  d.reg.out[ reg.out.cond.bv.lo.ndf.Bind.male , vn.total.trt.sample.size ] <- length(unique(d.reg.in[ cnd.bv.lo.ndf.Bind.male , 'ut.id']))
  d.reg.out[ reg.out.cond.bv.lo.ndf.Btau.male , vn.total.trt.sample.size ] <- length(unique(d.reg.in[ cnd.bv.lo.ndf.Btau.male , 'ut.id']))
  
  d.reg.out[ reg.out.cond.bv.lo.ndf.Bind.female , vn.total.trt.sample.size ] <- length(unique(d.reg.in[ cnd.bv.lo.ndf.Bind.female , 'ut.id']))
  d.reg.out[ reg.out.cond.bv.lo.ndf.Btau.female , vn.total.trt.sample.size ] <- length(unique(d.reg.in[ cnd.bv.lo.ndf.Btau.female , 'ut.id']))
  
  d.reg.out[ reg.out.cond.bv.hi.ndf.Bind.male , vn.total.trt.sample.size ] <- length(unique(d.reg.in[ cnd.bv.hi.ndf.Bind.male , 'ut.id']))
  d.reg.out[ reg.out.cond.bv.hi.ndf.Btau.male , vn.total.trt.sample.size ] <- length(unique(d.reg.in[ cnd.bv.hi.ndf.Btau.male , 'ut.id']))
  
  d.reg.out[ reg.out.cond.bv.hi.ndf.Bind.female , vn.total.trt.sample.size ] <- length(unique(d.reg.in[ cnd.bv.hi.ndf.Bind.female , 'ut.id']))
  d.reg.out[ reg.out.cond.bv.hi.ndf.Btau.female ,vn.total.trt.sample.size ] <- length(unique(d.reg.in[ cnd.bv.hi.ndf.Btau.female , 'ut.id']))
  

  
  # Sample sizes
  for ( r in 1:nrow(d.reg.out)) {
    
    ids <- de.listify( d.reg.out[r, 'experiment.IDs'] ) 
    
    weight.list <- 0
    
    for (id in ids) { 
      
      weight <- unique(d.reg.in[ d.reg.in$ue.id == id, 'Sample.size'])
      
      weight.list <- append (weight.list , weight )
      
    }
    
    weight.list <- weight.list[-c(1)]
    
    d.reg.out[ r, 'weight.list'] <- listify(weight.list)
    
    
    
  }
  
 # for (r in 1:nrow(d.reg.out)) { d.reg.out[ r, 'IDs.remaining'] <- list(list( d.reg.out[ r, 'treatment.IDs'])) }

  
  
  
  
if (reg.wt.method.null) { d.reg.in$reg.weight <- d.reg.in$reg.wt.null } 
if (reg.wt.method.cubed.root) { d.reg.in$reg.weight <- d.reg.in$reg.wt.cubed.root } 
if (reg.wt.method.log) { d.reg.in$reg.weight <- d.reg.in$reg.wt.log } 
if (reg.wt.method.sqrt) { d.reg.in$reg.weight <- d.reg.in$reg.wt.sqr } 
if (reg.wt.method.quartic.root) { d.reg.in$reg.weight <- d.reg.in$reg.wt.quartic.root } 
  
  
  d.reg.out$tier <- NA
  
  d.reg.out[d.reg.out$mod.form %in% mod.tier.1 , 'tier'] <- 1
  d.reg.out[d.reg.out$mod.form %in% mod.tier.2 , 'tier'] <- 2
  d.reg.out[d.reg.out$mod.form %in% mod.tier.3 , 'tier'] <- 3
  
  
  
  
  d.reg.out <- d.reg.out[ , -c(1)]
  
  d.reg.out[, 'loop.iter' ] <- seq(1, nrow(  d.reg.out))

  d.reg.out <<-  d.reg.out
  
  d.reg.out.null <<- d.reg.out
  
} # Data pre processing


