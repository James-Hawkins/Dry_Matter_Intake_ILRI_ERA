

# source('helpers/functions.R')



listify <<- function( d ){
  
  
  return (  list(list(d)) )

}

de.listify <<- function( d ){
  
  return (  d[[1]] )
  
}



gen.gg.df.specific <<- function(  cur.mod , mc , species , breed , ndf , sex){
  
  # cur.mod <- 2; vers <- 'pmetric' ; mc <- 1 ; species <- species.cattle ; breed <- breed.indig
  
  # cur.mod <- 1 ; vers <- 'n.pmetric'
  
  
  test.sp.lon <- function(){
    
    species <- species.cattle
    ndf <- ndf.lev.hi
    cur.mod <- 7; mc <- 1
    
    sex <- 2
    breed <- 1
    
  }
  
  test.sp.hin <- function(){
    
    species <- species.sheep
    ndf <- ndf.lev.hi
    cur.mod <- 1 ; mt <- 1
    
    gbm.cond <- reg.out.cond.shp.hi.ndf
    
  }
  
  # Params
  
  vers <- "pmetric"
  

  if (  cur.mod %in%  c(1,2,3)   ){ current.tier <- tier.label.1 }
  if (  cur.mod %in%  c(4,5,6)   ){ current.tier <-  tier.label.2 }
  if (  cur.mod %in%  c(7,8,9)   ){ current.tier <- tier.label.3 }
  
  
  if (species == species.sheep & ndf == ndf.lev.lo & sex %in% sex.male ) {   ss.cond<- reg.out.cond.shp.lo.ndf.male }
  if (species == species.sheep & ndf == ndf.lev.lo & sex %in% sex.female ) {   ss.cond <- reg.out.cond.shp.lo.ndf.female }
  
  if (species == species.sheep & ndf == ndf.lev.hi & sex %in% sex.male ) {   ss.cond <- reg.out.cond.shp.hi.ndf.male }
  if (species == species.sheep & ndf == ndf.lev.hi & sex %in% sex.female) {     ss.cond<- reg.out.cond.shp.hi.ndf.female }
  
  if (species == species.goat & ndf == ndf.lev.lo & sex %in% sex.male) {     ss.cond<- reg.out.cond.gt.lo.ndf.male }
  if (species == species.goat & ndf == ndf.lev.lo & sex %in% sex.female) {    ss.cond <- reg.out.cond.gt.lo.ndf.female }
  
  if (species == species.goat & ndf == ndf.lev.hi & sex %in% sex.male) {   ss.cond<- reg.out.cond.gt.hi.ndf.male }
  if (species == species.goat & ndf == ndf.lev.hi & sex %in% sex.female) {     ss.cond <- reg.out.cond.gt.hi.ndf.female }
  
  
  # Bovines
  if (  species == species.cattle & ndf == ndf.lev.lo & breed %in% breed.indig & sex %in% sex.male  ) {    ss.cond <- reg.out.cond.bv.lo.ndf.Bind.male  }
  if (  species == species.cattle & ndf == ndf.lev.lo & breed %in% breed.indig & sex %in% sex.female  ) {    ss.cond <- reg.out.cond.bv.lo.ndf.Bind.female }
  
  if (  species == species.cattle & ndf == ndf.lev.lo & breed %in% breed.cbred & sex %in% sex.male  ) {   ss.cond <- reg.out.cond.bv.lo.ndf.Btau.male }
  if (  species == species.cattle & ndf == ndf.lev.lo & breed %in% breed.cbred & sex %in% sex.female  ) {    ss.cond <- reg.out.cond.bv.lo.ndf.Btau.female }
  
  if (  species == species.cattle & ndf == ndf.lev.hi & breed %in% breed.indig & sex %in% sex.male  ) {     ss.cond <- reg.out.cond.bv.hi.ndf.Bind.male }
  if (  species == species.cattle & ndf == ndf.lev.hi & breed %in% breed.indig & sex %in% sex.female  ) {  ss.cond <- reg.out.cond.bv.hi.ndf.Bind.female }
  
  if (species == species.cattle & ndf == ndf.lev.hi & breed %in% breed.cbred & sex %in% sex.male) {    ss.cond <- reg.out.cond.bv.hi.ndf.Btau.male }
  if (species == species.cattle & ndf == ndf.lev.hi & breed %in% breed.cbred & sex %in% sex.female) {   ss.cond <- reg.out.cond.bv.hi.ndf.Btau.female }
  
  
  
  r.cond <-  ss.cond & d.reg.out$mod.form == cur.mod 
  
  if  (  !any(  r.cond )   ) {return (NULL)}
  
  {
    
    gg.dat.nrow <- unique( d.reg.out[ 
      
      (
        r.cond 
        #    & d.reg.out$mod.class == mc 
           & d.reg.out$is.best.model.in.formula 
      )
      
      ,   vn.total.trt.sample.size]) 
    
    
    
    
    tier.rank <- unique( d.reg.out[
      
      (
        r.cond 
        & d.reg.out$is.best.model.in.formula
      )
      
      
      , vn.tier.rank  ]
    
      )
    
    if  (   length(tier.rank) == 0   ) {return (NULL)}
    if  (   is.na(tier.rank)   ) {return (NULL)}
    
    #  if (  is.na(tier.rank)  ) { tier.rank <- 3 } # update this
    
    
    # Figure display items
    reg.out.cond.opt.mod <- (  
      r.cond 
      & d.reg.out$is.best.model.in.formula 
      )
    
    
    #  Get model class of best performing formula
    mc <- d.reg.out[  reg.out.cond.opt.mod , 'mod.class' ][1]
    
    mod.form <-  de.listify( d.reg.out[   reg.out.cond.opt.mod,  vn.formula ] )
    
    mod.form.label.w.eqn <- de.listify( gen.formula.label.1r( mod.form ) )
    
    # Treatment IDs
    mod.1.u.tid <- de.listify( d.reg.out[  
      reg.out.cond.opt.mod  
      ,    vn.all.treatment.IDs ] )
    
    mod.1.ccc <-  round( (  d.reg.out[  reg.out.cond.opt.mod  ,  vn.ws.w.CCC ]  ) , rd.decs.R2)[3]  
    
    mod.1.r2 <-  round( (  d.reg.out[  reg.out.cond.opt.mod  & !is.na(d.reg.out$w.R2.mean) ,  vn.ws.w.R2 ]  ) , rd.decs.R2)[1] 
    
    mod.1.r2.ucb <-  round( (d.reg.out[  reg.out.cond.opt.mod  & !is.na(d.reg.out$w.R2.mean) ,   vn.w.R2.ucb  ]) , rd.decs.R2)[1] 
    
    mod.1.nRMSE <-  round( (  d.reg.out[  reg.out.cond.opt.mod  & !is.na(d.reg.out$w.nRMSE.mean) ,   vn.ws.w.nRMSE ] ) , rd.decs.nRMSE )[1]
    
    
    gg.dat.labl.ccc <- paste0("\u03c1 = ",   mod.1.ccc )
    
    
    gg.dat.labl.R2 <- paste0("Adj.R", supsc("2") ,   "= ",  mod.1.r2 )
    
    
    gg.dat.labl.R2.ucb <-   paste0("Adj.R\u1D64",  "\u00B2" , " = " ,      mod.1.r2.ucb )
    
    
    gg.dat.labl.nRMSE <- paste0("nRMSE(%) = ",  mod.1.nRMSE )
    
    
    experiment.sample <- ( de.listify( d.reg.out[  reg.out.cond.opt.mod,  vn.total.exp.sample.size ][1]))
    
    treatment.sample <- (de.listify(d.reg.out[  reg.out.cond.opt.mod ,  vn.total.trt.sample.size][1]))
    
    animal.sample <- (de.listify( d.reg.out[  reg.out.cond.opt.mod ,  vn.total.animal.sample.size ][1]))
    
    mod.id <- gen.gg.model.label(  tier.rank, cur.mod , mc)
    
    model.label <- str_c( mod.id ) 
    
    gg.dat.fact.labl.r1.mod <- model.label 
    
    gg.dat.fact.labl.r2 <- str_c( mod.form.label.w.eqn )
    
    gg.dat.fact.labl.r3 <- str_c( 
      lab.total.experiment.sample , ' = ' ,  experiment.sample
      , '    ' ,  lab.total.treatment.sample , ' = ' ,     treatment.sample
      , '    ' , lab.total.animal.sample , ' = ' ,  animal.sample
    )
    
    

    
    
  } # Plot data prep
  
  
{
    
    col.mod.form <- c(  
      rep( cur.mod, times = (gg.dat.nrow ) )   
    ) 
    
    col.mod.form.label.r1 <- c(  
      rep(     gg.dat.fact.labl.r1.mod , times = (gg.dat.nrow  ) ) 
    )
    
    col.mod.form.label.r2 <- c(  
      rep(   gg.dat.fact.labl.r2 , times = (gg.dat.nrow   ) ) 
    )
    
    col.mod.form.label.r3 <- c(  
      rep(   gg.dat.fact.labl.r3  , times = (gg.dat.nrow   ) ) 
    )
    
    
    col.species <- c(  
      rep( species , times = (gg.dat.nrow ) )  
    ) 
    
    col.ndf <- c(  
      rep( ndf , times = ( gg.dat.nrow  ) ) 
    )
    
    col.tier.rank <- c(  
      rep( tier.rank , times = ( gg.dat.nrow  ) ) 
    )

    
    col.mod.tier <- c(  
      rep( current.tier , times = (gg.dat.nrow   ) ) 
    )
    
    
    #  col.sample.size <- c(  
    #  rep( species.ndf , times = (gg.dat.nrow  ) ) 
    #  )
    
    col.formula  <- c(  
      rep( cur.mod , times = (gg.dat.nrow ) )  
    ) 
    
    col.residual  <- c(
      de.listify(   d.reg.out[reg.out.cond.opt.mod ,  vn.ws.residuals ][1][1]   )  
    )
    

    col.modelled  <- c(  
      
      as.numeric(  de.listify(  d.reg.out[  reg.out.cond.opt.mod ,   vn.ws.predicted.base ] )  )  
      
    )
    

    col.observed  <- c(
      de.listify(d.reg.out[reg.out.cond.opt.mod  ,   vn.ws.observed ][1][1] )  
    )
    
    col.bw_kg <- c(
      
      as.numeric(  de.listify(   d.reg.out[reg.out.cond.opt.mod ,  'ws.bw_kg.measrs' ][1][1] ))  
      
    ) 
    
    col.label.ut.id <- c(
      mod.1.u.tid
    ) 
    
    col.label.ccc  <- c(
      rep( gg.dat.labl.ccc , times = (gg.dat.nrow   ) ) 
    )
    
    col.label.r2  <- c(
      rep( gg.dat.labl.R2 , times = (gg.dat.nrow   ) ) 
    )
    
    col.label.r2.ucb  <- c(
      rep( gg.dat.labl.R2.ucb , times = (gg.dat.nrow   ) ) 
    )
    
    col.label.nrmse  <- c(
      rep( gg.dat.labl.nRMSE , times = (gg.dat.nrow   ) ) 
    )
    
    if (  mc == model.class.GBR | mc == model.class.ME  ){
      
      
      col.coef.bw_kg <- c( 
        rep( d.reg.out[reg.out.cond.opt.mod ,  'ws.coef.bw_kg' ][1] , times = gg.dat.nrow )  
      )
      
      col.coef.intercept <- c( 
        rep( d.reg.out[   reg.out.cond.opt.mod ,  'ws.coef.intercept' ][1] , times = gg.dat.nrow)    
        
      )
      
    }
    
    
    
  } # Pre-define dataframe columns
  
  length(col.species)
  
  length(col.label.nrmse)
  length(col.label.r2)
  length(col.label.ut.id)
  length(col.ndf)
  length(col.observed)
  length(col.modelled)
  length(col.residual)
  
  
  
  gg.dat <- data.frame(
    
    
    col.mod.form =  col.mod.form
    
    ,  col.mod.form.label.r1 =  col.mod.form.label.r1
    
    ,  col.mod.form.label.r2 =  col.mod.form.label.r2
    
    ,  col.mod.form.label.r3 =  col.mod.form.label.r3
    
    , species   = col.species
    
    , ndf     = col.ndf
    
    ,  mod.tier = col.mod.tier 
    
    , tier.rank =   col.tier.rank
    
    , observed =   col.observed
    
    , modelled =  col.modelled
    
    ,  residual =   col.residual
    
    # ,  rms.residual.1 =   col.rms.residual
    
    #, bw_kg = col.bw_kg
    
    
    , label.ut.id = col.label.ut.id 
    
    , label.ccc = col.label.ccc
    
    , label.r2 = col.label.r2
    
    , label.r2.ucb = col.label.r2.ucb
    
    , label.nrmse = col.label.nrmse
    
  )
  
  if ( vers == "pmetric") { 
    
    gg.dat$coef.bw_kg <- col.coef.bw_kg 
    gg.dat$coef.intercept <- col.coef.intercept
    
    #gg.dat$label.form.r1 <- col.label.r1.form
    # gg.dat$label.form.r2 <- col.label.r2.form
    # gg.dat$label.form.r3 <- col.label.r3.form
    
  }
  
  
  return ( gg.dat )
  
}




gen.upper.confidence.bound <<- function( mean, sd  ){
  
  # test: mean <- d.reg.out[r.cnd.ss.mf.mv, vn.R2.mean ] ; sd <- d.reg.out[r.cnd.ss.mf.mv, vn.R2.sd ] 
  
   
  ucb <- mean +   optimization.metric.c.param  * sd
  
  return (ucb)
  
}

model.sum <<- function( d.frame , c.sp, c.ndf , c.stage){

#d.frame <- stepw.out.adt.sheep.hi.NDF ; c.sp <- species.sheep ; c.ndf <- ndf.lev.hi ; c.stage <- stage.growing.all
#d.frame <- stepw.out.gr.goats.lo.NDF   ; c.sp <- species.goat ; c.ndf <- ndf.lev.lo ; c.stage <- stage.growing.all
 
#d.frame <- stepw.out.gr.sheep.hi.NDF   ; c.sp <- species.sheep ; c.ndf <- ndf.lev.hi ; c.stage <- stage.growing.all
  
  
  
  
if (c.sp == species.sheep & c.ndf == ndf.lev.lo & c.stage == stage.growing.all) { d.frame <- stepw.out.gr.sheep.lo.NDF }
if (c.sp == species.sheep & c.ndf == ndf.lev.hi & c.stage == stage.growing.all) { d.frame <- stepw.out.gr.sheep.hi.NDF }
if (c.sp == species.goat & c.ndf == ndf.lev.lo & c.stage == stage.growing.all) { d.frame <- stepw.out.gr.goats.lo.NDF }
if (c.sp == species.goat & c.ndf == ndf.lev.hi & c.stage == stage.growing.all) { d.frame <- stepw.out.gr.goats.hi.NDF }

if (c.sp == species.sheep & c.ndf == ndf.lev.lo & c.stage == stage.adult.all) { d.frame <- stepw.out.adt.sheep.lo.NDF }
if (c.sp == species.sheep & c.ndf == ndf.lev.hi & c.stage == stage.adult.all) { d.frame <- stepw.out.adt.sheep.hi.NDF }
if (c.sp == species.goat & c.ndf == ndf.lev.lo & c.stage == stage.adult.all) { d.frame <- stepw.out.adt.goats.lo.NDF }
if (c.sp == species.goat & c.ndf == ndf.lev.hi & c.stage == stage.adult.all) { d.frame <- stepw.out.adt.goats.hi.NDF }

  

# Selection criteria 1  - normalized variables
n.var.cond <- (d.frame$normalized.vars & !is.na(d.frame$normalized.vars ))
min.aic.n.cond <-  min(d.frame[n.var.cond,'aic'])
min.bic.n.cond   <-  min(d.frame[n.var.cond,'bic'])
min.rmse.n.cond   <-  min(d.frame[n.var.cond,'rmse'])
max.r2.all.fx.n.cond   <-  max(d.frame[n.var.cond,'r2.cnd'])
max.r2.ffx.n.cond  <-  max(d.frame[n.var.cond,'r2.mrg'])  

rows.min.aic.n.cond <- which ( min.aic.n.cond == d.frame[,'aic'] )
rows.min.bic.n.cond<- which ( min.bic.n.cond == d.frame[,'bic'] )
rows.min.rmse.n.cond <- which ( min.rmse.n.cond == d.frame[,'rmse'] )
rows.max.r2.all.fx.n.cond  <- which ( max.r2.all.fx.n.cond == d.frame[,'r2.cnd'] )
rows.max.r2.ffx.n.cond <- which ( max.r2.ffx.n.cond == d.frame[,'r2.mrg'] )


#best.row.glob.2 <- rows.min.aic.glob 

  
# Selection criteria 2 -- optimal R2 (fixed effects)
min.aic.glob <-  min(d.frame[,'aic'])
min.bic.glob  <-  min(d.frame[,'bic'])
min.rmse.glob  <-  min(d.frame[,'rmse'])
max.r2.all.fx.glob  <-  max(d.frame[,'r2.cnd'])
max.r2.ffx.glob  <-  max(d.frame[,'r2.mrg'])  

rows.min.aic.glob <- which ( min.aic.glob == d.frame[,'aic'] )
rows.min.bic.glob <- which ( min.bic.glob == d.frame[,'bic'] )
rows.min.rmse.glob <- which ( min.rmse.glob == d.frame[,'rmse'] )
rows.max.r2.all.fx.glob  <- which ( max.r2.all.fx.glob == d.frame[,'r2.cnd'] )
rows.max.r2.ffx.glob <- which ( max.r2.ffx.glob == d.frame[,'r2.mrg'] )

# Selection criteria 3 -- best AIC
min.aic <-  min(d.frame[,'aic'])

rows.selc.2 <- which ( min.aic == d.frame[,'aic'] )


best.row.glob.2 <- rows.min.aic.glob 


if (selc.crit == selc.aic) { best.row.n.cond <- rows.min.aic.n.cond ; best.row.glob <- rows.min.aic.glob  }
if (selc.crit == selc.bic) {best.row.n.cond <- rows.min.bic.n.cond ; best.row.glob <- rows.min.bic.glob  }
if (selc.crit == selc.rmse) { best.row.n.cond <- rows.min.rmse.n.cond ; best.row.glob <- rows.min.rmse.glob   }
if (selc.crit == selc.r2.ffx ) { best.row.n.cond <- rows.max.r2.ffx.n.cond ; best.row.glob <- rows.max.r2.ffx.glob }





if (c.sp == species.sheep & c.ndf == ndf.lev.lo & c.stage == stage.growing.all) { 
  
best.model.cr.1.gr.sheep.lo.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;  
best.model.cr.1.gr.sheep.lo.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ;  
best.model.cr.1.gr.sheep.lo.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]  ; 
reg.inc.cond.gr.sheep.lo.ndf.glob.otpm <<- d.frame[best.row.glob , 'inc.cond'][[1]]  ; 
reg.inc.cond.gr.sheep.lo.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]]  ; 
best.model.cr.1.gr.sheep.lo.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; 
best.model.cr.1.gr.sheep.lo.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var']



}





if (c.sp == species.sheep & c.ndf == ndf.lev.hi & c.stage == stage.growing.all) { best.model.cr.1.gr.sheep.hi.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;   best.model.cr.1.gr.sheep.hi.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ; best.model.cr.1.gr.sheep.hi.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]    ; reg.inc.cond.gr.sheep.hi.ndf.glob.otpm <<- d.frame[best.row.glob , 'inc.cond'][[1]]  ; reg.inc.cond.gr.sheep.hi.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]]  ; best.model.cr.1.gr.sheep.hi.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.gr.sheep.hi.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }
if (c.sp == species.goat & c.ndf == ndf.lev.lo & c.stage == stage.growing.all) { best.model.cr.1.gr.goats.lo.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;   best.model.cr.1.gr.goats.lo.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ; best.model.cr.1.gr.goats.lo.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]    ; reg.inc.cond.gr.goats.lo.ndf.glob.otpm <<- d.frame[best.row.glob , 'inc.cond'][[1]] ; reg.inc.cond.gr.goats.lo.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]] ; best.model.cr.1.gr.goats.lo.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.gr.goats.lo.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }
if (c.sp == species.goat & c.ndf == ndf.lev.hi & c.stage == stage.growing.all) { best.model.cr.1.gr.goats.hi.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;   best.model.cr.1.gr.goats.hi.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ; best.model.cr.1.gr.goats.hi.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]     ; reg.inc.cond.gr.goats.hi.ndf.glob.otpm <<- d.frame[best.row.glob, 'inc.cond'][[1]]  ; reg.inc.cond.gr.goats.hi.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]] ; best.model.cr.1.gr.goats.hi.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.gr.goats.hi.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }

if (c.sp == species.sheep & c.ndf == ndf.lev.lo & c.stage == stage.adult.all) { best.model.cr.1.adt.sheep.lo.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;  best.model.cr.1.adt.sheep.lo.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ;  best.model.cr.1.adt.sheep.lo.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]  ; reg.inc.cond.adt.sheep.lo.ndf.glob.otpm <<- d.frame[best.row.glob , 'inc.cond'][[1]]  ; reg.inc.cond.adt.sheep.lo.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]]  ; best.model.cr.1.adt.sheep.lo.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.adt.sheep.lo.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }
if (c.sp == species.sheep & c.ndf == ndf.lev.hi & c.stage == stage.adult.all) { best.model.cr.1.adt.sheep.hi.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;  best.model.cr.1.adt.sheep.hi.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ; best.model.cr.1.adt.sheep.hi.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]    ; reg.inc.cond.adt.sheep.hi.ndf.glob.otpm <<- d.frame[best.row.glob , 'inc.cond'][[1]]  ; reg.inc.cond.adt.sheep.hi.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]]  ; best.model.cr.1.adt.sheep.hi.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.adt.sheep.hi.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }
if (c.sp == species.goat & c.ndf == ndf.lev.lo & c.stage == stage.adult.all) { best.model.cr.1.adt.goats.lo.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;  best.model.cr.1.adt.goats.lo.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ; best.model.cr.1.adt.goats.lo.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]    ; reg.inc.cond.adt.goats.lo.ndf.glob.otpm <<- d.frame[best.row.glob , 'inc.cond'][[1]] ; reg.inc.cond.adt.goats.lo.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]] ; best.model.cr.1.adt.goats.lo.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.adt.goats.lo.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }
if (c.sp == species.goat & c.ndf == ndf.lev.hi & c.stage == stage.adult.all) { best.model.cr.1.adt.goats.hi.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;  best.model.cr.1.adt.goats.hi.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ;  best.model.cr.1.adt.goats.hi.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]     ; reg.inc.cond.adt.goats.hi.ndf.glob.otpm <<- d.frame[best.row.glob, 'inc.cond'][[1]]  ; reg.inc.cond.adt.goats.hi.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]] ; best.model.cr.1.adt.goats.hi.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.adt.goats.hi.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }





# Obtain IDs of observations with largest residuals, in descending order
model <- d.frame[ best.row.glob.2  , 'model'][[1]]

residuals <- resid(model)

# First best
F1.b.resid <- max(abs(residuals))
residuals.m.1.b <- residuals[-which(abs(residuals) == F1.b.resid )]
first.max.id <- as.numeric(names(residuals[which(abs(residuals) == F1.b.resid )]))

# Second best
F2.b.resid <- max(abs(residuals.m.1.b ))
residuals.m.2.b <- residuals.m.1.b[-which(abs(residuals.m.1.b) == F2.b.resid  )]
second.max.id <- as.numeric(names(residuals.m.1.b[which(abs(residuals.m.1.b) == F2.b.resid)]))

# Third best
F3.b.resid <- max(abs(residuals.m.2.b ))
residuals.m.3.b <- residuals.m.2.b[-which(abs(residuals.m.2.b) == F3.b.resid  )]
third.max.id <- as.numeric(names(residuals.m.2.b[which(abs(residuals.m.2.b) == F3.b.resid)]))

 
first.max.outlier.id <- d.reg.in.cc[d.reg.in.cc$id == first.max.id & !is.na(d.reg.in.cc$id), 'B.Code']
second.max.outlier.id <- d.reg.in.cc[d.reg.in.cc$id == second.max.id & !is.na(d.reg.in.cc$id), 'B.Code']
third.max.outlier.id <- d.reg.in.cc[d.reg.in.cc$id == third.max.id  & !is.na(d.reg.in.cc$id), 'B.Code']


# SHEEP - LOW NDF
if (c.sp == species.sheep & c.ndf == ndf.lev.lo & c.stage == stage.growing.all) { 
  
gr.sheep.lo.ndf.max.resd.id.1 <<- first.max.outlier.id  
gr.sheep.lo.ndf.max.resd.id.2 <<- second.max.outlier.id
gr.sheep.lo.ndf.max.resd.id.3 <<- third.max.outlier.id

# remove outliers only if the boolean is set TRUE
if (ol.rm.gr.sheep.lo.ndf) {

outlier.b.codes <<- c( gr.sheep.lo.ndf.max.resd.id.1  )
outlier.b.codes <<- append(  outlier.b.codes  , gr.sheep.lo.ndf.max.resd.id.2 )
outlier.b.codes <<- append(  outlier.b.codes  , gr.sheep.lo.ndf.max.resd.id.3 )

source('outlier.removal.R')

}
  
}

# SHEEP - HIGH NDF
if (c.sp == species.sheep & c.ndf == ndf.lev.hi & c.stage == stage.growing.all) { 
  
  gr.sheep.hi.ndf.max.resd.id.1 <<- first.max.outlier.id  
  gr.sheep.hi.ndf.max.resd.id.2 <<- second.max.outlier.id
  gr.sheep.hi.ndf.max.resd.id.3 <<- third.max.outlier.id
  
  
  # remove outliers only if the boolean is set TRUE
  if (ol.rm.gr.sheep.hi.ndf) {
  
  outlier.b.codes <<- c( gr.sheep.hi.ndf.max.resd.id.1)
  outlier.b.codes <<- append(  outlier.b.codes  , gr.sheep.hi.ndf.max.resd.id.2 )
  outlier.b.codes <<- append(  outlier.b.codes  , gr.sheep.hi.ndf.max.resd.id.3 )
  
  source('outlier.removal.R')
  
  }
  
}

# GOATS - LOW NDF
if (c.sp == species.goat & c.ndf == ndf.lev.lo & c.stage == stage.growing.all) { 
  
  gr.goat.lo.ndf.max.resd.id.1 <<- first.max.outlier.id  
  gr.goat.lo.ndf.max.resd.id.2 <<- second.max.outlier.id
  gr.goat.lo.ndf.max.resd.id.3 <<- third.max.outlier.id
  
  
  # remove outliers only if the boolean is set TRUE
  if (ol.rm.gr.goat.lo.ndf) {
    
  outlier.b.codes <<- c( gr.goat.lo.ndf.max.resd.id.1  )
  outlier.b.codes <<- append(  outlier.b.codes  , gr.goat.lo.ndf.max.resd.id.2 )
  outlier.b.codes <<- append(  outlier.b.codes  , gr.goat.lo.ndf.max.resd.id.3 )
  
  source('outlier.removal.R')
  
  }
  
}

# GOATS - HIGH NDF
if (c.sp == species.goat & c.ndf == ndf.lev.hi & c.stage == stage.growing.all) { 
  
  gr.goat.hi.ndf.max.resd.id.1 <<- first.max.outlier.id  
  gr.goat.hi.ndf.max.resd.id.2 <<- second.max.outlier.id
  gr.goat.hi.ndf.max.resd.id.3 <<- third.max.outlier.id
  
  
  # remove outliers only if the boolean is set TRUE
  if (ol.rm.gr.goat.hi.ndf) {
    
  outlier.b.codes <<- c( gr.goat.hi.ndf.max.resd.id.1)
  outlier.b.codes <<- append(  outlier.b.codes  , gr.goat.hi.ndf.max.resd.id.2 )
  outlier.b.codes <<- append(  outlier.b.codes  , gr.goat.hi.ndf.max.resd.id.3 )
  
  source('outlier.removal.R')
  
  }
}

}


listify.coef.names <<- function(model , mt){
  
  test <- function(){
    
    model <- ws.model
    mc <- 1

  }
  
  names <- "(Intercept)"  #names(coef(model)[1])
  
  
  if (  mc %in% c(1,2)  ) { x.vars <- coef(model) }
  if (  mc %in% c(3)  ) { x.vars <- fixef(model) }
  

  names <- c(names ,    names( x.vars[1])   )
  names <- c(names ,    names( x.vars[2])   )
  names <- c(names ,    names( x.vars[3])   )
  names <- c(names ,    names( x.vars[4])   )
  names <- c(names ,    names( x.vars[5])   )
  names <- c(names ,    names( x.vars[6])   )
  
  # Remove id variables (random effects)
  names <- names[which(  !str_detect(names,'ue.')  )]

  return(names)
  
}


predict.manual <<- function(form , dat , offset , data.set.type , row  , x.vars.re){
  
  # test: form <- formula ; offset <-  ws.offset.coef ; dat <- all.data ; data.set.type <- 'test' ; row <- 1

  # From whole sample ; form <- formula ; dat <- all.data ; offset <- ws.offset.coef ; data.set.type <- 'whole' ; row <- 1 
  
  
  test.ds <- function(){
    
    form <- formula  
    dat <- test.data 
    offset <- model.offset 
    data.set.type <- 'test' 
    row <- r 
    
  }
  
  
  test.test <- function(){
    
    form <- formula 
    offset <-  model.offset 
    dat <- test.data 
    data.set.type <- 'test' 
    row <- 17
    
    
    
  }
 
  x.vars <- de.listify(return.x.vars(form , data.set.type , row )[1])[[1]]
  x.coefs <- de.listify(return.x.vars(form , data.set.type  , row)[2])[[1]]
  
  x.vars.re.coefs <- as.numeric(x.vars.re)
  x.vars.re.ids <- substr(names(x.vars.re),6,10)

  predicted.df <- data.frame(matrix(NA, nrow = nrow(dat)) )
  
  
  predicted.df$predicted <- offset + (x.coefs[1] +
                                       x.coefs[2] * dat[, x.vars[1]  ] ) 
  
  
  predicted.df$ue.id <- dat$ue.id
  
  for (r in 1:nrow(predicted.df)){
    
    if (FALSE){
      
     r <- 6
     
    }
    
    ue.id <-   as.character(predicted.df[r  , 'ue.id'])
    
    if (   ue.id %in% x.vars.re.ids ) {
    
    predicted.df[r  , 'predicted' ] <- predicted.df[r  , 'predicted' ]  + x.vars.re.coefs[ which(x.vars.re.ids ==ue.id) ]
    
    
    }
  }
  
  

  
  predicted <- predicted.df$predicted
  
  
  if (   !is.na(x.coefs[3]) ){ predicted <- predicted +  x.coefs[3] * dat[, x.vars[2] ]  }
  if (   !is.na(x.coefs[4]) ){ predicted <- predicted +  x.coefs[4] * dat[, x.vars[3] ]}
  if (   !is.na(x.coefs[5]) ){ predicted <- predicted +  x.coefs[5] * dat[, x.vars[4] ]}
  if (   !is.na(x.coefs[6]) ){ predicted <- predicted +  x.coefs[6] * dat[, x.vars[5] ]}
  if (   !is.na(x.coefs[7]) ){ predicted <- predicted +  x.coefs[7] * dat[, x.vars[6] ]}
  
  return (predicted)
  
}



gen.weights <- function( ids , ids.exmt ){
    
  test <- function(){
    
     ids.exmt <- e.ids
     ids  <- t.ids
    
    
  }
  


    
    experiments <- d.reg.in[  d.reg.in$ut.id %in%  ids , 'ue.id'  ]
    unique.experiments <- unique(experiments)
      
      
    ws.standard.deviations <- c()
    
    for (  e in 1 : length( experiments )   ){  
      
      e.id <- experiments[e]
      
      #e <- 1
      
      sd <- sd(   d.reg.in[  d.reg.in$ue.id ==   e.id   , 'feed_intake_g_d'  ]   )
      
      if ( is.na(sd) ){ sd  <- mean(  ws.standard.deviations) }
      
      ws.standard.deviations[e] <-   sd 
      
       }
    
    ws.variances <- ws.standard.deviations^2
    
    bs.standard.deviation <- sd(ws.standard.deviations)
    bs.standard.variance <- bs.standard.deviation^2

    
    weights <- 1 / (ws.variances + bs.standard.variance )
    
    weights.stdz <- (weights - min(weights))  / ( max(weights) - min(weights))
  
    return ( weights.stdz )
    
 }
  
    
gen.eval.metrics <- function( m , mt ,form ,d , y  , t.ids , e.ids  ,offset , data.set.type , row ){
  
test.criteria <- function(){
  
  row <- r.cnt
  m <-  model.NRC
  mc <- 3
  form <- formula
  d <- data
  y <-  data[,y.var.reg] 
  t.ids <- t.IDs
  e.ids <-  e.IDs
  offset <- model.offset
  data.set.type <-  'valid'
  
  
  
row <- 102
m <- model
mc <- mc
form <- formula
d <- test.data
y <-  test.data[,y.var.reg] 
t.ids <- test.ids 
e.ids <-  test.ids.exmt 
offset <- model.offset
data.set.type <-  'test'



row <- r
m <- ws.model
mc <- mc
form <- formula
d <- all.data
y <-  all.data[,y.var.reg] 
t.ids <- all.ids 
e.ids <-  all.ids.exmt 
offset <- ws.model.offset
data.set.type <-  'whole'



}


if ( !random.exp.int ) { 
  
if (mc == model.class.ME){ 
  pred <- predict( m , newdata = d , re.form = NA ) 
  } else {

pred <- predict( m , newdata = d  ) 
}}

if (  random.exp.int ) { pred <-  predict.manual( form , d , offset , data.set.type , row , x.vars.re)   }


NA.indices <- which( is.na(pred))




# Omit from pred the values which are NA
pred <- pred[ !is.na(pred) ]


form.lhs <- as.character(form[[2]])
observed.tformd <-  d[,  form.lhs  ] 

# If NAs are observed in predicted values, remove corresponding entries for observed
if ( length(NA.indices) >0 ){ observed.tformd <-  observed.tformd[  -c(as.vector(NA.indices))  ] }



  # Non-weighted
  # Normalized RMSE
  rsds <- observed.tformd  -  pred
  sq.rdsd <- rsds ^ 2
  msr <- mean( sq.rdsd )
  rmse <- sqrt(msr  )
  nrmse <- 100 * rmse / mean(observed.tformd)
  
  # Adjusted R2
  rss <- sum( sq.rdsd )
  tss <- sum( (y - mean(y))^2 )
  r2 <- 1 -  rss/ tss
  
  x.vars <- 4 # de.listify( return.x.vars(form , data.set.type , row)  )[[1]] 

  k <- length(x.vars) 
  n <- sum(d$Sample.size)
  
  r2.adj <-  1 -  rss/ (n - k) / tss / (n-1) 
  
  
  # WEIGHTED PERFORMANCE METRICS
  weights <- gen.weights( t.ids , e.ids  ) 
  
  # Normalized RMSE
  w.sq.rdsd  <- sq.rdsd * weights 
  
  sum.w.sq.rds <- sum(w.sq.rdsd  )
  mean.w.sq.rds <- sum.w.sq.rds / sum( weights )
  sqr.mean.w.sq.rds <- sqrt(  mean.w.sq.rds)
  
  w.nrmse <- 100 * sqr.mean.w.sq.rds / mean(observed.tformd)
  

  # Adjusted R2
  w.rss <- sum( weights * sq.rdsd)
  w.tss <- sum( weights * (y - mean(y))^2 )
  w.r2 <- 1 - w.rss / w.tss
  
  w.r2.adj <- 1 -  w.rss/ (n - k) / w.tss / (n-1)  
  
  
  
  # Round performance metrics
  r2 <- round( r2, rd.decs.R2 )
  nrmse <- round( nrmse , rd.decs.nRMSE )
  
  w.r2  <- round( w.r2, rd.decs.R2 )
  w.nrmse<- round( w.nrmse , rd.decs.nRMSE )
  
  return.item <- c(  listify( r2 ) ,  listify(nrmse )  , listify( w.r2 ) ,  listify(  w.nrmse )  )
  

  d.reg.in  <- rank.residual.outliers(  d , rsds , row )
  
  
  return ( return.item   )
  
}


rank.residual.outliers <- function( d ,  rsds  , row ){
  
  # test: 
  if (row == 1) { 
    
    d.resds.SDs <- data.frame((matrix( nrow = nrow(d.reg.in) , ncol = 4))) 
    
    colnames(d.resds.SDs)[1] <- 'ue.id'
    colnames(d.resds.SDs)[2] <- 'ut.id'
    colnames(d.resds.SDs)[3] <- 'B.Code'
    colnames(d.resds.SDs)[4] <- 'diet.code' 
    

    d.resds.SDs$ue.id <- d.reg.in$ue.id
    d.resds.SDs$ut.id <- d.reg.in$ut.id
    d.resds.SDs$B.Code <- d.reg.in$B.Code
    d.resds.SDs$diet.code <- d.reg.in$diet.code
    
    d.resds.SDs <<-  d.resds.SDs[ ,  -c(1) ]
    }
  
  
  t.ids <- d$ut.id
  
  sqd.resds <- as.numeric(rsds)^2 
  mean.sq.resd <- mean(   sqd.resds  )
  
  standard.devs.resds <-  sqd.resds /   mean.sq.resd
  
  
  mv <- d.reg.out[ row , 'mod.vers']
  
  col.id <- str_c( 'resd.SD.' , mv )
  
  for ( t in t.ids ){
    
    
    # t <- t.ids
    
    SD.resd <- round( standard.devs.resds[  which(t == t.ids)   ] , 1)
    
    
    d.resds.SDs[   d.reg.in$ut.id == t , col.id ] <- SD.resd 
    
  }
  
  
  d.resds.SDs <<-  d.resds.SDs
  #  return (  d.reg.in  )
  
}




gen.reg.model <- function(  data , form   , mod.v      ){
  
test <- function(){
    
    form <- formula
    data <- train.data
    mod.v <-  d.reg.out[r, 'mod.vers'] 
    
    
    #
    form <- formula
    data <- all.data
    mod.v <-  d.reg.out[r, 'mod.vers'] 
    
    
  }

cur.class.index <- as.numeric(substr(mod.v , 1 , 1))
cur.family.index <- as.numeric(substr(mod.v , 3 , 3))
cur.mstop.index <-  as.numeric( substr(mod.v , 5 , 5) )
cur.nu.index <- as.numeric(  substr(mod.v , 7 , 7))

family.list <- c(   1  ,  2  ) ;   mod.fam.gaussian <- 1 ; mod.fam.laplace <- 2


mstop.list <- seq( mstop.min , mstop.max , by =  m.stop.range /    n.mod.v.boost.control.mstop )
nu.list <- seq( nu.min , nu.max  , by =  nu.range /      n.mod.v.boost.control.nu  )

family <- de.listify(ml.boost.families[  cur.family.index ])
mstop <-  mstop.list[cur.mstop.index]
nu <- nu.list[cur.nu.index]


if (cur.class.index == model.class.ME ){   form <- update(  form, . ~ . - ue.id) }



  
  
if (  cur.class.index == model.class.GBR  ){  
    
    
  mod.0 <- glmboost( 
    
    form  
    
    , data =  data
    
    , family =    family
   # ,  control = boost_control(mstop =   mstop , nu = nu)
    , center = FALSE
    
  )
  
  


if (   m.stop.cv   ){
    
 # cv_folds <- cv(model.weights(mod.0 ), type = "kfold", k = 5)
  
   cvm <- cvrisk(mod.0 , grid = cv.risk.min.grid :cv.risk.max.grid  )
   mstop <- mstop(cvm)
   #mstop.opt <- mstop
   
  }
  
  x.vars <- xtract.x.vars(  form )
  cols.2.include <- c( x.vars )
  
  data$ue.id <- factor(  data$ue.id )
  
if (  TRUE ){ 
  
  model <- glmboost( 
  
    form  
    , data =  data
    
   # , family =  family
    ,  control = boost_control(mstop =   mstop, nu = nu)
    , center = FALSE
    
   
   , weights = reg.weight
  )
  
  

  # summary(  model )
  
  
  } else if (  family == mod.fam.laplace  ) { 
    

    model <- glmboost( 
      
      form  
      
      , data =  data
      
      , family = Gaussian()
      ,  control = boost_control(mstop =   mstop, nu = nu)
      , center = FALSE
      
    )
    
   # model <-  gbm(
    #  form  , 
     # data = data[,cols.2.include]  , 
     # distribution = "gaussian"
    #  , bag.fraction = 1
     # , n.minobsinnode = 1
      #  cv.folds = 5,         # Perform 10-fold cross-validation
      # shrinkage = .01,       # Learning rate
      # n.minobsinnode = 10,   # Minimum observations in a terminal node
      #  n.trees = 500          # Number of trees (boosting iterations)
   #@ )
    
    #  model <- xgboost(
    # data = data[,cols.2.include] 
    #, formula =     form 
    #, label = data[,  y.var  ]
    # , max.depth = 3
    # , eta = 1
     # , nthread = 2
     # , nrounds = 2
    # , objective = "reg:squarederror"
    #  )
 
 
    # xgb.importance(feature_names = cols.2.include, model = model)
    
  
  }

  } else if (  cur.class.index == model.class.ME  ){ # Mixed effect regression using LMER
    
    mstop <- NA  ;  nu <- NA 
    form <- gen.me.form(  as.formula(form)  )
    
    model <- lmer( 
      
      form
      , data =  data
      
      , weights = reg.weight
    )
    
      
    } 
    


  return.item <- c( listify(model) ,  mstop ,   nu )
 
  return ( return.item )
   
}


gen.me.form <- function(   form ){
  

  # form <- form 
  
  rd.intercept <- '+ (1' ; rd.unit <- '| ue.id)'
  
  
  form.me <-  update( form , as.formula(paste(". ~ .", paste(rd.intercept , rd.unit ))))
    
    
  return (form.me)
}

ols.status.residual.method <<- function( row.cond ){
  
test <- function(){
  
  row.cond <- cnd.bv.hi.ndf.Bind.female.pre.ol
  
}  

  if (   !any(row.cond) ) { return (d.reg.in) }  
  
  lin.mod <- lm( feed_intake_g_d ~ bw_kg + adg_kg_day + NDF_nutrition   , data = d.reg.in[ row.cond  ,]  )
  
  predicted <- predict(lin.mod   , data = d.reg.in[  row.cond  ,] )
  actual <- d.reg.in[  row.cond  , 'feed_intake_g_d']
  
  resds <- predicted -  actual
  resds.sqd <-  resds ^ 2

  msr <- mean(  resds.sqd)
  
  delta.msr <- resds.sqd / msr
  
  d.reg.in[row.cond,'delta.msr'] <-   delta.msr 
  

  d.reg.in[  row.cond & d.reg.in[,'delta.msr'] > cutoff.ol.s.residual.method   , ol.status.residual.method.var.name] <- TRUE
    
  
  
  return (d.reg.in)
}


ols.status.variable.range.method <<- function( row.cond ){
  
  
  # row.cond <-   cnd.bv.lo.ndf.male.pre.ol
  
if ( !any(row.cond) ) { return (d.reg.in) }
  
species <- d.reg.in[ row.cond , 'Species' ][1]
  
for ( v in 1 : length( vars.outlier.ranges ) ){
  
  # v <- 2
  
   var <- vars.outlier.ranges[v]
   
   if (  species %in% species.srs  ) {  min.value  <- outlier.range.minimum.sr[v] ;  max.value <- outlier.range.minimum.sr[v]   }
   if (  species %in% species.cattle  ) {  min.value  <- outlier.range.minimum.bv[v] ;  max.value <- outlier.range.maximum.bv[v]   }
   
   
    
    d.reg.in[  row.cond & ( d.reg.in[, var] > max.value | d.reg.in[, var] < min.value  ) & !is.na(d.reg.in[, var])  , ol.status.range.method.var.name ] <- TRUE
    
}
  
  return (d.reg.in)

}


gen.gg.valid.data <<- function(  model.class , species , breed , ndf.lev , sex  ){
  
  test <- function(){
    
    species <- species.cattle
    breed <- 1
    ndf.lev <- ndf.lev.hi
    sex <- sex.female
    model.class <- 1
    
  }
  

  df <- gen.gg.df.specific( 1 ,  model.class , species , breed, ndf.lev , sex) 
  
  if (  is.null(df)  ){   df <- gen.gg.df.specific( 2 ,  model.class , species , breed, ndf.lev , sex)  }
  if (  is.null(df)  ){   df <- gen.gg.df.specific( 3 ,  model.class , species , breed, ndf.lev , sex)  }
  if (  is.null(df)  ){  return (NULL) }
  
  
  for (  m in 2 : n.mod.form  ){ 
    
    test <- function(){
      
    m <- 6
      
    }
    
    #  d.reg.out[ , 'is.NULL']
    
    df <- rbind( df , gen.gg.df.specific(m ,  model.class , species , breed ,ndf.lev , sex)  )  
    
    if ( is.null(df)) { next}
    
    tier <- df$mod.tier
    
    if (  m %% 3 == 0 & m == 3 ){ tier.1.unq.lab.1rs <- unique(  df[ df$mod.tier == tier.label.1  , 'col.mod.form.label.r1' ]) ;  all.tiers.unq.lab.1rs <- tier.1.unq.lab.1rs  }
    if (  m %% 3 == 0 & m == 6 ){ tier.2.unq.lab.1rs <- unique(  df[ df$mod.tier == tier.label.2, 'col.mod.form.label.r1' ])  ;  all.tiers.unq.lab.1rs <- cbind( all.tiers.unq.lab.1rs  , tier.2.unq.lab.1rs ) }
    if (  m %% 3 == 0 & m == 9 ){ tier.3.unq.lab.1rs <- unique(  df[ df$mod.tier == tier.label.3 , 'col.mod.form.label.r1' ])  ;  all.tiers.unq.lab.1rs <- cbind( all.tiers.unq.lab.1rs  , tier.3.unq.lab.1rs )}
    
  }
  
  # all.tiers.unq.lab.1rs <- c( tier.1.unq.lab.1rs ,tier.2.unq.lab.1rs  , tier.3.unq.lab.1rs ) 
   df$col.mod.form.label.r1 <- factor(   df$col.mod.form.label.r1 , levels = order.labels(all.tiers.unq.lab.1rs )) 
  
  return (  df  )
  
  
}

order.labels <<- function(    labels){
  
  # test: labels <- unq.lab.1rs 
  
  label.ids <- substr(labels ,3,3 )

  index.rank.1 <- which(label.ids == 'a')
  index.rank.2 <- which(label.ids == 'b')
  index.rank.3 <- which(label.ids == 'c')
  
  ordered.labels <- c( labels[ index.rank.1  ] , labels[ index.rank.2  ] , labels[ index.rank.3  ])
  
  return (ordered.labels)
}

gen.formula.label.1r <<- function(  form.obj ){
  
  function(){
    
    form.obj <- mod.2.sp.lo.ndf 
    
    form.obj <- mod.form
    
  }
  

  form.obj <- Reduce(paste, deparse(  form.obj ))
  
  form.obj <- as.character(  form.obj  ) 
  
  form.list <- strsplit(  form.obj , " ")[[1]]
  
  form.list <-  form.list[ which(form.list != '')]
  
  rhs.c.1 <-  form.list[3]
  rhs.c.2 <- form.list[5]
  rhs.c.3  <- form.list[7]
  rhs.c.4  <- form.list[9]
  rhs.c.5  <- form.list[11]
  rhs.c.6  <- form.list[13]
  
  begin <- '\u0192( '
  subsc.i <<- '\u1D62'
  end <- ' )'
  comma <- ', '
  new.line <- '\n'
  
  
  b1 <- gen.var.alias(  all.x.vars[ which( all.x.vars ==  rhs.c.1 )  ]  )
  

  if (  !is.na(rhs.c.2) & !(  rhs.c.2 == vn.unique.exp.id)   ){ b2 <- gen.var.alias(  all.x.vars[ which(  all.x.vars ==  rhs.c.2 )  ] )  }
  if (  !is.na(rhs.c.2) & (  rhs.c.2 == vn.unique.exp.id)   ){ b2 <- str_c("E" ,   subsc.i )     }
  
  
  if (  !is.na(rhs.c.3) & !(  rhs.c.3 == vn.unique.exp.id)   ){ b3 <- gen.var.alias(  all.x.vars[ which(  all.x.vars ==  rhs.c.3 )  ] )  }
  if (  !is.na(rhs.c.3) & (  rhs.c.3 == vn.unique.exp.id)   ){ b3 <- str_c("E" ,   subsc.i ) }
  
  
  if (  !is.na(rhs.c.4) & !(  rhs.c.4 == vn.unique.exp.id)){ b4 <- gen.var.alias(  all.x.vars[ which(  all.x.vars ==  rhs.c.4 )  ] )  }
  if (  !is.na(rhs.c.4) & (  rhs.c.4 == vn.unique.exp.id)){ b4 <- str_c("E" ,   subsc.i ) }
  
  
  if (  !is.na(rhs.c.5) & !(  rhs.c.5 == vn.unique.exp.id)){ b5 <- gen.var.alias(  all.x.vars[ which(  all.x.vars ==  rhs.c.5 )  ] )  }
  if (  !is.na(rhs.c.5) & (  rhs.c.5 == vn.unique.exp.id)){ b5 <- str_c("E" ,   subsc.i )  }
  
  if (  !is.na(rhs.c.6) & !(  rhs.c.6 == vn.unique.exp.id)){ b6 <- gen.var.alias(  all.x.vars[ which(  all.x.vars ==  rhs.c.6 )  ] )  }
  if (  !is.na(rhs.c.6) & (  rhs.c.6 == vn.unique.exp.id)){ b6 <- str_c("E" ,   subsc.i ) }
  

  
  
  
  lab <- paste0(  begin,   b1 )
  
  if (   !is.na(rhs.c.2)  ){  lab <- paste0(  lab , comma , b2 )    }
  if (   !is.na(rhs.c.3)  ){  lab <- paste0(  lab , comma , b3 )    }
  if (   !is.na(rhs.c.4) ){    lab <- paste0(  lab , comma , b4 ) } 
  if (   !is.na(rhs.c.5)  ){  lab <- paste0(  lab , comma , b5 )    }
  if (   !is.na(rhs.c.6)  ){  lab <- paste0(  lab , comma , b6 )    }
  
  
  lab <- paste0(  lab , end )
  
  return.obj <-   lab  #listify( c(  lab.rhs.r.1 ,   lab.rhs.r.2 ,   lab.rhs.r.3 ) )
  
  return( return.obj )
}


return.x.vars <- function(  form.obj , data.set.type ,r ){
  
  # test: form.obj <-  form ; data.set.type <- 'test'  ; r <- 102
  # test: whole sampple ; form.obj <-  form ; data.set.type <- 'whole' ; r <- 3
  
  # form.obj <- form ; data.set.type <- 'valid' ; r <- 1
  
  form.obj <- Reduce(paste, deparse(  form.obj ))

  form.obj <- as.character(  form.obj  ) 
  
  form.list <- strsplit(  form.obj , " ")[[1]]
  
  form.list <-  form.list[ which(form.list != '')]
  
  rhs.c.1 <-  form.list[3]
  rhs.c.2 <- form.list[5]
  rhs.c.3  <- form.list[7]
  rhs.c.4  <- form.list[9]
  rhs.c.5  <- form.list[11]
  
  
  x.vars.list <- c(  rhs.c.1 )
  
  if (  !is.na(rhs.c.2) ){  x.vars.list <- c( x.vars.list,  rhs.c.2 ) }
  if (  !is.na(rhs.c.3) ){  x.vars.list <- c( x.vars.list,  rhs.c.3 ) }
  if (  !is.na(rhs.c.4) ){  x.vars.list <- c( x.vars.list,  rhs.c.4 ) }
  if (  !is.na(rhs.c.5) ){  x.vars.list <- c( x.vars.list,  rhs.c.5 ) }
  
  
 if (  data.set.type == 'test' ){
  
  intercept.plus.offset <- d.reg.out[r, 'coef.offset.plus.intercept'] 
  intercept <-  d.reg.out[r, vn.coef.int]  
  
  coef.bw <-  d.reg.out[r, vn.coef.BW ]   
  coef.adg <-  d.reg.out[r,  vn.coef.ADG] 
  coef.my <-  d.reg.out[r, vn.coef.MY ]   
  
  
  coef.ndf <-  d.reg.out[r, vn.coef.NDF ] 
  coef.adf <-  d.reg.out[r, vn.coef.ADF ] 
  coef.adl <-  d.reg.out[r, vn.coef.ADL ] 
  
  coef.cp <-  d.reg.out[r, vn.coef.CP] 
  coef.ee <-  d.reg.out[r, vn.coef.EE] 
  coef.ash <-  d.reg.out[r, vn.coef.Ash] 
  
  
  coef.ndf.d <- d.reg.out[r, vn.coef.NDF_digest]  
  coef.om.d <- d.reg.out[r, vn.coef.OM_digest]  
  coef.dm.d <- d.reg.out[r, vn.coef.DM_digest]  
  

  

  } else if (  data.set.type == 'whole' ){
  
 # intercept.plus.offset <- d.reg.out[r,  vn.coef.ws.offset.plus.intercept] 
    
  intercept <- d.reg.out[r, vn.coef.ws.intercept]
  offset <- d.reg.out[r, vn.coef.ws.offset]
  coef.bw <- d.reg.out[ r , vn.coef.ws.bw ]
  coef.adg <- d.reg.out[ r , vn.coef.ws.ADG]
  coef.my <- d.reg.out[ r , vn.coef.ws.MY]
  
  
  coef.ndf <- d.reg.out[ r , vn.coef.ws.NDF]
  coef.adf <- d.reg.out[ r , vn.coef.ws.ADF]
  coef.adl <- d.reg.out[ r , vn.coef.ws.ADL]
  
  coef.cp <- d.reg.out[ r , vn.coef.ws.CP]
  coef.ee <- d.reg.out[ r , vn.coef.ws.EE]
  coef.ash <- d.reg.out[ r , vn.coef.ws.Ash]
  
  coef.ndf.d <- d.reg.out[ r ,vn.coef.ws.NDF_Dig]
  coef.om.d <- d.reg.out[ r ,vn.coef.ws.OM_Dig]
  coef.dm.d <- d.reg.out[ r ,vn.coef.ws.DM_Dig]
  
  

  
  } else if ( data.set.type == 'valid' ){
    
    intercept <- d.valid[r, vn.coef.ws.intercept]
    offset <- d.valid[r, vn.coef.ws.offset]
    coef.bw <- d.valid[ r , vn.coef.ws.bw  ]
    coef.adg <- d.valid[ r , vn.coef.ws.ADG]
    coef.ndf <- d.valid[ r , vn.coef.ws.NDF]
    coef.cp <- d.valid[ r , vn.coef.ws.CP]
    coef.ndf.x.digest <- d.valid[ r ,vn.coef.ws.NDF_Dig]
    
    
    
  }
  
  x.coefs.list <- c()
  
  if (  !is.na( intercept) ){  x.coefs.list <- c( x.coefs.list ,   intercept )  }
  if (  is.na( intercept) ){  x.coefs.list <- c( x.coefs.list ,   0 )  }
  # if (  !is.na( offset) ){  x.coefs.list <- c( x.coefs.list ,   offset )  }
  
  if (  !is.na(coef.bw) ) {  x.coefs.list <- c( intercept ,  coef.bw )  }
  if (  !is.na(coef.adg )  ){    x.coefs.list <- c( x.coefs.list ,coef.adg  )   }
  if (  !is.na(coef.my )  ){    x.coefs.list <- c( x.coefs.list ,coef.my  )   }
  
  if (  !is.na( coef.ndf )  ){    x.coefs.list <- c( x.coefs.list , coef.ndf )   }
  if (  !is.na( coef.adf )  ){    x.coefs.list <- c( x.coefs.list , coef.adf )   }
  if (  !is.na( coef.adl )  ){    x.coefs.list <- c( x.coefs.list , coef.adl )   }
  
  
  if (  !is.na( coef.cp)  ){    x.coefs.list <- c( x.coefs.list , coef.cp )   }
  if (  !is.na( coef.ee)  ){    x.coefs.list <- c( x.coefs.list , coef.ee )   }
  if (  !is.na( coef.ash)  ){    x.coefs.list <- c( x.coefs.list , coef.ash )   }
  
  # Remove elements of x.vars.list for which no coefficients were estimated
  if (  is.na(coef.bw)   &  pred.var.BW %in% x.vars.list   ){  x.vars.list <- x.vars.list[   -which(x.vars.list == pred.var.BW )  ]  }
  if (  is.na(coef.adg ) & pred.var.ADG %in% x.vars.list  ){    x.vars.list <- x.vars.list[   -which(x.vars.list ==  pred.var.ADG )  ]   }
  if (  is.na(coef.my ) & pred.var.MY %in% x.vars.list  ){    x.vars.list <- x.vars.list[   -which(x.vars.list ==  pred.var.MY )  ]   }
  
  
  if (  is.na( coef.ndf )  & pred.var.NDF %in% x.vars.list){    x.vars.list <- x.vars.list[   -which(x.vars.list == pred.var.NDF )  ]}
  if (  is.na( coef.adf )  & pred.var.ADF %in% x.vars.list){    x.vars.list <- x.vars.list[   -which(x.vars.list == pred.var.ADF )  ]}
  if (  is.na( coef.adl )  & pred.var.ADL %in% x.vars.list){    x.vars.list <- x.vars.list[   -which(x.vars.list == pred.var.ADL )  ]}
  
  

  if (  is.na( coef.cp )  & pred.var.CP %in% x.vars.list ){    x.vars.list <- x.vars.list[   -which(x.vars.list == pred.var.CP )  ]}
  if (  is.na( coef.ee )  & pred.var.EE %in% x.vars.list ){    x.vars.list <- x.vars.list[   -which(x.vars.list == pred.var.EE )  ]}
  if (  is.na( coef.ash )  & pred.var.Ash %in% x.vars.list ){    x.vars.list <- x.vars.list[   -which(x.vars.list == pred.var.Ash )  ]}
  
  
  return.object.1 <- listify(    x.vars.list  )
  return.object.2 <- listify(    x.coefs.list )
  return.object.all <- listify( c(return.object.1 ,  return.object.2))
  
  return (  c(return.object.1 ,  return.object.2)  )
}


gen.complete.cases <<- function( id.list , formla ){
  
  # Test function: id.list <-  cur.t.id.list ; formla <- formula 

  formla.list <- all.vars(formla) 

  # Why can this not handle ue.id?
  # all.vars.plus.meta.d <- c(    formla.list , 'ut.id' , 'T.Animals') 
  
  all.vars.plus.meta.d <- c(    
    formla.list 
    , 'ut.id' 
    , 'Sample.size'
    ) 
  
  df.ss <- d.reg.in[  d.reg.in$ut.id %in% id.list , all.vars.plus.meta.d ] 
  
  new.id.list <- df.ss[  complete.cases(df.ss)  ,'ut.id' ]
  
  return (new.id.list)
  
}


# Export data
reg.d.out <<- function(){ 
  
  cols.not.2.export <- c(
    
    "gbr.model" 
    
    , "all.data"
    ,'r.cond'
   # ,vn.best.tier.w.AIC 
    
  )
  
  cols.fold.sets <- c()

  cols.2.export <- colnames(d.reg.out)[ which( !(colnames(d.reg.out) %in% cols.not.2.export) )  ]
  
  d.export <- d.reg.out[  , cols.2.export ]
  
  
  for (c in 1:ncol(d.export)){
    
    column <- colnames(d.export)[c]
    
    type <- typeof(d.export[,column ])
    
  #  print(paste('',type))
    
    if (type == 'list'){
      
      d.export[,column ] <- as.character(d.export[,column ])
    }
    
    
  }
  
  
  # All data
  d.export.sheep.lon.all <- d.export[ d.export$species  == "Sheep" & d.export$ndf == ndf.lev.lo , cols.2.export ]
  d.export.sheep.hin.all <- d.export[ d.export$species  == "Sheep" & d.export$ndf == ndf.lev.hi  , cols.2.export ]
  d.export.goat.lon.all <- d.export[ d.export$species  == "Goat" & d.export$ndf == ndf.lev.lo , cols.2.export ]
  d.export.goat.hin.all <- d.export[ d.export$species  == "Goat" & d.export$ndf == ndf.lev.hi , cols.2.export ]
  d.export.bv.lon.all <- d.export[ d.export$species  == species.cattle & d.export$ndf == ndf.lev.lo , cols.2.export ]
  d.export.bv.hin.all <- d.export[ d.export$species  == species.cattle & d.export$ndf == ndf.lev.hi , cols.2.export ]
  
  # Basic performance metrics
  d.export.sheep.lon.basic.perf <- d.export[ d.export$species  == "Sheep" & d.export$ndf == ndf.lev.lo , model.perf.summary.variables.all ]
  d.export.sheep.hin.basic.perf <- d.export[ d.export$species  == "Sheep" & d.export$ndf == ndf.lev.hi  , model.perf.summary.variables.all ]
  d.export.goat.lon.basic.perf <- d.export[ d.export$species  == "Goat" & d.export$ndf == ndf.lev.lo , model.perf.summary.variables.all ]
  d.export.goat.hin.basic.perf <- d.export[ d.export$species  == "Goat" & d.export$ndf == ndf.lev.hi  , model.perf.summary.variables.all]
  d.export.bv.lon.basic.perf <- d.export[ d.export$species  == species.cattle & d.export$ndf == ndf.lev.lo , model.perf.summary.variables.all ]
  d.export.bv.hin.basic.perf <- d.export[ d.export$species  == species.cattle & d.export$ndf == ndf.lev.hi  , model.perf.summary.variables.all ]
  

  
  # Fold conditions
  d.export.sheep.lon.fold.sets <- d.export.sheep.lon.all[ ,fold.cond.variables.all ]
  d.export.sheep.hin.fold.sets <- d.export.sheep.hin.all[ ,fold.cond.variables.all ]
  d.export.goat.lon.fold.sets <- d.export.goat.lon.all[ ,fold.cond.variables.all ]
  d.export.goat.hin.fold.sets <- d.export.goat.hin.all[ ,fold.cond.variables.all ]
  d.export.bv.lon.fold.sets <- d.export.bv.lon.all[ ,fold.cond.variables.all ]
  d.export.bv.hin.fold.sets <- d.export.bv.hin.all[ ,fold.cond.variables.all ]

  
  # Sub sample conditions
  d.export.sheep.lon.sample <- d.export.sheep.lon.all[ ,variable.names.sub.sample ]
  d.export.sheep.hin.sample <- d.export.sheep.hin.all[ ,variable.names.sub.sample ]
  d.export.goat.lon.sample <- d.export.goat.lon.all[ ,variable.names.sub.sample ]
  d.export.goat.hin.sample <- d.export.goat.hin.all[ ,variable.names.sub.sample ]
  d.export.bv.lon.sample <- d.export.bv.lon.all[ ,variable.names.sub.sample ]
  d.export.bv.hin.sample <- d.export.bv.hin.all[ ,variable.names.sub.sample ]
  
  # Regression coefficients
  d.export.sheep.lon.coefs  <- d.export.sheep.lon.all[ ,coefficients.variables.all ]
  d.export.sheep.hin.coefs <- d.export.sheep.hin.all[ ,coefficients.variables.all ]
  d.export.goat.lon.coefs  <- d.export.goat.lon.all[ ,coefficients.variables.all ]
  d.export.goat.hin.coefs <- d.export.goat.hin.all[ ,coefficients.variables.all ]
  d.export.bv.lon.coefs  <- d.export.bv.lon.all[ ,coefficients.variables.all ]
  d.export.bv.hin.coefs <- d.export.bv.hin.all[ ,coefficients.variables.all ]
  
  # Export to excel
  # All data
  write.xlsx(    d.export.sheep.lon.all , str_c(results.out.dir.sp , "reg_out_sheep_lon.all.xlsx") )
  write.xlsx( d.export.sheep.hin.all , str_c(results.out.dir , "reg_out_sheep_hin.all.xlsx"))
  write.xlsx( d.export.goat.lon.all, str_c(results.out.dir.gt , "reg_out_goat_lon.all.xlsx"))
  write.xlsx( d.export.goat.hin.all , str_c(results.out.dir.gt , "reg_out_goat_hin.all.xlsx"))
  write.xlsx( d.export.bv.lon.all,  str_c(results.out.dir.bv.data ,"reg_out_bv_lon.all.xlsx"))
  write.xlsx( d.export.bv.hin.all ,  str_c(results.out.dir.bv.data,"reg_out_bv_hin.all.xlsx"))
  
  
  # Basic model data
  write.xlsx(    d.export.sheep.lon.basic.perf , str_c(results.out.dir.sp , "reg_out_sheep_lon_perform_metrics.xlsx") )
  write.xlsx( d.export.sheep.hin.basic.perf   , str_c(results.out.dir.sp , "reg_out_sheep_hin_perform_metrics.xlsx"))
  write.xlsx(    d.export.goat.lon.basic.perf , str_c(results.out.dir.gt , "reg_out_goat_lon_perform_metrics.xlsx") )
  write.xlsx( d.export.goat.hin.basic.perf    , str_c(results.out.dir.gt , "reg_out_goat_hin_perform_metrics.xlsx"))
  write.xlsx(    d.export.bv.lon.basic.perf ,  str_c(results.out.dir.bv.data,"reg_out_bv_lon_perform_metrics.xlsx") )
  write.xlsx( d.export.bv.hin.basic.perf   , str_c(results.out.dir.bv.data,"reg_out_bv_hin_perform_metrics.xlsx"))
  
  

  # Fold conditions
  write.xlsx(   d.export.sheep.lon.fold.sets , str_c(results.out.dir.sp , "reg_out_sheep_lon_fold_sets.xlsx") )
  write.xlsx( d.export.sheep.hin.fold.sets , str_c(results.out.dir.sp , "reg_out_sheep_hin_fold_sets.xlsx"))
  write.xlsx(   d.export.goat.lon.fold.sets , str_c(results.out.dir.gt , "reg_out_goat_lon_fold_sets.xlsx") )
  write.xlsx( d.export.goat.hin.fold.sets , str_c(results.out.dir.gt , "reg_out_goat_hin_fold_sets.xlsx"))
  write.xlsx(   d.export.bv.lon.fold.sets ,  str_c(results.out.dir.bv.data,"reg_out_bv_lon_fold_sets.xlsx") )
  write.xlsx( d.export.bv.hin.fold.sets ,  str_c(results.out.dir.bv.data,"reg_out_bv_hin_fold_sets.xlsx"))
  
  # Sample data
  write.xlsx(    d.export.sheep.lon.sample  , str_c(results.out.dir.sp , "reg_out_sheep_lon_sample.xlsx") )
  write.xlsx( d.export.sheep.hin.sample  , str_c(results.out.dir.sp , "reg_out_sheep_hin_sample.xlsx"))
  write.xlsx(    d.export.goat.lon.sample  , str_c(results.out.dir.gt , "reg_out_goat_lon_fsample.xlsx") )
  write.xlsx(  d.export.goat.hin.sample , str_c(results.out.dir.gt , "reg_out_goat_hin_sample.xlsx"))
  write.xlsx(    d.export.bv.lon.sample  ,  str_c(results.out.dir.bv.data,"reg_out_bv_lon_sample.xlsx") )
  write.xlsx( d.export.bv.hin.sample  ,  str_c(results.out.dir.bv.data,"reg_out_bv_hin_sample.xlsx"))
  
  
  
  # Coefficients
  write.xlsx(      d.export.sheep.lon.coefs  , str_c(results.out.dir.sp , "reg_out_sheep_lon_coefs.xlsx") )
  write.xlsx(  d.export.sheep.hin.coefs , str_c(results.out.dir.sp , "reg_out_sheep_hin_coefs.xlsx"))
  write.xlsx(    d.export.goat.lon.coefs , str_c(results.out.dir.gt , "reg_out_goat_lon_coefs.xlsx") )
  write.xlsx(  d.export.goat.hin.coefs , str_c(results.out.dir.gt , "reg_out_goat_hin_coefs.xlsx"))
  write.xlsx(    d.export.bv.lon.coefs , str_c(results.out.dir.bv.data,"reg_out_bv_lon_coefs.xlsx") )
  write.xlsx(  d.export.bv.hin.coefs ,  str_c(results.out.dir.bv.data,"reg_out_bv_hin_coefs.xlsx"))
  
  
  
 
}



gen.var.alias <- function( var ){
  
#  var <- all.x.vars[c(38)]
  
 # print(paste('current var is', var))
  
  if (  str_detect( var , fixed("."))  ){
  
  prefx <- substr( var, 1 , (str_length(var)-4)  )
  suffx <- substr( var, (str_length(var)-3) , (str_length(var)) )
  
 
  new.prefx <- form.aliases[ which( all.x.vars == prefx ) ]
  
  if ( suffx %in% c(".e25" ,".sqt"   ,  ".e75" ,".sqd"  , ".cbd", ".mean.s" ,   ".min.max.s" )  ) {
    
  new.suffx <- suffixes.labs[ which(suffixes == suffx ) ]
  new.lab <- str_c(  new.prefx ,  new.suffx )
  
  } else if ( suffx %in% c(".log" )  ) {

    if (suffx == c(".log")){ new.lab <- str_c(  'ln(',  new.prefx,')' ) }
  
  }
  }else { 
    
    new.prefx <- form.aliases[ which( all.x.vars == var ) ]
    
    new.lab <-  new.prefx 
  
  }
  
  return (new.lab)
  
}






assign.best.model.tier <<- function( r.cond  , d.reg.out ){  # Best among all formulae in class
  
  # test: r.cond <-    ss.best.tier.cond 
  
  
  # Best ranked variant
  rank.1.optim.metric <- max( na.omit(  d.reg.out[   r.cond,   optim.metric.ucb ])  )
  rank.1.row.cond <- d.reg.out[,optim.metric.ucb]  ==  rank.1.optim.metric & !is.na( d.reg.out[,optim.metric.ucb]  )  
  
  rank.1.mod.vers <-  d.reg.out[which(  rank.1.row.cond   ) , 'mod.vers']
  rank.1.mod.formula <- d.reg.out[which(  rank.1.row.cond    ) , 'mod.form']
  
  
  # Second best ranked variant
  rank.2.optim.metric  <- max( na.omit(  d.reg.out[ r.cond &   !(d.reg.out$mod.form %in%   rank.1.mod.formula) ,   optim.metric.ucb ])  )
  rank.2.row.cond <-   r.cond & d.reg.out[,optim.metric.ucb]  ==  rank.2.optim.metric & !is.na( d.reg.out[,optim.metric.ucb]  )   &   !(d.reg.out$mod.form %in%   rank.1.mod.formula)  
  
  # rank.2.elements <- length(which( rank.2.row.cond == TRUE))
  
  # unique.form <- unique(d.reg.out[rank.2.row.cond , 'mod.form'])  
  
  # if (  rank.2.elements > n.folds ){    rank.2.row.cond <- rank.2.row.cond[     rank.2.row.cond &   which( d.reg.out[, 'mod.form'] ==   unique.form[1] )   ]     }
  
  
  rank.2.mod.vers <-  d.reg.out[which(   rank.2.row.cond    ) , 'mod.vers']
  rank.2.mod.formula <- d.reg.out[which(   rank.2.row.cond  ) , 'mod.form']
  
  # Third best ranked variant
  rank.3.optim.metric  <- max( na.omit(  d.reg.out[ r.cond &   !(d.reg.out$mod.form %in%   rank.1.mod.formula)  &  !(d.reg.out$mod.form %in%   rank.2.mod.formula) ,   optim.metric.ucb ])  )
  rank.3.row.cond <- d.reg.out[,optim.metric.ucb]  ==  rank.3.optim.metric  & !is.na( d.reg.out[,optim.metric.ucb]  )  
  
  # optim.mod.vers <-  d.reg.out[  optim.mod.vers.form.indx, 'mod.vers' ] 
  # optim.mod.form <-  d.reg.out[  optim.mod.vers.form.indx, 'mod.form' ] 
  
  # Assignments
  d.reg.out[    rank.1.row.cond , vn.tier.rank ] <- 1
  d.reg.out[    rank.2.row.cond  , vn.tier.rank ] <- 2
  d.reg.out[    rank.3.row.cond  , vn.tier.rank ] <- 3
  
  
  d.reg.out[rank.1.row.cond , vn.is.best.tier  ] <-  TRUE
  
  d.reg.out[r.cond  , vn.best.tier.w.R2  ] <- d.reg.out[rank.1.row.cond , vn.w.R2.mean ][1] 
  d.reg.out[r.cond , vn.best.tier.w.nRMSE ] <- d.reg.out[rank.1.row.cond , vn.w.nRMSE.mean ][1] 
  d.reg.out[r.cond  , vn.best.tier.w.CCC ] <- d.reg.out[rank.1.row.cond , vn.w.CCC.mean ][1]   
  
  d.reg.out[r.cond  , vn.best.tier.w.R2.ucb  ] <- d.reg.out[rank.1.row.cond , vn.w.R2.ucb][1] 
  
  
  # Add coefficient data here.
  
  return (d.reg.out)
}


assign.best.model <<- function( r.cond  , d.reg.out , optimum.type ){  # Best among all formulae in class
  
  # test: r.cond <- ss.best.glob.cond ; assignment.variable <- vn.best.global
  
  
  optim.metric <- max( na.omit(  d.reg.out[  r.cond ,   optim.metric.ucb  ] )  )
  
  optim.mod.vers.indx <- which( d.reg.out[,optim.metric.ucb ]  ==  optim.metric    )
  
  optim.mod.vers <-  d.reg.out[optim.mod.vers.indx,  vn.mod.vers ] 
  


  if (  optimum.type == 'global' ) {
    
    best.R2 <- vn.best.global.w.R2
    best.nRMSE <- vn.best.global.w.nRMSE
    best.CCC <- vn.best.global.w.CCC
    best.R2.ucb <- vn.best.global.w.R2.ucb
    
    assignment.label <- vn.best.global
  
  } else if (  optimum.type == 'formula'  ){
    
    best.R2 <- vn.best.hypar.w.R2
    best.nRMSE <- vn.best.hypar.w.nRMSE
    best.CCC <- vn.best.hypar.w.CCC
    best.R2.ucb <- vn.best.hypar.ucb.w.R2
    
    
    assignment.label <- vn.best.in.formula
    
  }
  
  d.reg.out[r.cond, best.R2  ] <- d.reg.out[optim.mod.vers.indx, vn.w.R2.mean ][1] 
  d.reg.out[r.cond,    best.nRMSE] <- d.reg.out[optim.mod.vers.indx, vn.w.nRMSE.mean ][1] 
  d.reg.out[r.cond , best.CCC ] <- d.reg.out[optim.mod.vers.indx, vn.w.CCC.mean ][1]   
  d.reg.out[r.cond,   best.R2.ucb ] <- d.reg.out[optim.mod.vers.indx, vn.w.R2.ucb ][1] 

  
  d.reg.out[optim.mod.vers.indx,  assignment.label ] <-  TRUE
  
  
  return (d.reg.out)
}



xtract.x.vars <<- function( formula   ){
  
  # test: formula <- form 
  
  formula <- Reduce(paste, deparse(   formula))
  
  # typeof(form.obj)
  
  formula <- as.character(   formula  ) 
  
  
 # formula[ which(formula  == "\"")]
  
  
 # form.x.vars  <- formula[ which(formula  != '' & formula  != '+' & formula  !=  y.var & formula  != '~')]
  
#  Reduce(paste, deparse(   form.x.vars))
  
  
  #strsplit(  form.x.vars , "  ")[[1]]
  
  form.list <- strsplit(  formula , " ")[[1]]
  
  form.x.vars <- form.list[ which( form.list  != '' & form.list != '+' & form.list  !=  y.var.reg & form.list  != '~' & form.list  != 'feed_intake_g_d')]
  
  
 # form.rhs <- form.list[c(3:length(form.list))]
 # form.x.vars <-  form.rhs[ which(form.rhs != '' & form.rhs != '+')]
  
  
  return (form.x.vars)
  
  
}


extract.ordered.vars <<- function( variable , model , mc){
  
  # test: model <- ws.model ; mc <- 1  ; variable <- 'milk_prod_kg_d'
  
  var.names <- listify.coef.names(model , mc)
  
  var.names.ordered <- c()
  
  var.e25 <-  str_c(variable ,'.e25' )
  var.sqt <-  str_c(variable ,'.sqt' )
  var.e75 <-  str_c(variable ,'.e75' )
  var.sqd <-  str_c(variable ,'.sqd' )
  var.cbd <-  str_c(variable ,'.cbd' )

  if (   any( var.names == variable ) )  { var.names.ordered[1] <- var.names[which(str_detect(var.names , variable ))]
  } else { var.names.ordered[1] <- NA }
  
  if (   any( na.omit(str_detect(var.names ,  var.e25 ) )  )  ){ var.names.ordered[2] <- var.names[which(str_detect(var.names ,  var.e25  ))]
  } else { var.names.ordered[2] <- NA }
  
  if (   any(  na.omit(str_detect(var.names ,   var.sqt))  )  ){ var.names.ordered[3] <- var.names[which(str_detect(var.names ,   var.sqt))]
  } else { var.names.ordered[3] <- NA }
  
  if (   any( na.omit(str_detect(var.names ,   var.e75) )  )  ){ var.names.ordered[4] <- var.names[which(str_detect(var.names ,   var.e75 ))]
  } else { var.names.ordered[4] <- NA }
  
  if (   any( na.omit(str_detect(var.names ,  var.sqd) )  )  ){ var.names.ordered[5] <- var.names[which(str_detect(var.names ,  var.sqd ))]
  } else { var.names.ordered[5] <- NA }
  
  if (   any( na.omit(str_detect(var.names ,  var.cbd) )  )  ){ var.names.ordered[6] <- var.names[which(str_detect(var.names ,  var.cbd ))]
  } else { var.names.ordered[7] <- NA }
  
  return (    var.names.ordered   )
  
}


rev.tform.y <<- function(form.lhs , ws.predicted){
  
  test <- function(){
    
    form.lhs <<- form.lhs 
    ws.predicted <<- ws.predicted
    
  }
  
  if ( str_detect( form.lhs , '.log' )  ){  ws.predicted.r.tformed <<- base.nat.log.e  ^ ws.predicted  }
  if ( str_detect( form.rhs , '.e25' )  ){  ws.predicted.r.tformed <<- ws.predicted ^ (1/.25) }
  if ( str_detect( form.rhs , '.sqt' )  ){  ws.predicted.r.tformed <<- ws.predicted ^ (1/.5) }
  if ( str_detect( form.rhs , '.e75' )  ){  ws.predicted.r.tformed <<- ws.predicted ^ (1/.75) }
  if ( str_detect( form.rhs , '.sqd' )  ){  ws.predicted.r.tformed <<- ws.predicted ^ (1/.25) }
  if ( str_detect( form.rhs , '.cbd' )  ){  ws.predicted.r.tformed <<- ws.predicted ^ (1/.25) }
  if ( str_detect( form.rhs , '.recip' )  ){  ws.predicted.r.tformed <<- 1 / ws.predicted }
  
  
  return (  ws.predicted.r.tformed  )
  
}







gb.boostrap.se <- function(  bs.d.reg.ina , bs.form , mod.vers , mod.vers.form.condition  , anim.samp   ){
  
  
if (   !compute.GBR.SEs ){return (d.reg.out)}
  
# Test: bs.d.reg.ina <- all.data  ; bs.form <- formula ; mod.vers <- d.reg.out[r, 'mod.vers'] ; mod.vers.form.condition <- mod.vers.form.condition
  

num.obsvs <- nrow(bs.d.reg.ina)  
  

for (x in 1:num.boostraps ){

# test: x <- 2
  
if (x == 1){ bs.dat <-  data.frame(matrix(NA, nrow = num.boostraps) ) ; bs.dat[, c(vns.coef.bs.All) ] <- NA  }

bs.dat[x ,'iter'] <- x 

bs.dat.r.cond <- (bs.dat$iter == x & !is.na(bs.dat$iter ))
x.vars.se <- NULL


# Re-sample
bs.d.reg.ina.resampled <- bs.d.reg.ina[sample(nrow(bs.d.reg.ina), size =  num.obsvs , replace = TRUE), ]

# Run model
bs.model.return <- gen.reg.model(bs.d.reg.ina.resampled  , bs.form , mod.vers ) 

bs.model <- de.listify(  bs.model.return )[[1]]


# Extract coefficients
bs.model.offset <- as.numeric(   bs.model$offset  )  

bs.x.vars <- coef( bs.model  )  

bs.x.vars.re <- x.vars[ which( str_detect( names(  bs.x.vars) , 'ue.'))  ]

# --- Define coefficient values from model output

ordered.var.names.Int <- extract.ordered.vars( pred.var.Intercept ,    bs.model , mc )


intercept.coef <- as.numeric(  bs.x.vars[ ordered.var.names.Int[1] ]  ) 
offset.coef <- as.numeric(   bs.model.offset   )


bs.dat[x ,'intercept'] <-   intercept.coef 
bs.dat[x ,'offset'] <-        offset.coef

bs.dat  <-    extract.coefficients(  bs.dat , 'boot' ,    bs.model , mc , bs.dat.r.cond , bs.x.vars , x.vars.se , NULL , anim.samp )


}




# Store standard deviation of coefficients in main dataframe, under standard error variable names

for (main.var in c( pred.var.BW , pred.var.ADG , pred.var.NDF , pred.var.ADF , pred.var.CP , pred.var.EE , pred.var.Ash , pred.var.ADL ,   pred.var.NDF_Dig , pred.var.OM_Dig , pred.var.DM_Dig  ,pred.var.DM)) {
  
  # test: main.var <- pred.var.BW
  # ordered.var.names <- extract.ordered.vars(  main.var , model, mc)
  

  if ( main.var == pred.var.BW) {  var.list <-  vns.coef.bs.BW   ; coef.se.var.list <- vns.coef.se.ws.BW 
  } else if(main.var == pred.var.ADG) { var.list <- vns.coef.bs.ADG  ; coef.se.var.list <- vns.coef.se.ws.ADG
  } else if(main.var == pred.var.NDF) {var.list <- vns.coef.bs.NDF ; coef.se.var.list <-  vns.coef.se.ws.NDF
  } else if(main.var == pred.var.ADF) {var.list <- vns.coef.bs.ADF ; coef.se.var.list <- vns.coef.se.ws.ADF
  }else if(main.var == pred.var.CP) {var.list <- vns.coef.bs.CP   ; coef.se.var.list <- vns.coef.se.ws.CP 
  }else if(main.var == pred.var.EE) {var.list <- vns.coef.bs.EE ; coef.se.var.list <- vns.coef.se.ws.EE
  }else if(main.var == pred.var.Ash) {var.list <- vns.coef.bs.Ash  ;  coef.se.var.list <- vns.coef.se.ws.Ash
  }else if(main.var == pred.var.ADL) {var.list <- vns.coef.bs.ADL  ;  coef.se.var.list <- vns.coef.se.ws.ADL
  
  
   }  else if(main.var == pred.var.NDF_Dig) {var.list <- vns.coef.bs.NDF_Dig;  coef.se.var.list <- vns.coef.se.ws.NDF_Dig
   }   else if(main.var == pred.var.OM_Dig) {var.list <- vns.coef.bs.OM_Dig ;  coef.se.var.list <- vns.coef.se.ws.OM_Dig
   }   else if(main.var == pred.var.DM_Dig) {var.list <- vns.coef.bs.DM_Dig ;  coef.se.var.list <- vns.coef.se.ws.DM_Dig
   }   else if(main.var == pred.var.DM) {var.list <- vns.coef.bs.DM;  coef.se.var.list <- vns.coef.se.ws.DM
   }
  
  
  
  
  for (  v in  1: ( length(var.list) -2 )  ){
    
    # v <- 1
    var <-  var.list[v]
    
    
    sd <-  sd(  na.omit(bs.dat[, var ] )) 
    sd <- round( sd , rd.decs.SEs )
    
    d.reg.out[mod.vers.form.condition , coef.se.var.list[v] ] <-   sd 
  
    
    
    
  }
  
}


sd.intercept <- sd( na.omit(bs.dat$intercept ))
sd.offset <- sd( na.omit( bs.dat$offset ))

d.reg.out[mod.vers.form.condition , vn.coef.se.ws.intercept  ] <- sd.intercept 
d.reg.out[mod.vers.form.condition , vn.coef.se.ws.offset ] <- sd.offset


return (d.reg.out)


}


stat.significance <- function( coef, se , anim.samp , k ){
  
  
  test <- function(){
    
    coef <- 3.7
    se <-   .4
    
  }
  
  # range.90.pca <- SE.scalar.90.pci * se
  # range.95.pca <- SE.scalar.95.pci * se
  # range.99.pca <- SE.scalar.99.pci * se
  
  # sign <- ifelse( !between(  0 , ( coef - range.90.pca ) , ( coef + range.90.pca  ) ) , 90 , 0)
  
  
  t.stat <-  abs( coef)  / se 
  
  #   t.stat <-  -6.3	/ 3.9

  df <- anim.samp - k
  
  p.value <- 2 * pt(  abs( t.stat)  , df = df, lower.tail = FALSE)
  
  
  # if (sign == 90){ sign <- ifelse( !between(0, ( coef - range.90.pca ), ( coef + range.90.pca  ) ), 95 , 0) }
  # if (sign == 95){ sign <-  ifelse( !between(0, ( coef - range.90.pca ), ( coef + range.90.pca  ) ), 99 , 0)}
  
  if ( p.value > 0.1000 ){  stars <- 'ns'  }
  if (  p.value < 0.1000 ){  stars <- '*'  }
  if (  p.value < 0.05000 ){  stars <- '**'  }
  if (  p.value < 0.0100 ){  stars <- '***'  }
  
  return (  stars  )
  
}

assign.sign <<- function( data , r ){
  
  
  # test: r <- r ; data <- d.reg.out
  
  data$ws.coef.se.bw_kg
  
  
  
  
  
  
  
  
}

gen.gg.model.label <<- function( rank , cur.mod , mc){
  
  # test: rank <- 3; cur.mod <- 6 ; mc <- 1
  
  
  # Define model class
  if (  mc == model.class.GBR ){   label <- "G" }
  if (  mc == model.class.ME  ){   label <- "M" }
  
  
  # Define tier (1 to 3) 
  if (cur.mod %in%  c(1,2,3) ){  label <- str_c( label , "1")  }
  if (cur.mod %in%  c(4,5,6) ){  label <- str_c( label , "2")  }
  if (cur.mod %in%  c(7,8,9) ){  label <- str_c( label , "3")  }
  
  
  # Define rank within tier (1 to 3)
  if ( rank == 1){   label <- str_c( label , "a") }
  if ( rank  == 2){   label <- str_c( label , "b") }
  if ( rank == 3){   label <- str_c( label , "c") }
  
  
  
  return (label)
}



extract.coefficients <<- function( data, sample , model, mc , row.cond , x.vars , x.vars.se , VIF.dat , anim.samp ){
  
  
  # sample <- 'whole' ; mc <- mc ; model <- ws.model ; row.cond <- mod.vers.form.condition ; data <- d.reg.out ; VIF.dat <- VIF.dat
  


  #  data <- bs.dat  ; sample <- 'boot'  ;  model <-  bs.model  ; mc <- mc  ; row.cond <- bs.dat.r.cond  ; x.vars <- bs.x.vars  ; x.vars.se <- x.vars.se  ; VIF.dat  <-  NULL 
  
  
  k <- length(  x.vars  )
  
  for (  main.var in pred.vars  ) {
    
    # test: main.var <-  pred.vars[2]
    
    ordered.var.names <- extract.ordered.vars(  main.var , model, mc)
    
    

    if ( main.var == pred.var.BW ) {  var.list <-  v.list.BW ; coef.se.var.list <- vns.coef.se.ws.BW  ; vif.vars <- vn.VIF.bw    ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.BW  ; sign.var.list <- vns.coef.sign.ws.BW  } else if (sample == "boot"){ coef.var.list <- vns.coef.bs.BW  }
    } else if( main.var == pred.var.ADG ) { var.list <- v.list.ADG ; coef.se.var.list <- vns.coef.se.ws.ADG  ; vif.vars <- vn.VIF.ADG  ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.ADG ; sign.var.list <- vns.coef.sign.ws.ADG } else if (sample == "boot"){ coef.var.list <- vns.coef.bs.ADG }
    } else if( main.var == pred.var.MY ) { var.list <- v.list.MY ; coef.se.var.list <- vns.coef.se.ws.MY ; vif.vars <- vn.VIF.MY  ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.MY ; sign.var.list <- vns.coef.sign.ws.MY} else if (sample == "boot"){ coef.var.list <- vns.coef.bs.MY }
    } else if( main.var == pred.var.GES ) { var.list <- v.list.GES ; coef.se.var.list <- vns.coef.se.ws.GES  ; vif.vars <- vn.VIF.GES  ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.GES ; sign.var.list <- vns.coef.sign.ws.GES } else if (sample == "boot"){ coef.var.list <- vns.coef.bs.GES }
    } else if( main.var == pred.var.NDF ) {var.list <- v.list.NDF ;  coef.se.var.list <- vns.coef.se.ws.NDF ; vif.vars <- vn.VIF.NDF  ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.NDF ; sign.var.list <- vns.coef.sign.ws.NDF } else if (sample == "boot"){ coef.var.list <- vns.coef.bs.NDF }
    } else if( main.var == pred.var.ADF ) {var.list <- v.list.ADF ; coef.se.var.list <- vns.coef.se.ws.ADF ; vif.vars <- vn.VIF.ADF  ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.ADF  ; sign.var.list <- vns.coef.sign.ws.ADF } else if (sample == "boot"){ coef.var.list <- vns.coef.bs.ADF }
    }else if( main.var == pred.var.CP ) {var.list <- v.list.CP ; coef.se.var.list <- vns.coef.se.ws.CP  ; vif.vars <- vn.VIF.CP  ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.CP ; sign.var.list <- vns.coef.sign.ws.CP } else if (sample == "boot"){ coef.var.list <- vns.coef.bs.CP }
    }else if( main.var == pred.var.EE ) {var.list <- v.list.EE ; coef.se.var.list <- vns.coef.se.ws.EE  ; vif.vars <- vn.VIF.EE  ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.EE ; sign.var.list <- vns.coef.sign.ws.EE } else if (sample == "boot"){ coef.var.list <- vns.coef.bs.EE }
    }else if( main.var == pred.var.Ash ) {var.list <- v.list.Ash ; coef.se.var.list <- vns.coef.se.ws.Ash ; vif.vars <- vn.VIF.Ash  ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.Ash  ; sign.var.list <- vns.coef.sign.ws.Ash } else if (sample == "boot"){ coef.var.list <- vns.coef.bs.Ash }
    }else if( main.var == pred.var.ADL ) {var.list <- v.list.ADL ; coef.se.var.list <- vns.coef.se.ws.ADL  ; vif.vars <- vn.VIF.ADL  ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.ADL ; sign.var.list <- vns.coef.sign.ws.ADL } else if (sample == "boot"){ coef.var.list <- vns.coef.bs.ADL }

    }else if( main.var == pred.var.NDF_Dig) {var.list <- v.list.NDF.dig ; coef.se.var.list <- vns.coef.se.ws.NDF_Dig  ; vif.vars <- vn.VIF.NDF_Dig  ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.NDF_Dig } else if (sample == "boot"){ coef.var.list <- vns.coef.bs.NDF_Dig }
    }else if( main.var == pred.var.OM_Dig ) {var.list <- v.list.OM.dig ;  coef.se.var.list <- vns.coef.se.ws.OM_Dig ; vif.vars <- vn.VIF.OM_Dig ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.OM_Digest  } else if (sample == "boot"){ coef.var.list <- vns.coef.bs.OM_Dig }
    }else if( main.var == pred.var.DM_Dig ) {var.list <- v.list.DM.dig ;  coef.se.var.list <- vns.coef.se.ws.DM_Dig  ; vif.vars <- vn.VIF.DM_Dig  ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.DM_Digest } else if (sample == "boot"){ coef.var.list <- vns.coef.bs.DM_Dig }
    }else if( main.var == pred.var.DM ) { var.list <- v.list.DM  ; coef.se.var.list <- vns.coef.se.ws.DM  ; vif.vars <- vn.VIF.DM ; if (sample == 'whole'){ coef.var.list <- vns.coef.ws.DM } else if (sample == "boot"){ coef.var.list <- vns.coef.bs.DM }
    }
    
      
    # Assign VIF value
    if ( length(VIF.dat) > 1 ){
    if (  sample == 'whole' ) {  
      
    # Update the vif vars set below to include variables for all transformations
    if (  mc == model.class.GBR ) { if (main.var %in%   VIF.dat$Variables    ) { data[ row.cond , vif.vars[1]] <-  round( VIF.dat[ VIF.dat$Variables == main.var , 'VIF' ] , rd.decs.VIF  ) }}
    if (   mc == model.class.ME ) { if ( main.var %in%   names(VIF.dat)   ) { data[ row.cond , vif.vars[1]] <-  round( as.numeric(VIF.dat[ main.var ]) , rd.decs.VIF  ) }}
    
    }}  
      
      
    for (  v in  2: ( length(var.list) - 2 )  ){
      
      
      # v <- 2
      var <-  var.list[v]
      
      # Assign VIF value
      if ( length(VIF.dat) > 1 ){
        if (  sample == 'whole' ) {  
          
          if (  mc == model.class.GBR ) { if (var %in%   VIF.dat$Variables    ) { data[ row.cond , vif.vars[]] <-  round( VIF.dat[ VIF.dat$Variables == main.var , 'VIF' ] , rd.decs.VIF  ) }}
          if (   mc == model.class.ME ) { if ( var %in%   names(VIF.dat)   ) { data[ row.cond , vif.vars[]] <-  round( as.numeric(VIF.dat[ main.var ]) , rd.decs.VIF  ) }}
          
        }}
      
    
      
      coef <- as.numeric(   x.vars[     var  ]   )[1]
      coef <- round( coef , rd.decs.coefs )

      
      data[ row.cond  , coef.var.list[v-1] ] <-  coef
      
      coef.se <- data[row.cond , coef.se.var.list[v-1] ][1]
      
      
      
      if (  mc == model.class.ME  ){
        
        coef.se <- as.numeric(   x.vars.se[     var  ]   )[1]
        coef.se <- round( coef.se, rd.decs.SEs )
        
      data[ row.cond  , coef.se.var.list[v-1] ] <- coef.se
      

      }
      
      
      if (  sample == 'whole' &    !( mc == model.class.GBR   & !compute.GBR.SEs  ) ){ 
        
    
       
      if ( is.na(coef.se)  | is.na(coef)  ){  
        
        sign <- ''
     
       } else {  
        
      sign <- stat.significance(  coef , coef.se , anim.samp , k )
      
       }
      
      data[ row.cond  , sign.var.list[v-1] ] <-  sign
      
      }
       
    }
    
   
    
  }
  
  
  return (data)
  
}

list.sample.conds <<- function( ndf.merge ){  
  
  conds.all <- c()
  
  if (ndf.merge){ 
    

  
  cond.Bind.male <- ( reg.in.cond.bv.lo.ndf.Bind.male |  reg.in.cond.bv.hi.ndf.Bind.male )
  
  
  conds.all[1] <- listify( ( reg.in.cond.bv.lo.ndf.Bind.male |  reg.in.cond.bv.hi.ndf.Bind.male )    )
  
  
  if (n.sexes == 2) { conds.all[length(conds.all) +1 ] <- listify( ( reg.in.cond.bv.lo.ndf.Bind.female |  reg.in.cond.bv.hi.ndf.Bind.female )    )
  } 
  
  if (  n.breeds== 2  ) { 
    
    conds.all[length(conds.all) +1 ] <- listify( ( reg.in.cond.bv.lo.ndf.Btau.male |  reg.in.cond.bv.hi.ndf.Btau.male )    )
    
    if (n.sexes == 2) { conds.all[length(conds.all) +1 ] <- listify( ( reg.in.cond.bv.lo.ndf.Btau.female |  reg.in.cond.bv.hi.ndf.Btau.female )    )
    } 
    
  } 
  
  } else {
    
    
    
    
    cond.lo.NDF.Bind.male <- ( reg.in.cond.bv.lo.ndf.Bind.male )
    cond.hi.NDF.Bind.male <- ( reg.in.cond.bv.hi.ndf.Bind.male )
    
    
    conds.all[1] <- listify( ( reg.in.cond.bv.lo.ndf.Bind.male  )    )
    
    conds.all[length(conds.all) +1] <- listify( ( reg.in.cond.bv.hi.ndf.Bind.male  )    )
    
    
    if (n.sexes == 2) { 
      
      conds.all[length(conds.all) +1 ] <- listify( ( reg.in.cond.bv.lo.ndf.Bind.female)    )
      conds.all[length(conds.all) +1 ] <- listify( (   reg.in.cond.bv.hi.ndf.Bind.female )    )
      
      
        } 
    
    if (  n.breeds== 2  ) { 
      
      
      conds.all[length(conds.all) +1 ] <- listify( ( reg.in.cond.bv.lo.ndf.Btau.male  )    )
      conds.all[length(conds.all) +1 ] <- listify( (  reg.in.cond.bv.hi.ndf.Btau.male )    )
      
      
      
      if (n.sexes == 2) { 
        
        conds.all[length(conds.all) +1 ] <- listify( ( reg.in.cond.bv.lo.ndf.Btau.female  )    )
        conds.all[length(conds.all) +1 ] <- listify( (  reg.in.cond.bv.hi.ndf.Btau.female )    )
        
        
        
          } 
      
    } 
    
    
  }
  
  return ( conds.all )
  
}

get_lower_tri <<- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
