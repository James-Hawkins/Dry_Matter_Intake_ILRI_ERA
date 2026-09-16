



source.all() # Refresh - reload all workspace components


CV.prep() # Prepare data frame to store k-fold CV results


formulae.defs() # Define formulae





for ( r in 1 : (  nrow(d.reg.out.null)  )  ){
  
if (r == 1) { d.reg.out <- d.reg.out.null ; start.time <- Sys.time()}



if (FALSE){
  
r <- 102

}

  #if ( r == 101 ){ break }

{

{
  
  
# Current sample identifiers
species <-   d.reg.out[r, vn.species]   
ndf  <-   d.reg.out[r, vn.ndf] 
sex <- d.reg.out[r,  vn.sex]  
breed <- d.reg.out[r,  vn.breed]  

k <-  d.reg.out[r, vn.k]
mv <- d.reg.out[ r,  vn.mod.vers ]
mc <-  d.reg.out[r ,   vn.mod.class ] 
mf <-  d.reg.out[ r ,  vn.mod.form ]  
mt <-  d.reg.out[ r , vn.tier] 
formula <- de.listify( d.reg.out[r,vn.formula])


print( paste( 'Iteration' ,r , 'of', nrow(d.reg.out) , 'for model', mv , 'and sub-sample: ', breed.labels[breed] , '/'  , ndf.levs[which(ndf == ndf.levs)] , 'NDF, ' , sex.labels[sex]    ) )


cond.ss <-  ( 
               d.reg.out$species == species
              &  d.reg.out$ndf == ndf 
              
                & d.reg.out$sex == sex
                & d.reg.out$breed == breed
)


mod.vers.form.condition <- ( cond.ss 
                             & d.reg.out$mod.vers == mv
                             & d.reg.out$mod.form == mf
                        )


if (   mc == model.class.ME   ){   formula <- update( formula , . ~ . - ue.id)  }



r.cnd.ss.mf <-  ( cond.ss & d.reg.out$mod.form == mf )
r.cnd.ss.mf.mv <-  ( cond.ss & d.reg.out$mod.form == mf & d.reg.out$mod.vers == mv )


# Extract complete id cases
# condition: if first iteration of sub-sample - formula 
if (    any(  is.na(  d.reg.out[ r.cnd.ss.mf , vn.t.IDS.CCs ] ) )  ){


cur.t.id.list <- de.listify( d.reg.out[r, vn.all.treatment.IDs  ] )




if (  fold.by.exp  ){  
  
cur.e.id.list <- de.listify( d.reg.out[r, vn.all.experiment.IDs ] )  
#cur.t.id.list <- d.reg.in[d.reg.in$ue.id %in% cur.e.id.list , 'ut.id'] 
num.exp.units <- length( cur.e.id.list )

if (num.exp.units  <= min.e.units ) { 
  print(paste('Skipping iteration due to insufficient sample size ' , r )) 
  d.reg.out[r, vn.iter.is.NULL ] <- TRUE
  next 
}


}




# Identify only complete cases of treatment IDs
cc.t.id.list <- listify( gen.complete.cases(    cur.t.id.list , formula   )  ) 


cc.e.id.list <- d.reg.in[ which(d.reg.in$ut.id %in% gen.complete.cases(    cur.t.id.list , formula   )  ) ,'ue.id']


if (   length(cc.e.id.list ) <= ( 2 * n.folds)  ) { 
  
  print(paste('Skipping iteration due to insufficient sample size ' , r )) 
  d.reg.out[r, vn.iter.is.NULL ] <- TRUE
  next 
  
  }

d.reg.out[ r.cnd.ss.mf , vn.t.IDS.CCs]  <- cc.t.id.list # Populate list of CC treatment IDs
d.reg.out[ r.cnd.ss.mf , vn.t.IDS.CCs.remaining] <- cc.t.id.list  # Since it is the first fold

}


prev.mod.vers <- n.mod.v.ids[  (which(mv == n.mod.v.ids) - 1)  ]

fold.k.condition <- ( 

  cond.ss 
  
  & d.reg.out$k == k 
  & r.cnd.ss.mf
  & d.reg.out$mod.vers == prev.mod.vers
  ) # If first fold, this takes a value of NA

#' Ideally the sub-samples defined over folds are identical for the entire loop.
#' This conditional therefore identifies the model version corresponding to one step in reverse,
#' for the sample species-NDF-sex-breed combination, formula, class, and fold. 
#' The definitions of samples from this previous iteration are then used again in the current iteration


if ( any(fold.k.condition)  ){ # Holds when there is a previously defined sample from which to replicate

if (       !is.na(( d.reg.out[ fold.k.condition , vn.test.t.IDS]  ) )   ) {  
  
  row <- which(fold.k.condition)
  
  ids.remaining <- de.listify( d.reg.out[row, vn.t.IDS.CCs.remaining]  )
  sampled.ids <- d.reg.out[[ row , vn.test.t.IDS]]
  
  d.reg.out[ r , vn.t.IDS.CCs.remaining] <- listify(ids.remaining )
  
}} else { # Holds when the sample must be newly created (first run)
  
   ids.remaining <- de.listify( d.reg.out[r, vn.t.IDS.CCs.remaining]  )   # In the first iteration, this equals the entire dataset
  
  

  q.total.unique.ids <-  length(unique( d.reg.out[[r, vn.t.IDS.CCs]]  ))
  
  q.ids.to.sample <- ceiling( ( 1  / n.folds ) * q.total.unique.ids)
  
  try.attempt  <- try ( sampled.ids <<- sample(  ids.remaining ,  q.ids.to.sample)   , silent = TRUE )
  
if ( inherits(  try.attempt , "try-error") ) { 
ids.to.sample <-   length(ids.remaining)  #q.ids.to.sample - 1 

try.attempt  <- try ( sampled.ids <<- sample(  ids.remaining ,  ids.to.sample )   , silent = TRUE )

if ( inherits(  try.attempt , "try-error") ) { 

ids.to.sample <-   q.ids.to.sample - 2

sampled.ids <<- sample( ids.remaining ,    ids.to.sample)  
}}

  # In the k+1 fold, set ids remaining equal to those not used up to this point
  ids.remaining <-  ids.remaining[  (which( !(ids.remaining %in% sampled.ids))) ]
  
  next.iter <- (
    cond.ss &  r.cnd.ss.mf.mv & d.reg.out$k == (k + 1)  )
  
  d.reg.out[next.iter , vn.t.IDS.CCs.remaining] <-  listify( ids.remaining )
  
}


} # Sample sub-setting (according to fold)


{
  
# (Treatment) ID lists
all.ids <- de.listify( d.reg.out[r, vn.t.IDS.CCs ] ) 
train.ids <-  de.listify( d.reg.out[r, vn.t.IDS.CCs])[  which(!( d.reg.out[[r, vn.t.IDS.CCs]]  %in% sampled.ids))  ]
test.ids <- de.listify(d.reg.out[r, vn.t.IDS.CCs])[  which(( d.reg.out[[r, vn.t.IDS.CCs]]  %in% sampled.ids))  ]

# Experiment ID lists
all.ids.exmt <- unique( d.reg.in[d.reg.in$ut.id %in% all.ids , 'ue.id'])
train.ids.exmt <- unique( d.reg.in[d.reg.in$ut.id %in% train.ids , 'ue.id'])
test.ids.exmt <- unique( d.reg.in[d.reg.in$ut.id %in% test.ids , 'ue.id'])


# Add to regression DF
d.reg.out[r, vn.train.t.IDS  ] <-  listify( train.ids )
d.reg.out[r, vn.train.t.IDS.length ] <-   length(train.ids)

d.reg.out[r, vn.test.t.IDS ] <-  listify( test.ids )
d.reg.out[r, vn.test.t.IDS.length ] <-   length(test.ids)


# Compute total quantity of animals across all experiments/treatments (to use as weight in regression)
main.data.r.cond <- d.reg.out[r, 'r.cond'][[1]]

all.data <- d.reg.in[ d.reg.in$ut.id %in% all.ids   , ]  #main.data.r.cond



d.reg.out[r, vn.all.data ] <- listify( all.data ) 

total.animal.sample <- 0 ; total.experiment.sample <- 0 ; total.treatment.sample <- 0


for (s.r in 1 : nrow(all.data) ){ 
  
total.animal.sample <- total.animal.sample + all.data[s.r , 'Sample.size'] 

}

total.experiment.sample <- length( unique( all.data$ue.id ) )
total.treatment.sample <- length( unique( all.data$ut.id ) ) 


# Store sample sizes in data frame
d.reg.out[r, vn.total.exp.sample.size ] <- total.experiment.sample
d.reg.out[r, vn.total.trt.sample.size] <- total.treatment.sample
d.reg.out[r, vn.total.animal.sample.size ] <- total.animal.sample



# Set observed values
train.data <- all.data[  all.data$ut.id %in%  train.ids, ]
test.data <- all.data[  all.data$ut.id %in%  test.ids, ]

d.reg.out[r, vn.all.treatment.IDs ] <- listify(all.ids)





} # Data sub-setting according to train/test 

  
{
  

try.reg <- try ( model.return <-  gen.reg.model(  train.data ,  formula , d.reg.out[r, 'mod.vers'] ) , silent = TRUE )

  
if (  inherits(try.reg, "try-error") ) {
  
  print(paste('Skipping iteration due to insufficient sample size ' , r )) 
  d.reg.out[r, vn.iter.is.NULL ] <- TRUE
  next 
  
}
  
model <-  de.listify( model.return )[[1]]

m.stop <-  de.listify( model.return[2] )

nu <-  de.listify( model.return[3] ) 


if (  mc != model.class.ME ){ model.offset <- as.numeric(model$offset) 
}else{ model.offset <- 0 }


# store in dataframe
d.reg.out[r,  vn.mod.object] <- listify ( model )  
d.reg.out[r,   vn.m.stop ] <- m.stop
d.reg.out[r, vn.nu  ]  <-   nu

#' NOTE: we do not need to compute/report coefficients or 
#' performance metrics on the train dataset (only test dataset needed)


} # -- MODEL TRAINING --


if ( k == n.folds ){
  
  
form.lhs <- as.character(formula[[2]])
ws.observed.tformd <-  all.data[,  form.lhs  ] 
  

# Estimate model on entire sample (all folds/observations)
model.return.ws <- gen.reg.model(  all.data  ,  formula , d.reg.out[r, vn.mod.vers] )


ws.model <- de.listify( model.return.ws )[[1]]
ws.m.stop <- de.listify( model.return.ws[2] )
ws.nu <- de.listify( model.return.ws[3] ) 


d.reg.out[ mod.vers.form.condition  , vn.mod.object.ws] <- listify ( ws.model )  

# Variance inflation factor
x.vars <- xtract.x.vars(  formula  )
cols.2.include <- c( x.vars )

pred.var.df <- all.data[  , which( colnames(all.data) %in%  x.vars      )      ]



if ( length( x.vars ) > 1  ) {
if (  mc == model.class.GBR   ) {  VIF.dat  <-  usdm::vif(  pred.var.df)   }
if (  mc == model.class.ME  ) {  VIF.dat  <- car::vif(   model )  }
} else { VIF.dat  <- NA  }


if (  mc %in% c(  model.class.ME , model.class.GBR  )  )  {  # Specific to parametric models
  
 

if ( mc == model.class.GBR ){ ws.model.offset <- as.numeric(ws.model$offset)  
} else { ws.model.offset <- 0  }


  
if (  mc == model.class.GBR ){ x.vars <- coef( ws.model  )  ; x.vars.se <- gb.boostrap.se(   all.data  ,  formula , d.reg.out[r, 'mod.vers'] , total.animal.sample )
}else { x.vars <- fixef(ws.model) ; x.vars.se <- sqrt(diag(vcov(ws.model))) }

x.vars.re <- x.vars[ which( str_detect( names(x.vars) , 'ue.'))  ]
  
  
# --- Define coefficient values from model output

ordered.var.names.Int <- extract.ordered.vars( pred.var.Intercept , ws.model, mc )

# Intercept
ws.intercept.coef <- as.numeric(  x.vars[ ordered.var.names.Int[1] ]  ) 


# Offset (GBR only)
ws.offset.coef <- as.numeric(   ws.model.offset  )



# Store coefficient results
d.reg.out[ mod.vers.form.condition , vn.coef.ws.intercept ] <-  round( ws.intercept.coef , rd.decs.coefs)



# Get bootstrap variable standard errors (GBR only)
if (mc == model.class.GBR ){
  
  d.reg.out <- gb.boostrap.se(  all.data  ,formula , d.reg.out[r, 'mod.vers'] , mod.vers.form.condition , total.animal.sample )
  
  d.reg.out[ mod.vers.form.condition , vn.coef.ws.offset.plus.intercept ] <- round(  (1) * ws.offset.coef + ws.intercept.coef ,  rd.decs.coefs )
  d.reg.out[ mod.vers.form.condition , vn.coef.ws.offset] <-  round( ws.offset.coef , rd.decs.coefs )
  
} else if (mc == model.class.ME ) { 
  ws.intercept.coef.se <- as.numeric(  x.vars.se[ ordered.var.names.Int[1] ]  ) 
  d.reg.out[ mod.vers.form.condition , vn.coef.se.ws.intercept ] <-  round( ws.intercept.coef.se , rd.decs.SEs)
 
}




# Get variable coefficients
d.reg.out <- extract.coefficients(  d.reg.out , 'whole' , ws.model , mc , mod.vers.form.condition , x.vars , x.vars.se  , VIF.dat ,total.animal.sample )



#d.reg.out$ws.coef.sign.bw_kg



if (  random.exp.int ){  ws.predicted <- predict.manual( formula , all.data ,  ws.offset.coef , 'whole' , r  , x.vars.re)              } 
if ( !random.exp.int & mc != model.class.ME ){  ws.predicted <- as.numeric(  predict(ws.model , newdata = all.data )  )          } 
if ( !random.exp.int & mc == model.class.ME ){  ws.predicted <- as.numeric(  predict(ws.model , newdata = all.data , re.form = NA))         } # For now leave as zero until this approach is made robust to ME model


if ( y.var.reg != form.lhs ){ ws.predicted.base <-  as.numeric(rev.tform.y(form.lhs , ws.predicted)) } else {
  
  ws.predicted.base <-  ws.predicted 
}
  
d.reg.out[ mod.vers.form.condition , vn.ws.predicted] <- listify( ws.predicted ) 
d.reg.out[ mod.vers.form.condition , vn.ws.predicted.base ] <- listify( ws.predicted.base ) 



ws.observed.base <- all.data[,y.var.reg]
d.reg.out[ mod.vers.form.condition ,  vn.ws.observed ] <- listify( ws.observed.base )


ws.residuals  <- ws.predicted.base  - ws.observed.base
d.reg.out[ mod.vers.form.condition ,  vn.ws.residuals  ] <- listify(ws.residuals )
d.reg.out[ mod.vers.form.condition , vn.ws.rms.residuals] <- listify( sqrt(ws.residuals^2) )

# Performance metrics



# Non-weighted
d.reg.out[mod.vers.form.condition, vn.ws.R2 ] <- de.listify( gen.eval.metrics( ws.model  , mc  , formula ,  all.data , all.data[,'feed_intake_g_d']  , all.ids , all.ids.exmt , ws.model.offset , 'whole' , r )[[1]] )   
d.reg.out[mod.vers.form.condition, vn.ws.nRMSE ] <-  de.listify( gen.eval.metrics( ws.model  , mc,formula ,   all.data , test.data[,'feed_intake_g_d']  , all.ids , all.ids.exmt , ws.model.offset , 'whole' , r)[[2]] )  
d.reg.out[mod.vers.form.condition, vn.ws.AIC ] <- round( AIC(  ws.model) , rd.decs.AIC )

ccc <- CCC(  as.numeric(ws.predicted.base )  ,  as.numeric(all.data[,y.var.reg] )  , ci = "z-transform", conf.level = 0.95, na.rm = FALSE)
d.reg.out[mod.vers.form.condition, vn.ws.CCC ] <- round( ccc$rho.c$est  , rd.decs.CCC )



# Weighted
d.reg.out[mod.vers.form.condition, vn.ws.w.R2  ] <-  de.listify( gen.eval.metrics( ws.model  ,  mc,formula ,  all.data , all.data[,'feed_intake_g_d']  , all.ids  , all.ids.exmt , ws.model.offset, 'whole' , r)[[3]] )   
d.reg.out[mod.vers.form.condition, vn.ws.w.nRMSE  ] <- de.listify( gen.eval.metrics( ws.model  , mc, formula ,  all.data , all.data[,'feed_intake_g_d']  , all.ids , all.ids.exmt , ws.model.offset , 'whole' , r)[[4]] )  
d.reg.out[mod.vers.form.condition, vn.ws.w.AIC ] <- round( AIC(  ws.model) , rd.decs.AIC )
d.reg.out[mod.vers.form.condition, vn.ws.w.CCC ] <- round(  ccc$rho.c$est  , rd.decs.CCC )




}

} # -- Whole sample model prediction and evaluation (only for k = number of folds)

  
{


if ( mc != model.class.ME  & !random.exp.int  ){  test.predicted <- predict( model , newdata = test.data)  }
if ( mc != model.class.ME   & random.exp.int ){   test.predicted <- predict.manual(  formula  , test.data , model.offset , 'test' , r  , x.vars.re)  }

if ( mc == model.class.ME & !random.exp.int  ){ test.predicted <- predict( model , newdata = test.data , re.form = NA ) }
if ( mc == model.class.ME & random.exp.int ){   test.predicted <- predict( model , newdata = test.data , re.form = NA ) }


# Unweighted metrics
d.reg.out[r, vn.test.r2 ] <- de.listify( gen.eval.metrics( model  , mc, formula ,  test.data , test.data[,'feed_intake_g_d']  , test.ids , test.ids.exmt , model.offset , 'test' , r )[[1]] )   
d.reg.out[r, vn.test.nrmse ] <-  de.listify( gen.eval.metrics( model  , mc,formula ,   test.data , test.data[,'feed_intake_g_d']  , test.ids , test.ids.exmt , model.offset , 'test' , r)[[2]] )  
d.reg.out[r, vn.test.aic  ] <- AIC(  model )

ccc <- CCC(  as.numeric(test.predicted)  ,  as.numeric(test.data[,y.var.reg] )  , ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

d.reg.out[r, vn.test.ccc ] <- ccc$rho.c$est  

# Weighted metrics
d.reg.out[r, vn.test.w.r2 ] <-  de.listify( gen.eval.metrics( model  ,  mc , formula ,  test.data , test.data[,'feed_intake_g_d']  , test.ids , test.ids.exmt , model.offset , 'test' , r)[[3]] )   
d.reg.out[r,  vn.test.w.nrmse ] <- de.listify( gen.eval.metrics( model  , mc, formula ,  test.data , test.data[,'feed_intake_g_d']  , test.ids , test.ids.exmt , model.offset , 'test' , r)[[4]] )  
d.reg.out[r, vn.test.w.aic  ] <- AIC(  model )
d.reg.out[r, vn.test.w.ccc ] <- ccc$rho.c$est  

} # MODEL EVALUATION ON TEST DATASET


if ( k == n.folds ) { # If current fold is last of all folds, calculate average performance metrics across folds
  
  
  # Non-weighted
  d.reg.out[r.cnd.ss.mf.mv, vn.R2.mean ] <- round( mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , vn.test.w.r2])) , rd.decs.R2)
  d.reg.out[r.cnd.ss.mf.mv,  vn.nRMSE.mean ] <- round( mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv , vn.test.w.nrmse])) , rd.decs.nRMSE)
  d.reg.out[r.cnd.ss.mf.mv,  vn.CCC.mean] <- round( mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv , vn.test.ccc])) , rd.decs.nRMSE)
  d.reg.out[r.cnd.ss.mf.mv,  vn.AIC.mean] <- round( mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv , vn.test.aic])) , rd.decs.nRMSE)
  
  
  d.reg.out[r.cnd.ss.mf.mv, vn.R2.sd ] <- round(  sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , vn.test.nrmse])) , rd.decs.R2)
  d.reg.out[r.cnd.ss.mf.mv, vn.nRMSE.sd ] <- round(  sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , vn.test.nrmse])) , rd.decs.nRMSE)
  d.reg.out[r.cnd.ss.mf.mv,  vn.CCC.mean] <- round( sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv , vn.test.ccc])) , rd.decs.nRMSE)
  d.reg.out[r.cnd.ss.mf.mv,  vn.AIC.mean] <- round( sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv , vn.test.aic])) , rd.decs.nRMSE)
  
  # Upper confidence bound
  d.reg.out[r.cnd.ss.mf.mv, vn.R2.ucb ] <-  round(  gen.upper.confidence.bound(  d.reg.out[r.cnd.ss.mf.mv, vn.R2.mean ]   ,  d.reg.out[r.cnd.ss.mf.mv, vn.R2.sd ] ) , rd.decs.R2)
  d.reg.out[r.cnd.ss.mf.mv, vn.nRMSE.ucb ] <- round(  gen.upper.confidence.bound(  d.reg.out[r.cnd.ss.mf.mv, vn.nRMSE.mean ]   ,  d.reg.out[r.cnd.ss.mf.mv, vn.nRMSE.sd ] ) , rd.decs.nRMSE)

  # Weighted
  d.reg.out[r.cnd.ss.mf.mv, vn.w.R2.mean ] <- round(  mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , vn.test.w.r2])) , rd.decs.R2)
  d.reg.out[r.cnd.ss.mf.mv,  vn.w.nRMSE.mean ] <- round(  mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv , vn.test.w.nrmse])) , rd.decs.nRMSE)
  d.reg.out[r.cnd.ss.mf.mv,  vn.CCC.mean] <- round( mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv , vn.test.w.ccc])) , rd.decs.nRMSE)
  d.reg.out[r.cnd.ss.mf.mv,  vn.AIC.mean] <- round( mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv , vn.test.w.aic])) , rd.decs.nRMSE)
  
  
  d.reg.out[r.cnd.ss.mf.mv, vn.w.R2.sd ] <- round(  sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , vn.test.w.r2]))  , rd.decs.R2)
  d.reg.out[r.cnd.ss.mf.mv, vn.w.nRMSE.sd ] <- round(  sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , vn.test.w.nrmse]))  , rd.decs.nRMSE)
  d.reg.out[r.cnd.ss.mf.mv,  vn.CCC.mean] <- round( sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv , vn.test.w.ccc])) , rd.decs.nRMSE)
  d.reg.out[r.cnd.ss.mf.mv,  vn.AIC.mean] <- round( sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv , vn.test.w.aic])) , rd.decs.nRMSE)
 
  
  # Upper confidence bound
  mean.r2 <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , vn.test.w.r2]))
  sd.r2 <- sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , vn.test.w.r2]))
  
  d.reg.out[r.cnd.ss.mf.mv, vn.w.R2.ucb.ur ] <- gen.upper.confidence.bound(    mean.r2  ,  sd.r2) 
  d.reg.out[r.cnd.ss.mf.mv, vn.w.R2.ucb ] <- round(  gen.upper.confidence.bound(    mean.r2  ,   sd.r2 )  , rd.decs.R2)
  d.reg.out[r.cnd.ss.mf.mv, vn.w.nRMSE.ucb ] <-  round(  gen.upper.confidence.bound(  d.reg.out[r.cnd.ss.mf.mv, vn.w.nRMSE.mean ]   ,  d.reg.out[r.cnd.ss.mf.mv, vn.w.nRMSE.sd ] )  , rd.decs.nRMSE)

  
  # Coefficients - means and standard deviations  of coefficients ~ Note: these metrics have little importance and the whole sample metrics are more important

d.reg.out$w.R2.ucb.ur
  
  if (FALSE){ 
  # Average feature importance coefficients
  d.reg.out[ r.cnd.ss.mf.mv , vn.mn.var.imp.int ] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.int']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.mn.var.imp.BW] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.BW']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.mn.var.imp.ADG] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.ADG']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.mn.var.imp.NDF ] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.NDF']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.mn.var.imp.CP ] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.CP']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.mn.var.imp.NDF_digest] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.NDF_digest']))
  
  
  # Average feature importance coefficients -- relative
  d.reg.out[ r.cnd.ss.mf.mv , vn.mn.var.rel.imp.int ] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.rel.int']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.mn.var.rel.imp.BW] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.rel.BW']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.mn.var.rel.imp.ADG ] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.rel.ADG']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.mn.var.rel.imp.NDF] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.rel.NDF']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.mn.var.rel.imp.CP ] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.rel.CP']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.mn.var.rel.imp.NDF_digest] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.rel.NDF_digest']))
  
  
  
  d.reg.out[ r.cnd.ss.mf.mv , vn.sd.var.imp.rel.int ] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.rel.int']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.sd.var.imp.rel.BW] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.rel.BW']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.sd.var.imp.rel.ADG ] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.rel.ADG']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.sd.var.imp.rel.NDF] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.rel.NDF']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.sd.var.imp.rel.CP ] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.rel.CP']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.sd.var.imp.rel.NDF_digest] <- mean(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.rel.NDF_digest']))
  
  
  # Standard deviation in feature importance 
  d.reg.out[ r.cnd.ss.mf.mv , vn.sd.var.imp.int ] <- sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.int']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.sd.var.imp.BW] <- sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.BW']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.sd.var.imp.ADG ] <- sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.ADG']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.sd.var.imp.NDF] <- sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.NDF']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.sd.var.imp.CP] <- sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.CP']))
  d.reg.out[ r.cnd.ss.mf.mv , vn.sd.var.imp.NDF_digest] <- sd(na.omit(d.reg.out[ r.cnd.ss.mf.mv  , 'var.imp.NDF_digest']))
  
  } # Omit variable importance for now
  
  
  
} # Averaging model metrics (across fold)


{
  
# Get starting positions of all instances
mf.later.rows <-  d.reg.out[ 
  
  cond.ss 
  # & d.reg.out$mod.class == mc  
  & d.reg.out$loop.iter > r 
  
  , vn.mod.form ] 


condition.is.final.in.formula <- !(mf %in% mf.later.rows) 


# Model ranking 1 -- best performing among given formula (i.e. across hyperparameters) and given model class

if  ( condition.is.final.in.formula & mc == n.mod.classes ){ # if current mv is last of its kind

  
  ss.mf.mc.cond <- ( 
    cond.ss & d.reg.out$mod.form == mf 
    #  & d.reg.out$mod.class == mc 
    )
  
  d.reg.out <- assign.best.model(   ss.mf.mc.cond, d.reg.out , 'formula' )
  

}



# Model ranking 2 -- rankings among formulae in current tier (all classes)
if ( k == n.folds & mf %% 3 == 0 & mc == n.mod.classes ) {
  

  ss.best.tier.cond <- (
   cond.ss
    & d.reg.out$is.best.model.in.formula 
   #  & d.reg.out$mod.class == mc
   & d.reg.out$tier == mt
      )
  
  d.reg.out <- assign.best.model.tier(ss.best.tier.cond , d.reg.out )


}

# Model ranking 3 -- best among all formulae and classes (for given sub sample)

if ( k == n.folds &  mc == n.mod.classes  & mf ==  n.mod.form ){
  

  ss.best.glob.cond <- (
    cond.ss
    #  & d.reg.out$is.best.model.in.formula 
    # & d.reg.out$is.best.model.in.tier
  )
  
  d.reg.out <- assign.best.model(  ss.best.glob.cond , d.reg.out , 'global' )
  
  
  
  
}





} # Optimal model selection criteria


if ( r == nrow(d.reg.out)){
  
time.elapsed <-  (Sys.time() - start.time)/60
print(paste('Evaluation completed in ' , round(time.elapsed ,1), ' minutes.'))

reg.d.out()


} # Report simulation time
  
  
} # Run all


} # k-fold Cross-validation




reg.d.out()



d.reg.out[d.reg.out$is.NULL ,  'ndf' ]  
d.reg.out[d.reg.out$is.NULL ,  'breed' ] 
d.reg.out[d.reg.out$is.NULL ,  'sex' ] 
d.reg.out[d.reg.out$is.NULL ,  'ndf' ] 
d.reg.out[d.reg.out$is.NULL ,  'ndf' ] 
d.reg.out[d.reg.out$is.NULL ,  'mod.form' ] 


d.reg.out[ 
  
  d.reg.out$mod.form == 9
  &  d.reg.out$ndf == ndf.lev.hi
  &  d.reg.out$sex == sex.female
  &  d.reg.out$breed == breed.indig
  
  & d.reg.out$is.best.model.in.formula , 'mod.class' ]


d.reg.out[ 
  
  d.reg.out$mod.form == 3
  &  d.reg.out$ndf == ndf.lev.hi
  &  d.reg.out$sex == sex.female
  &  d.reg.out$breed == breed.indig
  
  & d.reg.out$is.best.model.in.formula , "rank.among.tier" ]




# View(d.reg.out)


d.reg.out$ws.coef.se.intercept


valid.plots.out() # Produce validation (predicted vs. observed) plots

coefs.table.out() # Produce tables of regression coefficients

box.fig.out()  # Generate box plot figures










