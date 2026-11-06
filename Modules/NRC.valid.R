

MBW <- 600

d.valid <- data.frame( matrix( nrow = 1 , ncol = 10)  )


NRC.form.2.4 <- formula(  feed_intake_g_d ~    bw_kg)


NRC.forms.all <- c(  
  
  NRC.form.2.4 
  
  )


s.conds.valid <- c( listify(reg.in.cond.bv.lo.ndf.Btau.female ))

r.cnt <- 1


for (f in 1 : length(NRC.forms.all) ){
for (samp in 1:length( s.conds.valid )){
  
  if (FALSE){
    
  
  samp <- s.conds.valid[1] 
  f <- 1
  formula <- de.listify( NRC.forms.all[1] ) 
  
  }
  
  s.cond <- de.listify( samp   )[[1]]
  formula <- de.listify( NRC.forms.all[f] )
  
  
  data <- d.reg.in[  s.cond , ]  d.reg.in$feed_intake_g_d
  
  t.IDs <- data$ut.id
  e.IDs <- data$ue.id
  
  form.cond <- d.reg.out$mod.class == model.class.ME & d.reg.out$mod.form = 
    
    
    
 mc <- 3
  model.NRC <- lm(    formula , data =    data )
  
  
  coefs <- model.NRC$coefficients
  model.offset <- 1000
  
  d.valid[ r.cnt , vn.coef.ws.intercept] <- as.numeric(coefs['(Intercept)']) 
  d.valid[ r.cnt , vn.coef.ws.bw] <- as.numeric(coefs[pred.var.BW])
  d.valid[ r.cnt , vn.coef.ws.ADG] <- as.numeric(coefs[pred.var.ADG])
  d.valid[ r.cnt , vn.coef.ws.NDF] <- as.numeric(coefs[pred.var.NDF])
  d.valid[ r.cnt , vn.coef.ws.CP] <- as.numeric(coefs[pred.var.CP])
  d.valid[ r.cnt , vn.coef.ws.NDF_Dig] <- as.numeric(coefs[pred.var.NDF_Dig])
  
  
  
  
  # Unweighted metrics
  d.valid[r.cnt, 'R2' ] <- de.listify( gen.eval.metrics( model.NRC   , mc, formula ,   data  ,  data[,'feed_intake_g_d']  , t.IDs, e.IDs , model.offset , 'valid' , r.cnt )[[1]] )   
  d.valid[r.cnt, 'nrmse' ] <- de.listify( gen.eval.metrics( model.NRC   , mc, formula ,   data  ,  data[,'feed_intake_g_d']  , t.IDs, e.IDs , model.offset , 'valid' , r.cnt )[[2]] )   
  d.valid[r.cnt, 'w.R2' ] <- de.listify( gen.eval.metrics( model.NRC   , mc, formula ,   data  ,  data[,'feed_intake_g_d']  , t.IDs, e.IDs , model.offset , 'valid' , r.cnt )[[3]] )   
  d.valid[r.cnt, 'w.nrmse' ] <- de.listify( gen.eval.metrics( model.NRC   , mc, formula ,   data  ,  data[,'feed_intake_g_d']  , t.IDs, e.IDs , model.offset , 'valid' , r.cnt )[[4]] )   
  
 
  
  r.cnt  <- 1 + r.cnt 
}}




