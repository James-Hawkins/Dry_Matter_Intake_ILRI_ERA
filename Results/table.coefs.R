
# source(str_c(  results.dir ,'table.coefs.R')  )


# Table out

print('Running table coefs out')

dat.out <-  data.frame(
  species = NA
)

r.count <- 1

if ( LR.only ){ species <- species.cattle } else {   species <- species.srs }



for (s in species){
  for (b in breeds ) {
    for (sx in sexes  ){
  for (n in ndf.levs ){
    for (mf in 1:n.mod.form){
      #  for (mc in c( model.class.ME , model.class.GBR)){
      
      # Test: s <-  species[1] ; n <- ndf.levs[1] ; mf <- 1  ; mc <- 1 ;  b <- breeds[1] ;  sx <- sexes[1]
      
        # Test: s <-  species[1] ; n <- ndf.levs[1] ; mf <- 1  ; m2 <- 1
        
      print(paste(r.count))
      
      print(paste('Species, breed, sex, ndf, formula are: ' ,s , b , sx, n , mf))
    
      
      # Test: r <- 2
      dat.out[ r.count  , 'species'  ] <- s
      dat.out[ r.count  , 'breed'  ] <- b
      dat.out[ r.count  , 'sex'  ] <- sx
      dat.out[ r.count   , 'ndf'  ] <- n
      dat.out[ r.count  , 'mod.form'  ] <- mf
     
      
   {
        
      dat.row <- d.reg.out[ 
        
        ( 
          d.reg.out$species ==  s 
          &  d.reg.out$breed == b
         &  d.reg.out$ndf ==  n  
            &  d.reg.out$sex == sx
           & d.reg.out$mod.form == mf
             & d.reg.out$is.best.model.in.formula
         #  & d.reg.out$fold == 1
         #   & d.reg.out$mod.class == mc
        
        )
      
        ,    ]
      
      if (  nrow(  dat.row ) == 0 ) { next}
      
      dat.row <-    dat.row[1,]
      
    
      
     
      
      dat.out[ r.count  , 'mod.class'  ] <-  dat.row$mod.class
    
      
      formula <- de.listify(   dat.row$form  )
      

      anim.samp <- dat.row$Total.animal.sample
      
      num.rhs.vars <- length(attr(terms( formula), "term.labels"))
      
      
      

        # Coefficient values
        intercept <- round( dat.row[,vn.coef.ws.intercept ] , 1)
        offset <- round( dat.row[,vn.coef.ws.offset] , 1)
        
        coef.BW <- round( dat.row[,vn.coef.ws.bw] , 1)
        coef.ADG <- round( dat.row[,vn.coef.ws.ADG], 1)
        coef.MY <- round( dat.row[,vn.coef.ws.MY], 1)
        
        coef.NDF <- round( dat.row[,vn.coef.ws.NDF] , 1)
        coef.ADF <- round( dat.row[,vn.coef.ws.ADF] , 1)
        coef.ADL <- round( dat.row[,vn.coef.ws.ADL] , 1)
        
        coef.CP <- round( dat.row[,vn.coef.ws.CP], 1)
        coef.EE <- round( dat.row[,vn.coef.ws.EE], 1)
        coef.Ash <- round( dat.row[,vn.coef.ws.Ash], 1)
        
        coef.NDF.d <- round( dat.row[,vn.coef.ws.NDF_Dig], 1)
        coef.OM.d <- round( dat.row[,vn.coef.ws.OM_Dig], 1)
        coef.DM.d <- round( dat.row[,vn.coef.ws.DM_Dig], 1)
        
        
        
        # Standard error of coefficients
        if (   mc == model.class.GBR & report.GBR.stability  ){  # If model class is GBR, then estimate coefficient SEs from confint command
          
          
          
          
          #   gbr.mod.obj <- de.listify(dat.row[, vn.mod.object.ws]) ; gbr.m.stop <- dat.row[, vn.m.stop]   # }
          
          #   gbr.SEs <- confint( gbr.mod.obj , B = 100, level = 0.6827)
          
          #   gbr.SEs <- gbr.SEs$confint 
          
          
          stabsel.q.value <- num.rhs.vars - 1 
          
          # Compute model stability metrics
          stab <- stabsel(  gbr.mod.obj  , q = stabsel.q.value , cutoff = stabsel.cutoff.value )
          
          stab.probs <- stab$max 
          
          
    
            # Stability metrics
            intercept.stability <-  stab.probs['(Intercept)']
            
            BW.stability <-  stab.probs[  pred.var.BW]
            ADG.stability <-  stab.probs[  pred.var.ADG]
            MY.stability <-  stab.probs[ pred.var.MY]
            
            NDF.stability <-  stab.probs[ pred.var.NDF]
            ADF.stability <-  stab.probs[  pred.var.ADF]
            ADL.stability <-  stab.probs[  pred.var.ADL ]
            
            CP.stability <-  stab.probs[  pred.var.CP ]
            EE.stability <-  stab.probs[  pred.var.EE ]
            Ash.stability <-  stab.probs[  pred.var.Ash ]
            
            NDF.d.stability <-  stab.probs[  pred.var.NDF_Dig ]
            OM.d.stability <-  stab.probs[  pred.var.OM_Dig ]
            DM.d.stability <-  stab.probs[  pred.var.DM_Dig ]
            
            
          } else{
            
            
            intercept.stability <-  0
            
            BW.stability <-  0
            ADG.stability <-  0
            MY.stability <-  0
            
            NDF.stability <-  0
            ADF.stability <-  0
            ADL.stability <-  0
            
            CP.stability <-  0
            EE.stability <-  0
            Ash.stability <-  0
            
            NDF.d.stability <-  0
            OM.d.stability <-  0
            DM.d.stability <-  0
            
          }
          
          
      intercept.se <- round( dat.row[,vn.coef.se.ws.intercept] , 1)
      #offset.se <- round( dat.row[,vn.coef.se.ws.offset ] , 2)
      
      coef.se.BW <- round( dat.row[,  vn.coef.se.ws.bw  ] , 1)
      coef.se.ADG <- round( dat.row[,vn.coef.se.ws.ADG], 1)
      coef.se.MY  <- round( dat.row[,vn.coef.se.ws.MY], 1)
      
      
      coef.se.NDF <- round( dat.row[,vn.coef.se.ws.NDF] , 1)
      coef.se.ADF <- round( dat.row[,vn.coef.se.ws.ADF] , 1)
      coef.se.ADL <- round( dat.row[,vn.coef.se.ws.ADL] , 1)
      
      
      coef.se.CP <- round( dat.row[,vn.coef.se.ws.CP], 1)
      coef.se.EE <- round( dat.row[,vn.coef.se.ws.EE], 1)
      coef.se.Ash <- round( dat.row[,vn.coef.se.ws.Ash], 1)
      
      coef.se.NDF.d <- round( dat.row[,vn.coef.se.ws.NDF_Dig], 1)
      coef.se.OM.d <- round( dat.row[,vn.coef.se.ws.OM_Dig], 1)
      coef.se.DM.d <- round( dat.row[,vn.coef.se.ws.DM_Dig], 1)
      
      
      
   
      
      
      if (  !is.na( intercept )  & !is.na( intercept.se) ) { stars.int <- stat.significance( intercept   , intercept.se ,  anim.samp ,  num.rhs.vars  ) } else { stars.bw <- ''}
      #if (  !is.na( offset)  & !is.na( offset.se) ) { stars.offset <- stat.significance( offset  , offset.se) } else { stars.offset <- ''}
     
       if (  !is.na(coef.BW )  & !is.na(coef.se.BW) ) { stars.bw <- stat.significance( coef.BW , coef.se.BW ,  anim.samp ,  num.rhs.vars) } else { stars.bw <- ''}
      if (  !is.na(coef.ADG )  & !is.na(coef.se.ADG) ) { stars.adg <- stat.significance( coef.ADG  , coef.se.ADG ,  anim.samp ,  num.rhs.vars) } else { stars.adg <- ''}
      if (  !is.na(coef.MY )  & !is.na(coef.se.MY) ) { stars.MY <- stat.significance( coef.MY  , coef.se.MY ,  anim.samp ,  num.rhs.vars) } else { stars.my <- ''}
      
      
      
        if (  !is.na(coef.NDF )  & !is.na(coef.se.NDF) ) { stars.ndf <- stat.significance( coef.NDF  , coef.se.NDF ,  anim.samp,  num.rhs.vars ) } else { stars.ndf <- ''}
      if (  !is.na(coef.ADF )  & !is.na(coef.se.ADF) ) { stars.adf <- stat.significance( coef.ADF  , coef.se.ADF ,  anim.samp,  num.rhs.vars ) } else { stars.adf <- ''}
      if (  !is.na(coef.ADL )  & !is.na(coef.se.ADL) ) { stars.adl <- stat.significance( coef.ADL  , coef.se.ADL ,  anim.samp,  num.rhs.vars ) } else { stars.adl <- ''}
      
      
       if (  !is.na(coef.CP )  & !is.na(coef.se.CP) ) { stars.cp <-stat.significance( coef.CP  , coef.se.CP ,  anim.samp ,  num.rhs.vars) } else { stars.cp <- ''}
      if (  !is.na(coef.EE )  & !is.na(coef.se.EE) ) { stars.cp <-stat.significance( coef.EE  , coef.se.EE ,  anim.samp ,  num.rhs.vars) } else { stars.ee <- ''}
      if (  !is.na(coef.Ash )  & !is.na(coef.se.Ash) ) { stars.ash <-stat.significance( coef.Ash  , coef.se.Ash ,  anim.samp ,  num.rhs.vars) } else { stars.ash <- ''}
      
      if (  !is.na(coef.NDF.d )  & !is.na(coef.se.NDF.d) ) { stars.NDF.d  <-stat.significance( coef.NDF.d , coef.se.NDF.d ,  anim.samp ,  num.rhs.vars) } else { stars.NDF.d <- ''}
      if (  !is.na(coef.OM.d )  & !is.na(coef.se.OM.d) ) { stars.OM.d <-stat.significance( coef.OM.d , coef.se.OM.d ,  anim.samp ,  num.rhs.vars) } else { stars.OM.d <- ''}
      if (  !is.na(coef.DM.d )  & !is.na(coef.se.DM.d) ) { stars.DM.d <-stat.significance( coef.DM.d , coef.se.DM.d ,  anim.samp ,  num.rhs.vars) } else { stars.DM.d <- ''}
      
      
      
      
      pls.mns <- ' \u00B1 '
      intercept.plus.sd <- str_c(     intercept  , pls.mns ,   intercept.se  , stars.int)
      offset <-    offset 
      
      coef.BW.mn.plus.sd <- str_c( coef.BW  , pls.mns , coef.se.BW   , stars.bw)
      coef.ADG.mn.plus.sd <- str_c( coef.ADG , pls.mns , coef.se.ADG  , stars.adg)
      coef.MY.mn.plus.sd <- str_c( coef.MY , pls.mns , coef.se.MY  , stars.my)
      
      
      coef.NDF.mn.plus.sd <- str_c( coef.NDF , pls.mns , coef.se.NDF , stars.ndf)
      coef.ADF.mn.plus.sd <- str_c( coef.ADF , pls.mns , coef.se.ADF , stars.adf)
      coef.ADL.mn.plus.sd <- str_c( coef.ADL , pls.mns , coef.se.ADL , stars.adl)
      
      
      coef.CP.mn.plus.sd <- str_c( coef.CP  , pls.mns , coef.se.CP, stars.cp)
      coef.EE.mn.plus.sd <- str_c( coef.EE  , pls.mns , coef.se.EE, stars.ee)
      coef.Ash.mn.plus.sd <- str_c( coef.Ash  , pls.mns , coef.se.Ash, stars.ash)
      
      coef.NDF.d.mn.plus.sd <- str_c( coef.NDF.d  , pls.mns , coef.se.NDF.d, stars.NDF.d)
      coef.OM.d.mn.plus.sd <- str_c( coef.DM.d  , pls.mns , coef.se.OM.d, stars.OM.d)
      coef.DM.d.mn.plus.sd <- str_c( coef.OM.d  , pls.mns , coef.se.DM.d, stars.DM.d)
      
      
      dat.out[ r.count  , 'intercept'  ] <-  intercept.plus.sd 
      dat.out[ r.count   , 'offset'  ] <-  offset
      
      
      dat.out[ r.count   , 'all.coef.bw.kg'  ] <-  coef.BW.mn.plus.sd
      dat.out[ r.count   , 'all.coef.adg'  ] <-  coef.ADG.mn.plus.sd
      dat.out[ r.count   , 'all.coef.my'  ] <-  coef.MY.mn.plus.sd
      
      
      dat.out[ r.count   , 'all.coef.ndf'  ] <-  coef.NDF.mn.plus.sd
      dat.out[ r.count   , 'all.coef.adf'  ] <-  coef.ADF.mn.plus.sd
      dat.out[ r.count   , 'all.coef.adl'  ] <-  coef.ADL.mn.plus.sd
      
      
      dat.out[ r.count   , 'all.coef.cp'  ] <-  coef.CP.mn.plus.sd
      dat.out[ r.count   , 'all.coef.ee'  ] <-  coef.EE.mn.plus.sd
      dat.out[ r.count   , 'all.coef.ash'  ] <-  coef.Ash.mn.plus.sd
      
      
      dat.out[ r.count   , 'all.coef.ndf.d'  ] <-  coef.NDF.d.mn.plus.sd
      dat.out[ r.count   , 'all.coef.om.d'  ] <-  coef.OM.d.mn.plus.sd
      dat.out[ r.count   , 'all.coef.dm.d'  ] <-  coef.DM.d.mn.plus.sd
      
     # dat.out[ r.count  , 'sd.coef.bw.kg'  ] <-  coef.BW.se 
     # dat.out[ r.count  , 'sd.coef.adg.g.d'  ] <-  coef.ADG.se 
     # dat.out[ r.count  , 'sd.coef.ndf.g.kg'  ] <-  coef.NDF.se 
     # dat.out[ r.count  , 'sd.coef.cp.g.kg'  ] <-  coef.CP.se 
      
      
      # to add: full formula , feature importance, stars for variable significance
      
      
      # stability selection metrics
      dat.out[ r.count  , 'intercept.stability'  ] <-     intercept.stability 
      
      dat.out[ r.count  , 'BW.stability'  ]        <-     BW.stability 
      dat.out[ r.count , 'ADG.stability'  ]       <-     ADG.stability 
      dat.out[ r.count , 'MY.stability'  ]       <-     MY.stability 
      
      dat.out[ r.count , 'NDF.stability'  ] <-     NDF.stability 
      dat.out[ r.count , 'ADF.stability'  ] <-     ADF.stability 
      dat.out[ r.count , 'ADL.stability'  ] <-     ADL.stability 
      
      
      dat.out[ r.count  , 'CP.stability'  ] <-     CP.stability 
      dat.out[ r.count  , 'EE.stability'  ] <-     EE.stability 
      dat.out[ r.count  , 'Ash.stability'  ] <-     Ash.stability 
      
      
      
      if ( mc == model.class.ME ){
        
      me.mod.obj <- de.listify(dat.row[, vn.mod.object.ws ]) 
      
      
      sum.table <- coef(summary(me.mod.obj))  # Method 1 - Satterthwaite
      sum.table <- coef(summary(me.mod.obj, ddf = "Kenward-Roger")) # Method 2
      
      # Extract just the t-values
      t.vals <- sum.table[,"t value"]
      p.vals <- sum.table[,"Pr(>|t|)"]
      
     # p.vals.summd <- 
      
      
      
      t.val.intercept <- round(t.vals['(Intercept)'], 2)
      
      t.val.BW <- round(t.vals[  pred.var.BW], 2)
      t.val.ADG <- round(t.vals[  pred.var.ADG], 2)
      t.val.MY <- round(t.vals[  pred.var.MY], 2)
      
      
      t.val.NDF <- round(t.vals[  pred.var.NDF ], 2)
      t.val.ADF <- round(t.vals[  pred.var.ADF ], 2)
      t.val.ADL <- round(t.vals[   pred.var.ADL ], 2)
      
      
      t.val.CP <- round(t.vals[  pred.var.CP ], 2)
      t.val.EE <- round(t.vals[  pred.var.EE ], 2)
      t.val.Ash <- round(t.vals[  pred.var.Ash ], 2)
      
      
      t.val.NDF.d <- round(t.vals[  pred.var.NDF_Dig ], 2)
      t.val.OM.d <- round(t.vals[  pred.var.OM_Dig ], 2)
      t.val.DM.d <- round(t.vals[  pred.var.DM_Dig ], 2)
      
      p.val.intercept <- round(p.vals['(Intercept)'] , 3)
      
      p.val.BW <- round(p.vals[ pred.var.BW], 3)
      p.val.ADG <- round(p.vals[pred.var.ADG], 3)
      p.val.MY <- round(p.vals[pred.var.MY], 3)
      
      
      p.val.NDF <- round(p.vals[  pred.var.NDF], 3)
      p.val.ADF  <- round(p.vals[  pred.var.ADF], 3)
      p.val.ADL <- round(p.vals[  pred.var.ADL], 3)
      
      
      p.val.CP <- round(p.vals[pred.var.CP],3)
      p.val.EE <- round(p.vals[pred.var.EE],3)
      p.val.Ash <- round(p.vals[pred.var.Ash],3)
      
      p.val.NDF.d <- round(p.vals[pred.var.NDF_Dig],3)
      p.val.OM.d <- round(p.vals[pred.var.OM_Dig],3)
      p.val.DM.d <- round(p.vals[pred.var.DM_Dig],3)
      
      
      
      # stability selection metrics
      dat.out[ r.count  , 't.stat.intercept'  ] <- t.val.intercept 
      
      dat.out[ r.count , 't.stat.BW'  ] <- t.val.BW
      dat.out[ r.count  , 't.stat.ADG'  ] <- t.val.ADG
      dat.out[ r.count  , 't.stat.MY'  ] <- t.val.MY
      
      
      dat.out[ r.count , 't.stat.NDF'  ] <- t.val.NDF
      dat.out[ r.count , 't.stat.ADF'  ] <- t.val.ADF
      dat.out[ r.count , 't.stat.ADL'  ] <- t.val.ADL
      
      
      dat.out[ r.count , 't.stat.CP'  ] <- t.val.CP
      dat.out[ r.count , 't.stat.EE'  ] <- t.val.EE
      dat.out[ r.count , 't.stat.Ash'  ] <- t.val.Ash
      
      dat.out[ r.count , 't.stat.NDF.d'  ] <- t.val.NDF.d
      dat.out[ r.count , 't.stat.OM.d'  ] <- t.val.OM.d
      dat.out[ r.count , 't.stat.DM.d'  ] <- t.val.DM.d
      
    #  dat.out[ r.count , 'p.stat.intercept'  ] <- p.val.intercept 
    #  dat.out[ r.count  , 'p.stat.BW'  ] <- p.val.BW
    #  dat.out[ r.count  , 'p.stat.ADG'  ] <- p.val.ADG
     # dat.out[ r.count  , 'p.stat.NDF'  ] <- p.val.NDF
    #  dat.out[ r.count  , 'p.stat.CP'  ] <- p.val.CP
      
      
      }
      
      }  # Data tabulation

      r.count <- r.count + 1
      # if (r.count == 2) { break}
    }}}}}


nrow(dat.out)
colnames( dat.out)

View(dat.out)

#cols.omit.all <- ('mod.class' )

cols.omit.me <- c( 'offset' , colnames(dat.out)[which( str_detect( colnames(dat.out) ,'stabil'   )  )]  )
cols.omit.gbr <- c( colnames(dat.out)[which( str_detect( colnames(dat.out) ,'.stat'   )  )]  )





write.xlsx(   dat.out[,  !(names(dat.out) %in% cols.omit.gbr | names(dat.out) %in% cols.omit.me )]
             , str_c(results.out.dir.bv.data, "coefs.table.all.xlsx")
             #  ,  sheetName = "Sheet1"
             #  , col.names = TRUE
             #  , row.names = TRUE
             # , append = FALSE
)


