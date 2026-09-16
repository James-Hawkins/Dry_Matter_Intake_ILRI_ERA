

{
  
plot.vars <- c('bw' , 'adg' , 'ndf' , 'cp')
plot.vars.labs <- c('BW (kg)' , 'ADG (g/d)' , 'Diet NDF (g/kg)' , 'Diet CP (g/kg)')
  

y.tit <- paste(intToUtf8(916) , 'DMI (g/d)')

gg.coefs.gen <<- ggplot(  ) +
  theme(
    panel.grid.major = element_blank()
    ,panel.background = element_blank()
    ,panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    
    ,strip.background = element_rect(color='black', fill='white', size=1, linetype="solid")
    ,strip.text.x = element_text(size =  8.5 , color = 'black'  )
    
    ,strip.text.y = element_text(size =  8.5  , color = 'black'  )
    
    , axis.text.x = element_text( angle = 90 , hjust = 1 , vjust = 0.5 , size = 8.75)
    , axis.text.y = element_text( hjust = 1 , vjust = 0.5 , size = 10.5)
    
    ,   axis.title.x = element_blank()
  ) +
  # ylab('Coefficient (U+0394 DMI (g/d)') #+
  ylab(paste(intToUtf8(916) , 'DMI (g/d)'))
# xlab('Independent variable')


gen.gg.coefs <- function(data){
  
 # data <-  dat.coefs.gbr.sp.lon.male
  

plot <- gg.coefs.gen  %>% + 
    geom_point(   plot.dat , mapping = aes( x = var.lab ,y =coef.base) , stat = "identity" , fill = 'darkgrey' ) +
  geom_errorbar(   plot.dat  , mapping = aes( x = var.lab , ymin =  coef.base - se , ymax= coef.base + se   )   , width = 0.45 , linewidth = 0.35) +
    #facet_nested( . ~  species + ndf + mod.form  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "pink", linewidth = 0.4) +
    facet_nested(
      rows = vars(
        group.label 
         , sex.label
        )
      , cols = vars(
        model.label.r1 
         , model.label.r2
        )
      
      ,scales = "free"
      , space = "free"
      
    )+
  #  scale_x_discrete(drop = FALSE) 
  scale_x_discrete(limits = ordered.var.labs , drop = TRUE)

  return (  plot  )

}


gen.coef.df <- function(){
  
  dat.coefs <-  data.frame(
  species = NA
)

return (dat.coefs)
  
}

} # Data prep




{ 
  
r.count <- 1
  
if ( LR.only ){ species <- species.cattle } else {   species <- species.srs }
  
  

for (s in species){
  for (n in 1:length(ndf.levs) ){
    
    for (b in breeds )
    
    
    for (mf in 1:n.mod.form){
      for (v in all.x.vars   ){ 
        for (mc in c( model.class.GBR , model.class.ME)){
          for (sex in c(sex.male, sex.female)  ){
            
            if (r.count == 1){  dat.coefs <- gen.coef.df() }
            
            
            if (FALSE) { 
              
              s <-  species[1]
              b <- breed.indig
              n <- 1 
              mf <- 1  
              v <-  "bw_kg" 
              mc <- model.class.GBR
              sex <- sex.male
            
            }
            
            
            #    if( sex == sex.female & mc == model.class.GBR) { next }     
            
            print(paste('variable is ', v))
            
            #    if (  v =="NDF_nutrition" ) { return(1) }
            
            dat.row <- d.reg.out[ 
              
              (
                
                d.reg.out$species ==  s 
                &  d.reg.out$breed ==  b 
               &  d.reg.out$ndf ==  ndf.levs[n]  
               & d.reg.out$mod.form == mf
               & d.reg.out$mod.class == mc
               & d.reg.out$sex == sex
               #  & d.reg.out$is.best.model
               & d.reg.out$fold == 1
               
              )
              ,    ]
            
            
            formula <- dat.row$form
            formula <- Reduce(paste, deparse(   formula))
            formula <- as.character(   formula  ) 
            form.list <- strsplit(  formula , " ")[[1]]
            form.list <- gsub(")", "", form.list)
            
            if(  !( v %in%    form.list )  ) { print(paste("Skipping for variable",v )) ; next}
            
          
            dat.coefs[ r.count  , 'species'  ] <- s 
            dat.coefs[ r.count  , 'breed'  ] <- b
            dat.coefs[ r.count  , 'ndf'  ] <-  ndf.levs[n] 
            dat.coefs[ r.count  , 'ndf.lab'  ] <-  ndf.labs[n]
            dat.coefs[ r.count  , 'mod.class'  ] <- mc 
            dat.coefs[ r.count  , 'mod.form'  ] <- mf 
            dat.coefs[ r.count  , 'mod.form.lab'  ] <- str_c('Model ', mf)
            dat.coefs[ r.count  , 'var'  ] <- v
            dat.coefs[ r.count  , 'var.lab'  ] <-  as.character(gen.var.alias(  v  ) ) # 'test.lab' #plot.vars.labs[which(plot.vars == v)]
            dat.coefs[ r.count  , 'sex'  ] <- sex
            
            
            #    print(paste('Variable and label are:' , v ,            dat.coefs[ r.count  , 'var.lab'  ] ))
        
            tier.rank <- dat.row$rank.among.tier
            
            mod.id <- gen.gg.model.label(  tier.rank, mf ,mc )
            
            model.label.r1 <- str_c( mod.id ) 
            
   
            experiment.sample <- dat.row[ , vn.total.exp.sample.size ] 
            treatment.sample <-  dat.row[ , vn.total.trt.sample.size ] 
            animal.sample <-  dat.row[ , vn.total.animal.sample.size ] 
            
            
            model.label.r2 <- str_c( 
              lab.total.experiment.sample , ' = ' ,  experiment.sample
              , '    ' ,  lab.total.treatment.sample , ' = ' ,     treatment.sample
              , '    ' , lab.total.animal.sample , ' = ' ,  animal.sample
            )
            
            
            dat.coefs[ r.count  , 'model.label.r1'  ] <-  model.label.r1
            dat.coefs[ r.count  , 'model.label.r2'  ] <-  model.label.r2
            
            
            if (  mc == model.class.ME  ){  group.label <- 'Mixed effects' ; sex.label <- 'All sexes'
            } else if (  model.class.GBR  == 1 & sex == sex.male){  group.label <- 'Gradient boosted'  ; sex.label <- 'Male'
            }else if ( model.class.GBR == 1 & sex == sex.female){  group.label <- 'Gradient boosted'  ; sex.label <- 'Female'
            }
            
            dat.coefs[ r.count  , 'group.label'  ] <-  group.label
            dat.coefs[ r.count  , 'sex.label'  ] <-  sex.label
            
            # Coefficients
            
            if ( v %in% all.BW.vars ) {    
              
              if (  v == all.BW.vars[1]  ) { var.lab.coef <-  vn.coef.ws.bw ; var.lab.se <-  vns.coef.se.ws.BW[1] # ; var.lab.vimp <- vn.mn.var.rel.imp.BW ; var.lab.vimp.se <- vn.sd.var.imp.BW 
              } else if (  v == all.BW.vars[2]  ) { var.lab.coef <-  vn.coef.ws.bw.e25 ; var.lab.se <-  vns.coef.se.ws.BW[2] # ; var.lab.vimp <- vn.mn.var.rel.imp.BW ; var.lab.vimp.se <- vn.sd.var.imp.BW 
              } else if (  v == all.BW.vars[3]  ) { var.lab.coef <-  vn.coef.ws.bw.sqt ; var.lab.se <-  vns.coef.se.ws.BW[3]# ; var.lab.vimp <- vn.mn.var.rel.imp.BW ; var.lab.vimp.se <- vn.sd.var.imp.BW  
              } else if (  v == all.BW.vars[4]  ) { var.lab.coef <-  vn.coef.ws.bw.e75 ; var.lab.se <-  vns.coef.se.ws.BW[4] # ; var.lab.vimp <- vn.mn.var.rel.imp.BW ; var.lab.vimp.se <- vn.sd.var.imp.BW 
              } else  if (  v == all.BW.vars[5]  ) { var.lab.coef <-  vn.coef.ws.bw.sqd ; var.lab.se <-  vns.coef.se.ws.BW[5] # ; var.lab.vimp <- vn.mn.var.rel.imp.BW ; var.lab.vimp.se <- vn.sd.var.imp.BW 
              } else if (  v == all.BW.vars[6]  ) { var.lab.coef <-  vn.coef.ws.bw.cbd ; var.lab.se <-  vns.coef.se.ws.BW[6] # ; var.lab.vimp <- vn.mn.var.rel.imp.BW ; var.lab.vimp.se <- vn.sd.var.imp.BW 
              } # else  if (  v == all.BW.vars[7]  ) { var.lab.coef <-  vn.coef.ws.bw.log ; var.lab.se <-  vn.sd.coef.BW  ; var.lab.vimp <- vn.mn.var.rel.imp.BW ; var.lab.vimp.se <- vn.sd.var.imp.BW  }
              

            } else if ( v %in% all.ADG.vars ) {
              
              if (  v == all.ADG.vars[1]  ) { var.lab.coef <-  vn.coef.ws.ADG ; var.lab.se <-  vns.coef.se.ws.ADG[1] # ; var.lab.vimp <- vn.mn.var.rel.imp.ADG ; var.lab.vimp.se <- vn.sd.var.imp.ADG 
              } else if (  v == all.ADG.vars[2]  ) { var.lab.coef <-  vn.coef.ws.ADG.e25 ; var.lab.se <-  vns.coef.se.ws.ADG[2] # ; var.lab.vimp <- vn.mn.var.rel.imp.ADG ; var.lab.vimp.se <- vn.sd.var.imp.ADG 
              } else if (  v == all.ADG.vars[3]  ) { var.lab.coef <-  vn.coef.ws.ADG.sqt ; var.lab.se <-  vns.coef.se.ws.ADG[3] # ; var.lab.vimp <- vn.mn.var.rel.imp.ADG ; var.lab.vimp.se <- vn.sd.var.imp.ADG  
              } else  if (  v == all.ADG.vars[4]  ) { var.lab.coef <-  vn.coef.ws.ADG.e75 ; var.lab.se <-  vns.coef.se.ws.ADG[4] # ; var.lab.vimp <- vn.mn.var.rel.imp.ADG ; var.lab.vimp.se <- vn.sd.var.imp.ADG 
              } else if (  v == all.ADG.vars[5]  ) { var.lab.coef <-  vn.coef.ws.ADG.sqd ; var.lab.se <-  vns.coef.se.ws.ADG[5]# ; var.lab.vimp <- vn.mn.var.rel.imp.ADG ; var.lab.vimp.se <- vn.sd.var.imp.ADG 
              } else  if (  v == all.ADG.vars[6]  ) { var.lab.coef <-  vn.coef.ws.ADG.cbd ; var.lab.se <-  vns.coef.se.ws.ADG[6] # ; var.lab.vimp <- vn.mn.var.rel.imp.ADG ; var.lab.vimp.se <- vn.sd.var.imp.ADG  
              } # else  if (  v == all.ADG.vars[7]  ) { var.lab.coef <-  vn.coef.ws.ADG.log ; var.lab.se <-  vn.sd.coef.ADG  ; var.lab.vimp <- vn.mn.var.rel.imp.ADG ; var.lab.vimp.se <- vn.sd.var.imp.ADG  }
              
            } else  if ( v %in% all.NDF.vars ) {
              
              if (  v == all.NDF.vars[1]  ) { var.lab.coef <-  vn.coef.ws.NDF ; var.lab.se <-  vns.coef.se.ws.NDF[1] # ; var.lab.vimp <- vn.mn.var.rel.imp.NDF ; var.lab.vimp.se <- vn.sd.var.imp.NDF 
              } else  if (  v == all.NDF.vars[2]  ) { var.lab.coef <-  vn.coef.ws.NDF.e25 ; var.lab.se <-  vns.coef.se.ws.NDF[2]# ; var.lab.vimp <- vn.mn.var.rel.imp.NDF ; var.lab.vimp.se <- vn.sd.var.imp.NDF 
              } else if (  v == all.NDF.vars[3]  ) { var.lab.coef <-  vn.coef.ws.NDF.sqt ; var.lab.se <-  vns.coef.se.ws.NDF[3] # ; var.lab.vimp <- vn.mn.var.rel.imp.NDF ; var.lab.vimp.se <- vn.sd.var.imp.NDF 
              } else  if (  v == all.NDF.vars[4]  ) { var.lab.coef <-  vn.coef.ws.NDF.e75 ; var.lab.se <-  vns.coef.se.ws.NDF[4]  #; var.lab.vimp <- vn.mn.var.rel.imp.NDF ; var.lab.vimp.se <- vn.sd.var.imp.NDF 
              } else if (  v == all.NDF.vars[5]  ) { var.lab.coef <-  vn.coef.ws.NDF.sqd ; var.lab.se <-  vns.coef.se.ws.NDF[5] # ; var.lab.vimp <- vn.mn.var.rel.imp.NDF ; var.lab.vimp.se <- vn.sd.var.imp.NDF 
              } else if (  v == all.NDF.vars[6]  ) { var.lab.coef <-  vn.coef.ws.NDF.cbd ; var.lab.se <- vns.coef.se.ws.NDF[6] # ; var.lab.vimp <- vn.mn.var.rel.imp.NDF ; var.lab.vimp.se <- vn.sd.var.imp.NDF
              } # else   if (  v == all.NDF.vars[7]  ) { var.lab.coef <-  vn.coef.ws.NDF.log ; var.lab.se <-  vn.sd.coef.NDF  ; var.lab.vimp <- vn.mn.var.rel.imp.NDF ; var.lab.vimp.se <- vn.sd.var.imp.NDF  }
              
            } else    if ( v %in% all.ADF.vars ) {
              
              if (  v == all.ADF.vars[1]  ) { var.lab.coef <-  vn.coef.ws.ADF ; var.lab.se <-  vn.coef.se.ws.ADF # ; var.lab.vimp <- vn.mn.var.rel.imp.ADF ; var.lab.vimp.se <- vn.sd.var.imp.ADF   
              } else  if (  v == all.ADF.vars[2]  ) { var.lab.coef <-  vn.coef.ws.ADF.e25 ; var.lab.se <-  vn.coef.se.ws.ADF.e25 #  ; var.lab.vimp <- vn.mn.var.rel.imp.ADF ; var.lab.vimp.se <- vn.sd.var.imp.ADF 
              } else if (  v == all.ADF.vars[3]  ) { var.lab.coef <-  vn.coef.ws.ADF.sqt ; var.lab.se <-  vn.coef.se.ws.ADF.sqt  # ; var.lab.vimp <- vn.mn.var.rel.imp.ADF ; var.lab.vimp.se <- vn.sd.var.imp.ADF 
              } else  if (  v == all.ADF.vars[4]  ) { var.lab.coef <-  vn.coef.ws.ADF.e75 ; var.lab.se <-  vn.coef.se.ws.ADF.e75  # ; var.lab.vimp <- vn.mn.var.rel.imp.ADF ; var.lab.vimp.se <- vn.sd.var.imp.ADF 
              } else if (  v == all.ADF.vars[5]  ) { var.lab.coef <-  vn.coef.ws.ADF.sqd ; var.lab.se <-  vn.coef.se.ws.ADF.sqd #  ; var.lab.vimp <- vn.mn.var.rel.imp.ADF ; var.lab.vimp.se <- vn.sd.var.imp.ADF
              } else  if (  v == all.ADF.vars[6]  ) { var.lab.coef <-  vn.coef.ws.ADF.cbd ; var.lab.se <-  vn.coef.se.ws.ADF.cbd  # ; var.lab.vimp <- vn.mn.var.rel.imp.ADF ; var.lab.vimp.se <- vn.sd.var.imp.ADF 
              } #  if (  v == all.ADF.vars[7]  ) { var.lab.coef <-  vn.coef.ws.ADF.log ; var.lab.se <-  vn.sd.coef.ADF  ; var.lab.vimp <- vn.mn.var.rel.imp.ADF ; var.lab.vimp.se <- vn.sd.var.imp.ADF  }
              
            } else  if ( v %in% all.CP.vars ) {
              
              if (  v == all.CP.vars[1]  ) { var.lab.coef <-  vn.coef.ws.CP ; var.lab.se <-  vn.coef.se.ws.CP # ; var.lab.vimp <- vn.mn.var.rel.imp.CP ; var.lab.vimp.se <- vn.sd.var.imp.CP 
              } else if (  v == all.CP.vars[2]  ) { var.lab.coef <-  vn.coef.ws.CP.e25 ; var.lab.se <-  vn.coef.se.ws.CP.e25  #; var.lab.vimp <- vn.mn.var.rel.imp.CP ; var.lab.vimp.se <- vn.sd.var.imp.CP 
              } else  if (  v == all.CP.vars[3]  ) { var.lab.coef <-  vn.coef.ws.CP.sqt ; var.lab.se <-  vn.coef.se.ws.CP.sqd # ; var.lab.vimp <- vn.mn.var.rel.imp.CP ; var.lab.vimp.se <- vn.sd.var.imp.CP 
              } else  if (  v == all.CP.vars[4]  ) { var.lab.coef <-  vn.coef.ws.CP.e75 ; var.lab.se <-  vn.coef.se.ws.CP.e75  #; var.lab.vimp <- vn.mn.var.rel.imp.CP ; var.lab.vimp.se <- vn.sd.var.imp.CP 
              } else  if (  v == all.CP.vars[5]  ) { var.lab.coef <-  vn.coef.ws.CP.sqd ; var.lab.se <-  vn.coef.se.ws.CP.sqd # ; var.lab.vimp <- vn.mn.var.rel.imp.CP ; var.lab.vimp.se <- vn.sd.var.imp.CP  
              } else if (  v == all.CP.vars[6]  ) { var.lab.coef <-  vn.coef.ws.CP.cbd ; var.lab.se <-  vn.coef.se.ws.CP.cbd # ; var.lab.vimp <- vn.mn.var.rel.imp.CP ; var.lab.vimp.se <- vn.sd.var.imp.CP 
              } #  if (  v == all.CP.vars[7]  ) { var.lab.coef <-  vn.coef.ws.CP.log ; var.lab.se <-  vn.coef.se.ws.CP  ; var.lab.vimp <- vn.mn.var.rel.imp.CP ; var.lab.vimp.se <- vn.sd.var.imp.CP  }
              
            } else  if ( v %in% all.EE.vars ) {
              
              if (  v == all.EE.vars[1]  ) { var.lab.coef <-  vn.coef.ws.EE ; var.lab.se <-  vn.coef.se.ws.EE  ; var.lab.vimp <- vn.mn.var.rel.imp.EE ; var.lab.vimp.se <- vn.sd.var.imp.EE  
              } else  if (  v == all.EE.vars[2]  ) { var.lab.coef <-  vn.coef.ws.EE.e25 ; var.lab.se <-  vn.coef.se.ws.EE.e25  ; var.lab.vimp <- vn.mn.var.rel.imp.EE ; var.lab.vimp.se <- vn.sd.var.imp.EE 
              } else  if (  v == all.EE.vars[3]  ) { var.lab.coef <-  vn.coef.ws.EE.sqt ; var.lab.se <-  vn.coef.se.ws.EE.sqt  ; var.lab.vimp <- vn.mn.var.rel.imp.EE ; var.lab.vimp.se <- vn.sd.var.imp.EE 
              } else  if (  v == all.EE.vars[4]  ) { var.lab.coef <-  vn.coef.ws.EE.e75 ; var.lab.se <-  vn.coef.se.ws.EE.e75  ; var.lab.vimp <- vn.mn.var.rel.imp.EE ; var.lab.vimp.se <- vn.sd.var.imp.EE 
              } else  if (  v == all.EE.vars[5]  ) { var.lab.coef <-  vn.coef.ws.EE.sqd ; var.lab.se <-  vn.coef.se.ws.EE.sqd  ; var.lab.vimp <- vn.mn.var.rel.imp.EE ; var.lab.vimp.se <- vn.sd.var.imp.EE 
              } else  if (  v == all.EE.vars[6]  ) { var.lab.coef <-  vn.coef.ws.EE.cbd ; var.lab.se <-  vn.coef.se.ws.EE.cbd  ; var.lab.vimp <- vn.mn.var.rel.imp.EE ; var.lab.vimp.se <- vn.sd.var.imp.EE  
              } # if (  v == all.EE.vars[7]  ) { var.lab.coef <-  vn.coef.ws.EE.log ; var.lab.se <-  vn.sd.coef.EE  ; var.lab.vimp <- vn.mn.var.rel.imp.EE ; var.lab.vimp.se <- vn.sd.var.imp.EE  }
              
            } else  if ( v %in% all.Ash.vars ) {
              
              if (  v == all.Ash.vars[1]  ) { var.lab.coef <-  vn.coef.ws.Ash ; var.lab.se <-  vn.coef.se.ws.Ash # ; var.lab.vimp <- vn.mn.var.rel.imp.Ash ; var.lab.vimp.se <- vn.sd.var.imp.Ash  
              } else if (  v == all.Ash.vars[2]  ) { var.lab.coef <-  vn.coef.ws.Ash.e25 ; var.lab.se <-  vn.coef.se.ws.Ash.e25 #  ; var.lab.vimp <- vn.mn.var.rel.imp.Ash ; var.lab.vimp.se <- vn.sd.var.imp.Ash 
              } else if (  v == all.Ash.vars[3]  ) { var.lab.coef <-  vn.coef.ws.Ash.sqt ; var.lab.se <-  vn.coef.se.ws.Ash.sqt #  ; var.lab.vimp <- vn.mn.var.rel.imp.Ash ; var.lab.vimp.se <- vn.sd.var.imp.Ash 
              } else if (  v == all.Ash.vars[4]  ) { var.lab.coef <-  vn.coef.ws.Ash.e75 ; var.lab.se <-  vn.coef.se.ws.Ash.e75 #  ; var.lab.vimp <- vn.mn.var.rel.imp.Ash ; var.lab.vimp.se <- vn.sd.var.imp.Ash 
              } else if (  v == all.Ash.vars[5]  ) { var.lab.coef <-  vn.coef.ws.Ash.sqd ; var.lab.se <-  vn.coef.se.ws.Ash.sqd #  ; var.lab.vimp <- vn.mn.var.rel.imp.Ash ; var.lab.vimp.se <- vn.sd.var.imp.Ash
              } else if (  v == all.Ash.vars[6]  ) { var.lab.coef <-  vn.coef.ws.Ash.cbd ; var.lab.se <-  vn.coef.se.ws.Ash.cbd #  ; var.lab.vimp <- vn.mn.var.rel.imp.Ash ; var.lab.vimp.se <- vn.sd.var.imp.Ash 
              } # if (  v == all.Ash.vars[7]  ) { var.lab.coef <-  vn.coef.ws.Ash.log ; var.lab.se <-  vn.sd.coef.Ash  ; var.lab.vimp <- vn.mn.var.rel.imp.Ash ; var.lab.vimp.se <- vn.sd.var.imp.Ash  }
              
            }
            
            
            coef.base <- round( dat.row[,  var.lab.coef ]  , 10)
            se <- round( dat.row[, var.lab.se   ], 5)
    
           
            
            # coef.base <- - 1* coef.base
            
            if (is.na(coef.base)) {next}
            

            if ( any( str_detect( var.lab.coef , suffixes) ) ) { # Check if variable is transformed 
              
              if ( str_detect( var.lab.coef , suffixes[1] ) ){  if (coef.base < 0) { coef.base <- -1* coef.base ; corrector <- -1 } ; coef.scaled  <- corrector * coef.base  ^(1/0.25) ; marg.fct <- coef.scaled * mean(na.omit(reg.dat[reg.dat$Species == species & reg.dat$ndf.level == ndf & !is.na(reg.dat$ndf.level) & !is.na(reg.dat$Species),v])) }
              if ( str_detect( var.lab.coef , suffixes[2] ) ){ if (coef.base < 0) { coef.base <- -1* coef.base ; corrector <- -1 } ; coef.scaled  <- corrector * coef.base ^(1/0.5)  ; marg.fct <- coef.scaled * mean(na.omit(reg.dat[reg.dat$Species == species & reg.dat$ndf.level == ndf & !is.na(reg.dat$ndf.level) & !is.na(reg.dat$Species),v])) }
              if ( str_detect( var.lab.coef , suffixes[3] ) ){  if (coef.base < 0) { coef.base <- -1* coef.base ; corrector <- -1 } ; coef.scaled  <- corrector * coef.base  ^(1/0.75) ; marg.fct <- coef.scaled * mean(na.omit(reg.dat[reg.dat$Species == species & reg.dat$ndf.level == ndf & !is.na(reg.dat$ndf.level) & !is.na(reg.dat$Species),v]))  }
              if ( str_detect( var.lab.coef , suffixes[4] ) ){ if (coef.base < 0) { coef.base <- -1* coef.base ; corrector <- -1 } ; coef.scaled  <- corrector * coef.base  ^(1/2) ; marg.fct <- coef.base * mean(na.omit(reg.dat[reg.dat$Species == species & reg.dat$ndf.level == ndf & !is.na(reg.dat$ndf.level) & !is.na(reg.dat$Species),v]))^2  }
              if ( str_detect( var.lab.coef , suffixes[5] ) ){  if (coef.base < 0) { coef.base <- -1* coef.base ; corrector <- -1 } ; coef.scaled  <- corrector * coef.base  ^(1/3)  ; marg.fct <- coef.base * mean(na.omit(reg.dat[reg.dat$Species == species & reg.dat$ndf.level == ndf & !is.na(reg.dat$ndf.level) & !is.na(reg.dat$Species),v]))^3  }
              if ( str_detect( var.lab.coef , suffixes[6] ) ){ if (coef.base < 0) { coef.base <- -1* coef.base ; corrector <- -1 } ; coef.scaled  <- corrector * base.nat.log.e^(coef.base ) ; marg.fct <- coef.scaled * mean(na.omit(reg.dat[reg.dat$Species == species & reg.dat$ndf.level == ndf & !is.na(reg.dat$ndf.level) & !is.na(reg.dat$Species),v]))  }  
              if ( str_detect( var.lab.coef , suffixes[7] ) ){ if (coef.base < 0) { coef.base <- -1* coef.base ; corrector <- -1 } ; coef.scaled  <- corrector * 1 / coef.base ; marg.fct <- coef.scaled * mean(na.omit(reg.dat[reg.dat$Species == species & reg.dat$ndf.level == ndf & !is.na(reg.dat$ndf.level) & !is.na(reg.dat$Species),v]))  } 
            } else { coef.scaled <- coef.base ; marg.fct <-coef.scaled   } ; corrector <- 1
            
      
            
            dat.coefs[ r.count  , 'coef.base'  ] <-  coef.base
            dat.coefs[ r.count  , 'coef.scaled'  ] <-  coef.scaled
            dat.coefs[ r.count  , 'marg.fct'  ] <-  marg.fct
            
            dat.coefs[ r.count  , 'se'  ] <-  se
          
            
            #  var.imp <- round(   dat.row[  ,  var.lab.vimp ]   , 2)
            # var.imp.se <- round( dat.row[  ,  var.lab.vimp.se ]  , 0)
            
            
            #  dat.coefs[ r.count  , 'var.imp'  ] <-  var.imp 
            #  dat.coefs[ r.count  , 'var.imp.se'  ] <-  var.imp.se 
            
            
            r.count <- r.count + 1
    
          }
        }
      }
      
    }
  } 
  }

ordered.var.labs <- unique( dat.coefs$var.lab  )
  
dat.coefs$var.lab <- factor( dat.coefs$var.lab , levels =  ordered.var.labs) 

                             
} # Loop


View(dat.coefs)

# Data sub setting
# Sheep - Low NDF
dat.coefs.gbr.sp.lon.male <- dat.coefs[ dat.coefs$mod.class == 1 & dat.coefs$species == species.sheep & dat.coefs$ndf == ndf.lev.lo &  dat.coefs$sex == sex.male, ]
dat.coefs.gbr.sp.lon.female <- dat.coefs[ dat.coefs$mod.class == 1 & dat.coefs$species == species.sheep & dat.coefs$ndf == ndf.lev.lo &  dat.coefs$sex == sex.female, ]
dat.coefs.me.sp.lon.all <- dat.coefs[ dat.coefs$mod.class == 3 & dat.coefs$species == species.sheep & dat.coefs$ndf == ndf.lev.lo &  dat.coefs$sex == sex.male, ]



gg.coefs.gbr.sp.lon.male.0 <- gen.gg.coefs(dat.coefs.gbr.sp.lon.male) 
gg.coefs.gbr.sp.lon.female.0 <- gen.gg.coefs(dat.coefs.gbr.sp.lon.female) 
gg.coefs.me.sp.lon.all.0 <- gen.gg.coefs(dat.coefs.me.sp.lon.all) 

# Sheep - High NDF
dat.coefs.gbr.sp.hin.male <- dat.coefs[ dat.coefs$mod.class == 1 & dat.coefs$species == species.sheep & dat.coefs$ndf == ndf.lev.hi &  dat.coefs$sex == sex.male, ]
dat.coefs.gbr.sp.hin.female <- dat.coefs[ dat.coefs$mod.class == 1 & dat.coefs$species == species.sheep & dat.coefs$ndf == ndf.lev.hi &  dat.coefs$sex == sex.female, ]
dat.coefs.me.sp.hin.all <- dat.coefs[ dat.coefs$mod.class == 3 & dat.coefs$species == species.sheep & dat.coefs$ndf == ndf.lev.hi &  dat.coefs$sex == sex.male,  ]


gg.coefs.gbr.sp.hin.male.0 <- gen.gg.coefs(dat.coefs.gbr.sp.hin.male) 
gg.coefs.gbr.sp.hin.female.0 <- gen.gg.coefs(dat.coefs.gbr.sp.hin.female) 
gg.coefs.me.sp.hin.all.0 <- gen.gg.coefs(dat.coefs.me.sp.hin.all) 


# Merged plot prep
# Low NDF
gg.coefs.gbr.sp.lon.male <- gg.coefs.gbr.sp.lon.male.0 %>% +
  theme(
    axis.text.x = element_blank()
    ,   axis.ticks.x = element_blank()
    ,   axis.title.y = element_blank()
  )

gg.coefs.gbr.sp.lon.female <- gg.coefs.gbr.sp.lon.female.0 %>% +
  theme(
    axis.text.x = element_blank()
    ,   axis.ticks.x = element_blank()
    ,   axis.title.y = element_blank()
  )

gg.coefs.me.sp.lon.all <- gg.coefs.me.sp.lon.all.0 %>% +
  theme(
    axis.ticks.x = element_blank()
    ,   axis.title.y = element_blank()
  )


# High NDF
gg.coefs.gbr.sp.hin.male <- gg.coefs.gbr.sp.hin.male.0 %>% +
  theme(
    axis.text.x = element_blank()
    ,   axis.ticks.x = element_blank()
    ,   axis.title.y = element_blank()
  )

gg.coefs.gbr.sp.hin.female <- gg.coefs.gbr.sp.hin.female.0 %>% +
  theme(
    axis.text.x = element_blank()
    ,   axis.ticks.x = element_blank()
    ,   axis.title.y = element_blank()
  )

gg.coefs.me.sp.hin.all <- gg.coefs.me.sp.hin.all.0 %>% +
  theme(
    axis.ticks.x = element_blank()
    ,   axis.title.y = element_blank()
  )


# GG arrange

gg.coefs.all.sp.lon.0 <- ggarrange(
  
  gg.coefs.gbr.sp.lon.male
  ,gg.coefs.gbr.sp.lon.female 
  , gg.coefs.me.sp.lon.all
  
  
  , nrow = 3
  , heights = c(1,1,1.15)
)


gg.coefs.all.sp.lon <- annotate_figure(gg.coefs.all.sp.lon.0 ,
                left = text_grob( y.tit, color = "black", rot = 90, size = 12.25)
)


gg.coefs.all.sp.hin.0 <- ggarrange(
  
  gg.coefs.gbr.sp.hin.male
  ,gg.coefs.gbr.sp.hin.female 
  , gg.coefs.me.sp.hin.all
  
  
  , nrow = 3
  , heights = c(1,1,1.15)
)

gg.coefs.all.sp.hin <- annotate_figure(gg.coefs.all.sp.hin.0 ,
                                       left = text_grob( y.tit, color = "black", rot = 90, size = 12.25)
)



# global plot settings
gg.coefs.dpi <- 1500

gg.coefs.glob.scalar <- 0.625
gg.coefs.width <-  2.25  * (n.mod.form ) * gg.coefs.glob.scalar 
gg.coefs.height  <- 11.5* gg.coefs.glob.scalar

# Low NDF
filename.coefs.sp.lon = str_c(results.out.dir , 'coefs.sp.lon.jpeg')
ggsave(filename =  filename.coefs.sp.lon ,  gg.coefs.all.sp.lon, width = gg.coefs.width, height = gg.coefs.height  , dpi = gg.coefs.dpi)


# High NDF
filename.coefs.sp.hin = str_c(results.out.dir , 'coefs.sp.hin.jpeg')
ggsave(filename =  filename.coefs.sp.hin ,  gg.coefs.all.sp.hin , width = gg.coefs.width, height = gg.coefs.height  , dpi = gg.coefs.dpi)







gg.var.imp.gbr.sp.lon.male


gg.vimp.dpi <- 1500
gg.vimp.glob.scalar <- 0.625
gg.vimp.width <- 10 * gg.coefs.glob.scalar
gg.vimp.height  <- 4.5* gg.coefs.glob.scalar

filename.vimp = str_c(results.out.dir , 'vimp.jpeg')


ggsave(filename =  filename.vimp,  gg.var.imp, width = gg.vimp.width, height = gg.vimp.height  , dpi = gg.vimp.dpi)



gg.coefs.dpi <- 1500
gg.coefs.glob.scalar <- 0.625
gg.coefs.width <- 10 * gg.coefs.glob.scalar
gg.coefs.height  <- 4.5* gg.coefs.glob.scalar

filename.coefs = str_c(results.out.dir , 'coefs.jpeg')


ggsave(filename =  filename.coefs,  gg.coefs, width = gg.coefs.width, height = gg.coefs.height  , dpi = gg.coefs.dpi)



