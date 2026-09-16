
print(' Creating validation plots ')


{
  
  gg.valid.x.tit <<- bquote(~~~~~~~~~~~~~~Predicted~DMI~(kg~d^-1)  )  
  gg.valid.y.tit <<-   bquote(Measured~DMI~(kg~d^-1)  )  
  
  
  gg.resid.y.tit <<- 'Residuals (g/d)'
  gg.resid.x.tit <<- 'Fitted intake (g/d)'
  

  
  gg.valid.form.lab.x.crd.lev.1  <- .650
  gg.valid.form.lab.x.crd.lev.2  <-  gg.valid.form.lab.x.crd.lev.1 
  gg.valid.form.lab.x.crd.lev.3  <-  gg.valid.form.lab.x.crd.lev.1 
  
  # Y coordinates of formula labels
  inc <- 0.078
  row.2.sclr <- 1 - inc 
  row.3.sclr <- 1 - inc * 2
  row.4.sclr <- 1 - inc * 3
  
  sp.lon.max.y <- 1.500 #max( s.rums[cond.sheep.hi.ndf , ]) 
  gg.valid.form.lab.y.crd.sp.lon.lev.1  <- sp.lon.max.y * row.2.sclr
  gg.valid.form.lab.y.crd.sp.lon.lev.2  <- sp.lon.max.y * row.3.sclr
  gg.valid.form.lab.y.crd.sp.lon.lev.3  <- sp.lon.max.y * row.4.sclr
  
  
  sp.hin.max.y <- 1.300 #max( s.rums[cond.sheep.hi.ndf , ]) 
  gg.valid.form.lab.y.crd.sp.hin.lev.1  <- sp.hin.max.y * row.2.sclr
  gg.valid.form.lab.y.crd.sp.hin.lev.2  <- sp.hin.max.y * row.3.sclr
  gg.valid.form.lab.y.crd.sp.hin.lev.3  <- sp.hin.max.y * row.4.sclr
  
  
  
  gg.valid.lab.x.crd.sp.lon   <- 1.300
  gg.valid.lab.x.crd.sp.hin   <- 1.150
  
  gg.valid.lab.x.crd.gt.lon   <- 1.050
  gg.valid.lab.x.crd.gt.hin   <- 1.050
  
  gg.valid.lab.x.crd.male.bv.lon   <- 9.000
  gg.valid.lab.x.crd.male.bv.hin   <- 9.000
  
  gg.valid.lab.x.crd.female.bv.lon   <- 1.6000
  gg.valid.lab.x.crd.female.bv.hin   <- 1.2000
  
  
  gg.valid.lab.fs <- 2.5
  
  gg.valid.min.dmi.sp <- 0.400
  gg.valid.max.dmi.sp <- 1.600
  gg.valid.dmi.increment.sp <<- 200
  
  gg.valid.min.dmi.bv <- 2.000
  gg.valid.max.dmi.male.bv <- 11.000
  gg.valid.max.dmi.female.bv <- 20.000
  
  
  gg.valid.dmi.increment.bv <<- 1.500
  


  
  gg.y.valid.theme <- ggplot(  ) +
    theme(
      panel.grid.major = element_blank()
      ,panel.background = element_blank()
      ,panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
      
      ,strip.background = element_rect(color='black', fill='white', linewidth=1, linetype="solid")
      ,strip.text.x = element_text(size =  7.0 , color = 'black' , face = "bold"  )
      
      ,strip.text.y = element_text(size =  9.75 , color = 'black' , face = "bold" )
      
      , axis.text.x = element_text( angle = 90 , hjust = 1 , vjust = 0.5 , size = 8.5)
      , axis.text.y = element_text( hjust = 1 , vjust = 0.5 , size = 8.5)
      
      , legend.background = element_rect(fill = "transparent", color = NA)
    ) 
  
  gg.valid.pnt.size <<- 0.7
  gg.valid.pnt.colr <<- 'black'
  
  gen.gg.valid <<- function( 
    dat 
    # , xc 
    # ,  yc
    # , min.dmi
    # , max.dmi 
    #, dmi.increment
    
    ){
    
    # test:
    test <- function(){
      
      dat <- gg.d.me.bv.lon.Bind.male
      #  xc <- gg.valid.lab.x.crd.sp.lon
      #  yc <- gg.valid.lab.male.bv.y.crd
      
      min.dmi <- 3.000
      max.dmi <- 8.000
      dmi.increment <- .500
      
    }
    
   
    label.fontface <- "bold.italic"
    
    
    modelled <- na.omit( dat[  , 'modelled' ] ) ;   observed <- na.omit(dat[  , 'observed' ] )
    
    max.DMI <- max (   c(   modelled  ,  observed ) ) / 1000
    min.DMI <- min (   c(   modelled  ,  observed  ) ) / 1000
    
    max.DMI <- round( max.DMI , 0)
    min.DMI <- round( min.DMI , 0)
    
    DMI.range <- ( max.DMI  - min.DMI)
    
    
    dmi.increment <- 1.0
    if (DMI.range > 10 ) {  dmi.increment <- 2 }
   
    

    y.lab.plot.coord <- 0.4
    x.lab.plot.coord <- 0.99
    
    start <-   y.lab.plot.coord * ( max.DMI - min.DMI ) + min.DMI
    
    y.lab.inc <-    ( start - min.DMI ) *.25
    
    yc  <- c(  start 
                                                   ,  start -      y.lab.inc  #* start 
                                                   ,  start -  2 *     y.lab.inc   #* start
                                                   ,  start -  3 *     y.lab.inc   #* start
    )
    
    
    xc <-  max.DMI  *  x.lab.plot.coord
    max.x <- max.DMI
    
    
    label.h.just <- 1
    
    if ( is.null(dat)) {return (NULL)}
    
    
    plot <- gg.y.valid.theme   %>% +
      #  ggplot() + 
      geom_point( data = dat  ,  aes(y = observed / 1000, x = modelled / 1000), size = gg.valid.pnt.size ,  color = gg.valid.pnt.colr ) +
      
      ggh4x::facet_nested( 
        mod.tier ~ col.mod.form.label.r1 + col.mod.form.label.r2 + col.mod.form.label.r3
        , strip = strip_nested(size = "variable")
        , render_empty = FALSE
      ) +
      
      geom_line(data = data.frame(x = c(-Inf, Inf), y = c(-Inf, Inf)), 
                aes(x = x , y = y   , color = "y = x"), linetype = 'solid') + 
      # geom_abline( aes(  x = y , y = x ,  color = 'Predicted = Observed') , linetype = "solid") +
     
       geom_smooth(data = dat  ,  aes(x = observed /1000 , y = modelled /1000, color = 'Best fit' ), method="lm", se=FALSE , linewidth = .6 , linetype = 'solid') +
      scale_x_continuous(breaks = seq( min.DMI, max.DMI , by = dmi.increment))  +
      scale_y_continuous(breaks = seq( min.DMI, max.DMI , by = dmi.increment )) +
    
    
      
       # LABELS
      geom_label( 
        data = dat ,
        aes( x = xc  
             , y =   yc[1]
             , label =  label.ccc )
        , label.size = NA
        , parse = FALSE
        , size = gg.valid.lab.fs 
        , hjust =   label.h.just
        , fontface = label.fontface
      ) +
      geom_label( 
        data = dat  ,
        aes( x = xc 
             , y = yc[2]
             , label =  label.r2 )
        , label.size = NA
        , parse = FALSE
        , size = gg.valid.lab.fs 
        , hjust =   label.h.just
        , fontface = label.fontface
      ) +
      
      geom_label( 
        data = dat  ,
        aes( x = xc    
             , y = yc[3]
             , label =  label.r2.ucb )
        , label.size = NA
        , parse = FALSE
        , size = gg.valid.lab.fs 
        , hjust =   label.h.just
        , fontface = label.fontface
      )  +
      
      geom_label( 
        data = dat  ,
        aes( x = xc    
             , y = yc[4]
             , label =  label.nrmse )
        , label.size = NA
        , parse = FALSE
        , size = gg.valid.lab.fs 
        , hjust =   label.h.just
        , fontface = label.fontface
      )  +
      
     ylab(gg.valid.y.tit) + 
      xlab(gg.valid.x.tit) +
      theme(
        # Render HTML in the strips
        #  strip.text = element_markdown(hjust = 0.5)
        # strip.x ==  strip_nested(size = "variable")
        #legend.position = 'outside'
        
        
        
        , legend.position = c( 0.12 , -0.3)
        , legend.title = element_blank()
        , legend.text =   element_text(size = 6.5)
        , legend.key.width =  unit(0.25, "cm")
        
      ) + # + scale_color_manual(values = c( "LOBF" = "red")) +
      scale_color_manual(
        name = ''
        , values =   c( 
          "y = x"  = 'black'
          ,   "Best fit" = "#8EC5FF"
        ) 
        , breaks = c(
          'y = x'
          , "Best fit"
          
        )) +
      guides( color = guide_legend(override.aes = list( ncol = 2 , linetype = c(1, 1))))+
      guides( color = guide_legend(nrow = 1)) #+
     # coord_fixed(ratio = 1)
    
    
    return (   plot  ) 
  }
  
} # Plot params


# -- ~~~~~~~~~~~~~~ --- CATTLE --- ~~~~~~~~~~~~~~ 
{
  
  # Plot basic GBR model (model class = 1)
  
  # Bos indicus ( breed.indig )
  model.class <- model.class.GBR
  breed <- breed.indig[1]
  
  sex <- sex.male[1] 
  
  gg.d.reg.out.bv.lon.Bind.male.all <- gen.gg.valid.data( 1 , species.cattle , breed  , ndf.lev.lo , sex )
  

  
  # FEMALE
  if ( n.sexes == 2 ){
    
  sex <- sex.female 
  
  gg.d.reg.out.bv.lon.Bind.female.all <- gen.gg.valid.data( 1 , species.cattle ,   breed , ndf.lev.lo , sex)

    }
  
  # High NDF
  sex <- sex.male[1]
  
  try(
    
  gg.d.reg.out.bv.hin.Bind.male.all <- gen.gg.valid.data( 1 , species.cattle ,  breed , ndf.lev.hi , sex )
 
  , silent = TRUE)
  
  # FEMALE
  
  if ( n.sexes == 2 ){
    
  sex <- sex.female
  
  gg.d.reg.out.bv.hin.Bind.female.all <- gen.gg.valid.data( 1 , species.cattle ,  breed , ndf.lev.hi , sex )
  
  }
  
  # Crossbred( breed.cbred )
  if (  n.breeds == 2  ){
  
  breed <- breed.cbred
  sex <- sex.male[1] 
  
  gg.d.reg.out.bv.lon.Btau.male.all <- gen.gg.valid.data( 1 , species.cattle , breed  , ndf.lev.lo , sex)
  
  
  # FEMALE
  if ( n.sexes == 2 ){
    
    sex <- sex.female 
    
    gg.d.reg.out.bv.lon.Btau.female.all <- gen.gg.valid.data( 1 , species.cattle ,   breed , ndf.lev.lo , sex )
    
  }
  
  # High NDF
  sex <- sex.male[1]
  
  gg.d.reg.out.bv.hin.Btau.male.all <- gen.gg.valid.data( 1 , species.cattle ,  breed , ndf.lev.hi , sex  )
  
  
  # FEMALE
  
  if ( n.sexes == 2 ){
    
    sex <- sex.female
    
    gg.d.reg.out.bv.hin.Btau.female.all <- gen.gg.valid.data( 1 , species.cattle ,  breed , ndf.lev.hi , sex )
    
  }
  }
  

  # Plot ME models 
  

if ( model.class.ME %in% model.classes.all ){ 
    
    # Bos indicus
    
  model.class <- model.class.ME ; breed <- breed.indig[1] ; sex <- sex.male[1]
  
  gg.d.me.bv.lon.Bind.male <- gen.gg.valid.data(  model.class , species.cattle ,  breed , ndf.lev.lo , sex )
  
  
  if ( n.sexes == 2 ){
    
    sex <- sex.female

    gg.d.me.bv.lon.Bind.female <- gen.gg.valid.data( model.class , species.cattle ,  breed , ndf.lev.lo , sex )
    
  }
  
  # High NDF
  sex <- sex.male[1]
  
  try (
  
  gg.d.me.bv.hin.Bind.male <- gen.gg.valid.data( model.class  , species.cattle , breed , ndf.lev.hi , sex )
  
  
  , silent = TRUE)
  
  if ( n.sexes == 2 ){
    
    sex <- sex.female
    
    gg.d.me.bv.hin.Bind.female <- gen.gg.valid.data( model.class , species.cattle ,  breed , ndf.lev.hi , sex )
    
  }
  
  
  # Crossbred
  if ( n.breeds == 2 ){ 
    
    breed <- breed.cbred ; sex <- sex.male[1]
    
    gg.d.me.bv.lon.Btau.male <- gen.gg.valid.data(  model.class , species.cattle ,  breed , ndf.lev.lo , sex )
    
    
    if ( n.sexes == 2 ){
      
      sex <- sex.female
      
      gg.d.me.bv.lon.Btau.female <- gen.gg.valid.data( model.class , species.cattle ,  breed , ndf.lev.lo , sex )
      
    }
    
    # High NDF
    sex <- sex.male[1]
    
    gg.d.me.bv.hin.Btau.male <- gen.gg.valid.data( model.class , species.cattle , breed , ndf.lev.hi , sex )
    
    
    if ( n.sexes == 2 ){
      
      sex <- sex.female
      
      gg.d.me.bv.hin.Btau.female <- gen.gg.valid.data( model.class , species.cattle ,  breed , ndf.lev.hi , sex )
      
    }
    
  }
  
}
  
  
} # Data frame tabulation 



{
  # GBR Model plots
  
  # Bos indicus
  # Cattle low NDF
  # Male
  gg.gbr.y.valid.bv.lon.Bind.male.0.tier.1 <- try( gen.gg.valid( gg.d.reg.out.bv.lon.Bind.male.all[gg.d.reg.out.bv.lon.Bind.male.all$mod.tier == tier.label.1 ,]   ) , silent = TRUE)
  
  # gg.gbr.y.valid.sp.lon.Bind.male.0.tier.1
  
  gg.gbr.y.valid.bv.lon.Bind.male.0.tier.2 <- try( gen.gg.valid(gg.d.reg.out.bv.lon.Bind.male.all[gg.d.reg.out.bv.lon.Bind.male.all$mod.tier == tier.label.2,]), silent = TRUE)
  
  # gg.gbr.y.valid.sp.lon.Bind.male.0.tier.2
  
  gg.gbr.y.valid.bv.lon.Bind.male.0.tier.3 <- try(  gen.gg.valid(gg.d.reg.out.bv.lon.Bind.male.all[gg.d.reg.out.bv.lon.Bind.male.all$mod.tier == tier.label.2,] )   , silent = TRUE)
  
  # gg.gbr.y.valid.sp.lon.Bind.male.0.tier.3
  
  # Female
  if (  n.sexes == 2 ){
    
  gg.gbr.y.valid.bv.lon.Bind.female.0.tier.1 <- try( gen.gg.valid(  gg.d.reg.out.bv.lon.Bind.female.all[gg.d.reg.out.bv.lon.Bind.female.all$mod.tier == tier.label.1,] ) , silent = TRUE)
  
  # gg.gbr.y.valid.bv.lon.Bind.female.0.tier.1
  
  gg.gbr.y.valid.bv.lon.Bind.female.0.tier.2 <- try( gen.gg.valid(gg.d.reg.out.bv.lon.Bind.female.all[gg.d.reg.out.bv.lon.Bind.female.all$mod.tier == tier.label.2,]) , silent = TRUE)
  
  # gg.gbr.y.valid.bv.lon.female.0.tier.2
  
  gg.gbr.y.valid.bv.lon.Bind.female.0.tier.3 <- try( gen.gg.valid(gg.d.reg.out.bv.lon.Bind.female.all[gg.d.reg.out.bv.lon.Bind.female.all$mod.tier == tier.label.3,]  ) , silent = TRUE)
  
  # gg.gbr.y.valid.bv.lon.Bind.female.0.tier.3
  
  }
  
  # Cattle high NDF
  # Male
  gg.gbr.y.valid.bv.hin.Bind.male.0.tier.1 <- try( gen.gg.valid(  gg.d.reg.out.bv.hin.Bind.male.all[gg.d.reg.out.bv.hin.Bind.male.all$mod.tier == tier.label.1,] ) , silent = TRUE)
  
  # gg.gbr.y.valid.bv.hin.Bind.male.0.tier.1
  
  gg.gbr.y.valid.bv.hin.Bind.male.0.tier.2 <- try( gen.gg.valid(gg.d.reg.out.bv.hin.Bind.male.all[gg.d.reg.out.bv.hin.Bind.male.all$mod.tier == tier.label.2,]  ) , silent = TRUE)
  
  # gg.gbr.y.valid.bv.hin.Bind.male.0.tier.2 
  
  gg.gbr.y.valid.bv.hin.Bind.male.0.tier.3 <- try( gen.gg.valid(gg.d.reg.out.bv.hin.Bind.male.all[gg.d.reg.out.bv.hin.Bind.male.all$mod.tier == tier.label.3,]  ) , silent = TRUE)
  
  # gg.gbr.y.valid.bv.hin.Bind.male.0.tier.3 
  
  # Female
  if (  n.sexes == 2  ){
    
    gg.gbr.y.valid.bv.hin.Bind.female.0.tier.1 <- try( gen.gg.valid(  gg.d.reg.out.bv.hin.Bind.female.all[gg.d.reg.out.bv.hin.Bind.female.all$mod.tier == tier.label.1,]  ) , silent = TRUE)
    
    # gg.gbr.y.valid.bv.hin.female.0.tier.1
    
    gg.gbr.y.valid.bv.hin.Bind.female.0.tier.2 <- try( gen.gg.valid(gg.d.reg.out.bv.hin.Bind.female.all[gg.d.reg.out.bv.hin.Bind.female.all$mod.tier == tier.label.2,]  ) , silent = TRUE)
    
    # gg.gbr.y.valid.bv.hin.female.0.tier.2
    
    gg.gbr.y.valid.bv.hin.Bind.female.0.tier.3 <- try( gen.gg.valid(gg.d.reg.out.bv.hin.Bind.female.all[gg.d.reg.out.bv.hin.Bind.female.all$mod.tier == tier.label.3,]) , silent = TRUE)
    
    # gg.gbr.y.valid.bv.hin.female.0.tier.3
    
  }
  
  if ( n.breeds == 2 ){
    
    # Crossbreds
    # Cattle low NDF
    # Male
    gg.gbr.y.valid.bv.lon.Btau.male.0.tier.1 <- try( gen.gg.valid( gg.d.reg.out.bv.lon.Btau.male.all[gg.d.reg.out.bv.lon.Btau.male.all$mod.tier == tier.label.1  ,]   ) , silent = TRUE)
    
    # gg.gbr.y.valid.bv.lon.Btau.male.0.tier.1
    
    gg.gbr.y.valid.bv.lon.Btau.male.0.tier.2 <- try( gen.gg.valid(gg.d.reg.out.bv.lon.Btau.male.all[gg.d.reg.out.bv.lon.Btau.male.all$mod.tier == tier.label.2,]  ), silent = TRUE)
    
    # gg.gbr.y.valid.bv.lon.Btau.male.0.tier.2
    
    gg.gbr.y.valid.bv.lon.Btau.male.0.tier.3 <- try(  gen.gg.valid(gg.d.reg.out.bv.lon.Btau.male.all[gg.d.reg.out.bv.lon.Btau.male.all$mod.tier == tier.label.3,]  ) , silent = TRUE)
    
    # gg.gbr.y.valid.bv.lon.Btau.male.0.tier.3
    
    # Female
    if (  n.sexes == 2 ){
      
      gg.gbr.y.valid.bv.lon.Btau.female.0.tier.1 <- try( gen.gg.valid(gg.d.reg.out.bv.lon.Btau.female.all[gg.d.reg.out.bv.lon.Btau.female.all$mod.tier == tier.label.1 ,]) , silent = TRUE)
      
      # gg.gbr.y.valid.bv.lon.Btau.female.0.tier.1
      
      gg.gbr.y.valid.bv.lon.Btau.female.0.tier.2 <- try( gen.gg.valid(gg.d.reg.out.bv.lon.Btau.female.all[gg.d.reg.out.bv.lon.Btau.female.all$mod.tier == tier.label.2 ,]  ) , silent = TRUE)
      
      # gg.gbr.y.valid.bv.lon.Btau.female.0.tier.2 
      
      gg.gbr.y.valid.bv.lon.Btau.female.0.tier.3 <- try( gen.gg.valid(gg.d.reg.out.bv.lon.Btau.female.all[gg.d.reg.out.bv.lon.Btau.female.all$mod.tier == tier.label.3,]  ) , silent = TRUE)
      
      #  gg.gbr.y.valid.bv.lon.Btau.female.0.tier.3
      
    }
    
    # Cattle high NDF
    # Male
    gg.gbr.y.valid.bv.hin.Btau.male.0.tier.1 <- try( gen.gg.valid(  gg.d.reg.out.bv.hin.Btau.male.all[gg.d.reg.out.bv.hin.Btau.male.all$mod.tier == tier.label.1,] ) , silent = TRUE)
    
    # gg.gbr.y.valid.sp.hin.Btau.male.0.tier.1
    
    gg.gbr.y.valid.bv.hin.Btau.male.0.tier.2 <- try( gen.gg.valid(gg.d.reg.out.bv.hin.Btau.male.all[gg.d.reg.out.bv.hin.Btau.male.all$mod.tier == tier.label.2,]  ) , silent = TRUE)
    
    # gg.gbr.y.valid.sp.hin.Btau.male.0.tier.2
    
    gg.gbr.y.valid.bv.hin.Btau.male.0.tier.3 <- try( gen.gg.valid(gg.d.reg.out.bv.hin.Btau.male.all[gg.d.reg.out.bv.hin.Btau.male.all$mod.tier == tier.label.3,]  ) , silent = TRUE)
    
    # gg.gbr.y.valid.sp.hin.Btau.male.0.tier.3
    
    # Female
    if (n.sexes == 2){
      
      gg.gbr.y.valid.bv.hin.Btau.female.0.tier.1 <- try( gen.gg.valid(gg.d.reg.out.bv.hin.Btau.female.all[gg.d.reg.out.bv.hin.Btau.female.all$mod.tier == tier.label.1,]  ) , silent = TRUE)
      
      # gg.gbr.y.valid.bv.hin.Btau.female.0.tier.1
      
      gg.gbr.y.valid.bv.hin.Btau.female.0.tier.2 <- try( gen.gg.valid(gg.d.reg.out.bv.hin.Btau.female.all[gg.d.reg.out.bv.hin.Btau.female.all$mod.tier == tier.label.2,]  ) , silent = TRUE)
      
      # gg.gbr.y.valid.bv.hin.Btau.female.0.tier.2
      
      gg.gbr.y.valid.bv.hin.Btau.female.0.tier.3 <- try( gen.gg.valid(gg.d.reg.out.bv.hin.Btau.female.all[gg.d.reg.out.bv.hin.Btau.female.all$mod.tier == tier.label.3,] ) , silent = TRUE)
      
      # gg.gbr.y.valid.bv.hin.Btau.female.0.tier.3
      
    }
    
    
    
    
    
  }
  
  
  # Crossbreds
  
} # Generate plots



# Plot re-formatting
{
  
  gg.theme.plot.merge <- theme(axis.text.x = element_blank()
                               , axis.title.y = element_blank()
                               , axis.title.x = element_blank()
                               ,  axis.ticks.x = element_blank()
                               ,   legend.position  = "none")
  
  gg.theme.plot.merge.bott.row <- theme(
                                  axis.title.y = element_blank()
                                    , axis.title.x = element_blank()
                                  
                               )
  
  
  # Bos indicus
  # Male
  
  try(
    
  gg.y.valid.reg.out.bv.lon.Bind.male.p.tier.1 <- gg.gbr.y.valid.bv.lon.Bind.male.0.tier.1  %>% +  gg.theme.plot.merge 

  , silent = TRUE)
  
  
  
  if (n.mod.form == 6) { 
  
    
    try(
      
      gg.y.valid.reg.out.bv.lon.Bind.male.p.tier.2 <- gg.gbr.y.valid.bv.lon.Bind.male.0.tier.2  %>% +   gg.theme.plot.merge.bott.row
      , silent = TRUE)
    
  }
  
  if (n.mod.form == 9) { 
    
  try(
    
  gg.y.valid.reg.out.bv.lon.Bind.male.p.tier.2 <- gg.gbr.y.valid.bv.lon.Bind.male.0.tier.2  %>% +   gg.theme.plot.merge 
  , silent = TRUE)
  
  try(
    
  gg.y.valid.reg.out.bv.lon.Bind.male.p.tier.3 <- gg.gbr.y.valid.bv.lon.Bind.male.0.tier.3  %>% + gg.theme.plot.merge.bott.row
  , silent = TRUE)
    
  }
  
  # Female
  if ( n.sexes == 2 ){
    
    
    try(
      
  gg.y.valid.reg.out.bv.lon.Bind.female.p.tier.1 <- gg.gbr.y.valid.bv.lon.Bind.female.0.tier.1  %>% +   gg.theme.plot.merge 
  , silent = TRUE)
    
    if (n.mod.form == 6) {   
    
  try(
    
  gg.y.valid.reg.out.bv.lon.Bind.female.p.tier.2 <- gg.gbr.y.valid.bv.lon.Bind.female.0.tier.2  %>% + gg.theme.plot.merge.bott.row

  , silent = TRUE)
      
    }
    
    
    if (n.mod.form == 9) { 
    
    try(
      
   gg.y.valid.reg.out.bv.lon.Bind.female.p.tier.2 <- gg.gbr.y.valid.bv.lon.Bind.female.0.tier.2  %>% + gg.theme.plot.merge 
      
      , silent = TRUE)
      
      
      try(  
  gg.y.valid.reg.out.bv.lon.Bind.female.p.tier.3 <- gg.gbr.y.valid.bv.lon.Bind.female.0.tier.3   %>% + gg.theme.plot.merge.bott.row

  , silent = TRUE)

    
  }
  
    
  }
  
  # High NDF
  # Male
  
  try(
    
  gg.y.valid.reg.out.bv.hin.Bind.male.p.tier.1 <- gg.gbr.y.valid.bv.hin.Bind.male.0.tier.1  %>% +  gg.theme.plot.merge
  
  , silent = TRUE)
  

  if ( n.mod.form == 6 ) {   
    
    try(
      
      gg.y.valid.reg.out.bv.hin.Bind.male.p.tier.2  <- gg.gbr.y.valid.bv.hin.Bind.male.0.tier.2 %>% + gg.theme.plot.merge.bott.row
      
      , silent = TRUE)
    
  }
  
  
  if (n.mod.form == 9) { 
    
    try(
      
      gg.y.valid.reg.out.bv.hin.Bind.male.p.tier.2 <- gg.gbr.y.valid.bv.lon.Bind.male.0.tier.2  %>% + gg.theme.plot.merge 
      
      , silent = TRUE)
    
    
    try(  
      
      gg.y.valid.reg.out.bv.hin.Bind.male.p.tier.3  <- gg.gbr.y.valid.bv.lon.Bind.male.0.tier.3   %>% + gg.theme.plot.merge.bott.row
      
      , silent = TRUE)
    
    
  }
  
  
  # Female
  
  if (  n.sexes == 2  ){
    
    
    try(
      
  gg.y.valid.reg.out.bv.hin.Bind.female.p.tier.1 <- gg.gbr.y.valid.bv.hin.Bind.female.0.tier.1  %>% +  gg.theme.plot.merge
  , silent = TRUE)
    
  
    
    if (n.mod.form == 6) {   
      
      try(
        
        gg.y.valid.reg.out.bv.hin.Bind.female.p.tier.2 <-  gg.gbr.y.valid.bv.hin.Bind.female.0.tier.2  %>% + gg.theme.plot.merge.bott.row
        
        , silent = TRUE)
      
    }
    
    
    if (n.mod.form == 9) { 
      
      try(
        
        gg.y.valid.reg.out.bv.hin.Bind.female.p.tier.2 <- gg.gbr.y.valid.bv.hin.Bind.female.0.tier.2  %>% + gg.theme.plot.merge 
        
        , silent = TRUE)
      
      
      try(  
        
        gg.y.valid.reg.out.bv.hin.Bind.female.p.tier.3 <- gg.gbr.y.valid.bv.hin.Bind.female.0.tier.3   %>% + gg.theme.plot.merge.bott.row
        
        , silent = TRUE)
      
      
    }
    

  
}
  
  if (n.breeds == 2){
    
 # Crossbred
  # Male
  
  try(
    
    gg.y.valid.reg.out.bv.lon.Btau.male.p.tier.1 <- gg.gbr.y.valid.bv.lon.Btau.male.0.tier.1  %>% +  gg.theme.plot.merge
    , silent = TRUE)
  

    
    
    if (n.mod.form == 6) {   
      
      try(
        
        gg.y.valid.reg.out.bv.lon.Btau.male.p.tier.2 <-  gg.gbr.y.valid.bv.lon.Btau.male.0.tier.2  %>% + gg.theme.plot.merge.bott.row
        
        , silent = TRUE)
      
    }
    
    
    if (n.mod.form == 9) { 
      
      try(
        
        gg.y.valid.reg.out.bv.lon.Btau.male.p.tier.2 <- gg.gbr.y.valid.bv.lon.Btau.male.0.tier.2  %>% + gg.theme.plot.merge 
        
        , silent = TRUE)
      
      
      try(  
        
        gg.y.valid.reg.out.bv.lon.Btau.male.p.tier.3 <- gg.gbr.y.valid.bv.lon.Btau.male.0.tier.3   %>% + gg.theme.plot.merge.bott.row
        
        , silent = TRUE)
      
      
    }
    
    
    
    
  
  # Female
  if ( n.sexes == 2 ){
    
    try(
      
      gg.y.valid.reg.out.bv.lon.Btau.female.p.tier.1 <- gg.gbr.y.valid.bv.lon.Btau.female.0.tier.1  %>% +  gg.theme.plot.merge
      , silent = TRUE)
    
    if (n.mod.form == 6) {   
      
      try(
        
        gg.y.valid.reg.out.bv.lon.Btau.female.p.tier.2 <-  gg.gbr.y.valid.bv.lon.Btau.female.0.tier.2  %>% + gg.theme.plot.merge.bott.row
        
        , silent = TRUE)
      
    }
    
    
    if (n.mod.form == 9) { 
      
      try(
        
        gg.y.valid.reg.out.bv.lon.Btau.female.p.tier.2 <- gg.gbr.y.valid.bv.lon.Btau.female.0.tier.2  %>% + gg.theme.plot.merge 
        
        , silent = TRUE)
      
      
      try(  
        
        gg.y.valid.reg.out.bv.lon.Btau.female.p.tier.3 <- gg.gbr.y.valid.bv.lon.Btau.female.0.tier.3   %>% + gg.theme.plot.merge.bott.row
        
        , silent = TRUE)
      
      
    }
    
    
  }
  
  
  # High NDF
  # Male
  
  try(
    
    gg.y.valid.reg.out.bv.hin.Btau.male.p.tier.1 <- gg.gbr.y.valid.bv.hin.Btau.male.0.tier.1  %>% +  gg.theme.plot.merge
    
    , silent = TRUE)
  
    if (n.mod.form == 6) {   
      
      try(
        
        gg.y.valid.reg.out.bv.hin.Btau.male.p.tier.2 <-  gg.gbr.y.valid.bv.hin.Btau.male.0.tier.2  %>% + gg.theme.plot.merge.bott.row
        
        , silent = TRUE)
      
    }
    
    
    if (n.mod.form == 9) { 
      
      try(
        
        gg.y.valid.reg.out.bv.hin.Btau.male.p.tier.2 <- gg.gbr.y.valid.bv.hin.Btau.male.0.tier.2  %>% + gg.theme.plot.merge 
        
        , silent = TRUE)
      
      
      try(  
        
        gg.y.valid.reg.out.bv.hin.Btau.male.p.tier.3 <- gg.gbr.y.valid.bv.hin.Btau.male.0.tier.3   %>% + gg.theme.plot.merge.bott.row
        
        , silent = TRUE)
      
      
    }
  
  # Female
  
  if (n.sexes == 2){
    
    
    try(
      
      gg.y.valid.reg.out.bv.hin.Btau.female.p.tier.1 <- gg.gbr.y.valid.bv.hin.Btau.female.0.tier.1  %>% +  gg.theme.plot.merge
      , silent = TRUE)
    
    
    if (n.mod.form == 6) {   
      
      try(
        
        gg.y.valid.reg.out.bv.hin.Btau.female.p.tier.2 <-  gg.gbr.y.valid.bv.hin.Btau.female.0.tier.2  %>% + gg.theme.plot.merge.bott.row
        
        , silent = TRUE)
      
    } else if (n.mod.form == 9) { 
      
      try(
        
        gg.y.valid.reg.out.bv.hin.Btau.female.p.tier.2 <- gg.gbr.y.valid.bv.hin.Btau.female.0.tier.2  %>% + gg.theme.plot.merge 
        
        , silent = TRUE)
      
      
      try(  
        
        gg.y.valid.reg.out.bv.hin.Btau.female.p.tier.3 <- gg.gbr.y.valid.bv.hin.Btau.female.0.tier.3   %>% + gg.theme.plot.merge.bott.row
        
        , silent = TRUE)
      
      
    }
    
    
  }
  
  
  }
  
  
}


# GG ARRANGEs 
{
  
  if ( n.mod.form  == 3) { heights <- c( 0.9 ) }
  if ( n.mod.form  == 6) { heights <- c( 0.9 , 0.9  ) }
  if  ( n.mod.form  == 9) { heights <- c( 0.9 , 0.9 , 1 ) }
  
  
  # Bos indicus

  try( 
    
  gg.valid.bv.lon.Bind.male.all.base  <- ggarrange(
    

    gg.y.valid.reg.out.bv.lon.Bind.male.p.tier.1
  , gg.y.valid.reg.out.bv.lon.Bind.male.p.tier.2
  # , gg.y.valid.reg.out.bv.lon.Bind.male.p.tier.3
  
    , nrow =  n.mod.form / 3 
    , heights =  heights
  )
  , silent = TRUE ) 
  

  
  try( 
  
  gg.valid.bv.lon.Bind.female.all.base  <- ggarrange(
    
    gg.y.valid.reg.out.bv.lon.Bind.female.p.tier.1
       ,   gg.y.valid.reg.out.bv.lon.Bind.female.p.tier.2
    #   ,  gg.y.valid.reg.out.bv.lon.Bind.female.p.tier.3
    
    
    , nrow =  n.mod.form /3 
    , heights =  heights
  )
  , silent = TRUE )
  
  try( 
    
  gg.valid.bv.hin.Bind.male.all.base  <- ggarrange(
    
    gg.y.valid.reg.out.bv.hin.Bind.male.p.tier.1
        ,   gg.y.valid.reg.out.bv.hin.Bind.male.p.tier.2
    #  ,  gg.y.valid.reg.out.bv.hin.Bind.male.p.tier.3
    
    
    , nrow = n.mod.form /3 
    , heights =  heights
  )
  , silent = TRUE )
  
  
  try( 
    
  gg.valid.bv.hin.Bind.female.all.base  <- ggarrange(
    
    gg.y.valid.reg.out.bv.hin.Bind.female.p.tier.1
       ,   gg.y.valid.reg.out.bv.hin.Bind.female.p.tier.2
    #   ,  gg.y.valid.reg.out.bv.hin.Bind.female.p.tier.3
    
    
    , nrow =  n.mod.form /3 
    , heights =  heights
  )
  
  , silent = TRUE )
  
  
  # Crossbreds
  
  heights <- c( 0.9 , 0.9 , 1 )
  
  try( 
    
    gg.valid.bv.lon.Btau.male.all.base  <- ggarrange(
      
      gg.y.valid.reg.out.bv.lon.Btau.male.p.tier.1
         , gg.y.valid.reg.out.bv.lon.Btau.male.p.tier.2
      #   ,  gg.y.valid.reg.out.bv.lon.Btau.male.p.tier.3
      
      
      , nrow =  n.mod.form /3 
      , heights =  heights
    )
    , silent = TRUE ) 
  
  
  
  try( 
    
    gg.valid.bv.lon.Btau.female.all.base  <- ggarrange(
      
           gg.y.valid.reg.out.bv.lon.Btau.female.p.tier.1
               ,   gg.y.valid.reg.out.bv.lon.Btau.female.p.tier.2
           #   ,   gg.y.valid.reg.out.bv.lon.Btau.female.p.tier.3
      
      
      , nrow =  n.mod.form /3 
      , heights =  heights
    )
    , silent = TRUE )
  
  try( 
    
    gg.valid.bv.hin.Btau.male.all.base  <- ggarrange(
      
      gg.y.valid.reg.out.bv.hin.Btau.male.p.tier.1
           ,   gg.y.valid.reg.out.bv.hin.Btau.male.p.tier.2
      #     ,  gg.y.valid.reg.out.bv.hin.Btau.male.p.tier.3
      
      
      , nrow =  n.mod.form /3 
      , heights =  heights
    )
    , silent = TRUE )
  
  
  try( 
    
  
    gg.valid.bv.hin.Btau.female.all.base  <- ggarrange(
      
      gg.y.valid.reg.out.bv.hin.Btau.female.p.tier.1
           ,   gg.y.valid.reg.out.bv.hin.Btau.female.p.tier.2
      #    ,  gg.y.valid.reg.out.bv.hin.Btau.female.p.tier.3
      
      
      , nrow = n.mod.form /3 
      , heights =  heights
    )
    
    , silent = TRUE )
  

  
  
}



# Annotations
{
  
  # Bos indicus
  try(
 
  gg.valid.bv.lon.Bind.male.all <- annotate_figure(  gg.valid.bv.lon.Bind.male.all.base  , 
                                                    bottom = text_grob(
                                                      gg.valid.x.tit
                                                      , color = "black"
                                                      , hjust = 0.5
                                                      , vjust = 0.5
                                                      , x = 0.5
                                                      , size = 12
                                                    ),
                                                    left= text_grob(
                                                      gg.valid.y.tit
                                                      , color = "black"
                                                      , hjust = 0.5
                                                      , vjust = 0.5
                                                      , x = 0.5
                                                      , rot = 90
                                                      , size = 12
                                                    )
  )
  , silent = TRUE )
  
  try(
  gg.valid.bv.lon.Bind.female.all <- annotate_figure(  gg.valid.bv.lon.Bind.female.all.base  , 
                                                      bottom = text_grob(
                                                        gg.valid.x.tit
                                                        , color = "black"
                                                        , hjust = 0.5
                                                        , vjust = 0.5
                                                        , x = 0.5
                                                        , size = 12
                                                      ),
                                                      left= text_grob(
                                                        gg.valid.y.tit
                                                        , color = "black"
                                                        , hjust = 0.5
                                                        , vjust = 0.5
                                                        , x = 0.5
                                                        , rot = 90
                                                        , size = 12
                                                      )
  )
  , silent = TRUE )
  
  
  try(
    
  gg.valid.bv.hin.Bind.male.all <- annotate_figure(  gg.valid.bv.hin.Bind.male.all.base  , 
                                                    bottom = text_grob(
                                                      gg.valid.x.tit
                                                      , color = "black"
                                                      , hjust = 0.5
                                                      , vjust = 0.5
                                                      , x = 0.5
                                                      , size = 12
                                                    ),
                                                    left= text_grob(
                                                      gg.valid.y.tit
                                                      , color = "black"
                                                      , hjust = 0.5
                                                      , vjust = 0.5
                                                      , x = 0.5
                                                      , rot = 90
                                                      , size = 12
                                                    )
  ) 
  , silent = TRUE )
  
  try(

  gg.valid.bv.hin.Bind.female.all <- annotate_figure(  gg.valid.bv.hin.Bind.female.all.base  , 
                                                      bottom = text_grob(
                                                        gg.valid.x.tit
                                                        , color = "black"
                                                        , hjust = 0.5
                                                        , vjust = 0.5
                                                        , x = 0.5
                                                        , size = 12
                                                      ),
                                                      left= text_grob(
                                                        gg.valid.y.tit
                                                        , color = "black"
                                                        , hjust = 0.5
                                                        , vjust = 0.5
                                                        , x = 0.5
                                                        , rot = 90
                                                        , size = 12
                                                      )
  )
  , silent = TRUE )
  
  
  # Crossbred
  if (n.breeds == 2){
    
    try(
      gg.valid.bv.lon.Btau.male.all <- annotate_figure(  gg.valid.bv.lon.Btau.male.all.base  , 
                                                             bottom = text_grob(
                                                               gg.valid.x.tit
                                                               , color = "black"
                                                               , hjust = 0.5
                                                               , vjust = 0.5
                                                               , x = 0.5
                                                               , size = 12
                                                             ),
                                                             left= text_grob(
                                                               gg.valid.y.tit
                                                               , color = "black"
                                                               , hjust = 0.5
                                                               , vjust = 0.5
                                                               , x = 0.5
                                                               , rot = 90
                                                               , size = 12
                                                             )
      )
      , silent = TRUE )
    
    try(
      gg.valid.bv.lon.Btau.female.all <- annotate_figure(  gg.valid.bv.lon.Btau.female.all.base  , 
                                                               bottom = text_grob(
                                                                 gg.valid.x.tit
                                                                 , color = "black"
                                                                 , hjust = 0.5
                                                                 , vjust = 0.5
                                                                 , x = 0.5
                                                                 , size = 12
                                                               ),
                                                               left= text_grob(
                                                                 gg.valid.y.tit
                                                                 , color = "black"
                                                                 , hjust = 0.5
                                                                 , vjust = 0.5
                                                                 , x = 0.5
                                                                 , rot = 90
                                                                 , size = 12
                                                               )
      )
      , silent = TRUE )
    
    
    try(
      
      gg.valid.bv.hin.Btau.male.all <- annotate_figure(  gg.valid.bv.hin.Btau.male.all.base  , 
                                                             bottom = text_grob(
                                                               gg.valid.x.tit
                                                               , color = "black"
                                                               , hjust = 0.5
                                                               , vjust = 0.5
                                                               , x = 0.5
                                                               , size = 12
                                                             ),
                                                             left= text_grob(
                                                               gg.valid.y.tit
                                                               , color = "black"
                                                               , hjust = 0.5
                                                               , vjust = 0.5
                                                               , x = 0.5
                                                               , rot = 90
                                                               , size = 12
                                                             )
      ) 
      , silent = TRUE )
    
    try(
      
      gg.valid.bv.hin.Btau.female.all <- annotate_figure(  gg.valid.bv.hin.Btau.female.all.base  , 
                                                               bottom = text_grob(
                                                                 gg.valid.x.tit
                                                                 , color = "black"
                                                                 , hjust = 0.5
                                                                 , vjust = 0.5
                                                                 , x = 0.5
                                                                 , size = 12
                                                               ),
                                                               left= text_grob(
                                                                 gg.valid.y.tit
                                                                 , color = "black"
                                                                 , hjust = 0.5
                                                                 , vjust = 0.5
                                                                 , x = 0.5
                                                                 , rot = 90
                                                                 , size = 12
                                                               )
      )
      , silent = TRUE )
    
    
    
  }

} # GBR



{
plot.dpi  <-  1000

p.glob.scalar <- 0.675
p.width.bv <- 3.15 * p.glob.scalar * 3
p.height.bv  <- 3.25 * ( n.mod.form / 3 ) * p.glob.scalar 

filename.bv.lon.Bind.male =   str_c(results.out.dir.bv.figs , 'bovine.lon.Bind.male.jpeg')
filename.bv.lon.Bind.female=   str_c(results.out.dir.bv.figs , 'bovine.lon.Bind.female.jpeg')

filename.bv.hin.Bind.male =   str_c(results.out.dir.bv.figs  , 'bovine.hin.Bind.male.jpeg')
filename.bv.hin.Bind.female=   str_c(results.out.dir.bv.figs  , 'bovine.hin.Bind.female.jpeg')

filename.bv.lon.Btau.male =   str_c(results.out.dir.bv.figs  , 'bovine.lon.Btau.male.jpeg')
filename.bv.lon.Btau.female=   str_c(results.out.dir.bv.figs , 'bovine.lon.Btau.female.jpeg')

filename.bv.hin.Btau.male =   str_c(results.out.dir.bv.figs  , 'bovine.hin.Btau.male.jpeg')
filename.bv.hin.Btau.female =   str_c(results.out.dir.bv.figs  , 'bovine.hin.Btau.female.jpeg')


}  # Plot settings/file names



# Bos indicus
ggsave(filename =  filename.bv.lon.Bind.male,  gg.valid.bv.lon.Bind.male.all , width = p.width.bv, height = p.height.bv  , dpi = plot.dpi )
if ( n.sexes == 2 ) { ggsave(filename =  filename.bv.lon.Bind.female,  gg.valid.bv.lon.Bind.female.all , width = p.width.bv, height = p.height.bv  , dpi = plot.dpi ) }

ggsave(filename =  filename.bv.hin.Bind.male,  gg.valid.bv.hin.Bind.male.all , width = p.width.bv, height = p.height.bv  , dpi = plot.dpi )
if ( n.sexes == 2 & n.breeds == 2 ) { ggsave(filename =  filename.bv.hin.Bind.female,  gg.valid.bv.hin.Bind.female.all , width = p.width.bv, height = p.height.bv  , dpi = plot.dpi ) }


# Exotic
if ( n.breeds == 2  ){ 
  
ggsave(filename =  filename.bv.lon.Btau.male,  gg.valid.bv.lon.Btau.male.all , width = p.width.bv, height = p.height.bv  , dpi = plot.dpi )
if ( n.sexes == 2  ){ ggsave(filename =  filename.bv.lon.Btau.female,  gg.valid.bv.lon.Btau.female.all , width = p.width.bv, height = p.height.bv  , dpi = plot.dpi ) }

ggsave(filename =  filename.bv.hin.Btau.male,  gg.valid.bv.hin.Btau.male.all , width = p.width.bv, height = p.height.bv  , dpi = plot.dpi )
if ( n.sexes == 2 & n.breeds == 2 ) { ggsave(filename =  filename.bv.hin.Btau.female,  gg.valid.bv.hin.Btau.female.all , width = p.width.bv, height = p.height.bv  , dpi = plot.dpi ) }

}





# -- ~~~~~~~~~~~~~~ --- SHEEP --- ~~~~~~~~~~~~~~ 

{

# Plot basic GBR model (model class = 1)
model.class <- 1
sex <- sex.male

gg.d.reg.out.sp.lon.male.all <- gen.gg.valid.data( 1 , species.sheep , ndf.lev.lo , sex.male )


# FEMALE
sex <- sex.female

gg.d.reg.out.sp.lon.female.all <- gen.gg.valid.data( 1 , species.sheep , ndf.lev.lo , sex.female )


# High NDF
sex <- sex.male

gg.d.reg.out.sp.hin.male.all <- gen.gg.valid.data( 1 , species.sheep , ndf.lev.hi , sex.male )


# FEMALE
sex <- sex.female

gg.d.reg.out.sp.hin.female.all <- gen.gg.valid.data( 1 , species.sheep , ndf.lev.hi , sex.female )



# Plot ME models - no sex de-lineation
  model.class <- 3
  
  gg.d.me.sp.lon.all <- gen.gg.valid.data( 3 , species.sheep , ndf.lev.lo , sex.male )
  

  gg.d.me.sp.hin.all <- gen.gg.valid.data( 3 , species.sheep , ndf.lev.hi , sex.male )
  

  
} # Data frame tabulation 


{
# GBR Model plots

# Sheep low NDF
# Male
gg.gbr.y.valid.sp.lon.male.0.tier.1 <- gen.gg.valid(gg.d.reg.out.sp.lon.male.all[gg.d.reg.out.sp.lon.male.all$mod.tier == "Tier 1",]  ,gg.valid.lab.x.crd.sp.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

  # gg.gbr.y.valid.sp.lon.male.0.tier.1

gg.gbr.y.valid.sp.lon.male.0.tier.2 <- gen.gg.valid(gg.d.reg.out.sp.lon.male.all[gg.d.reg.out.sp.lon.male.all$mod.tier == "Tier 2",]  ,gg.valid.lab.x.crd.sp.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

# gg.gbr.y.valid.sp.lon.male.0.tier.2

gg.gbr.y.valid.sp.lon.male.0.tier.3 <- gen.gg.valid(gg.d.reg.out.sp.lon.male.all[gg.d.reg.out.sp.lon.male.all$mod.tier == "Tier 3",]  ,gg.valid.lab.x.crd.sp.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

# gg.gbr.y.valid.sp.lon.male.0.tier.3

# Female
gg.gbr.y.valid.sp.lon.female.0.tier.1 <- gen.gg.valid(gg.d.reg.out.sp.lon.female.all[gg.d.reg.out.sp.lon.female.all$mod.tier == "Tier 1",]  ,gg.valid.lab.x.crd.sp.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

# gg.gbr.y.valid.sp.lon.female.0.tier.1

gg.gbr.y.valid.sp.lon.female.0.tier.2 <- gen.gg.valid(gg.d.reg.out.sp.lon.female.all[gg.d.reg.out.sp.lon.female.all$mod.tier == "Tier 2",]  ,gg.valid.lab.x.crd.sp.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

# gg.gbr.y.valid.sp.lon.female.0.tier.2

gg.gbr.y.valid.sp.lon.female.0.tier.3 <- gen.gg.valid(gg.d.reg.out.sp.lon.female.all[gg.d.reg.out.sp.lon.female.all$mod.tier == "Tier 3",]  ,gg.valid.lab.x.crd.sp.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

# gg.gbr.y.valid.sp.lon.female.0.tier.3


# Sheep high NDF
# Male
gg.gbr.y.valid.sp.hin.male.0.tier.1 <- gen.gg.valid(gg.d.reg.out.sp.hin.male.all[gg.d.reg.out.sp.hin.male.all$mod.tier == "Tier 1",]  ,gg.valid.lab.x.crd.sp.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

# gg.gbr.y.valid.sp.hin.male.0.tier.1

gg.gbr.y.valid.sp.hin.male.0.tier.2 <- gen.gg.valid(gg.d.reg.out.sp.hin.male.all[gg.d.reg.out.sp.hin.male.all$mod.tier == "Tier 2",]  ,gg.valid.lab.x.crd.sp.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

# gg.gbr.y.valid.sp.hin.male.0.tier.2

gg.gbr.y.valid.sp.hin.male.0.tier.3 <- gen.gg.valid(gg.d.reg.out.sp.hin.male.all[gg.d.reg.out.sp.hin.male.all$mod.tier == "Tier 3",]  ,gg.valid.lab.x.crd.sp.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

# gg.gbr.y.valid.sp.hin.male.0.tier.3

# Female
gg.gbr.y.valid.sp.hin.female.0.tier.1 <- gen.gg.valid(gg.d.reg.out.sp.hin.female.all[gg.d.reg.out.sp.hin.female.all$mod.tier == "Tier 1",]  ,gg.valid.lab.x.crd.sp.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

# gg.gbr.y.valid.sp.hin.female.0.tier.1

gg.gbr.y.valid.sp.hin.female.0.tier.2 <- gen.gg.valid(gg.d.reg.out.sp.hin.female.all[gg.d.reg.out.sp.hin.female.all$mod.tier == "Tier 2",]  ,gg.valid.lab.x.crd.sp.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

# gg.gbr.y.valid.sp.hin.female.0.tier.2

gg.gbr.y.valid.sp.hin.female.0.tier.3 <- gen.gg.valid(gg.d.reg.out.sp.hin.female.all[gg.d.reg.out.sp.hin.female.all$mod.tier == "Tier 3",]  ,gg.valid.lab.x.crd.sp.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

# gg.gbr.y.valid.sp.hin.female.0.tier.3

} # Generate GBR plots


{
# Sheep  
# Low NDF
gg.me.y.valid.sp.lon.0.tier.1 <- gen.gg.valid(gg.d.me.sp.lon.all[gg.d.me.sp.lon.all$mod.tier == "Tier 1",]   ,gg.valid.lab.x.crd.sp.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4  )

  # gg.me.y.valid.sp.lon.0.tier.1
  
gg.me.y.valid.sp.lon.0.tier.2 <- gen.gg.valid(gg.d.me.sp.lon.all[gg.d.me.sp.lon.all$mod.tier == "Tier 2",]    ,gg.valid.lab.x.crd.sp.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4  )

# gg.me.y.valid.sp.lon.0.tier.2
  
gg.me.y.valid.sp.lon.0.tier.3 <- gen.gg.valid(gg.d.me.sp.lon.all[gg.d.me.sp.lon.all$mod.tier == "Tier 3",]   ,gg.valid.lab.x.crd.sp.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  , gg.valid.lab.y.crd.lev.4 )

# gg.me.y.valid.sp.lon.0.tier.3
  

# High NDF
gg.me.y.valid.sp.hin.0.tier.1 <- gen.gg.valid(gg.d.me.sp.hin.all[gg.d.me.sp.hin.all$mod.tier == "Tier 1",]   ,gg.valid.lab.x.crd.sp.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4  )
# gg.me.y.valid.sp.hin.0.tier.1

gg.me.y.valid.sp.hin.0.tier.2 <- gen.gg.valid(gg.d.me.sp.hin.all[gg.d.me.sp.hin.all$mod.tier == "Tier 2",]    ,gg.valid.lab.x.crd.sp.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4  )
# gg.me.y.valid.sp.hin.0.tier.2

gg.me.y.valid.sp.hin.0.tier.3 <- gen.gg.valid(gg.d.me.sp.hin.all[gg.d.me.sp.hin.all$mod.tier == "Tier 3",]   ,gg.valid.lab.x.crd.sp.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  , gg.valid.lab.y.crd.lev.4 )
# gg.me.y.valid.sp.hin.0.tier.3


} # Generate ME Model plots


# MERGED PLOT OBJECTS
# GBR
{
# Male
gg.y.valid.reg.out.sp.lon.male.p.tier.1 <- gg.gbr.y.valid.sp.lon.male.0.tier.1  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,  axis.ticks.x = element_blank()
    ,   legend.position  = "none"
  )

gg.y.valid.reg.out.sp.lon.male.p.tier.2 <- gg.gbr.y.valid.sp.lon.male.0.tier.2  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,   legend.position  = "none"
  )

gg.y.valid.reg.out.sp.lon.male.p.tier.3 <- gg.gbr.y.valid.sp.lon.male.0.tier.3  %>% + 
  theme(
    axis.title.y = element_blank()
    , axis.title.x = element_blank()
  )

# Female
gg.y.valid.reg.out.sp.lon.female.p.tier.1 <- gg.gbr.y.valid.sp.lon.female.0.tier.1  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,  axis.ticks.x = element_blank()
    ,   legend.position  = "none"
  )

gg.y.valid.reg.out.sp.lon.female.p.tier.2 <- gg.gbr.y.valid.sp.lon.female.0.tier.2  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,   legend.position  = "none"
  )

gg.y.valid.reg.out.sp.lon.female.p.tier.3 <- gg.gbr.y.valid.sp.lon.female.0.tier.3  %>% + 
  theme(
    axis.title.y = element_blank()
    , axis.title.x = element_blank()
  )


# Sheep High NDF
# Male
gg.y.valid.reg.out.sp.hin.male.p.tier.1 <- gg.gbr.y.valid.sp.hin.male.0.tier.1  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,  axis.ticks.x = element_blank()
    ,   legend.position  = "none"
  )

gg.y.valid.reg.out.sp.hin.male.p.tier.2 <- gg.gbr.y.valid.sp.hin.male.0.tier.2  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,   legend.position  = "none"
  )

gg.y.valid.reg.out.sp.hin.male.p.tier.3 <- gg.gbr.y.valid.sp.hin.male.0.tier.3  %>% + 
  theme(
    axis.title.y = element_blank()
    , axis.title.x = element_blank()
  )

# Female
gg.y.valid.reg.out.sp.hin.female.p.tier.1 <- gg.gbr.y.valid.sp.hin.female.0.tier.1  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,  axis.ticks.x = element_blank()
    ,   legend.position  = "none"
  )

gg.y.valid.reg.out.sp.hin.female.p.tier.2 <- gg.gbr.y.valid.sp.hin.female.0.tier.2  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,   legend.position  = "none"
  )

gg.y.valid.reg.out.sp.hin.female.p.tier.3 <- gg.gbr.y.valid.sp.hin.female.0.tier.3  %>% + 
  theme(
    axis.title.y = element_blank()
    , axis.title.x = element_blank()
  )


}

# ME 
{
# Low NDF
gg.y.valid.me.sp.lon.p.tier.1 <- gg.me.y.valid.sp.lon.0.tier.1  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,   axis.ticks.x = element_blank()
    ,   legend.position  = "none"
  )

gg.y.valid.me.sp.lon.p.tier.2 <- gg.me.y.valid.sp.lon.0.tier.2  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,   axis.ticks.x = element_blank()
    ,   legend.position  = "none"
  )

gg.y.valid.me.sp.lon.p.tier.3 <- gg.me.y.valid.sp.lon.0.tier.3  %>% + 
  theme(
  axis.title.y = element_blank()
    , axis.title.x = element_blank()
  )


# High NDF
gg.y.valid.me.sp.hin.p.tier.1 <- gg.me.y.valid.sp.hin.0.tier.1  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,   axis.ticks.x = element_blank()
    ,   legend.position  = "none"
  )

gg.y.valid.me.sp.hin.p.tier.2 <- gg.me.y.valid.sp.hin.0.tier.2  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,   axis.ticks.x = element_blank()
    ,   legend.position  = "none"
  )

gg.y.valid.me.sp.hin.p.tier.3 <- gg.me.y.valid.sp.hin.0.tier.3  %>% + 
  theme(
   axis.title.y = element_blank()
    , axis.title.x = element_blank()
  )

}


# GG ARRANGEs
# GBR
{
  heights <- c( 0.9 , 0.9 , 1 )
  
gg.valid.sp.lon.male.gbr.all.base  <- ggarrange(
  
  gg.y.valid.reg.out.sp.lon.male.p.tier.1
  ,   gg.y.valid.reg.out.sp.lon.male.p.tier.2
  ,  gg.y.valid.reg.out.sp.lon.male.p.tier.3
  

  , nrow = 3
  , heights =  heights
)
  
  gg.valid.sp.lon.female.gbr.all.base  <- ggarrange(
    
    gg.y.valid.reg.out.sp.lon.female.p.tier.1
    ,   gg.y.valid.reg.out.sp.lon.female.p.tier.2
    ,  gg.y.valid.reg.out.sp.lon.female.p.tier.3
    
    
    , nrow = 3
    , heights =  heights
  )

gg.valid.sp.hin.male.gbr.all.base  <- ggarrange(
  
  gg.y.valid.reg.out.sp.hin.male.p.tier.1
  ,   gg.y.valid.reg.out.sp.hin.male.p.tier.2
  ,  gg.y.valid.reg.out.sp.hin.male.p.tier.3
  
  
  , nrow = 3
  , heights =  heights
)

gg.valid.sp.hin.female.gbr.all.base  <- ggarrange(
  
  gg.y.valid.reg.out.sp.hin.female.p.tier.1
  ,   gg.y.valid.reg.out.sp.hin.female.p.tier.2
  ,  gg.y.valid.reg.out.sp.hin.female.p.tier.3
  
  
  , nrow = 3
  , heights =  heights
)
}

# ME
{
gg.valid.sp.lon.me.all.base  <- ggarrange(
  
  gg.y.valid.me.sp.lon.p.tier.1
  ,   gg.y.valid.me.sp.lon.p.tier.2
  ,  gg.y.valid.me.sp.lon.p.tier.3
  
  
  , nrow = 3
  , heights =  heights
)

gg.valid.sp.hin.me.all.base  <- ggarrange(
  
  gg.y.valid.me.sp.hin.p.tier.1
  ,   gg.y.valid.me.sp.hin.p.tier.2
  ,  gg.y.valid.me.sp.hin.p.tier.3
  
  
  , nrow = 3
  , heights =  heights
)
}

# Annotations
{
gg.valid.sp.lon.male.gbr.all <- annotate_figure(  gg.valid.sp.lon.male.gbr.all.base  , 
                                     bottom = text_grob(
                                       gg.valid.x.tit
                                       , color = "black"
                                       , hjust = 0.5
                                       , vjust = 0.5
                                       , x = 0.5
                                       , size = 12
                                     ),
                                     left= text_grob(
                                       gg.valid.y.tit
                                       , color = "black"
                                       , hjust = 0.5
                                       , vjust = 0.5
                                       , x = 0.5
                                       , rot = 90
                                       , size = 12
                                     )
)
  
  gg.valid.sp.lon.female.gbr.all <- annotate_figure(  gg.valid.sp.lon.female.gbr.all.base  , 
                                                    bottom = text_grob(
                                                      gg.valid.x.tit
                                                      , color = "black"
                                                      , hjust = 0.5
                                                      , vjust = 0.5
                                                      , x = 0.5
                                                      , size = 12
                                                    ),
                                                    left= text_grob(
                                                      gg.valid.y.tit
                                                      , color = "black"
                                                      , hjust = 0.5
                                                      , vjust = 0.5
                                                      , x = 0.5
                                                      , rot = 90
                                                      , size = 12
                                                    )
  )
  

gg.valid.sp.hin.male.gbr.all <- annotate_figure(  gg.valid.sp.hin.male.gbr.all.base  , 
                                             bottom = text_grob(
                                               gg.valid.x.tit
                                               , color = "black"
                                               , hjust = 0.5
                                               , vjust = 0.5
                                               , x = 0.5
                                               , size = 12
                                             ),
                                             left= text_grob(
                                               gg.valid.y.tit
                                               , color = "black"
                                               , hjust = 0.5
                                               , vjust = 0.5
                                               , x = 0.5
                                               , rot = 90
                                               , size = 12
                                             )
)

gg.valid.sp.hin.female.gbr.all <- annotate_figure(  gg.valid.sp.hin.female.gbr.all.base  , 
                                                  bottom = text_grob(
                                                    gg.valid.x.tit
                                                    , color = "black"
                                                    , hjust = 0.5
                                                    , vjust = 0.5
                                                    , x = 0.5
                                                    , size = 12
                                                  ),
                                                  left= text_grob(
                                                    gg.valid.y.tit
                                                    , color = "black"
                                                    , hjust = 0.5
                                                    , vjust = 0.5
                                                    , x = 0.5
                                                    , rot = 90
                                                    , size = 12
                                                  )
)

} # GBR

{
gg.valid.sp.lon.me.all <- annotate_figure(  gg.valid.sp.lon.me.all.base  , 
                                             bottom = text_grob(
                                               gg.valid.x.tit
                                               , color = "black"
                                               , hjust = 0.5
                                               , vjust = 0.5
                                               , x = 0.5
                                               , size = 12
                                             ),
                                             left= text_grob(
                                               gg.valid.y.tit
                                               , color = "black"
                                               , hjust = 0.5
                                               , vjust = 0.5
                                               , x = 0.5
                                               , rot = 90
                                               , size = 12
                                             )
)

gg.valid.sp.hin.me.all <- annotate_figure(  gg.valid.sp.hin.me.all.base  , 
                                             bottom = text_grob(
                                               gg.valid.x.tit
                                               , color = "black"
                                               , hjust = 0.5
                                               , vjust = 0.5
                                               , x = 0.5
                                               , size = 12
                                             ),
                                             left= text_grob(
                                               gg.valid.y.tit
                                               , color = "black"
                                               , hjust = 0.5
                                               , vjust = 0.5
                                               , x = 0.5
                                               , rot = 90
                                               , size = 12
                                             )
)

} # ME

plot.dpi  <-  1000

p.glob.scalar <- 0.675
p.width.sp <- 2.9 * p.glob.scalar * 3
p.height.sp  <- 9.75 * p.glob.scalar

filename.sp.lon.male.gbr =   str_c(results.out.dir  , 'sheep.gbr.lon.male.jpeg')
filename.sp.lon.female.gbr =   str_c(results.out.dir  , 'sheep.gbr.lon.female.jpeg')

filename.sp.hin.male.gbr =   str_c(results.out.dir  , 'sheep.gbr.hin.male.jpeg')
filename.sp.hin.female.gbr =   str_c(results.out.dir  , 'sheep.gbr.hin.female.jpeg')


filename.sp.lon.me =   str_c(results.out.dir  , 'sheep.me.lon.jpeg')
filename.sp.hin.me =   str_c(results.out.dir  , 'sheep.me.hin.jpeg')

ggsave(filename =  filename.sp.lon.male.gbr,  gg.valid.sp.lon.male.gbr.all , width = p.width.sp, height = p.height.sp  , dpi = plot.dpi )
ggsave(filename =  filename.sp.lon.female.gbr,  gg.valid.sp.lon.female.gbr.all , width = p.width.sp, height = p.height.sp  , dpi = plot.dpi )


ggsave(filename =  filename.sp.hin.male.gbr,  gg.valid.sp.hin.male.gbr.all , width = p.width.sp, height = p.height.sp  , dpi = plot.dpi )
ggsave(filename =  filename.sp.hin.female.gbr,  gg.valid.sp.hin.female.gbr.all , width = p.width.sp, height = p.height.sp  , dpi = plot.dpi )


ggsave(filename =  filename.sp.lon.me,  gg.valid.sp.lon.me.all , width = p.width.sp, height = p.height.sp  , dpi = plot.dpi )
ggsave(filename =  filename.sp.hin.me,  gg.valid.sp.hin.me.all , width = p.width.sp, height = p.height.sp  , dpi = plot.dpi )




# GOATS
# Dataframe generation
{
model.class <- 1
  
gg.d.reg.out.gt.lon.all <- gen.gg.df.specific( 1 ,  model.class , species.goat , ndf.lev.lo) 

for (m in 2:n.mod.form  ){ 
  gg.d.reg.out.gt.lon.all <- rbind(gg.d.reg.out.gt.lon.all , gen.gg.df.specific(m ,  model.class , species.goat , ndf.lev.lo)  ) 
  if (m == n.mod.form){ unq.lab.1rs <- unique(    gg.d.reg.out.gt.lon.all$col.mod.form.label.tier.1) ;     gg.d.reg.out.gt.lon.all$col.mod.form.label.tier.1 <- factor(     gg.d.reg.out.gt.lon.all$col.mod.form.label.tier.1 , levels = unq.lab.1rs)
  }}

gg.d.reg.out.gt.hin.all <- gen.gg.df.specific( 1 ,  model.class, species.goat , ndf.lev.hi) 

for (m in 2:n.mod.form ){ 
  gg.d.reg.out.gt.hin.all <- rbind( gg.d.reg.out.gt.hin.all , gen.gg.df.specific(m ,  model.class, species.goat , ndf.lev.hi)) 
  if (m == n.mod.form){ unq.lab.1rs <- unique(    gg.d.reg.out.gt.hin.all$col.mod.form.label.tier.1) ;    gg.d.reg.out.gt.hin.all$col.mod.form.label.tier.1 <- factor(     gg.d.reg.out.gt.hin.all$col.mod.form.label.tier.1 , levels = unq.lab.1rs)
  }}



gg.d.me.gt.lon.all <- gen.gg.df.specific( 1 ,  model.class , species.goat , ndf.lev.lo) 

for (m in 2:n.mod.form  ){
  gg.d.me.gt.lon.all <- rbind(gg.d.me.gt.lon.all  , gen.gg.df.specific(m ,  model.class , species.goat , ndf.lev.lo)  )  
  if (m == n.mod.form){ unq.lab.1rs <- unique(      gg.d.me.gt.lon.all$col.mod.form.label.tier.1) ;     gg.d.me.gt.lon.all$col.mod.form.label.tier.1 <- factor(  gg.d.me.gt.lon.all$col.mod.form.label.tier.1 , levels = unq.lab.1rs) }
}


gg.d.me.gt.hin.all <- gen.gg.df.specific( 1 , 1, species.goat , ndf.lev.hi) 

for (m in 2:n.mod.form ){   
  gg.d.me.gt.hin.all <- rbind(   gg.d.me.gt.hin.all , gen.gg.df.specific(m ,  model.class, species.goat, ndf.lev.hi))  
  if (m == n.mod.form){ unq.lab.1rs <- unique(      gg.d.me.gt.hin.all$col.mod.form.label.tier.1) ;     gg.d.me.gt.hin.all$col.mod.form.label.tier.1 <- factor(  gg.d.me.gt.hin.all$col.mod.form.label.tier.1 , levels = unq.lab.1rs) }
}

}


{
  
  # Plot basic GBR model (model class = 1)
  model.class <- 1
  sex <- sex.male
  
  gg.d.reg.out.gt.lon.male.all <- gen.gg.valid.data( 1 , species.sheep , ndf.lev.lo , sex.male )
  
  
  # FEMALE
  sex <- sex.female
  
  gg.d.reg.out.gt.lon.female.all <- gen.gg.valid.data( 1 , species.sheep , ndf.lev.lo , sex.female )
  
  
  # High NDF
  sex <- sex.male
  
  gg.d.reg.out.gt.hin.male.all <- gen.gg.valid.data( 1 , species.sheep , ndf.lev.hi , sex.male )
  
  
  # FEMALE
  sex <- sex.female
  
  gg.d.reg.out.gt.hin.female.all <- gen.gg.valid.data( 1 , species.sheep , ndf.lev.hi , sex.female )
  
  
  
  # Plot ME models - no sex de-lineation
  model.class <- 3
  
  gg.d.me.gt.lon.all <- gen.gg.valid.data( 3 , species.sheep , ndf.lev.lo , sex.male )
  
  
  gg.d.me.gt.hin.all <- gen.gg.valid.data( 3 , species.sheep , ndf.lev.hi , sex.male )
  
  
  
} # Data frame tabulation 


{

# Low NDF
gg.gbr.y.valid.gt.lon.0.tier.1 <- gen.gg.valid(gg.d.reg.out.gt.lon.all[gg.d.reg.out.gt.lon.all$mod.tier == "Tier 1",]  ,gg.valid.lab.x.crd.gt.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

gg.gbr.y.valid.gt.lon.0.tier.1

gg.gbr.y.valid.gt.lon.0.tier.2 <- gen.gg.valid(gg.d.reg.out.gt.lon.all[gg.d.reg.out.gt.lon.all$mod.tier == "Tier 2",]  ,gg.valid.lab.x.crd.gt.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

gg.gbr.y.valid.gt.lon.0.tier.2

gg.gbr.y.valid.gt.lon.0.tier.3 <- gen.gg.valid(gg.d.reg.out.gt.lon.all[gg.d.reg.out.gt.lon.all$mod.tier == "Tier 3",]  ,gg.valid.lab.x.crd.gt.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

gg.gbr.y.valid.gt.lon.0.tier.3


# High NDF
gg.gbr.y.valid.gt.hin.0.tier.1 <- gen.gg.valid(gg.d.reg.out.gt.hin.all[gg.d.reg.out.gt.hin.all$mod.tier == "Tier 1",]  ,gg.valid.lab.x.crd.gt.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

gg.gbr.y.valid.gt.hin.0.tier.1

gg.gbr.y.valid.gt.hin.0.tier.2 <- gen.gg.valid(gg.d.reg.out.gt.hin.all[gg.d.reg.out.gt.hin.all$mod.tier == "Tier 2",]  ,gg.valid.lab.x.crd.gt.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

gg.gbr.y.valid.gt.hin.0.tier.2

gg.gbr.y.valid.gt.hin.0.tier.3 <- gen.gg.valid(gg.d.reg.out.gt.hin.all[gg.d.reg.out.gt.hin.all$mod.tier == "Tier 3",]  ,gg.valid.lab.x.crd.gt.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4 )

gg.gbr.y.valid.gt.hin.0.tier.3


} # Generate GBR plots


{
# Low NDF
gg.me.y.valid.gt.lon.0.tier.1 <- gen.gg.valid(gg.d.me.gt.lon.all[gg.d.reg.out.gt.lon.all$mod.tier == "Tier 1",]   ,gg.valid.lab.x.crd.gt.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4  )
gg.me.y.valid.gt.lon.0.tier.1

gg.me.y.valid.gt.lon.0.tier.2 <- gen.gg.valid(gg.d.me.gt.lon.all[gg.d.reg.out.gt.lon.all$mod.tier == "Tier 2",]    ,gg.valid.lab.x.crd.gt.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4  )
gg.me.y.valid.gt.lon.0.tier.2

gg.me.y.valid.gt.lon.0.tier.3 <- gen.gg.valid(gg.d.me.gt.lon.all[gg.d.reg.out.gt.lon.all$mod.tier == "Tier 3",]   ,gg.valid.lab.x.crd.gt.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  , gg.valid.lab.y.crd.lev.4 )
gg.me.y.valid.gt.lon.0.tier.3


# High NDF
gg.me.y.valid.gt.hin.0.tier.1 <- gen.gg.valid(gg.d.me.gt.hin.all[gg.d.reg.out.gt.hin.all$mod.tier == "Tier 1",]   ,gg.valid.lab.x.crd.gt.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4  )
gg.me.y.valid.gt.hin.0.tier.1

gg.me.y.valid.gt.hin.0.tier.2 <- gen.gg.valid(gg.d.me.gt.hin.all[gg.d.reg.out.gt.hin.all$mod.tier == "Tier 2",]    ,gg.valid.lab.x.crd.gt.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3 , gg.valid.lab.y.crd.lev.4  )
gg.me.y.valid.gt.hin.0.tier.2

gg.me.y.valid.gt.hin.0.tier.3 <- gen.gg.valid(gg.d.me.gt.hin.all[gg.d.reg.out.gt.hin.all$mod.tier == "Tier 3",]   ,gg.valid.lab.x.crd.gt.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  , gg.valid.lab.y.crd.lev.4 )
gg.me.y.valid.gt.hin.0.tier.3

} # Generate ME plots

# Plot merges

{
# Low NDF
gg.y.valid.reg.out.gt.lon.p.tier.1 <- gg.gbr.y.valid.gt.lon.0.tier.1  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,  , axis.ticks.x = element_blank()
  )

gg.y.valid.reg.out.gt.lon.p.tier.2 <- gg.gbr.y.valid.gt.lon.0.tier.2  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
  )

gg.y.valid.reg.out.gt.lon.p.tier.3 <- gg.gbr.y.valid.gt.lon.0.tier.3  %>% + 
  theme(
    axis.title.y = element_blank()
    , axis.title.x = element_blank()
  )


# High NDF
gg.y.valid.reg.out.gt.hin.p.tier.1 <- gg.gbr.y.valid.gt.hin.0.tier.1  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,   axis.ticks.x = element_blank()
  )

gg.y.valid.reg.out.gt.hin.p.tier.2 <- gg.gbr.y.valid.gt.hin.0.tier.2  %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    ,   axis.ticks.x = element_blank()
  )

gg.y.valid.reg.out.gt.hin.p.tier.3 <- gg.gbr.y.valid.gt.hin.0.tier.3  %>% + 
  theme(
    axis.title.y = element_blank()
    , axis.title.x = element_blank()
  )
} # GBR

{
  # Goats
  gg.y.valid.me.gt.lon.p.tier.1 <- gg.me.y.valid.gt.lon.0.tier.1  %>% + 
    theme(
      axis.text.x = element_blank()
      , axis.title.y = element_blank()
      , axis.title.x = element_blank()
      ,  axis.ticks.x = element_blank()
    )
  
  gg.y.valid.me.gt.lon.p.tier.2 <- gg.me.y.valid.gt.lon.0.tier.2  %>% + 
    theme(
      axis.text.x = element_blank()
      , axis.title.y = element_blank()
      , axis.title.x = element_blank()
      ,   axis.ticks.x = element_blank()
    )
  
  gg.y.valid.me.gt.lon.p.tier.3 <- gg.me.y.valid.gt.lon.0.tier.3  %>% + 
    theme(
      
      axis.title.y = element_blank()
      , axis.title.x = element_blank()
      
    )
  
} # ME

# GG Arranges
{
gg.valid.gt.lon.gbr.all.base  <- ggarrange(
  
  gg.y.valid.reg.out.gt.lon.p.tier.1
  ,   gg.y.valid.reg.out.gt.lon.p.tier.2
  ,  gg.y.valid.reg.out.gt.lon.p.tier.3
  
  
  , nrow = 3
  , heights =  heights
)

gg.valid.gt.hin.gbr.all.base  <- ggarrange(
  
  gg.y.valid.reg.out.gt.hin.p.tier.1
  ,   gg.y.valid.reg.out.gt.hin.p.tier.2
  ,  gg.y.valid.reg.out.gt.hin.p.tier.3
  
  
  , nrow = 3
  , heights =  heights
)
} # GBR

{
  gg.valid.gt.lon.me.all.base  <- ggarrange(
    
    gg.y.valid.me.gt.lon.p.tier.1
    ,   gg.y.valid.me.gt.lon.p.tier.2
    ,  gg.y.valid.me.gt.lon.p.tier.3
    
    
    , nrow = 3
    , heights =  heights
  )
  
  gg.valid.gt.hin.me.all.base  <- ggarrange(
    
    gg.y.valid.me.gt.hin.p.tier.1
    ,   gg.y.valid.me.gt.hin.p.tier.2
    ,  gg.y.valid.me.gt.hin.p.tier.3
    
    
    , nrow = 3
    , heights =  heights
  )
} # ME


# Annotations
{
gg.valid.gt.lon.gbr.all <- annotate_figure(  gg.valid.gt.lon.gbr.all.base  , 
                                             bottom = text_grob(
                                               gg.valid.x.tit
                                               , color = "black"
                                               , hjust = 0.5
                                               , vjust = 0.5
                                               , x = 0.5
                                               , size = 12
                                             ),
                                             left= text_grob(
                                               gg.valid.y.tit
                                               , color = "black"
                                               , hjust = 0.5
                                               , vjust = 0.5
                                               , x = 0.5
                                               , rot = 90
                                               , size = 12
                                             )
)



gg.valid.gt.hin.gbr.all <- annotate_figure(  gg.valid.gt.hin.gbr.all.base  , 
                                             bottom = text_grob(
                                               gg.valid.x.tit
                                               , color = "black"
                                               , hjust = 0.5
                                               , vjust = 0.5
                                               , x = 0.5
                                               , size = 12
                                             ),
                                             left= text_grob(
                                               gg.valid.y.tit
                                               , color = "black"
                                               , hjust = 0.5
                                               , vjust = 0.5
                                               , x = 0.5
                                               , rot = 90
                                               , size = 12
                                             )
                                             
                                             
                                             gg.valid.gt.lon.me.all <- annotate_figure(  gg.valid.gt.lon.me.all.base  , 
                                                                                         bottom = text_grob(
                                                                                           gg.valid.x.tit
                                                                                           , color = "black"
                                                                                           , hjust = 0.5
                                                                                           , vjust = 0.5
                                                                                           , x = 0.5
                                                                                           , size = 12
                                                                                         ),
                                                                                         left= text_grob(
                                                                                           gg.valid.y.tit
                                                                                           , color = "black"
                                                                                           , hjust = 0.5
                                                                                           , vjust = 0.5
                                                                                           , x = 0.5
                                                                                           , rot = 90
                                                                                           , size = 12
                                                                                         )
                                             )
                                             
                                             
                                             
                                             gg.valid.gt.hin.me.all <- annotate_figure(  gg.valid.gt.hin.me.all.base  , 
                                                                                         bottom = text_grob(
                                                                                           gg.valid.x.tit
                                                                                           , color = "black"
                                                                                           , hjust = 0.5
                                                                                           , vjust = 0.5
                                                                                           , x = 0.5
                                                                                           , size = 12
                                                                                         ),
                                                                                         left= text_grob(
                                                                                           gg.valid.y.tit
                                                                                           , color = "black"
                                                                                           , hjust = 0.5
                                                                                           , vjust = 0.5
                                                                                           , x = 0.5
                                                                                           , rot = 90
                                                                                           , size = 12
                                                                                         )
                                             )
                                             
                                             
)
} # GBR

{                               
gg.valid.gt.lon.me.all <- annotate_figure(  gg.valid.gt.lon.me.all.base  , 
                                                                                           bottom = text_grob(
                                                                                             gg.valid.x.tit
                                                                                             , color = "black"
                                                                                             , hjust = 0.5
                                                                                             , vjust = 0.5
                                                                                             , x = 0.5
                                                                                             , size = 12
                                                                                           ),
                                                                                           left= text_grob(
                                                                                             gg.valid.y.tit
                                                                                             , color = "black"
                                                                                             , hjust = 0.5
                                                                                             , vjust = 0.5
                                                                                             , x = 0.5
                                                                                             , rot = 90
                                                                                             , size = 12
                                                                                           )
                                               )
                                               
                                               
                                               
                                               gg.valid.gt.hin.me.all <- annotate_figure(  gg.valid.gt.hin.me.all.base  , 
                                                                                           bottom = text_grob(
                                                                                             gg.valid.x.tit
                                                                                             , color = "black"
                                                                                             , hjust = 0.5
                                                                                             , vjust = 0.5
                                                                                             , x = 0.5
                                                                                             , size = 12
                                                                                           ),
                                                                                           left= text_grob(
                                                                                             gg.valid.y.tit
                                                                                             , color = "black"
                                                                                             , hjust = 0.5
                                                                                             , vjust = 0.5
                                                                                             , x = 0.5
                                                                                             , rot = 90
                                                                                             , size = 12
                                                                                           )
                                               )
                                               
                                               
  )
} # ME


filename.gt.lon.gbr =   str_c(results.out.dir  , 'sheep.gbr.lon.jpeg')
filename.gt.hin.gbr =   str_c(results.out.dir  , 'sheep.gbr.hin.jpeg')
filename.gt.lon.me =   str_c(results.out.dir  , 'sheep.me.lon.jpeg')
filename.gt.hin.me =   str_c(results.out.dir  , 'sheep.me.hin.jpeg')

ggsave(filename =  filename.gt.lon.gbr,  gg.valid.gt.lon.gbr.all , width = p.width.gt, height = p.height.gt  , dpi = plot.dpi )
ggsave(filename =  filename.gt.hin.gbr,  gg.valid.gt.hin.gbr.all , width = p.width.gt, height = p.height.gt  , dpi = plot.dpi )
ggsave(filename =  filename.gt.lon.me,  gg.valid.gt.lon.me.all , width = p.width.gt, height = p.height.gt  , dpi = plot.dpi )
ggsave(filename =  filename.gt.hin.me,  gg.valid.gt.hin.me.all , width = p.width.gt, height = p.height.gt  , dpi = plot.dpi )






