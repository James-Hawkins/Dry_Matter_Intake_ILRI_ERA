
print('Running box plots out')


{ 
  
# Assign sample size (SS) values based on subsetting of data
  #d.reg.in[d.reg.in$species.ndf == sheep.lo.ndf & !is.na(d.reg.in$species.ndf)  , 'gg.SS.label'] <-    gg.x.label.sheep.lo.ndf 
  #d.reg.in[d.reg.in$species.ndf == sheep.hi.ndf & !is.na(d.reg.in$species.ndf)  , 'gg.SS.label'] <-    gg.x.label.sheep.hi.ndf 

  #d.reg.in[d.reg.in$species.ndf == goat.lo.ndf & !is.na(d.reg.in$species.ndf)  , 'gg.SS.label'] <-    gg.x.label.goat.lo.ndf 
  #d.reg.in[d.reg.in$species.ndf == goat.hi.ndf & !is.na(d.reg.in$species.ndf)  , 'gg.SS.label'] <-    gg.x.label.goat.hi.ndf 


# Assign labels
d.reg.in[d.reg.in$breed.type == breed.indig , 'breed.label'] <- "Bos indicus"
d.reg.in[d.reg.in$breed.type == breed.cbred, 'breed.label'] <-  "Exotic"

d.reg.in[d.reg.in$sex == sex.male , 'sex.label'] <-  "M"
d.reg.in[d.reg.in$sex == sex.female , 'sex.label'] <-  "F"

d.reg.in[ d.reg.in$ndf.level == ndf.lev.lo & !is.na(d.reg.in$ndf.level) , 'NDF.lev.label'] <-  "Low NDF"
d.reg.in[ d.reg.in$ndf.level == ndf.lev.hi & !is.na(d.reg.in$ndf.level)  , 'NDF.lev.label'] <-  "High NDF"



# Factorisation
#d.reg.in$gg.SS.label <- factor(d.reg.in$gg.SS.label, levels = c( gg.x.label.sheep.lo.ndf , gg.x.label.sheep.hi.ndf ,gg.x.label.goat.lo.ndf ,gg.x.label.goat.hi.ndf , gg.x.label.cattle.lo.ndf ,gg.x.label.cattle.hi.ndf )  )
  

y.tit.bw<<- bquote(Bodyweight~(kg))
y.tit.lwg<<- bquote(Average~daily~gain~(kg~d^-1))
y.tit.dmi<<- bquote(Dry~matter~intake~(kg~d^-1))
y.tit.my<<- bquote(Milk~yield~(kg~d^-1))

y.tit.ndf<<- bquote(Neutral~detergent~fibre~(g~kg^-1))
y.tit.adf<<- bquote(Acid~detergent~fibre~(g~kg^-1))
y.tit.cp<<- bquote(Crude~protein~(g~kg^-1))
y.tit.ee<<- bquote(Ether~extract~(g~kg^-1))
y.tit.ash<<- bquote(Ash~(g~kg^-1))
y.tit.adl<<- bquote(Acid~detergent~lignin~(g~kg^-1))

y.tit.NDFd<<- "NDF Digestibility (%)"
y.tit.ADFd<<-  "ADF Digestibility (%)" 
y.tit.CPd<<- "CP Digestibility (%)"
y.tit.OMd<<- "OM Digestibility (%)"
y.tit.DMd<<- "DM Digestibility (%)"



samp.sz.lab.fs <<- 2.4
samp.sz.lab.color <<- '#606060'



# DEFINE SAMPLE SIZES FOR TEXT ON BOX PLOTS
{
  
# BOVINES
# Low NDF
  
  

  
conds.all  <- list.sample.conds( TRUE )



for (c in 1:length(conds.all)){
for (  pv in c(pred.vars, 'feed_intake_kg_d')  ){
  
  # test: pv <- 'feed_intake_kg_d' ; c <- 4
  
cond <- de.listify(conds.all[c][[1]]) 

exp.SS <- length( unique( d.reg.in[ cond & !is.na(d.reg.in[ ,    pv   ]) ,  'ue.id' ] ))
trt.SS <- length( unique( d.reg.in[ cond & !is.na(d.reg.in[ ,    pv   ]) ,  'ut.id' ] ))
anim.SS <- sum( d.reg.in[ cond & !is.na(d.reg.in[ ,    pv   ]) ,  'Sample.size' ] )

str.exp <- str_c(lab.total.experiment.sample , ' = ' , exp.SS)
str.trt <- str_c(lab.total.treatment.sample , ' = ' , trt.SS )
str.anim <- str_c(lab.total.animal.sample  , ' = ' , anim.SS)

gg.lab.exp <- str_c( 'gg.exp.sample.lab.' , pv)
gg.lab.trt <- str_c( 'gg.trt.sample.lab.' , pv)
gg.lab.anim <- str_c( 'gg.anim.sample.lab.' , pv)

d.reg.in[ cond  ,gg.lab.exp ] <- str.exp
d.reg.in[ cond  ,gg.lab.trt ] <- str.trt
d.reg.in[ cond  ,gg.lab.anim ] <- str.anim



}}


#d.reg.in[  which(gbm.cond.bv.lo.ndf.Bind.male)[1]     , 'label.row'] <- TRUE

if (FALSE){

if ( n.sexes == 2 ){

cond <- reg.in.cond.bv.lo.ndf.Bind.female
  
d.reg.in[ cond  ,'gg.exp.sample.lab' ] <- str_c(lab.total.experiment.sample , ' = ' , d.reg.out[ gbm.cond.bv.lo.ndf.Bind.female , vn.total.exp.sample.size ][1] )  
d.reg.in[ cond ,'gg.trt.sample.lab' ] <- str_c(lab.total.treatment.sample , ' = ' , d.reg.out[ gbm.cond.bv.lo.ndf.Bind.female , vn.total.trt.sample.size ][1] )  
d.reg.in[ cond ,'gg.ani.sample.lab'] <- str_c(lab.total.animal.sample , ' = ' , d.reg.out[ gbm.cond.bv.lo.ndf.Bind.female , vn.total.animal.sample.size ][1] )  

#d.reg.in[  which(gbm.cond.bv.lo.ndf.Bind.female)[1]     , 'label.row'] <- TRUE

d.reg.out$Total.animal.sample

}

if ( n.breeds == 2 ){
  
cond <- reg.in.cond.bv.lo.ndf.Btau.male
  
d.reg.in[  cond,'gg.exp.sample.lab' ] <- str_c(lab.total.experiment.sample , ' = ' , d.reg.out[ gbm.cond.bv.lo.ndf.Btau.male , vn.total.exp.sample.size ][1] )  
d.reg.in[  cond,'gg.trt.sample.lab' ] <- str_c(lab.total.treatment.sample  , ' = ' , d.reg.out[ gbm.cond.bv.lo.ndf.Btau.male , vn.total.trt.sample.size ][1] )  
d.reg.in[  cond,'gg.ani.sample.lab'] <- str_c(lab.total.animal.sample  , ' = ' , d.reg.out[ gbm.cond.bv.lo.ndf.Btau.male , vn.total.animal.sample.size][1] )  

#d.reg.in[  which(gbm.cond.bv.lo.ndf.Btau.male)[1]     , 'label.row'] <- TRUE


if ( n.sexes == 2){
  
cond <- reg.in.cond.bv.lo.ndf.Btau.female
  
  d.reg.in[ cond,'gg.exp.sample.lab' ] <- str_c(lab.total.experiment.sample , ' = ' , d.reg.out[ gbm.cond.bv.lo.ndf.Btau.female , vn.total.exp.sample.size ][1] )  
  d.reg.in[ cond,'gg.trt.sample.lab' ] <- str_c(lab.total.treatment.sample , ' = ' , d.reg.out[ gbm.cond.bv.lo.ndf.Btau.female , vn.total.trt.sample.size ][1] )  
  d.reg.in[ cond,'gg.ani.sample.lab'] <- str_c(lab.total.animal.sample  , ' = ' , d.reg.out[ gbm.cond.bv.lo.ndf.Btau.female , vn.total.animal.sample.size ][1] )  
  
  #d.reg.in[  which(gbm.cond.bv.lo.ndf.Btau.female)[1]     , 'label.row'] <- TRUE
  
  
}



}

# High NDF
cond <- reg.in.cond.bv.hi.ndf.Bind.male



d.reg.in[ cond  ,'gg.exp.sample.lab' ] <- str_c(lab.total.experiment.sample , ' = ' , d.reg.out[ gbm.cond.bv.hi.ndf.Bind.male , vn.total.exp.sample.size ][1] )  
d.reg.in[ cond ,'gg.trt.sample.lab' ] <- str_c( lab.total.treatment.sample , ' = ' , d.reg.out[ gbm.cond.bv.hi.ndf.Bind.male , vn.total.trt.sample.size ][1] )  
d.reg.in[ cond ,'gg.ani.sample.lab'] <- str_c(lab.total.animal.sample  , ' = ' , d.reg.out[ gbm.cond.bv.hi.ndf.Bind.male , vn.total.animal.sample.size][1] )  


#d.reg.in[  which(gbm.cond.bv.hi.ndf.Bind.male)[1]     , 'label.row'] <- TRUE


if ( n.sexes == 2){
  
  cond <- reg.in.cond.bv.hi.ndf.Bind.female
  
  d.reg.in[ cond ,'gg.exp.sample.lab' ] <- str_c(lab.total.experiment.sample , ' = ' , d.reg.out[ gbm.cond.bv.hi.ndf.Bind.female , vn.total.exp.sample.size ][1] )  
  d.reg.in[ cond ,'gg.trt.sample.lab' ] <- str_c(lab.total.treatment.sample  , ' = ' , d.reg.out[ gbm.cond.bv.hi.ndf.Bind.female , vn.total.trt.sample.size ][1] )  
  d.reg.in[ cond ,'gg.ani.sample.lab'] <- str_c(lab.total.animal.sample  , ' = ' , d.reg.out[ gbm.cond.bv.hi.ndf.Bind.female , vn.total.animal.sample.size ][1] )  
  
  
  # d.reg.in[  which(gbm.cond.bv.hi.ndf.Bind.female)[1]     , 'label.row'] <- TRUE
  
  
}

if ( n.breeds == 2  ){
  
  cond <- reg.in.cond.bv.hi.ndf.Btau.male
  
  
  d.reg.in[ cond ,'gg.exp.sample.lab' ] <- str_c(lab.total.experiment.sample , ' = ' , d.reg.out[ gbm.cond.bv.hi.ndf.Btau.male , vn.total.exp.sample.size ][1] )  
  d.reg.in[ cond ,'gg.trt.sample.lab' ] <- str_c(lab.total.treatment.sample , ' = ' , d.reg.out[ gbm.cond.bv.hi.ndf.Btau.male , vn.total.trt.sample.size ][1] )  
  d.reg.in[ cond ,'gg.ani.sample.lab'] <- str_c(lab.total.animal.sample , ' = ' , d.reg.out[ gbm.cond.bv.hi.ndf.Btau.male , vn.total.animal.sample.size][1] )  
  
  
  #  d.reg.in[  which(gbm.cond.bv.hi.ndf.Btau.male)[1]     , 'label.row'] <- TRUE
  
  
  if ( n.sexes == 2){
    
    cond <- reg.in.cond.bv.hi.ndf.Btau.female
    
    d.reg.in[    cond,'gg.exp.sample.lab' ] <- str_c(lab.total.experiment.sample , ' = ' , d.reg.out[ gbm.cond.bv.hi.ndf.Btau.female , vn.total.exp.sample.size ][1] )  
    d.reg.in[    cond,'gg.trt.sample.lab' ] <- str_c(lab.total.treatment.sample  , ' = ' , d.reg.out[ gbm.cond.bv.hi.ndf.Btau.female , vn.total.trt.sample.size ][1] )  
    d.reg.in[    cond,'gg.ani.sample.lab'] <- str_c(lab.total.animal.sample , ' = ' , d.reg.out[ gbm.cond.bv.hi.ndf.Btau.female , vn.total.animal.sample.size ][1] )  
    
    
    # d.reg.in[  which(gbm.cond.bv.hi.ndf.Btau.female)[1]     , 'label.row'] <- TRUE
    
  }
  
  
}

}


}

# DEFINE ROWS IN DATAFRAME USED FOR PLOTTING TEXT ON BOXPLOTS
{
  
  
d.reg.in$label.row <- FALSE
  
if (  n.breeds == 1  ){
  
if (  n.sexes == 1   ){
  

  d.reg.in[ which(  reg.in.cond.bv.lo.ndf.Bind.male )[1]     , 'label.row'] <- TRUE
  d.reg.in[ which(  reg.in.cond.bv.hi.ndf.Bind.male )[1]     , 'label.row'] <- TRUE
  
}  else {
  
  d.reg.in[ which(reg.in.cond.bv.lo.ndf.Bind.male)[1]     , 'label.row'] <- TRUE
  d.reg.in[ which(reg.in.cond.bv.lo.ndf.Bind.female)[1]     , 'label.row'] <- TRUE
  

  d.reg.in[ which(  reg.in.cond.bv.hi.ndf.Bind.male )[1]     , 'label.row'] <- TRUE
  d.reg.in[ which(reg.in.cond.bv.hi.ndf.Bind.female)[1]     , 'label.row'] <- TRUE
  
  
}
  
} else {
  
  
  if ( n.sexes == 1 ){
    
    d.reg.in[ which(reg.in.cond.bv.lo.ndf.Bind.male)[1]     , 'label.row'] <- TRUE
    d.reg.in[ which(reg.in.cond.bv.lo.ndf.Btau.male)[1]     , 'label.row'] <- TRUE
    
    d.reg.in[ which(reg.in.cond.bv.hi.ndf.Bind.male)[1]     , 'label.row'] <- TRUE
    d.reg.in[ which(reg.in.cond.bv.hi.ndf.Btau.male)[1]     , 'label.row'] <- TRUE
    
    
  } else {
    
    d.reg.in[ which(reg.in.cond.bv.lo.ndf.Bind.male)[1]     , 'label.row'] <- TRUE
    d.reg.in[ which(reg.in.cond.bv.lo.ndf.Bind.female)[1]     , 'label.row'] <- TRUE
    
    d.reg.in[ which(reg.in.cond.bv.hi.ndf.Bind.male)[1]     , 'label.row'] <- TRUE
    d.reg.in[ which(reg.in.cond.bv.hi.ndf.Bind.female)[1]     , 'label.row'] <- TRUE
    
    d.reg.in[ which(reg.in.cond.bv.lo.ndf.Btau.male)[1]     , 'label.row'] <- TRUE
    d.reg.in[ which(reg.in.cond.bv.lo.ndf.Btau.female)[1]     , 'label.row'] <- TRUE
    
    d.reg.in[ which(reg.in.cond.bv.hi.ndf.Btau.male)[1]     , 'label.row'] <- TRUE
    d.reg.in[ which(reg.in.cond.bv.hi.ndf.Btau.female)[1]     , 'label.row'] <- TRUE
    
    
  }
  
}
  

d.reg.in[  
  
  (
  d.reg.in$label.row 
  & !is.na(d.reg.in$label.row ) 
  )
  , 
  c(  'sex.label'
    
      
      , 'breed.label'
      , 'NDF.lev.label'
  )  ] 
  




}


gg.bp.0 <- ggplot( d.reg.in[ 
  
  ( 
    
    cnd.sp.lo.ndf.male   | cnd.sp.lo.ndf.female
    |  cnd.sp.hi.ndf.male | cnd.sp.hi.ndf.female  
 
    | cnd.gt.lo.ndf.male   | cnd.gt.lo.ndf.female
    |  cnd.gt.hi.ndf.male | cnd.gt.hi.ndf.female  
    
    | cnd.bv.lo.ndf.Bind.male   | cnd.bv.lo.ndf.Bind.female
    | cnd.bv.lo.ndf.Btau.male   | cnd.bv.lo.ndf.Btau.female
    
    | cnd.bv.hi.ndf.Bind.male   | cnd.bv.hi.ndf.Bind.female
    | cnd.bv.hi.ndf.Btau.male   | cnd.bv.hi.ndf.Btau.female
    
  ) 
  , ]
  ) +
    theme(
      axis.title.x = element_blank()
      , axis.text.x = element_text( size = 8.25 ,vjust = 0.5 , hjust = 1)
      , axis.ticks.x = element_blank()
      
      # Y text
      ,   axis.title.y = element_text( size = 9.0)
      , axis.text.y = element_text( size = 6.5)
      ,  panel.grid.major = element_blank()
      ,  panel.background = element_blank(),
      ,  panel.border = element_rect(colour = "black", fill=NA, linewidth = 1  ),
      ,  strip.background = element_rect(color='black', fill='white', linewidth = 1, linetype="solid")
      , strip.text.x = element_text(size = 12 , color = 'black' )
    )
  
gg.bp.mn.ps <-  24
gg.bp.mn.color <- 'red'
gg.bp.mn.fill <- 'orange'
gg.bp.mn.size <- 1.15
gg.bp.lw <- 0.35
gg.bp.circ.size <- .5

gg.bp.width <- 1.5


gg.bp.generic <- function(   plot.raw ,  data  ,  variable  ,  y.lab ){
  
  test <- function(){
    
    plot.raw <- gg.bp.0  
    data <- d.reg.in  
    variable <- 'feed_intake_kg_d'
    y.lab <- 'blah'
    
    
  } 
  
  

 
  data$plot.var.boxplot <- data[,variable]
  
  
  gg.lab.exp <- str_c( 'gg.exp.sample.lab.' , variable)
  gg.lab.trt <- str_c( 'gg.trt.sample.lab.' , variable)
  gg.lab.anim <- str_c( 'gg.anim.sample.lab.' , variable)
  
  
  data$gg.exp.sample.lab <- data[,gg.lab.exp ]
  data$gg.trt.sample.lab <- data[, gg.lab.trt ]
  data$gg.ani.sample.lab <- data[,gg.lab.anim ]

    
  gg.ss.lab.fs <- 1.6
  
  
  max.y <- as.numeric(quantile(  na.omit(data$plot.var.boxplot) , probs = c(.95)))
  min.y <- - 0.21 * abs(max.y)  #  as.numeric(quantile(  data$plot.var.boxplot , probs = c(.05)))
  
  y.range <- (max.y - 0)
  
  y.inc <- 0.1
  
  y.crd.1 <- -(y.inc *  y.range)
  y.crd.2 <- -( y.inc * 2 *  y.range)
  y.crd.3 <- -( y.inc * 3 *  y.range)
  
  #  - abs(min.y) * 0.45
  #y.crd.3 <- - abs(min.y) * 0.45 * 2.0
  
  data$NDF.lev.label <- factor( data$NDF.lev.label , levels = c( "Low NDF" ,   "High NDF") )
  data$sex.label <- factor(    data$sex.label , levels = c('M',  'F') )
  data$breed.label <- factor(     data$breed.label  , levels = c(  "Bos indicus" ,  "Exotic") )
  
  gg.ss.lab.fs <<- 2.88 ; gg.ss.lab.font.face <- "bold.italic"
  
  
  plot.new <- plot.raw %>%   +
    
   
 
    geom_boxplot( data , mapping = aes(x = sex.label , y = plot.var.boxplot ) , size = gg.bp.lw ,  outlier.size = gg.bp.circ.size   )   +
   
    geom_point( data , mapping = aes(x = sex.label, y = plot.var.boxplot ) , stat = 'summary' , shape = gg.bp.mn.ps , color = gg.bp.mn.color , fill =  gg.bp.mn.fill , size = gg.bp.mn.size  )   +
    
    ylab( y.tit.dmi ) +
    ylab( y.lab  ) + 
    ylim( limits = c( y.crd.3 * 1.15,  max.y )    ) +
    
    facet_nested(  
      .  ~  breed.label 
      #  + NDF.lev.label 
      , scales = "free_x" , space = "free_x") +
    
    theme(
      
      plot.margin = margin(t = 0.1  , r = 0.1 , b = 0.1 , l = 0.1 , unit = "cm")
      
      , strip.text.x = element_text( 
        
       ,  size = 6.8
        
        , margin = margin(  t = 0.075  , r = 0.075 , b = 0.075 , l = 0.075, unit = "cm") 
        
        )
      
    
      
      , axis.title.y = element_text(  size = 7.0)
      
      , axis.text.x = element_text( size = 6.3 , angle = 0 , vjust = 0.5 , hjust = 0.5)
   
      
      
      
       )    +
    
    
    #  if (  FALSE  ) {
      
    geom_text(   data [  
      data$label.row 
      & !is.na(data$label.row ) 
      , 
      c(  'sex.label'
          , 'gg.exp.sample.lab'
          
          , 'breed.label'
          , 'NDF.lev.label'
      )  ]  , 
      , mapping = aes(x = sex.label , y =  y.crd.1 , label = gg.exp.sample.lab )  
      , size = gg.ss.lab.fs 
      , fontface = gg.ss.lab.font.face 
      # , color = NA
    ) +
    
    
    geom_text(   data[  
      data$label.row 
      & !is.na(data$label.row ) 
      , 
      c(  'sex.label'
          , 'gg.trt.sample.lab'
          
          , 'breed.label'
          , 'NDF.lev.label'
      )  ]  , 
      , mapping = aes(x = sex.label , y =  y.crd.2 , label = gg.trt.sample.lab)  
      , size = gg.ss.lab.fs 
      , fontface = gg.ss.lab.font.face 
      # , color = NA
    ) +
    
    geom_text(   data[  
      data$label.row 
      & !is.na(data$label.row ) 
      , 
      c(  'sex.label'
          , 'gg.ani.sample.lab'
          
          , 'breed.label'
          , 'NDF.lev.label'
      )  ]  , 
      , mapping = aes(x = sex.label , y = y.crd.3 , label = gg.ani.sample.lab)  
      , size = gg.ss.lab.fs 
      , fontface = gg.ss.lab.font.face 
      # , color = NA
    ) 
      
  # }
  
  return (  plot.new  )
  
}




} # Data prep


{
  
  d.reg.in$adg_kg_day
  

gg.bp.dmi.0 <- gg.bp.generic(   gg.bp.0 ,  d.reg.in , 'feed_intake_kg_d' , y.tit.dmi )

gg.bp.bw.0 <- gg.bp.generic(   gg.bp.0 ,  d.reg.in , 'bw_kg' , y.tit.bw )


gg.bp.adg.0 <- gg.bp.generic(   gg.bp.0 ,  d.reg.in , 'adg_kg_day' , y.tit.lwg )


gg.bp.my.0 <- gg.bp.generic(   gg.bp.0 ,  d.reg.in , 'milk_prod_kg_d' , y.tit.my)



gg.bp.ndf.0 <- gg.bp.generic(   gg.bp.0 ,  d.reg.in , 'NDF_nutrition' , y.tit.ndf )


gg.bp.adf.0 <- gg.bp.generic(   gg.bp.0 ,  d.reg.in , 'ADF_nutrition' , y.tit.adf )


gg.bp.cp.0 <- gg.bp.generic(   gg.bp.0 ,  d.reg.in , 'CP_nutrition' , y.tit.cp )


gg.bp.ee.0 <- gg.bp.generic(   gg.bp.0 ,  d.reg.in , 'EE_nutrition' , y.tit.ee )

gg.bp.ash.0 <- gg.bp.generic(   gg.bp.0 ,  d.reg.in , 'Ash_nutrition' , y.tit.ash )

gg.bp.ndf.d.0 <- gg.bp.generic(   gg.bp.0 ,  d.reg.in , 'NDF_digest' , y.tit.NDFd)


gg.bp.adl.0 <- gg.bp.generic(   gg.bp.0 ,  d.reg.in , 'ADL_nutrition' , y.tit.adl )


gg.bp.dm.d.0 <- gg.bp.generic(   gg.bp.0 ,  d.reg.in , 'DM_digest' , y.tit.DMd  )

gg.bp.om.d.0 <- gg.bp.generic(   gg.bp.0 ,  d.reg.in , 'OM_digest' , y.tit.OMd  )


} # Gen plots


# Plot prep
{
  
  gg.bp.cp  <- gg.bp.cp.0 %>%   + theme(  axis.text.x = element_blank())
  gg.bp.ndf <- gg.bp.ndf.0  %>%   + theme(  axis.text.x = element_blank())
  gg.bp.adf <- gg.bp.adf.0  %>%   + theme(  axis.text.x = element_blank())
  
   gg.bp.ash <- gg.bp.ash.0   %>%   + theme(  axis.text.x = element_blank())
   gg.bp.ee <- gg.bp.ee.0   %>%   + theme(  axis.text.x = element_blank())
   gg.bp.adl <- gg.bp.adl.0   %>%   + theme(  axis.text.x = element_blank())
   
   gg.bp.ndf.d <- gg.bp.ndf.d.0   
   gg.bp.dm.d <- gg.bp.dm.d.0  
   gg.bp.om.d <- gg.bp.om.d.0  
   
  
  gg.bp.adg <- gg.bp.adg.0 
  gg.bp.my <- gg.bp.my.0   
  gg.bp.dmi <- gg.bp.dmi.0  %>%   + theme(  axis.text.x = element_blank())   
  gg.bp.bw  <- gg.bp.bw.0  %>%   + theme(  axis.text.x = element_blank()) 
  
}


gg.bp.nutr.0 <- ggarrange(
  

   gg.bp.ndf
  , gg.bp.adf
  , gg.bp.adl

  
  , gg.bp.cp
  , gg.bp.ee
  , gg.bp.ash
 
  
  , gg.bp.ndf.d
  , gg.bp.dm.d 
  , gg.bp.om.d

  
  , nrow = 3
  , ncol = 3
  
  , heights = c(1 , 1 , 1.04)
  , labels = c(
    
     'a'
    ,'b'
    ,'c'
    
    ,'d'
    ,'e'
    ,'f'
    
    ,'g'
    ,'h'
    ,'i'
    
    )
 , font.label = list(size = 7.25, color = "black", face = "bold")
)



gg.bp.anim.0 <- ggarrange(
  
  gg.bp.dmi
  , gg.bp.bw
  , gg.bp.my

  , gg.bp.adg
  
  , nrow = 2
  , ncol = 2
  
  , widths = c( 1, 1 )
  , heights = c( 1, 1 )
  , labels = c(
    
    'a'
    ,'b'
    ,'c'
    
    ,'d'
    
    
  )
  , font.label = list(size = 7.25, color = "black", face = "bold")
  
  
)



gg.bp.dpi <- 1500
gg.bp.glob.scalar.nutr <- 0.425
gg.bp.width.nutr <- 13.8 * gg.bp.glob.scalar.nutr
gg.bp.height.nutr  <- 12* gg.bp.glob.scalar.nutr

gg.bp.glob.scalar.anim <- 0.6
gg.bp.width.anim <- 6.8 * gg.bp.glob.scalar.anim
gg.bp.height.anim  <- 6.4* gg.bp.glob.scalar.anim


if ( LR.only ) {
  
  filename.bp.nutr = str_c(results.out.dir.bv, 'boxplots.bovine.nutrition.jpeg')
  filename.bp.anim = str_c(results.out.dir.bv, 'boxplots.bovine.animal.jpeg')
  
  }

if ( !LR.only ) { filename.bp = str_c(results.out.dir , 'boxplots.caprinae.jpeg') }



ggsave(filename =    filename.bp.nutr ,  gg.bp.nutr.0 , width = gg.bp.width.nutr  , height = gg.bp.height.nutr    , dpi = gg.bp.dpi)
ggsave(filename =    filename.bp.anim ,  gg.bp.anim.0 , width = gg.bp.width.anim  , height = gg.bp.height.anim    , dpi = gg.bp.dpi)



