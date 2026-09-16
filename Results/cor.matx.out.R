

{
cor.vars <- c(pred.vars )

cor.vars <- cor.vars[ -which(cor.vars =="DM_nutrition" )]

cor.vars.labels <- c(
  
  'Bodyweight'
  ,    'Average daily gain'
  
  , 'Milk yield'
  , 'Neutral detergent fibre'
  , 'Acid detergent fibre'
  , 'Crude protein'
  , 'Ash'
  , 'Ether extract'
  , 'NDF digestibility'
  , 'Acid detergent lignin'
  , 'Dry matter digestibility'
  , 'Organic matter digestibility'
  # , 'Dry matter'
)

n.row <- length( unique(  cor.vars   ) )



cor.mtx <- data.frame( matrix(  nrow = n.row , ncol = n.row   )    )


colnames(cor.mtx) <- cor.vars
row.names(cor.mtx) <- cor.vars


conds.all  <- list.sample.conds( FALSE )

cor.mtx.s1 <- cor.mtx.s2 <- cor.mtx.s3 <- cor.mtx.s4 <- cor.mtx.s5 <- cor.mtx.s6 <- cor.mtx.s7 <- cor.mtx.s8 <- cor.mtx


for (  v1 in cor.vars  ){
  
# v1 <- cor.vars[1] ; v2 <- cor.vars[3]  

for (v2 in cor.vars[ -c(  which(cor.vars == v1)  )  ]   ){


sample.cond.1 <- de.listify(conds.all[[1]])  
sample.cond.2 <- de.listify(conds.all[[2]])  
sample.cond.3 <- de.listify(conds.all[[3]])  
sample.cond.4 <- de.listify(conds.all[[4]])  
sample.cond.5 <- de.listify(conds.all[[5]])  
sample.cond.6 <- de.listify(conds.all[[6]])  
sample.cond.7 <- de.listify(conds.all[[7]])  
sample.cond.8 <- de.listify(conds.all[[8]])  
  

complete.cond.1 <-  ( sample.cond.1  & !is.na(  d.reg.in[      ,v1]  ) & !is.na(d.reg.in[   ,v2]) ) 
complete.cond.2 <-  ( sample.cond.2  & !is.na(  d.reg.in[      ,v1]  ) & !is.na(d.reg.in[   ,v2]) ) 
complete.cond.3 <-  ( sample.cond.3  & !is.na(  d.reg.in[      ,v1]  ) & !is.na(d.reg.in[   ,v2]) ) 
complete.cond.4 <-  ( sample.cond.4  & !is.na(  d.reg.in[      ,v1]  ) & !is.na(d.reg.in[   ,v2]) ) 
complete.cond.5 <-  ( sample.cond.5  & !is.na(  d.reg.in[      ,v1]  ) & !is.na(d.reg.in[   ,v2]) ) 
complete.cond.6 <-  ( sample.cond.6  & !is.na(  d.reg.in[      ,v1]  ) & !is.na(d.reg.in[   ,v2]) ) 
complete.cond.7 <-  ( sample.cond.7  & !is.na(  d.reg.in[      ,v1]  ) & !is.na(d.reg.in[   ,v2]) ) 
complete.cond.8 <-  ( sample.cond.8  & !is.na(  d.reg.in[      ,v1]  ) & !is.na(d.reg.in[   ,v2]) ) 


samp.1.sers.1 <- d.reg.in[  complete.cond.1 ,v1  ] ; samp.1.sers.2 <- d.reg.in[  complete.cond.1 ,v2  ]
samp.2.sers.1 <- d.reg.in[  complete.cond.2 ,v1  ] ; samp.2.sers.2 <- d.reg.in[  complete.cond.2  ,v2  ]
samp.3.sers.1 <- d.reg.in[  complete.cond.3 ,v1  ] ; samp.3.sers.2 <- d.reg.in[  complete.cond.3  ,v2  ]
samp.4.sers.1 <- d.reg.in[  complete.cond.4, v1  ] ; samp.4.sers.2 <- d.reg.in[  complete.cond.4  ,v2  ]
samp.5.sers.1 <- d.reg.in[  complete.cond.5 ,v1  ] ; samp.5.sers.2 <- d.reg.in[  complete.cond.5  ,v2  ]
samp.6.sers.1 <- d.reg.in[  complete.cond.6 ,v1  ] ; samp.6.sers.2 <- d.reg.in[  complete.cond.6  ,v2  ]
samp.7.sers.1 <- d.reg.in[  complete.cond.7 ,v1  ] ; samp.7.sers.2 <- d.reg.in[  complete.cond.7  ,v2  ]
samp.8.sers.1 <- d.reg.in[  complete.cond.8 ,v1  ] ; samp.8.sers.2 <- d.reg.in[  complete.cond.8  ,v2  ]



corr.s1 <- round ( cor(   samp.1.sers.1 , samp.1.sers.2  ) , rd.decs.corr  ) 
corr.s2 <- round ( cor(   samp.2.sers.1 , samp.2.sers.2  ) , rd.decs.corr  )
corr.s3 <- round ( cor(   samp.3.sers.1 , samp.3.sers.2  ) , rd.decs.corr  )
corr.s4 <- round ( cor(   samp.4.sers.1 , samp.4.sers.2  ) , rd.decs.corr  )
corr.s5 <- round ( cor(   samp.5.sers.1 , samp.5.sers.2  ) , rd.decs.corr  )
corr.s6 <- round ( cor(   samp.6.sers.1 , samp.6.sers.2  ) , rd.decs.corr  )
corr.s7 <- round ( cor(   samp.7.sers.1 , samp.7.sers.2  ) , rd.decs.corr  )
corr.s8 <- round ( cor(   samp.8.sers.1 , samp.8.sers.2  ) , rd.decs.corr  )



cor.mtx.s1[  v1   ,  c(v2)   ] <-    corr.s1 
cor.mtx.s2[  v1   ,  c(v2)   ] <-    corr.s2
cor.mtx.s3[  v1   ,  c(v2)   ] <-    corr.s3
cor.mtx.s4[  v1   ,  c(v2)   ] <-    corr.s4
cor.mtx.s5[  v1   ,  c(v2)   ] <-    corr.s5
cor.mtx.s6[  v1   ,  c(v2)   ] <-    corr.s6
cor.mtx.s7[  v1   ,  c(v2)   ] <-    corr.s7
cor.mtx.s8[  v1   ,  c(v2)   ] <-    corr.s8

}
}

} # Pre-processing

{


masked_cm.1 <- get_lower_tri(cor.mtx.s1 ) 
masked_cm.2 <- get_lower_tri(cor.mtx.s2 )  
masked_cm.3 <- get_lower_tri(cor.mtx.s3 )   
masked_cm.4 <- get_lower_tri(cor.mtx.s4 )  
masked_cm.5 <- get_lower_tri(cor.mtx.s5 ) 
masked_cm.6 <- get_lower_tri(cor.mtx.s6 ) 
masked_cm.7 <- get_lower_tri(cor.mtx.s7 ) 
masked_cm.8 <- get_lower_tri(cor.mtx.s8 ) 


narm <- FALSE
melted_cm.1 <- melt( as.matrix(masked_cm.1) , na.rm = narm)
melted_cm.2 <- melt( as.matrix(masked_cm.2) , na.rm = narm)
melted_cm.3 <- melt( as.matrix(masked_cm.3) , na.rm = narm)
melted_cm.4 <- melt( as.matrix(masked_cm.4) , na.rm = narm)
melted_cm.5 <- melt( as.matrix(masked_cm.5) , na.rm = narm)
melted_cm.6 <- melt( as.matrix(masked_cm.6) , na.rm = narm)
melted_cm.7 <- melt( as.matrix(masked_cm.7) , na.rm = narm)
melted_cm.8 <- melt( as.matrix(masked_cm.8) , na.rm = narm)


# NDF labels
melted_cm.1$ndf.lab <- ndf.lev.lo.lab
melted_cm.2$ndf.lab <- ndf.lev.hi.lab
melted_cm.3$ndf.lab <- ndf.lev.lo.lab
melted_cm.4$ndf.lab <- ndf.lev.hi.lab
melted_cm.5$ndf.lab <- ndf.lev.lo.lab
melted_cm.6$ndf.lab <- ndf.lev.hi.lab
melted_cm.7$ndf.lab <- ndf.lev.lo.lab
melted_cm.8$ndf.lab <- ndf.lev.hi.lab

# Sex labels
melted_cm.1$sex.lab <- sex.labels[1]
melted_cm.2$sex.lab <- sex.labels[1]
melted_cm.3$sex.lab <- sex.labels[2]
melted_cm.4$sex.lab <- sex.labels[2]
melted_cm.5$sex.lab <- sex.labels[1]
melted_cm.6$sex.lab <- sex.labels[1]
melted_cm.7$sex.lab <- sex.labels[2]
melted_cm.8$sex.lab <- sex.labels[2]

# Breed labels
melted_cm.1$brd.lab <- breed.labels[1]
melted_cm.2$brd.lab <- breed.labels[1]
melted_cm.3$brd.lab <- breed.labels[1]
melted_cm.4$brd.lab <- breed.labels[1]
melted_cm.5$brd.lab <- breed.labels[2]
melted_cm.6$brd.lab <- breed.labels[2]
melted_cm.7$brd.lab <- breed.labels[2]
melted_cm.8$brd.lab <- breed.labels[2]


} # gg data prep





gg.txt.fs <- 2.85

gg.corrs.theme <- ggplot( data.frame() )  +
  theme(
    
      panel.grid.major = element_line(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
    
    
      ,  axis.text.x = element_blank()
      ,  axis.text.y = element_blank()
    
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    
    
    ,  strip.background = element_rect(color='black', fill='white', linewidth = 1, linetype="solid")
    , strip.text.x = element_text(size = 11.75 , color = 'black' )
    
    
  ) +
  #  coord_fixed()  + "#4477AA" "#EE6677"
  scale_fill_gradient2(low = "#0FFF50" , mid = "white", high = "#BF40BF", 
                       limit = c(-1, 1)
                       , name = "Pearson\nCorrrelation"
                       , na.value = "transparent"
                       
                       )  



{
  
dat <- melted_cm.1 ; gg.tile.colr <- 'grey' ; gg.tile.lw  <- 0.15 ; gg.axis.txt.fs <- 9.5 

gg.corr.1.0 <- gg.corrs.theme  %>% +
  geom_tile( data = dat
             , mapping = aes(x = Var1  , y = Var2, fill = value)  ,
             color = gg.tile.colr
             , linewidth = gg.tile.lw 
             ) +
  geom_text(
    data =  dat  , na.rm = TRUE , 
    aes(x = Var1  , y = Var2,  label = round(value, 2)), color = "black", size = gg.txt.fs) +
  facet_nested_wrap(   ~ brd.lab  + ndf.lab + sex.lab  , nrow = 2 
                       ) 

dat  <- melted_cm.2

gg.corr.2.0 <- gg.corrs.theme  %>% +
  geom_tile( data =  dat 
             , mapping = aes(x = Var1  , y = Var2, fill = value)  ,
             color = gg.tile.colr
             , linewidth = gg.tile.lw 
  ) +
  geom_text(
    data = dat  ,  na.rm = TRUE , 
    aes(x = Var1  , y = Var2,  label = round(value, 2)), color = "black", size = gg.txt.fs) +
  facet_nested_wrap(   ~ brd.lab  + ndf.lab + sex.lab  , nrow = 2 ) 

dat <- melted_cm.3

gg.corr.3.0 <- gg.corrs.theme  %>% +
  geom_tile( data =  dat
             , mapping = aes(x = Var1  , y = Var2, fill = value)  ,
             color = gg.tile.colr
             , linewidth = gg.tile.lw 
  ) +
  geom_text(
    data =  dat , na.rm = TRUE , 
    aes(x = Var1  , y = Var2,  label = round(value, 2)), color = "black", size = gg.txt.fs) +
  facet_nested_wrap(   ~ brd.lab  + ndf.lab + sex.lab  , nrow = 2 )  


dat <- melted_cm.4

gg.corr.4.0 <- gg.corrs.theme  %>% +
  geom_tile( data =  dat
             , mapping = aes(x = Var1  , y = Var2, fill = value)  ,
             color = gg.tile.colr
             , linewidth = gg.tile.lw 
  ) +
  geom_text(
    data =  dat , na.rm = TRUE , 
    aes(x = Var1  , y = Var2,  label = round(value, 2)), color = "black", size = gg.txt.fs) +
  facet_nested_wrap(   ~ brd.lab  + ndf.lab + sex.lab  , nrow = 2 )  

#  Row 2
dat <- melted_cm.5

gg.corr.5.0 <- gg.corrs.theme  %>% +
  geom_tile( data =  dat
             , mapping = aes(x = Var1  , y = Var2, fill = value)  ,
             color = gg.tile.colr
             , linewidth = gg.tile.lw 
  ) +
  geom_text(
    data =  dat , na.rm = TRUE , 
    aes(x = Var1  , y = Var2,  label = round(value, 2)), color = "black", size = gg.txt.fs) +
  facet_nested_wrap(   ~ brd.lab  + ndf.lab + sex.lab  , nrow = 2 )  


dat <- melted_cm.6

gg.corr.6.0 <- gg.corrs.theme  %>% +
  geom_tile( data =  dat
             , mapping = aes(x = Var1  , y = Var2, fill = value)  ,
             color = gg.tile.colr
             , linewidth = gg.tile.lw 
  ) +
  geom_text(
    data =  dat , na.rm = TRUE , 
    aes(x = Var1  , y = Var2,  label = round(value, 2)), color = "black", size = gg.txt.fs) +
  facet_nested_wrap(   ~ brd.lab  + ndf.lab + sex.lab  , nrow = 2 )  


dat <- melted_cm.7

gg.corr.7.0 <- gg.corrs.theme  %>% +
  geom_tile( data =  dat
             , mapping = aes(x = Var1  , y = Var2, fill = value)  ,
             color = gg.tile.colr
             , linewidth = gg.tile.lw 
  ) +
  geom_text(
    data =  dat , na.rm = TRUE , 
    aes(x = Var1  , y = Var2,  label = round(value, 2)), color = "black", size = gg.txt.fs) +
  facet_nested_wrap(   ~ brd.lab  + ndf.lab + sex.lab  , nrow = 2 )  


dat <- melted_cm.8

gg.corr.8.0 <- gg.corrs.theme  %>% +
  geom_tile( data =  dat
             , mapping = aes(x = Var1  , y = Var2, fill = value)  ,
             color = gg.tile.colr
             , linewidth = gg.tile.lw 
  ) +
  geom_text(
    data =  dat , na.rm = TRUE , 
    aes(x = Var1  , y = Var2,  label = round(value, 2)), color = "black", size = gg.txt.fs) +
  facet_nested_wrap(   ~ brd.lab  + ndf.lab + sex.lab  , nrow = 2 )  


}  #  all plots

{
gg.corr.5.0 <- gg.corr.5.0
gg.corr.6.0 <- gg.corr.6.0
gg.corr.7.0 <- gg.corr.7.0
gg.corr.8.0 <- gg.corr.8.0

gg.corr.1 <- gg.corr.1.0 %>% + theme(   axis.text.y = element_text( size = gg.axis.txt.fs  , hjust = 1 )) + scale_x_discrete(labels = cor.vars.labels) + scale_y_discrete(labels = cor.vars.labels)

gg.corr.2 <- gg.corr.2.0
gg.corr.3 <- gg.corr.3.0
gg.corr.4 <- gg.corr.4.0

# Row 2
gg.corr.5 <- gg.corr.5.0 %>% + theme(  axis.text.y = element_text(  size = gg.axis.txt.fs  , hjust = 1 ) ,  axis.text.x = element_text( angle = 90 , hjust = 1 , vjust = 0.5  )) + scale_x_discrete(labels = cor.vars.labels) + scale_y_discrete(labels = cor.vars.labels)

gg.corr.6 <- gg.corr.6.0 %>% + theme(   axis.text.x = element_text( size = gg.axis.txt.fs  , angle = 90 , hjust = 1 , vjust = 0.5 )) + scale_x_discrete(labels = cor.vars.labels) + scale_y_discrete(labels = cor.vars.labels)
gg.corr.7 <- gg.corr.7.0 %>% + theme(   axis.text.x = element_text( size = gg.axis.txt.fs  , angle = 90 , hjust = 1 , vjust = 0.5 )) + scale_x_discrete(labels = cor.vars.labels) + scale_y_discrete(labels = cor.vars.labels)
gg.corr.8 <- gg.corr.8.0 %>% + theme(   axis.text.x = element_text( size = gg.axis.txt.fs  , angle = 90 , hjust = 1 , vjust = 0.5 )) + scale_x_discrete(labels = cor.vars.labels) + scale_y_discrete(labels = cor.vars.labels)


gg.corr.2 <- gg.corr.2 %>% + theme(  legend.position = "none" )
gg.corr.3 <- gg.corr.3 %>% + theme(  legend.position = "none" )
gg.corr.4 <- gg.corr.4 %>% + theme(  legend.position = "none" )
gg.corr.5 <- gg.corr.5 %>% + theme(  legend.position = "none" )
gg.corr.6 <- gg.corr.6 %>% + theme(  legend.position = "none" )
gg.corr.7 <- gg.corr.7 %>% + theme(  legend.position = "none" )
gg.corr.8 <- gg.corr.8 %>% + theme(  legend.position = "none" )


} # Plot reformatting

shared_legend <- get_legend(gg.corr.1)

gg.corrs.all <- ggarrange(
  
  gg.corr.1
  , gg.corr.2
  , gg.corr.3
  , gg.corr.4
  
  , gg.corr.5
  , gg.corr.6
  , gg.corr.7
  , gg.corr.8
  
  , nrow = 2
  , ncol  = 4
  
  , widths = c( 1.36 , 1 , 1 ,1)
  , heights = c( 1.0 , 1.36)
  
  , legend.grob =   shared_legend 
  , legend  = 'right'


)

gg.corrs.scalar <- 1.22
gg.corrs.width <- 12 * gg.corrs.scalar
gg.corrs.height <- 7 * gg.corrs.scalar


ggsave( filename = str_c(results.out.dir.bv , 'gg.corrs.all.jpg')  , gg.corrs.all, width =gg.corrs.width, height = gg.corrs.height , dpi =1500)




# Export csv files of correlation matrices to results file
write.csv( masked_cm.1 , str_c(results.out.dir.bv , 'cor.mtx.lo.NDF.Bind.male.csv') , row.names = TRUE  )
write.csv( masked_cm.2 , str_c(results.out.dir.bv , 'cor.mtx.hi.NDF.Bind.male.csv') , row.names = TRUE  )
write.csv( masked_cm.3 , str_c(results.out.dir.bv , 'cor.mtx.lo.NDF.Bind.female.csv') , row.names = TRUE  )
write.csv( masked_cm.4 , str_c(results.out.dir.bv , 'cor.mtx.hi.NDF.Bind.female.csv') , row.names = TRUE  )

write.csv( masked_cm.5 , str_c(results.out.dir.bv , 'cor.mtx.lo.NDF.Btau.male.csv') , row.names = TRUE  )
write.csv( masked_cm.6 , str_c(results.out.dir.bv , 'cor.mtx.hi.NDF.Btau.male.csv') , row.names = TRUE  )
write.csv( masked_cm.7 , str_c(results.out.dir.bv , 'cor.mtx.lo.NDF.Btau.female.csv') , row.names = TRUE  )
write.csv( masked_cm.8 , str_c(results.out.dir.bv , 'cor.mtx.hi.NDF.Btau.female.csv') , row.names = TRUE  )





