
gg.resids <- gg.y.valid.theme   %>% +  
  geom_point( data = gg.dat ,  aes(x = modelled, y = residual.1) , color = 'darkgrey') +
  # stat_summary(data = gg.dat ,  aes(x = modelled, y = residual.1) , fun = mean, geom = "line", color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 0, color = "black", linetype = "solid") + 
  
  geom_smooth(data = gg.dat , aes(x = modelled , y = residual.1)  , method = 'loess' , se = FALSE , color = 'black' , span = 0.9, size = .75)+
  #geom_text( data = gg.dat ,  aes(x = observed , y = modelled , label = label.ut.id) , color = 'black' , size = 2.5 , nudge_x = 1 , nudge_y =1  )  + 
  facet_grid( . ~ species.ndf ) + 
  ylab(gg.resid.y.tit) + 
  xlab(gg.resid.x.tit) 

gg.resids




gg.rmsr <- gg.y.valid.theme   %>% +  
  geom_point( data = gg.dat ,  aes(x = modelled, y = rms.residual.1) , color = 'darkgrey') +
  # stat_summary(data = gg.dat ,  aes(x = modelled, y = residual.1) , fun = mean, geom = "line", color = "blue", size = 1) +
  # geom_abline(intercept = 0, slope = 0, color = "black", linetype = "solid") + 
  
  geom_smooth(data = gg.dat , aes(x = modelled , y = rms.residual.1)  , method = 'loess' , se = FALSE , color = 'black' , span = 4, size = .75) +
  #geom_text( data = gg.dat ,  aes(x = observed , y = modelled , label = label.ut.id) , color = 'black' , size = 2.5 , nudge_x = 1 , nudge_y =1  )  + 
  facet_grid( . ~ species.ndf ) 

gg.resids


gg.y.bw <- gg.y.valid.theme   %>% +  
  geom_point( data = gg.dat ,  aes(x = bw_kg , y = modelled) , color = 'darkgrey')  + 
  
  geom_abline( data = gg.dat , aes( intercept = coef.intercept , slope = coef.bw_kg ) , color = "black", linetype = "solid")  +
  facet_grid( . ~ species.ndf ) 




