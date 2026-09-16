



all.cols <- colnames(d.resds.SDs)
all.cols <- all.cols[-c(1,2,3,4)]

col.name.ut.id <- 'ut.id' ; col.name.ue.id <- 'ue.id' ; col.name.b.code <- 'B.Code' ; col.name.diet.code <- 'diet.code'

resds.SDs <- c()


for (m in 1:length( unique(d.reg.out$mod.vers)  )   ) {
  
  
  df <- d.resds.SDs[  !is.na( d.resds.SDs[,all.cols[m]]  )  ,  c( col.name.ue.id , col.name.ut.id  , col.name.b.code , col.name.diet.code , all.cols[m]) ]

  df <- df[    order( df[, c(5)]  ,  decreasing = TRUE) ,       ]
  
  resds.SDs[m] <- listify(  df)
  

}





de.listify(  resds.SDs[1]  ) 


de.listify(  resds.SDs[2]  ) 

de.listify(  resds.SDs[3]  ) 
de.listify(  resds.SDs[4]  ) 
de.listify(  resds.SDs[5]  ) 
de.listify(  resds.SDs[6]  ) 
  
