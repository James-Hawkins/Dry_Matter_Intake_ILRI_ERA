#  rm(list = ls())


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#getwd()




source(   'Settings/libraries.R')  ;  source( 'Settings/parameters.R')  ; source( 'Settings/variables.R') ; source( 'Settings/lists.R') ; source(  str_c(  helpers.dir   ,'functions.R') )   


d.raw <<- read_excel( 
  str_c( 
   
    # parent.dir ,
         
    #  'Master_ERA_data_2.0.xlsx'   # - My version
    "Master_ERA_data_2.0_Sisay+Final_Update_Sep2.xlsx"  # - Sisay's version
         
         )
                 , sheet = 'feed_intake_raw' 
  
                 , col_types = "text")



{
  
d <- as.data.frame(   d.raw   )
  
  
names(d)[2] <- 'diet.code'

for(r in 1:nrow(d)){

d[r,'id'] <- (r-1)

}

  
convert.numeric <- c(
'feed_intake_value'
, 'DM_nutrition'
, 'bw_kg'
, 'adg_kg_day'
, 'NDF_nutrition'
,  'ADF_nutrition'
, 'NDF_digest'
, 'EE_nutrition'
,'Ash_nutrition'


# new ones
, 'DM_nutrition'
, 'ADL_nutrition'
, 'DM_digest'	
 , 'OM_digest'

# ,  'ME_nutrition'
#  , 'DM_digest'
, 'CP_nutrition'
, 'milk_kg_day'
, 'Sample.size'

)


	
for (v in convert.numeric){

d[,v] <- as.numeric(d[,v] )

}

  

d$breed.type <- NA

#' Categories for cattle
#' 2 sexes (male, female)
#' 2 breed types (Bos indicus, Crossbred)
#' Groupings:
#' Male bos indicus
#' Female Bos indicus
#' Male Crossbred (beef)
#' Female crossbred (beef)
#' 
#' Alternatively
#' Bos indicus Beef
#' Crossbred beef
#' Dairy



# For cattle, breeds are categorized into purebred Bos indicus (1) and Crossbred Bos indicus x Bos taurus or Purebred Bos taurus (2)
d[!is.na(d$Breed.type.cattle  ) &  d$Species == species.cattle & d$Species == species.cattle & d$Breed.type.cattle == "bos.indicus" , 'breed.type'] <- breed.indig
d[!is.na(d$Breed.type.cattle  ) &  d$Species == species.cattle & d$Species == species.cattle & d$Breed.type.cattle == "cross.bred" , 'breed.type'] <- breed.cbred
d[!is.na(d$Breed.type.cattle  ) &  d$Species == species.cattle & d$Species == species.cattle & d$Breed.type.cattle == "pure.bred.exotic" , 'breed.type'] <- breed.cbred

d[ !is.na(d$Breed.type.sheep  ) &  d$Species == species.sheep & d$Breed.type.sheep == "indigenous" , 'breed.type'] <- breed.indig
d[ !is.na(d$Breed.type.sheep  ) &  d$Species == species.sheep & d$Breed.type.sheep == "cross.bred" , 'breed.type'] <- breed.cbred
d[ !is.na(d$Breed.type.sheep  ) &  d$Species == species.sheep & d$Breed.type.sheep == "pure.bred.exotic" , 'breed.type'] <- breed.cbred

d[!is.na(d$Breed.type.goat  ) &  d$Species == species.goat & d$Species == species.goat & d$Breed.type.goat == "indigenous" , 'breed.type'] <- breed.indig
d[!is.na(d$Breed.type.goat  ) &  d$Species == species.goat & d$Species == species.goat & d$Breed.type.goat == "cross.bred" , 'breed.type'] <- breed.cbred
d[!is.na(d$Breed.type.goat  ) &  d$Species == species.goat & d$Species == species.goat & d$Breed.type.goat == "pure.bred.exotic" , 'breed.type'] <- breed.cbred





d$is.growing <- FALSE
d[ !is.na(d$Stage.harmonized) & (  d$Stage.harmonized == stage.growing.male | d$Stage.harmonized == stage.growing.female | d$Stage.harmonized == stage.growing.all  ) , 'is.growing'] <- TRUE

d$is.adult <- FALSE
d[ !is.na(d$Stage.harmonized) & (  d$Stage.harmonized == stage.adult.male | d$Stage.harmonized == stage.adult.female | d$Stage.harmonized == stage.adult.all) , 'is.adult'] <- TRUE


d$is.male <- FALSE
d[ !is.na(d$Stage.harmonized) & (  d$Stage.harmonized %in% stages.male ) , 'is.male'] <- TRUE

d$sex <- NA
d[d$is.male ,'sex'] <- sex.male
d[ !d$is.male ,'sex'] <- sex.female


d[d$is.growing , 'stage.cond'] <-   unique.stages.cond[1]
d[d$is.adult , 'stage.cond'] <-   unique.stages.cond[2]

  
d$is.lt.20.kg <- NA
d[ d$bw_kg < 20 & !is.na(d$bw_kg) , 'is.lt.20.kg'] <- TRUE
d[ d$bw_kg >= 20 & !is.na(d$bw_kg) , 'is.lt.20.kg'] <- FALSE


# Define NDF levels
d$ndf.level <- NA

# Sheep
d[ !is.na(d$NDF_nutrition) & d$Species == species.sheep & d$NDF_nutrition <= ndf.thresh.sheep, 'ndf.level'] <- ndf.lev.lo
d[ !is.na(d$NDF_nutrition) & d$Species == species.sheep & d$NDF_nutrition > ndf.thresh.sheep, 'ndf.level'] <- ndf.lev.hi

# Goats
d[ !is.na(d$NDF_nutrition) & d$Species == species.goat & d$NDF_nutrition <= ndf.thresh.goat , 'ndf.level'] <- ndf.lev.lo
d[ !is.na(d$NDF_nutrition) & d$Species == species.goat & d$NDF_nutrition > ndf.thresh.goat, 'ndf.level'] <- ndf.lev.hi




# Bovines
d[ !is.na(d$NDF_nutrition) & d$Species == species.goat & d$NDF_nutrition <= ndf.thresh.goat , 'ndf.level'] <- ndf.lev.lo
d[ !is.na(d$NDF_nutrition) & d$Species == species.goat & d$NDF_nutrition > ndf.thresh.goat, 'ndf.level'] <- ndf.lev.hi



# Production system - re-write NDF category to be based on production system
#unq.prod.sys <- na.omit((unique( d$production.system )))


#d[!is.na(d$Breed.type.cattle  ) &  d$Species == species.cattle & d$production.system == unq.prod.sys[1]  & !is.na(d$production.system)  , 'prod.sys'] <- prod.sys.sint
#d[!is.na(d$Breed.type.cattle  ) &  d$Species == species.cattle & d$production.system == unq.prod.sys[2]  & !is.na(d$production.system)  , 'prod.sys'] <- prod.sys.sint
#d[!is.na(d$Breed.type.cattle  ) &  d$Species == species.cattle & d$production.system == unq.prod.sys[3]  & !is.na(d$production.system)  , 'prod.sys'] <- prod.sys.ext



#d[ d$prod.sys == prod.sys.ext &  !is.na(d$prod.sys) ,   'ndf.level'] <- ndf.lev.hi
#d[ d$prod.sys == prod.sys.sint &  !is.na(d$prod.sys) ,   'ndf.level'] <- ndf.lev.lo





unique.b.codes <- unique( d[d$Species == species.cattle & !is.na(d$NDF_nutrition), 'B.Code'  ] ) 






for ( b.code in unique.b.codes   ){
  
  # test: b.code <- unique.b.codes[1]
  
  mean.NDF <- mean( d[ !is.na(d$NDF_nutrition) & d$B.Code == b.code  , 'NDF_nutrition'] )

  if ( mean.NDF <= ndf.thresh.cattle) { 
    
    d[ d$B.Code == b.code  , 'ndf.level'] <- ndf.lev.lo
    
    } else if ( mean.NDF > ndf.thresh.cattle) { 
      
      d[ d$B.Code == b.code  , 'ndf.level'] <- ndf.lev.hi
      
    }
  
  
}




#d[ !is.na(d$NDF_nutrition) & d$Species == species.cattle & d$NDF_nutrition <= ndf.thresh.cattle , 'ndf.level'] <- ndf.lev.lo
#d[ !is.na(d$NDF_nutrition) & d$Species == species.cattle & d$NDF_nutrition > ndf.thresh.cattle, 'ndf.level'] <- ndf.lev.hi


unq.intake.units <- unique(d$feed_intake_unit)
  
d[ !is.na(d$feed_intake_unit ) &  d$feed_intake_unit == "kg/day" , 'feed_intake_kg_d'] <- d[ !is.na(d$feed_intake_unit ) &    d$feed_intake_unit == "kg/day" , 'feed_intake_value'] 
  
  
d <- d[d$feed_intake_unit == unq.intake.units[1] , ] 
  
  
d[ d$feed_intake_unit == unq.intake.units[1] & !is.na(d$feed_intake_unit ), 'feed_intake_kg_d'] <-  d[ d$feed_intake_unit == unq.intake.units[1] & !is.na(d$feed_intake_unit ) , 'feed_intake_value']


d$feed_intake_g_d <- d$feed_intake_kg_d * 1000

d$feeding.level.g.d.kg.bw <-   d$feed_intake_g_d / d$bw_kg
d$feeding.level.g.d.g.bw <-   d$feeding.level.g.d.kg.bw / 1000


# Lactation/gestation controls
d$milk_prod_kg_d <- 0 
d[d$Stage.harmonized ==  "lactating.adult.female" & !is.na(d$Stage.harmonized) , 'milk_prod_kg_d' ] <-  d[d$Stage.harmonized ==  "lactating.adult.female" & !is.na(d$Stage.harmonized) , 'milk_kg_day' ]
d[d$Stage.harmonized !=  "lactating.adult.female" & !is.na(d$Stage.harmonized) , 'milk_prod_kg_d' ] <- 0  

d$is_gestating <- 0 
d[d$Stage.harmonized ==  "gestating.adult.female" & !is.na(d$Stage.harmonized) , 'is_gestating' ] <-  1 
d[d$Stage.harmonized !=  "gestating.adult.female" & !is.na(d$Stage.harmonized) , 'is_gestating' ] <-  0


d$is_lactating <- 0 
d[d$Stage.harmonized ==  "lactating.adult.female" & !is.na(d$Stage.harmonized) , 'is_lactating' ] <-  1 
d[d$Stage.harmonized !=  "lactating.adult.female" & !is.na(d$Stage.harmonized) , 'is_lactating' ] <-  0
  
  
d$min.data <- FALSE

d[  !is.na(d$NDF_nutrition) & !is.na(d$bw_kg) & !is.na(d$adg_kg_day)   , 'min.data'] <- TRUE



# If animal is not lactating, set milk yield to zero
#d[  d$is_lactating ==  0 & !is.na(d$is_lactating ) , 'milk_kg_day' ]  <- 0
#d[  is.na(d$is_lactating ) , 'milk_kg_day' ]  <- 0

#d[  d$is_lactating ==  1 & !is.na(d$is_lactating ) , 'milk_kg_day' ] 




print(paste('Quantity of studies before outlier removal: ', length(unique(d$B.Code))))
print(paste('For cattle: ', length(unique(  d[d$Species == species.cattle , 'B.Code']  ))))


  
# Take subset of data for small ruminants
d.reg.in <- d[  d$Species %in% species.sheep | d$Species %in% species.goat  | d$Species %in% species.cattle, ]
  

d.reg.in$ut.id <- NA
d.reg.in$ue.id <- NA  

experiment.records <- c()

ue.count <- 0

gen.ue.str <- function(x){ return( str_c('e',x)  ) }
gen.ut.str <- function(x){ return( str_c('t',x)  ) }

d.reg.in[1, 'ue.id'] <- gen.ue.str(1)

for (r in 1:nrow(d.reg.in)){

# r <- 1
  
  t.id <- gen.ut.str(r)

d.reg.in[r, 'ut.id'] <- r

ue.id <-  d.reg.in[r, 'B.Code'] 


if (  !(ue.id %in% experiment.records)   ){

experiment.records <- c(experiment.records , ue.id )
ue.count <- ue.count + 1

} 


if (r > 1) { d.reg.in[r, 'ue.id'] <- gen.ue.str(ue.count)  }

}

d.reg.in$ue.id



  
d <- d[d$excluded != 1 & !is.na(d$excluded ), ]

d <- data.frame(d)



  d.reg.in[d.reg.in$ADF_nutrition < 50 & !is.na(d.reg.in$ADF_nutrition) , 'ADF_nutrition'] <-  10 * d.reg.in[d.reg.in$ADF_nutrition < 50 & !is.na(d.reg.in$ADF_nutrition), 'ADF_nutrition'] 
  d.reg.in[d.reg.in$NDF_nutrition < 50 & !is.na(d.reg.in$NDF_nutrition) , 'NDF_nutrition'] <-  10 *   d.reg.in[d.reg.in$NDF_nutrition < 50 & !is.na(d.reg.in$NDF_nutrition) , 'NDF_nutrition'] 
  d.reg.in[d.reg.in$CP_nutrition < 30 & !is.na(d.reg.in$CP_nutrition) , 'CP_nutrition'] <-  10 * d.reg.in[d.reg.in$CP_nutrition < 30 & !is.na(d.reg.in$CP_nutrition) , 'CP_nutrition'] 
  
  d.reg.in[d.reg.in$EE_nutrition < 20 & !is.na(d.reg.in$EE_nutrition) , 'EE_nutrition'] <-  10 * d.reg.in[d.reg.in$EE_nutrition < 20 & !is.na(d.reg.in$EE_nutrition), 'EE_nutrition'] 
  d.reg.in[d.reg.in$Ash_nutrition < 20 & !is.na(d.reg.in$Ash_nutrition) , 'Ash_nutrition'] <-  10 * d.reg.in[d.reg.in$Ash_nutrition < 20 & !is.na(d.reg.in$Ash_nutrition), 'Ash_nutrition'] 
  
  
  
  d.reg.in[d.reg.in$NDF_digest >100  & !is.na(d.reg.in$NDF_digest) , 'NDF_digest'] <-  (1/10) * d.reg.in[d.reg.in$NDF_digest > 100 & !is.na(d.reg.in$NDF_digest) , 'NDF_digest'] 
  d.reg.in[d.reg.in$DM_digest >100  & !is.na(d.reg.in$DM_digest) , 'DM_digest'] <-  (1/10) * d.reg.in[d.reg.in$DM_digest > 100 & !is.na(d.reg.in$DM_digest) , 'DM_digest'] 
  d.reg.in[d.reg.in$OM_digest >100  & !is.na(d.reg.in$OM_digest) , 'OM_digest'] <-  (1/10) * d.reg.in[d.reg.in$OM_digest > 100 & !is.na(d.reg.in$OM_digest) , 'OM_digest'] 
  
  
  
  
  # Variable interaction declarations and re-configurations
  d.reg.in$NDF_x_NDF_digest <- d.reg.in$NDF_digest * d.reg.in$NDF_nutrition
  
  
  d.reg.in[ d.reg.in$Species == species.sheep , 'BW_frac_Mat_BW'] <- d.reg.in[ d.reg.in$Species == species.sheep , 'bw_kg']  / 50
  d.reg.in[ d.reg.in$Species == species.goat, 'BW_frac_Mat_BW'] <- d.reg.in[ d.reg.in$Species == species.goat, 'bw_kg']  / 35
  
  
  d.reg.in$Met_bw_kg <- d.reg.in$bw_kg ^ 0.75
  
  
  pred.vars <- c( pred.vars ,  'Met_bw_kg' , 'BW_frac_Mat_BW')
  

  for (v in all.vars){
    
    v.e25 <- str_c( v , '.e25')
    v.sqt <- str_c( v , '.sqt')
    v.e75 <- str_c( v , '.e75')
    v.sqd <- str_c( v , '.sqd')
    v.cbd <- str_c( v , '.cbd')
    v.log <- str_c( v , '.log')
    v.recip <- str_c( v , '.recip')
    
   d.reg.in[ , v.e25 ] <- d.reg.in[,v]^0.25
   d.reg.in[ , v.sqt ] <- d.reg.in[,v]^0.5
   d.reg.in[ , v.e75 ] <- d.reg.in[,v]^0.75
   d.reg.in[,v.sqd] <- d.reg.in[,v]^2
   d.reg.in[,v.cbd] <- d.reg.in[,v]^3
   d.reg.in[,v.log] <-  log(d.reg.in[,v])
   d.reg.in[,v.recip] <-  1 /d.reg.in[,v]
    
    
  }
  
  
 
  
  for (s in suffixes){ 
    
    new.var.DV <- str_c( v.list.DV[1] ,s )
    v.list.DV <- append( v.list.DV , new.var.DV )
    
    new.var.BW <- str_c( v.list.BW[2] ,s )
    v.list.BW <- append(v.list.BW , new.var.BW )
    
    new.var.ADG <- str_c( v.list.ADG[2] ,s )
    v.list.ADG <- append(v.list.ADG , new.var.ADG )
    
    new.var.MY <- str_c( v.list.MY[2] ,s )
    v.list.MY <- append(v.list.MY , new.var.MY )
    
    new.var.GES <- str_c( v.list.GES[2] ,s )
    v.list.GES <- append(v.list.GES , new.var.GES )
    
    new.var.CP <- str_c( v.list.CP[2] ,s )
    v.list.CP <- append(v.list.CP , new.var.CP)
    
    new.var.NDF <- str_c( v.list.NDF[2] ,s )
    v.list.NDF <- append(v.list.NDF , new.var.NDF )
    
    new.var.ADF <- str_c( v.list.ADF[2] ,s )
    v.list.ADF <- append(v.list.ADF , new.var.ADF )
    
    new.var.EE <- str_c( v.list.EE[2] ,s )
    v.list.EE <- append(v.list.EE , new.var.EE )
    
    new.var.ASH <- str_c( v.list.Ash[2] ,s )
    v.list.Ash <- append(v.list.Ash , new.var.ASH )
    
    new.var.NDF.dig <- str_c( v.list.NDF.dig[2] ,s )
    v.list.NDF.dig <- append(v.list.NDF.dig , new.var.NDF.dig  )
    

    new.var.ADL<- str_c( v.list.ADL[2] ,s )
    v.list.ADL <- append(v.list.ADL , new.var.ADL )
    
    new.var.OM.dig <- str_c( v.list.OM.dig[2] ,s )
    v.list.OM.dig <- append(v.list.OM.dig , new.var.OM.dig )
    
    new.var.DM.dig <- str_c( v.list.DM.dig[2] ,s )
    v.list.DM.dig <- append(v.list.DM.dig , new.var.DM.dig )
    
    new.var.DM <- str_c( v.list.DM[2] ,s )
    v.list.DM <- append(v.list.DM , new.var.DM )
    
    
  }
  
  v.list.all <<- c( 
    
    v.list.DV 
    , v.list.BW 
    , v.list.ADG 
    
    , v.list.MY
    , v.list.GES
    
    
    , v.list.CP
    , v.list.NDF
    , v.list.ADF
    , v.list.EE 
    , v.list.Ash
    , v.list.NDF.dig
    
    
    
    , v.list.ADL
    , v.list.DM.dig
    , v.list.OM.dig 
    , v.list.DM 
    )
  
  
 
  
  
  # Assign regression weights
  n_tilde <- c()
  
  for (i in 1:length((unique( d.reg.in$ue.id)))) {
    
    # test: i <- 1
    
    e.id <- (unique(d.reg.in$ue.id))[i]
    
    
    all.sample.sizes <-   d.reg.in[d.reg.in$ue.id == e.id ,  'Sample.size']
    sample.size.unique <- unique(    all.sample.sizes)
    num.exp.units <- length(d.reg.in[d.reg.in$ue.id == e.id ,  'Sample.size'])
    
    
    n_tilde_numer <-   sample.size.unique ^  num.exp.units
    n_tilde_denom <-   sum(all.sample.sizes)
    
    n_tilde[ i ] <- n_tilde_numer /    n_tilde_denom
    
    d.reg.in[d.reg.in$ue.id == e.id ,'weight' ] <- n_tilde[ i ]
    
  }
  
  d.reg.in$reg.wt.null <- rep(1 , length(d.reg.in$weight) )
  d.reg.in$reg.wt.sqr <- sqrt(d.reg.in$weight)
  d.reg.in$reg.wt.log <- log(d.reg.in$weight) 
  d.reg.in$reg.wt.cubed.root <- d.reg.in$weight^0.33
  d.reg.in$reg.wt.quartic.root <- d.reg.in$weight^0.25
  

  d.reg.in <- d.reg.in[  !(   d.reg.in$Species %in% species.srs & ( !d.reg.in$is_gestating | !d.reg.in$is_lactating )), ]
  
 # d.reg.in <- d.reg.in[d.reg.in$breed.type %in% c(2,3), ]
  
  LR.only <- TRUE
  
  if ( LR.only == TRUE ) { d.reg.in <- d.reg.in[  (d.reg.in$Species %in% species.cattle) , ] }
  if ( LR.only == FALSE  ) { d.reg.in <- d.reg.in[  (d.reg.in$Species %in% species.srs) , ] }

  
  d.reg.in <- d.reg.in[  d.reg.in$bw_kg > 75 & !is.na(d.reg.in$bw_kg) ,  ]
  
 d.reg.in <- d.reg.in[  !is.na(d.reg.in$ndf.level) ,  ]
 
 # Remove experiments where both (1) DMI is consumed while grazing, and (2) the marker method is not used
 d.reg.in <- d.reg.in[ 
   !( d.reg.in$`Feed.consumed.while.grazing?` == "yes"  
      & !is.na(d.reg.in$`Feed.consumed.while.grazing?`) 
      & d.reg.in$marker.method ==  "no"  
      &   !is.na(d.reg.in$marker.method)  )
   ,  ]

 
 
} # Data pre-processing



source(  str_c(  settings.dir   ,  'model.specs.R') ) ; source(  'refresh.R') 




