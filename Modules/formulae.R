

# source( str_c(modules.dir ,'formulae.R')  )



if (FALSE){ 
  

  # SELECT WHICH SAMPLE YOU ARE WORKING WITH
  #current.ds.ids <- ue.ids.sp.lo.ndf.male
  #current.ds.ids <- ue.ids.sp.lo.ndf.female

  #current.ds.ids <- ue.ids.sp.hi.ndf.male
  #current.ds.ids <- ue.ids.sp.hi.ndf.female

  #current.ds.ids <- ue.ids.gt.lo.ndf.male
  #current.ds.ids <- ue.ids.gt.lo.ndf.female

  #current.ds.ids <- ue.ids.gt.hi.ndf.male
  #current.ds.ids <- ue.ids.gt.hi.ndf.female

# Cattle

  current.ds.ids <- ue.ids.bv.lo.ndf.Bind.male 
  current.ds.ids <- ue.ids.bv.lo.ndf.Btau.male 

  current.ds.ids <- ue.ids.bv.lo.ndf.Bind.female 
  current.ds.ids <- ue.ids.bv.lo.ndf.Btau.female 

  current.ds.ids <- ue.ids.bv.hi.ndf.Bind.male 
  current.ds.ids <- ue.ids.bv.hi.ndf.Btau.male 

  current.ds.ids <- ue.ids.bv.hi.ndf.Bind.female 
  current.ds.ids <- ue.ids.bv.hi.ndf.Btau.female 



{

nrow(d.reg.in[d.reg.in$ue.id %in% current.ds.ids , ] ) 


# Breed type
nrow((d.reg.in[d.reg.in$ue.id %in% current.ds.ids & d.reg.in$breed.type == 1, ])) / nrow((d.reg.in[d.reg.in$ue.id %in% current.ds.ids & !is.na(d.reg.in$breed.type), ])) 


# ADF
sum(!is.na(d.reg.in[d.reg.in$ue.id %in% current.ds.ids , 'ADF_nutrition'])) /  nrow((d.reg.in[d.reg.in$ue.id %in% current.ds.ids & !is.na(d.reg.in$breed.type), ])) 


# NDF
sum(!is.na(d.reg.in[d.reg.in$ue.id %in% current.ds.ids , 'NDF_nutrition'])) /  nrow((d.reg.in[d.reg.in$ue.id %in% current.ds.ids & !is.na(d.reg.in$breed.type), ])) 


# CP
sum(!is.na(d.reg.in[d.reg.in$ue.id %in% current.ds.ids , 'CP_nutrition'])) /  nrow((d.reg.in[d.reg.in$ue.id %in% current.ds.ids & !is.na(d.reg.in$breed.type), ])) 


nrow((d.reg.in[d.reg.in$ue.id %in% current.ds.ids & !is.na(d.reg.in$breed.type), ])) 


# Ash
sum(!is.na(d.reg.in[d.reg.in$ue.id %in% current.ds.ids , 'Ash_nutrition'])) / nrow((d.reg.in[d.reg.in$ue.id %in% current.ds.ids & !is.na(d.reg.in$breed.type), ]))  



# ADL
sum(!is.na(d.reg.in[d.reg.in$ue.id %in% current.ds.ids , 'ADL_nutrition'])) / nrow((d.reg.in[d.reg.in$ue.id %in% current.ds.ids & !is.na(d.reg.in$breed.type), ]))  


# EE
sum(!is.na(d.reg.in[d.reg.in$ue.id %in% current.ds.ids , 'EE_nutrition'])) /  nrow((d.reg.in[d.reg.in$ue.id %in% current.ds.ids & !is.na(d.reg.in$breed.type), ])) 



# NDF deigestibility
sum(!is.na(d.reg.in[d.reg.in$ue.id %in% current.ds.ids , 'NDF_digest'])) /  nrow((d.reg.in[d.reg.in$ue.id %in% current.ds.ids & !is.na(d.reg.in$breed.type), ])) 



# OM de=igestibility
sum(!is.na(d.reg.in[d.reg.in$ue.id %in% current.ds.ids , 'OM_digest'])) /  nrow((d.reg.in[d.reg.in$ue.id %in% current.ds.ids & !is.na(d.reg.in$breed.type), ])) 

# OM de=igestibility
sum(!is.na(d.reg.in[d.reg.in$ue.id %in% current.ds.ids , 'DM_digest'])) /  nrow((d.reg.in[d.reg.in$ue.id %in% current.ds.ids & !is.na(d.reg.in$breed.type), ])) 





} # Quantify fraction complete cases per variable



# all predictor variables to choose from
all.x.vars

x.vars.reg  <- c(
     
  
  "bw_kg"   
  ,"adg_kg_day"  
  
  #  ,"milk_prod_kg_d" 
  # ,"is_gestating"  
   ,"NDF_nutrition.sqt" 
  #  , "ADF_nutrition" 
  #  ,"CP_nutrition"  
  #  ,"NDF_digest"   
  # ,"EE_nutrition"  
     ,"Ash_nutrition" 
  # , "ADL_nutrition" 
  #  ,"NDF_digest"  
  #,"OM_digest" 
#,"DM_digest"
   )
   
   

for (x in x.vars.reg   ){
     
     if (x == x.vars.reg[1]) { formula <- paste(    "feed_intake_g_d  ~ ", x.vars.reg[1]) ; next}
     
     # x <- x.vars[2]
     
  formula <- paste( formula  , ' + ' ,x )
  
  if ( which(x == x.vars.reg) == length(x.vars.reg)){ 
    
    formula.me <- paste( formula , '+ ( 1 | ue.id)') 
    formula <- as.formula(  formula )
    formula.me <- as.formula(  formula.me )
    
    }
     
} # Generate formula 
   
    
    
# Generate dataframes for regression and for variance inflation factor (only Xs/predictors)
reg.data <-  d.reg.in[ d.reg.in$ue.id %in% current.ds.ids    , c('feed_intake_g_d' ,  x.vars.reg ) ]
reg.data.me <-  d.reg.in[ d.reg.in$ue.id %in% current.ds.ids    , c('feed_intake_g_d' ,  x.vars.reg , 'ue.id' ) ]
pred.data <-  reg.data[  -( which( colnames(reg.data) == 'feed_intake_g_d') )  ]

# Report variance inflation factor on predictor variables
usdm::vif(       pred.data ) 

# GRADIENT BOOSTED REGRESSION
{
  
scope.gbr <- glmboost( 


formula

, data =  reg.data 
, family = Gaussian() 
, control = boost_control(mstop = 5000 , nu = 0.15)
, center = FALSE
)



names <-  as.vector(names(scope.gbr$coef()))
    
    var.imps <- as.numeric(  varimp(scope.gbr )  )
    var.imps.ordered <- sort(var.imps, decreasing = TRUE)
    
    # Summarize coefficient results
    summary(scope.gbr)
    varimp(scope.gbr)
    
    
    # Order variables according to importance
    names[which( var.imps == var.imps.ordered[1] ) ]
    names[which( var.imps == var.imps.ordered[2] ) ]
    names[which( var.imps == var.imps.ordered[3] ) ]
    names[which( var.imps == var.imps.ordered[4] ) ]
    names[which( var.imps == var.imps.ordered[5] ) ]
    names[which( var.imps == var.imps.ordered[6] ) ]
    names[which( var.imps == var.imps.ordered[7] ) ]
    names[which( var.imps == var.imps.ordered[8] ) ]
    
    
}
  
# MIXED EFFECTS
{
scope.me <- lmer(  

formula.me 

, data = reg.data.me 

)



summary(  scope.me)

}
    
    
# Model comparisons
AIC( scope.gbr)
AIC( scope.me)
    
  
  

} # Formula initialisations


# ~ ------ ~ FORMULA DECLARATIONS

{
  
  

{
  # LOW NDF
  # Bos indicus
  # MALE
  GBR.mod.1.bv.lo.ndf.Bind.male <- as.formula(   feed_intake_g_d ~  bw_kg    )
  GBR.mod.2.bv.lo.ndf.Bind.male <- as.formula(   feed_intake_g_d ~  bw_kg + adg_kg_day   )
  GBR.mod.3.bv.lo.ndf.Bind.male <- as.formula(  feed_intake_g_d  ~  bw_kg + adg_kg_day  +  adg_kg_day.sqd  )
  
  GBR.mod.4.bv.lo.ndf.Bind.male <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day +  NDF_nutrition  )
  GBR.mod.5.bv.lo.ndf.Bind.male <- as.formula(   feed_intake_g_d ~  bw_kg +  adg_kg_day + CP_nutrition  )
  GBR.mod.6.bv.lo.ndf.Bind.male <- as.formula(   feed_intake_g_d ~  bw_kg +  adg_kg_day + ADF_nutrition )
  
  GBR.mod.7.bv.lo.ndf.Bind.male <- as.formula(  feed_intake_g_d  ~  bw_kg +  adg_kg_day  +  NDF_nutrition + EE_nutrition    )
  GBR.mod.8.bv.lo.ndf.Bind.male <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day  +  CP_nutrition +  EE_nutrition   )
  GBR.mod.9.bv.lo.ndf.Bind.male <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day +  ADF_nutrition + EE_nutrition )
  
  
  # FEMALE
  GBR.mod.1.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d ~  bw_kg  + milk_prod_kg_d  )
  GBR.mod.2.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d ~  bw_kg + milk_prod_kg_d + adg_kg_day  )
  GBR.mod.3.bv.lo.ndf.Bind.female <- as.formula(  feed_intake_g_d  ~  bw_kg + milk_prod_kg_d + adg_kg_day + adg_kg_day.sqd)
  
  GBR.mod.4.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day +  ADF_nutrition +  milk_prod_kg_d )
  GBR.mod.5.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d ~  bw_kg +  adg_kg_day + CP_nutrition +  milk_prod_kg_d)
  GBR.mod.6.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d ~  bw_kg +  adg_kg_day + NDF_nutrition + milk_prod_kg_d)
  
  GBR.mod.7.bv.lo.ndf.Bind.female <- as.formula(  feed_intake_g_d  ~  bw_kg +  adg_kg_day  +  adg_kg_day + ADF_nutrition + Ash_nutrition  + milk_prod_kg_d)
  GBR.mod.8.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day  +  adg_kg_day + ADF_nutrition  + NDF_nutrition + milk_prod_kg_d)
  GBR.mod.9.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day +  adg_kg_day + EE_nutrition  + CP_nutrition +  milk_prod_kg_d)
  
  # Crossbreds
  # MALE
  GBR.mod.1.bv.lo.ndf.Btau.male <- GBR.mod.1.bv.lo.ndf.Bind.male
  GBR.mod.2.bv.lo.ndf.Btau.male <- GBR.mod.2.bv.lo.ndf.Bind.male
  GBR.mod.3.bv.lo.ndf.Btau.male <- GBR.mod.3.bv.lo.ndf.Bind.male
  
  GBR.mod.4.bv.lo.ndf.Btau.male <- GBR.mod.4.bv.lo.ndf.Bind.male
  GBR.mod.5.bv.lo.ndf.Btau.male <- GBR.mod.5.bv.lo.ndf.Bind.male
  GBR.mod.6.bv.lo.ndf.Btau.male <- GBR.mod.6.bv.lo.ndf.Bind.male
  
  GBR.mod.7.bv.lo.ndf.Btau.male <- GBR.mod.7.bv.lo.ndf.Bind.male
  GBR.mod.8.bv.lo.ndf.Btau.male <- GBR.mod.8.bv.lo.ndf.Bind.male
  GBR.mod.9.bv.lo.ndf.Btau.male <- GBR.mod.9.bv.lo.ndf.Bind.male
  
  
  # FEMALE
  GBR.mod.1.bv.lo.ndf.Btau.female <- GBR.mod.1.bv.lo.ndf.Bind.female
  GBR.mod.2.bv.lo.ndf.Btau.female <- GBR.mod.2.bv.lo.ndf.Bind.female
  GBR.mod.3.bv.lo.ndf.Btau.female <- GBR.mod.3.bv.lo.ndf.Bind.female
  
  GBR.mod.4.bv.lo.ndf.Btau.female <- GBR.mod.4.bv.lo.ndf.Bind.female
  GBR.mod.5.bv.lo.ndf.Btau.female <- GBR.mod.5.bv.lo.ndf.Bind.female
  GBR.mod.6.bv.lo.ndf.Btau.female <- GBR.mod.6.bv.lo.ndf.Bind.female
  
  GBR.mod.7.bv.lo.ndf.Btau.female <- GBR.mod.7.bv.lo.ndf.Bind.female
  GBR.mod.8.bv.lo.ndf.Btau.female <- GBR.mod.8.bv.lo.ndf.Bind.female
  GBR.mod.9.bv.lo.ndf.Btau.female <- GBR.mod.9.bv.lo.ndf.Bind.female
  
  
  
  # High NDF
  # Bos indicus
  # MALE
  GBR.mod.1.bv.hi.ndf.Bind.male <-  GBR.mod.1.bv.lo.ndf.Bind.male
  GBR.mod.2.bv.hi.ndf.Bind.male <-  GBR.mod.2.bv.lo.ndf.Bind.male
  GBR.mod.3.bv.hi.ndf.Bind.male <-  GBR.mod.3.bv.lo.ndf.Bind.male
  
  GBR.mod.4.bv.hi.ndf.Bind.male <- GBR.mod.4.bv.lo.ndf.Bind.male
  GBR.mod.5.bv.hi.ndf.Bind.male <- GBR.mod.5.bv.lo.ndf.Bind.male
  GBR.mod.6.bv.hi.ndf.Bind.male <- GBR.mod.6.bv.lo.ndf.Bind.male
  
  GBR.mod.7.bv.hi.ndf.Bind.male <- GBR.mod.7.bv.lo.ndf.Bind.male
  GBR.mod.8.bv.hi.ndf.Bind.male <- GBR.mod.8.bv.lo.ndf.Bind.male
  GBR.mod.9.bv.hi.ndf.Bind.male <- GBR.mod.9.bv.lo.ndf.Bind.male
  
  
  # FEMALE
  GBR.mod.1.bv.hi.ndf.Bind.female <-  GBR.mod.1.bv.lo.ndf.Bind.female
  GBR.mod.2.bv.hi.ndf.Bind.female <- GBR.mod.2.bv.lo.ndf.Bind.female
  GBR.mod.3.bv.hi.ndf.Bind.female <- GBR.mod.3.bv.lo.ndf.Bind.female
  
  GBR.mod.4.bv.hi.ndf.Bind.female <- GBR.mod.4.bv.lo.ndf.Bind.female
  GBR.mod.5.bv.hi.ndf.Bind.female <- GBR.mod.5.bv.lo.ndf.Bind.female
  GBR.mod.6.bv.hi.ndf.Bind.female <- GBR.mod.6.bv.lo.ndf.Bind.female
  
  GBR.mod.7.bv.hi.ndf.Bind.female <- GBR.mod.7.bv.lo.ndf.Bind.female
  GBR.mod.8.bv.hi.ndf.Bind.female <- GBR.mod.8.bv.lo.ndf.Bind.female
  GBR.mod.9.bv.hi.ndf.Bind.female <- GBR.mod.9.bv.lo.ndf.Bind.female
  
  # Crossbreds
  # MALE
  GBR.mod.1.bv.hi.ndf.Btau.male <-  GBR.mod.1.bv.lo.ndf.Btau.male
  GBR.mod.2.bv.hi.ndf.Btau.male <- GBR.mod.2.bv.lo.ndf.Btau.male
  GBR.mod.3.bv.hi.ndf.Btau.male <- GBR.mod.3.bv.lo.ndf.Btau.male
  
  GBR.mod.4.bv.hi.ndf.Btau.male <- GBR.mod.4.bv.lo.ndf.Btau.male
  GBR.mod.5.bv.hi.ndf.Btau.male <- GBR.mod.5.bv.lo.ndf.Btau.male
  GBR.mod.6.bv.hi.ndf.Btau.male <- GBR.mod.6.bv.lo.ndf.Btau.male
  
  GBR.mod.7.bv.hi.ndf.Btau.male <- GBR.mod.7.bv.lo.ndf.Btau.male
  GBR.mod.8.bv.hi.ndf.Btau.male <- GBR.mod.8.bv.lo.ndf.Btau.male
  GBR.mod.9.bv.hi.ndf.Btau.male <- GBR.mod.9.bv.lo.ndf.Btau.male
  
  
  # FEMALE
  GBR.mod.1.bv.hi.ndf.Btau.female <-  GBR.mod.1.bv.lo.ndf.Btau.female
  GBR.mod.2.bv.hi.ndf.Btau.female <-  GBR.mod.2.bv.lo.ndf.Btau.female
  GBR.mod.3.bv.hi.ndf.Btau.female <-   GBR.mod.3.bv.lo.ndf.Btau.female
  
  GBR.mod.4.bv.hi.ndf.Btau.female <-   GBR.mod.4.bv.lo.ndf.Btau.female
  GBR.mod.5.bv.hi.ndf.Btau.female <-   GBR.mod.5.bv.lo.ndf.Btau.female
  GBR.mod.6.bv.hi.ndf.Btau.female <-   GBR.mod.6.bv.lo.ndf.Btau.female
  
  GBR.mod.7.bv.hi.ndf.Btau.female <-  GBR.mod.7.bv.lo.ndf.Btau.female
  GBR.mod.8.bv.hi.ndf.Btau.female <-   GBR.mod.8.bv.lo.ndf.Btau.female
  GBR.mod.9.bv.hi.ndf.Btau.female <-    GBR.mod.9.bv.lo.ndf.Btau.female
  
  }  # GRADIENT BOOSTING
  
{
    # LOW NDF
    # Bos indicus
    # MALE
  
  
  ME.mod.1.bv.lo.ndf.Bind.male  <- as.formula(   feed_intake_g_d ~  bw_kg    )
  ME.mod.2.bv.lo.ndf.Bind.male  <- as.formula(   feed_intake_g_d ~  bw_kg + adg_kg_day   )
  ME.mod.3.bv.lo.ndf.Bind.male<- as.formula(  feed_intake_g_d  ~  bw_kg + adg_kg_day  +  adg_kg_day.sqd  )
  

    ME.mod.4.bv.lo.ndf.Bind.male <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day +   CP_nutrition + EE_nutrition  )
    ME.mod.5.bv.lo.ndf.Bind.male <- as.formula(   feed_intake_g_d ~  bw_kg +  adg_kg_day + ADF_nutrition + EE_nutrition )
    ME.mod.6.bv.lo.ndf.Bind.male <- as.formula(   feed_intake_g_d ~  bw_kg +  adg_kg_day + NDF_nutrition  + EE_nutrition  )
    
    ME.mod.7.bv.lo.ndf.Bind.male <- as.formula(  feed_intake_g_d  ~  bw_kg +  adg_kg_day  +  adg_kg_day + NDF_nutrition + CP_nutrition + Ash_nutrition  )
    ME.mod.8.bv.lo.ndf.Bind.male <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day  +  adg_kg_day + CP_nutrition  + ADF_nutrition  + EE_nutrition )
    ME.mod.9.bv.lo.ndf.Bind.male <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day +  adg_kg_day + EE_nutrition  + CP_nutrition + ADF_nutrition  + NDF_nutrition)
    
    
    # FEMALE
    ME.mod.1.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d ~  bw_kg  + adg_kg_day + NDF_nutrition  + milk_prod_kg_d )
    ME.mod.2.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d ~  bw_kg + adg_kg_day  + ADF_nutrition + milk_prod_kg_d )
    ME.mod.3.bv.lo.ndf.Bind.female <- as.formula(  feed_intake_g_d  ~  bw_kg + adg_kg_day  + CP_nutrition  + milk_prod_kg_d )
    
    ME.mod.4.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day +  ADF_nutrition + NDF_nutrition + milk_prod_kg_d )
    ME.mod.5.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d ~  bw_kg +  adg_kg_day + CP_nutrition +  ADF_nutrition + milk_prod_kg_d)
    ME.mod.6.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d ~  bw_kg +  adg_kg_day + NDF_nutrition + CP_nutrition + milk_prod_kg_d)
    
    ME.mod.7.bv.lo.ndf.Bind.female <- as.formula(  feed_intake_g_d  ~  bw_kg +  adg_kg_day  +  adg_kg_day+ ADF_nutrition + Ash_nutrition  + milk_prod_kg_d)
    ME.mod.8.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day  +  adg_kg_day + ADF_nutrition  + NDF_nutrition + CP_nutrition + milk_prod_kg_d)
    ME.mod.9.bv.lo.ndf.Bind.female <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day +  adg_kg_day + EE_nutrition  + CP_nutrition + ADF_nutrition  + NDF_nutrition + milk_prod_kg_d)
    
    # Crossbreds
    # MALE
    ME.mod.1.bv.lo.ndf.Btau.male <- ME.mod.1.bv.lo.ndf.Bind.male
    ME.mod.2.bv.lo.ndf.Btau.male <- ME.mod.2.bv.lo.ndf.Bind.male
    ME.mod.3.bv.lo.ndf.Btau.male <- ME.mod.3.bv.lo.ndf.Bind.male
    
    ME.mod.4.bv.lo.ndf.Btau.male <- ME.mod.4.bv.lo.ndf.Bind.male
    ME.mod.5.bv.lo.ndf.Btau.male <- ME.mod.5.bv.lo.ndf.Bind.male
    ME.mod.6.bv.lo.ndf.Btau.male <- ME.mod.6.bv.lo.ndf.Bind.male
    
    ME.mod.7.bv.lo.ndf.Btau.male <- ME.mod.7.bv.lo.ndf.Bind.male
    ME.mod.8.bv.lo.ndf.Btau.male <- ME.mod.8.bv.lo.ndf.Bind.male
    ME.mod.9.bv.lo.ndf.Btau.male <- ME.mod.9.bv.lo.ndf.Bind.male
    
    
    # FEMALE
    ME.mod.1.bv.lo.ndf.Btau.female <- ME.mod.1.bv.lo.ndf.Bind.female
    ME.mod.2.bv.lo.ndf.Btau.female <- ME.mod.2.bv.lo.ndf.Bind.female
    ME.mod.3.bv.lo.ndf.Btau.female <- ME.mod.3.bv.lo.ndf.Bind.female
    
    ME.mod.4.bv.lo.ndf.Btau.female <- ME.mod.4.bv.lo.ndf.Bind.female
    ME.mod.5.bv.lo.ndf.Btau.female <- ME.mod.5.bv.lo.ndf.Bind.female
    ME.mod.6.bv.lo.ndf.Btau.female <- ME.mod.6.bv.lo.ndf.Bind.female
    
    ME.mod.7.bv.lo.ndf.Btau.female <- ME.mod.7.bv.lo.ndf.Bind.female
    ME.mod.8.bv.lo.ndf.Btau.female <- ME.mod.8.bv.lo.ndf.Bind.female
    ME.mod.9.bv.lo.ndf.Btau.female <- ME.mod.9.bv.lo.ndf.Bind.female
    
    
    
    # High NDF
    # Bos indicus
    # MALE
    ME.mod.1.bv.hi.ndf.Bind.male <-  ME.mod.1.bv.lo.ndf.Bind.male
    ME.mod.2.bv.hi.ndf.Bind.male <-  ME.mod.2.bv.lo.ndf.Bind.male
    ME.mod.3.bv.hi.ndf.Bind.male <-  ME.mod.3.bv.lo.ndf.Bind.male
    
    ME.mod.4.bv.hi.ndf.Bind.male <- ME.mod.4.bv.lo.ndf.Bind.male
    ME.mod.5.bv.hi.ndf.Bind.male <- ME.mod.5.bv.lo.ndf.Bind.male
    ME.mod.6.bv.hi.ndf.Bind.male <- ME.mod.6.bv.lo.ndf.Bind.male
    
    ME.mod.7.bv.hi.ndf.Bind.male <- ME.mod.7.bv.lo.ndf.Bind.male
    ME.mod.8.bv.hi.ndf.Bind.male <- ME.mod.8.bv.lo.ndf.Bind.male
    ME.mod.9.bv.hi.ndf.Bind.male <- ME.mod.9.bv.lo.ndf.Bind.male
    
    
    # FEMALE
    ME.mod.1.bv.hi.ndf.Bind.female <-  ME.mod.1.bv.lo.ndf.Bind.female
    ME.mod.2.bv.hi.ndf.Bind.female <- ME.mod.2.bv.lo.ndf.Bind.female
    ME.mod.3.bv.hi.ndf.Bind.female <- ME.mod.3.bv.lo.ndf.Bind.female
    
    ME.mod.4.bv.hi.ndf.Bind.female <- ME.mod.4.bv.lo.ndf.Bind.female
    ME.mod.5.bv.hi.ndf.Bind.female <- ME.mod.5.bv.lo.ndf.Bind.female
    ME.mod.6.bv.hi.ndf.Bind.female <- ME.mod.6.bv.lo.ndf.Bind.female
    
    ME.mod.7.bv.hi.ndf.Bind.female <- ME.mod.7.bv.lo.ndf.Bind.female
    ME.mod.8.bv.hi.ndf.Bind.female <- ME.mod.8.bv.lo.ndf.Bind.female
    ME.mod.9.bv.hi.ndf.Bind.female <- ME.mod.9.bv.lo.ndf.Bind.female
    
    # Crossbreds
    # MALE
    ME.mod.1.bv.hi.ndf.Btau.male <-  ME.mod.1.bv.lo.ndf.Btau.male
    ME.mod.2.bv.hi.ndf.Btau.male <- ME.mod.2.bv.lo.ndf.Btau.male
    ME.mod.3.bv.hi.ndf.Btau.male <- ME.mod.3.bv.lo.ndf.Btau.male
    
    ME.mod.4.bv.hi.ndf.Btau.male <- ME.mod.4.bv.lo.ndf.Btau.male
    ME.mod.5.bv.hi.ndf.Btau.male <- ME.mod.5.bv.lo.ndf.Btau.male
    ME.mod.6.bv.hi.ndf.Btau.male <- ME.mod.6.bv.lo.ndf.Btau.male
    
    ME.mod.7.bv.hi.ndf.Btau.male <- ME.mod.7.bv.lo.ndf.Btau.male
    ME.mod.8.bv.hi.ndf.Btau.male <- ME.mod.8.bv.lo.ndf.Btau.male
    ME.mod.9.bv.hi.ndf.Btau.male <- ME.mod.9.bv.lo.ndf.Btau.male
    
    
    # FEMALE
    ME.mod.1.bv.hi.ndf.Btau.female <-  ME.mod.1.bv.lo.ndf.Btau.female
    ME.mod.2.bv.hi.ndf.Btau.female <-  ME.mod.2.bv.lo.ndf.Btau.female
    ME.mod.3.bv.hi.ndf.Btau.female <-   ME.mod.3.bv.lo.ndf.Btau.female
    
    ME.mod.4.bv.hi.ndf.Btau.female <-   ME.mod.4.bv.lo.ndf.Btau.female
    ME.mod.5.bv.hi.ndf.Btau.female <-   ME.mod.5.bv.lo.ndf.Btau.female
    ME.mod.6.bv.hi.ndf.Btau.female <-   ME.mod.6.bv.lo.ndf.Btau.female
    
    ME.mod.7.bv.hi.ndf.Btau.female <-  ME.mod.7.bv.lo.ndf.Btau.female
    ME.mod.8.bv.hi.ndf.Btau.female <-   ME.mod.8.bv.lo.ndf.Btau.female
    ME.mod.9.bv.hi.ndf.Btau.female <-    ME.mod.9.bv.lo.ndf.Btau.female
    
  }  # MIXED EFFECTS
  
  
  
} # BOVINES

{

# LOW NDF
# MALE
mod.1.sp.lo.ndf.male <- as.formula(   feed_intake_g_d ~  bw_kg  + adg_kg_day  )
mod.2.sp.lo.ndf.male <- as.formula(   feed_intake_g_d ~  bw_kg + adg_kg_day   )
mod.3.sp.lo.ndf.male <- as.formula(  feed_intake_g_d  ~  bw_kg + adg_kg_day    )

mod.4.sp.lo.ndf.male <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day +  ADF_nutrition + NDF_nutrition  )
mod.5.sp.lo.ndf.male <- as.formula(   feed_intake_g_d ~  bw_kg +  adg_kg_day + CP_nutrition +  ADF_nutrition )
mod.6.sp.lo.ndf.male <- as.formula(   feed_intake_g_d ~  bw_kg +  adg_kg_day + NDF_nutrition + CP_nutrition )

mod.7.sp.lo.ndf.male <- as.formula(  feed_intake_g_d  ~  bw_kg +  adg_kg_day  +  adg_kg_day+ ADF_nutrition + Ash_nutrition  )
mod.8.sp.lo.ndf.male <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day  +  adg_kg_day + ADF_nutrition  + NDF_nutrition + CP_nutrition )
mod.9.sp.lo.ndf.male <- as.formula(   feed_intake_g_d  ~ bw_kg +  adg_kg_day +  adg_kg_day + Ash_nutrition  + CP_nutrition + ADF_nutrition  + NDF_nutrition)

# FEMALE
mod.1.sp.lo.ndf.female <-   mod.1.sp.lo.ndf.male
mod.2.sp.lo.ndf.female <- mod.2.sp.lo.ndf.male
mod.3.sp.lo.ndf.female <-  mod.3.sp.lo.ndf.male

mod.4.sp.lo.ndf.female <-  mod.4.sp.lo.ndf.male
mod.5.sp.lo.ndf.female <-  mod.5.sp.lo.ndf.male
mod.6.sp.lo.ndf.female <-  mod.6.sp.lo.ndf.male

mod.7.sp.lo.ndf.female <-  mod.7.sp.lo.ndf.male
mod.8.sp.lo.ndf.female <-  mod.8.sp.lo.ndf.male
mod.9.sp.lo.ndf.female <-  mod.9.sp.lo.ndf.male


# High NDF
# MALE
mod.1.sp.hi.ndf.male <-   mod.1.sp.lo.ndf.male
mod.2.sp.hi.ndf.male <- mod.2.sp.lo.ndf.male
mod.3.sp.hi.ndf.male <-  mod.3.sp.lo.ndf.male

mod.4.sp.hi.ndf.male <-  mod.4.sp.lo.ndf.male
mod.5.sp.hi.ndf.male <-  mod.5.sp.lo.ndf.male
mod.6.sp.hi.ndf.male <-  mod.6.sp.lo.ndf.male

mod.7.sp.hi.ndf.male <-  mod.7.sp.lo.ndf.male
mod.8.sp.hi.ndf.male <-  mod.8.sp.lo.ndf.male
mod.9.sp.hi.ndf.male <-  mod.9.sp.lo.ndf.male


# FEMALE
mod.1.sp.hi.ndf.female <-   mod.1.sp.lo.ndf.male
mod.2.sp.hi.ndf.female <- mod.2.sp.lo.ndf.male
mod.3.sp.hi.ndf.female <-  mod.3.sp.lo.ndf.male

mod.4.sp.hi.ndf.female <-  mod.4.sp.lo.ndf.male
mod.5.sp.hi.ndf.female <-  mod.5.sp.lo.ndf.male
mod.6.sp.hi.ndf.female <-  mod.6.sp.lo.ndf.male

mod.7.sp.hi.ndf.female <-  mod.7.sp.lo.ndf.male
mod.8.sp.hi.ndf.female <-  mod.8.sp.lo.ndf.male
mod.9.sp.hi.ndf.female <-  mod.9.sp.lo.ndf.male

} #  SHEEP

{
# Low NDF - male
mod.1.gt.lo.ndf.male <- mod.1.sp.lo.ndf.male
mod.2.gt.lo.ndf.male <- mod.2.sp.lo.ndf.male
mod.3.gt.lo.ndf.male <- mod.3.sp.lo.ndf.male
mod.4.gt.lo.ndf.male <- mod.4.sp.lo.ndf.male
mod.5.gt.lo.ndf.male <- mod.5.sp.lo.ndf.male
mod.6.gt.lo.ndf.male <- mod.6.sp.lo.ndf.male
mod.7.gt.lo.ndf.male <- mod.7.sp.lo.ndf.male
mod.8.gt.lo.ndf.male <- mod.8.sp.lo.ndf.male
mod.9.gt.lo.ndf.male <- mod.9.sp.lo.ndf.male


# Low NDF - female
mod.1.gt.lo.ndf.female <- mod.1.sp.lo.ndf.female
mod.2.gt.lo.ndf.female <- mod.2.sp.lo.ndf.female
mod.3.gt.lo.ndf.female <- mod.3.sp.lo.ndf.female
mod.4.gt.lo.ndf.female <- mod.4.sp.lo.ndf.female
mod.5.gt.lo.ndf.female <- mod.5.sp.lo.ndf.female
mod.6.gt.lo.ndf.female <- mod.6.sp.lo.ndf.female
mod.7.gt.lo.ndf.female <- mod.7.sp.lo.ndf.female
mod.8.gt.lo.ndf.female <- mod.8.sp.lo.ndf.female
mod.9.gt.lo.ndf.female <- mod.9.sp.lo.ndf.female



# hiw NDF - male
mod.1.gt.hi.ndf.male <- mod.1.sp.hi.ndf.male
mod.2.gt.hi.ndf.male <- mod.2.sp.hi.ndf.male
mod.3.gt.hi.ndf.male <- mod.3.sp.hi.ndf.male
mod.4.gt.hi.ndf.male <- mod.4.sp.hi.ndf.male
mod.5.gt.hi.ndf.male <- mod.5.sp.hi.ndf.male
mod.6.gt.hi.ndf.male <- mod.6.sp.hi.ndf.male
mod.7.gt.hi.ndf.male <- mod.7.sp.hi.ndf.male
mod.8.gt.hi.ndf.male <- mod.8.sp.hi.ndf.male
mod.9.gt.hi.ndf.male <- mod.9.sp.hi.ndf.male


# high NDF - female
mod.1.gt.hi.ndf.female <- mod.1.sp.hi.ndf.female
mod.2.gt.hi.ndf.female <- mod.2.sp.hi.ndf.female
mod.3.gt.hi.ndf.female <- mod.3.sp.hi.ndf.female
mod.4.gt.hi.ndf.female <- mod.4.sp.hi.ndf.female
mod.5.gt.hi.ndf.female <- mod.5.sp.hi.ndf.female
mod.6.gt.hi.ndf.female <- mod.6.sp.hi.ndf.female
mod.7.gt.hi.ndf.female <- mod.7.sp.hi.ndf.female
mod.8.gt.hi.ndf.female <- mod.8.sp.hi.ndf.female
mod.9.gt.hi.ndf.female <- mod.9.sp.hi.ndf.female

} # GOATS


# ~ ------ ~ DATAFRAME INSERTIONS


{
  
{
  model.cond <- d.reg.out$mod.class == model.class.GBR
  
  # low NDF - B ind - male & d.reg.out$mod.class == model.class.GBR
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 1 &    model.cond , vn.formula] <- listify(  GBR.mod.1.bv.lo.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 2 &    model.cond, vn.formula] <- listify( GBR.mod.2.bv.lo.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 3 &    model.cond, vn.formula] <- listify( GBR.mod.3.bv.lo.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 4 &    model.cond, vn.formula] <- listify( GBR.mod.4.bv.lo.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  GBR.mod.5.bv.lo.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 6 &    model.cond, vn.formula] <- listify( GBR.mod.6.bv.lo.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 7 &    model.cond, vn.formula] <- listify( GBR.mod.7.bv.lo.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 8 &    model.cond, vn.formula] <- listify( GBR.mod.8.bv.lo.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( GBR.mod.9.bv.lo.ndf.Bind.male )
  
  # low NDF - B ind - female
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  GBR.mod.1.bv.lo.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 2 &    model.cond , vn.formula] <- listify( GBR.mod.2.bv.lo.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 3 &    model.cond , vn.formula] <- listify( GBR.mod.3.bv.lo.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 4 &    model.cond , vn.formula] <- listify( GBR.mod.4.bv.lo.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  GBR.mod.5.bv.lo.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 6 &    model.cond , vn.formula] <- listify( GBR.mod.6.bv.lo.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 7 &    model.cond , vn.formula] <- listify( GBR.mod.7.bv.lo.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 8 &    model.cond , vn.formula] <- listify( GBR.mod.8.bv.lo.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( GBR.mod.9.bv.lo.ndf.Bind.female )
  
  
  
  # low NDF - B tau - male
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  GBR.mod.1.bv.lo.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 2  &    model.cond, vn.formula] <- listify( GBR.mod.2.bv.lo.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 3  &    model.cond, vn.formula] <- listify( GBR.mod.3.bv.lo.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 4 &    model.cond , vn.formula] <- listify( GBR.mod.4.bv.lo.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  GBR.mod.5.bv.lo.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 6  &    model.cond, vn.formula] <- listify( GBR.mod.6.bv.lo.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 7  &    model.cond, vn.formula] <- listify( GBR.mod.7.bv.lo.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 8  &    model.cond, vn.formula] <- listify( GBR.mod.8.bv.lo.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( GBR.mod.9.bv.lo.ndf.Btau.male )
  
  # low NDF - B tau - female
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  GBR.mod.1.bv.lo.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 2 &    model.cond , vn.formula] <- listify( GBR.mod.2.bv.lo.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 3 &    model.cond , vn.formula] <- listify( GBR.mod.3.bv.lo.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 4  &    model.cond, vn.formula] <- listify( GBR.mod.4.bv.lo.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  GBR.mod.5.bv.lo.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 6  &    model.cond, vn.formula] <- listify( GBR.mod.6.bv.lo.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 7  &    model.cond, vn.formula] <- listify( GBR.mod.7.bv.lo.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 8  &    model.cond, vn.formula] <- listify( GBR.mod.8.bv.lo.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( GBR.mod.9.bv.lo.ndf.Btau.female )
  
  
  # High NDF - B ind - male
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  GBR.mod.1.bv.hi.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 2 &    model.cond , vn.formula] <- listify( GBR.mod.2.bv.hi.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 3  &    model.cond, vn.formula] <- listify( GBR.mod.3.bv.hi.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 4  &    model.cond, vn.formula] <- listify( GBR.mod.4.bv.hi.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  GBR.mod.5.bv.hi.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 6  &    model.cond, vn.formula] <- listify( GBR.mod.6.bv.hi.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 7  &    model.cond, vn.formula] <- listify( GBR.mod.7.bv.hi.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 8  &    model.cond, vn.formula] <- listify( GBR.mod.8.bv.hi.ndf.Bind.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( GBR.mod.9.bv.hi.ndf.Bind.male )
  
  # low NDF - B ind - female
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  GBR.mod.1.bv.hi.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 2 &    model.cond , vn.formula] <- listify( GBR.mod.2.bv.hi.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 3  &    model.cond, vn.formula] <- listify( GBR.mod.3.bv.hi.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 4 &    model.cond , vn.formula] <- listify( GBR.mod.4.bv.hi.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  GBR.mod.5.bv.hi.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 6  &    model.cond, vn.formula] <- listify( GBR.mod.6.bv.hi.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 7  &    model.cond, vn.formula] <- listify( GBR.mod.7.bv.hi.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 8  &    model.cond, vn.formula] <- listify( GBR.mod.8.bv.hi.ndf.Bind.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( GBR.mod.9.bv.hi.ndf.Bind.female )
  
  
  
  # low NDF - B tau - male
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  GBR.mod.1.bv.hi.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 2  &    model.cond, vn.formula] <- listify( GBR.mod.2.bv.hi.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 3  &    model.cond, vn.formula] <- listify( GBR.mod.3.bv.hi.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 4  &    model.cond, vn.formula] <- listify( GBR.mod.4.bv.hi.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  GBR.mod.5.bv.hi.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 6  &    model.cond, vn.formula] <- listify( GBR.mod.6.bv.hi.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 7  &    model.cond, vn.formula] <- listify( GBR.mod.7.bv.hi.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 8  &    model.cond, vn.formula] <- listify( GBR.mod.8.bv.hi.ndf.Btau.male )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( GBR.mod.9.bv.hi.ndf.Btau.male )
  
  # low NDF - B ind - female
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  GBR.mod.1.bv.hi.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 2  &    model.cond, vn.formula] <- listify( GBR.mod.2.bv.hi.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 3  &    model.cond, vn.formula] <- listify( GBR.mod.3.bv.hi.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 4  &    model.cond, vn.formula] <- listify( GBR.mod.4.bv.hi.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  GBR.mod.5.bv.hi.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 6  &    model.cond, vn.formula] <- listify( GBR.mod.6.bv.hi.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 7  &    model.cond, vn.formula] <- listify( GBR.mod.7.bv.hi.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 8  &    model.cond, vn.formula] <- listify( GBR.mod.8.bv.hi.ndf.Btau.female )
  d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( GBR.mod.9.bv.hi.ndf.Btau.female )
  
  
  }  # GRADIENT BOOSTING
  
{
    model.cond <- d.reg.out$mod.class == model.class.ME
    
    # low NDF - B ind - male & d.reg.out$mod.class == model.class.ME
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 1 &    model.cond , vn.formula] <- listify(  ME.mod.1.bv.lo.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 2 &    model.cond, vn.formula] <- listify( ME.mod.2.bv.lo.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 3 &    model.cond, vn.formula] <- listify( ME.mod.3.bv.lo.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 4 &    model.cond, vn.formula] <- listify( ME.mod.4.bv.lo.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  ME.mod.5.bv.lo.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 6 &    model.cond, vn.formula] <- listify( ME.mod.6.bv.lo.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 7 &    model.cond, vn.formula] <- listify( ME.mod.7.bv.lo.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 8 &    model.cond, vn.formula] <- listify( ME.mod.8.bv.lo.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.male & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( ME.mod.9.bv.lo.ndf.Bind.male )
    
    # low NDF - B ind - female
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  ME.mod.1.bv.lo.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 2 &    model.cond , vn.formula] <- listify( ME.mod.2.bv.lo.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 3 &    model.cond , vn.formula] <- listify( ME.mod.3.bv.lo.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 4 &    model.cond , vn.formula] <- listify( ME.mod.4.bv.lo.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  ME.mod.5.bv.lo.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 6 &    model.cond , vn.formula] <- listify( ME.mod.6.bv.lo.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 7 &    model.cond , vn.formula] <- listify( ME.mod.7.bv.lo.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 8 &    model.cond , vn.formula] <- listify( ME.mod.8.bv.lo.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Bind.female & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( ME.mod.9.bv.lo.ndf.Bind.female )
    
    
    
    # low NDF - B tau - male
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  ME.mod.1.bv.lo.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 2  &    model.cond, vn.formula] <- listify( ME.mod.2.bv.lo.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 3  &    model.cond, vn.formula] <- listify( ME.mod.3.bv.lo.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 4 &    model.cond , vn.formula] <- listify( ME.mod.4.bv.lo.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  ME.mod.5.bv.lo.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 6  &    model.cond, vn.formula] <- listify( ME.mod.6.bv.lo.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 7  &    model.cond, vn.formula] <- listify( ME.mod.7.bv.lo.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 8  &    model.cond, vn.formula] <- listify( ME.mod.8.bv.lo.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.male & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( ME.mod.9.bv.lo.ndf.Btau.male )
    
    # low NDF - B tau - female
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  ME.mod.1.bv.lo.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 2 &    model.cond , vn.formula] <- listify( ME.mod.2.bv.lo.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 3 &    model.cond , vn.formula] <- listify( ME.mod.3.bv.lo.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 4  &    model.cond, vn.formula] <- listify( ME.mod.4.bv.lo.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  ME.mod.5.bv.lo.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 6  &    model.cond, vn.formula] <- listify( ME.mod.6.bv.lo.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 7  &    model.cond, vn.formula] <- listify( ME.mod.7.bv.lo.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 8  &    model.cond, vn.formula] <- listify( ME.mod.8.bv.lo.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.lo.ndf.Btau.female & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( ME.mod.9.bv.lo.ndf.Btau.female )
    
    
    # High NDF - B ind - male
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  ME.mod.1.bv.hi.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 2 &    model.cond , vn.formula] <- listify( ME.mod.2.bv.hi.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 3  &    model.cond, vn.formula] <- listify( ME.mod.3.bv.hi.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 4  &    model.cond, vn.formula] <- listify( ME.mod.4.bv.hi.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  ME.mod.5.bv.hi.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 6  &    model.cond, vn.formula] <- listify( ME.mod.6.bv.hi.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 7  &    model.cond, vn.formula] <- listify( ME.mod.7.bv.hi.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 8  &    model.cond, vn.formula] <- listify( ME.mod.8.bv.hi.ndf.Bind.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.male & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( ME.mod.9.bv.hi.ndf.Bind.male )
    
    # low NDF - B ind - female
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  ME.mod.1.bv.hi.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 2 &    model.cond , vn.formula] <- listify( ME.mod.2.bv.hi.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 3  &    model.cond, vn.formula] <- listify( ME.mod.3.bv.hi.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 4 &    model.cond , vn.formula] <- listify( ME.mod.4.bv.hi.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  ME.mod.5.bv.hi.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 6  &    model.cond, vn.formula] <- listify( ME.mod.6.bv.hi.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 7  &    model.cond, vn.formula] <- listify( ME.mod.7.bv.hi.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 8  &    model.cond, vn.formula] <- listify( ME.mod.8.bv.hi.ndf.Bind.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Bind.female & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( ME.mod.9.bv.hi.ndf.Bind.female )
    
    
    
    # low NDF - B tau - male
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  ME.mod.1.bv.hi.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 2  &    model.cond, vn.formula] <- listify( ME.mod.2.bv.hi.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 3  &    model.cond, vn.formula] <- listify( ME.mod.3.bv.hi.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 4  &    model.cond, vn.formula] <- listify( ME.mod.4.bv.hi.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  ME.mod.5.bv.hi.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 6  &    model.cond, vn.formula] <- listify( ME.mod.6.bv.hi.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 7  &    model.cond, vn.formula] <- listify( ME.mod.7.bv.hi.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 8  &    model.cond, vn.formula] <- listify( ME.mod.8.bv.hi.ndf.Btau.male )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.male & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( ME.mod.9.bv.hi.ndf.Btau.male )
    
    # low NDF - B ind - female
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 1 &    model.cond, vn.formula] <- listify(  ME.mod.1.bv.hi.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 2  &    model.cond, vn.formula] <- listify( ME.mod.2.bv.hi.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 3  &    model.cond, vn.formula] <- listify( ME.mod.3.bv.hi.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 4  &    model.cond, vn.formula] <- listify( ME.mod.4.bv.hi.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 5 &    model.cond, vn.formula] <- listify(  ME.mod.5.bv.hi.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 6  &    model.cond, vn.formula] <- listify( ME.mod.6.bv.hi.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 7  &    model.cond, vn.formula] <- listify( ME.mod.7.bv.hi.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 8  &    model.cond, vn.formula] <- listify( ME.mod.8.bv.hi.ndf.Btau.female )
    d.reg.out[reg.out.cond.bv.hi.ndf.Btau.female & d.reg.out$mod.form == 9 &    model.cond, vn.formula] <- listify( ME.mod.9.bv.hi.ndf.Btau.female )
    
    
  }  # MIXED EFFECTS
  
} # BOVINES



{
  # Sheep low NDF male
  d.reg.out[reg.out.cond.shp.lo.ndf.male & d.reg.out$mod.form == 1, vn.formula] <- listify(  mod.1.sp.lo.ndf.male )
  d.reg.out[reg.out.cond.shp.lo.ndf.male & d.reg.out$mod.form == 2 , vn.formula] <- listify( mod.2.sp.lo.ndf.male )
  d.reg.out[reg.out.cond.shp.lo.ndf.male & d.reg.out$mod.form == 3 , vn.formula] <- listify( mod.3.sp.lo.ndf.male )
  d.reg.out[reg.out.cond.shp.lo.ndf.male & d.reg.out$mod.form == 4 , vn.formula] <- listify( mod.4.sp.lo.ndf.male )
  d.reg.out[reg.out.cond.shp.lo.ndf.male & d.reg.out$mod.form == 5, vn.formula] <- listify(  mod.5.sp.lo.ndf.male )
  d.reg.out[reg.out.cond.shp.lo.ndf.male & d.reg.out$mod.form == 6 , vn.formula] <- listify( mod.6.sp.lo.ndf.male )
  d.reg.out[reg.out.cond.shp.lo.ndf.male & d.reg.out$mod.form == 7 , vn.formula] <- listify( mod.7.sp.lo.ndf.male )
  d.reg.out[reg.out.cond.shp.lo.ndf.male & d.reg.out$mod.form == 8 , vn.formula] <- listify( mod.8.sp.lo.ndf.male )
  d.reg.out[reg.out.cond.shp.lo.ndf.male & d.reg.out$mod.form == 9, vn.formula] <- listify( mod.9.sp.lo.ndf.male )
  
  
  # Sheep low ndf female
  d.reg.out[reg.out.cond.shp.lo.ndf.female & d.reg.out$mod.form == 1, vn.formula] <- listify(  mod.1.sp.lo.ndf.female )
  d.reg.out[reg.out.cond.shp.lo.ndf.female & d.reg.out$mod.form == 2 , vn.formula] <- listify( mod.2.sp.lo.ndf.female )
  d.reg.out[reg.out.cond.shp.lo.ndf.female & d.reg.out$mod.form == 3 , vn.formula] <- listify( mod.3.sp.lo.ndf.female )
  d.reg.out[reg.out.cond.shp.lo.ndf.female & d.reg.out$mod.form == 4 , vn.formula] <- listify( mod.4.sp.lo.ndf.female )
  d.reg.out[reg.out.cond.shp.lo.ndf.female & d.reg.out$mod.form == 5, vn.formula] <- listify(  mod.5.sp.lo.ndf.female )
  d.reg.out[reg.out.cond.shp.lo.ndf.female & d.reg.out$mod.form == 6 , vn.formula] <- listify( mod.6.sp.lo.ndf.female )
  d.reg.out[reg.out.cond.shp.lo.ndf.female & d.reg.out$mod.form == 7 , vn.formula] <- listify( mod.7.sp.lo.ndf.female )
  d.reg.out[reg.out.cond.shp.lo.ndf.female & d.reg.out$mod.form == 8 , vn.formula] <- listify( mod.8.sp.lo.ndf.female )
  d.reg.out[reg.out.cond.shp.lo.ndf.female & d.reg.out$mod.form == 9, vn.formula] <- listify( mod.9.sp.lo.ndf.female )
  
  
  # Sheep hi ndf male
  d.reg.out[reg.out.cond.shp.hi.ndf.male & d.reg.out$mod.form == 1 , vn.formula] <- listify( mod.1.sp.hi.ndf.male )
  d.reg.out[reg.out.cond.shp.hi.ndf.male  & d.reg.out$mod.form == 2, vn.formula] <-  listify( mod.2.sp.hi.ndf.male )
  d.reg.out[reg.out.cond.shp.hi.ndf.male  & d.reg.out$mod.form == 3, vn.formula] <-  listify( mod.3.sp.hi.ndf.male  )
  d.reg.out[reg.out.cond.shp.hi.ndf.male  & d.reg.out$mod.form == 4, vn.formula] <-  listify( mod.4.sp.hi.ndf.male  )
  d.reg.out[reg.out.cond.shp.hi.ndf.male & d.reg.out$mod.form == 5 , vn.formula] <- listify( mod.5.sp.hi.ndf.male )
  d.reg.out[reg.out.cond.shp.hi.ndf.male  & d.reg.out$mod.form == 6, vn.formula] <-  listify( mod.6.sp.hi.ndf.male  )
  d.reg.out[reg.out.cond.shp.hi.ndf.male  & d.reg.out$mod.form == 7, vn.formula] <-  listify( mod.7.sp.hi.ndf.male  )
  d.reg.out[reg.out.cond.shp.hi.ndf.male  & d.reg.out$mod.form == 8, vn.formula] <-  listify( mod.8.sp.hi.ndf.male )
  d.reg.out[reg.out.cond.shp.hi.ndf.male  & d.reg.out$mod.form == 9, vn.formula] <-  listify( mod.9.sp.hi.ndf.male )
  
  # Sheep hi ndf female
  d.reg.out[reg.out.cond.shp.hi.ndf.female  & d.reg.out$mod.form == 1 , vn.formula] <-  listify( mod.1.sp.hi.ndf.female )
  d.reg.out[reg.out.cond.shp.hi.ndf.female  & d.reg.out$mod.form == 2, vn.formula] <-  listify( mod.2.sp.hi.ndf.female )
  d.reg.out[reg.out.cond.shp.hi.ndf.female  & d.reg.out$mod.form == 3, vn.formula] <-  listify( mod.3.sp.hi.ndf.female )
  d.reg.out[reg.out.cond.shp.hi.ndf.female  & d.reg.out$mod.form == 4, vn.formula] <-  listify( mod.4.sp.hi.ndf.female )
  d.reg.out[reg.out.cond.shp.hi.ndf.female  & d.reg.out$mod.form == 5 , vn.formula] <-  listify( mod.5.sp.hi.ndf.female )
  d.reg.out[reg.out.cond.shp.hi.ndf.female  & d.reg.out$mod.form == 6, vn.formula] <-  listify( mod.6.sp.hi.ndf.female )
  d.reg.out[reg.out.cond.shp.hi.ndf.female  & d.reg.out$mod.form == 7, vn.formula] <-  listify( mod.7.sp.hi.ndf.female )
  d.reg.out[reg.out.cond.shp.hi.ndf.female  & d.reg.out$mod.form == 8, vn.formula] <-  listify( mod.8.sp.hi.ndf.female )
  d.reg.out[reg.out.cond.shp.hi.ndf.female  & d.reg.out$mod.form == 9, vn.formula] <-  listify( mod.9.sp.hi.ndf.female )
  
} # SHEEP

{
  
  d.reg.out[ reg.out.cond.gt.lo.ndf.male & d.reg.out$mod.form == 1, vn.formula] <- listify( mod.1.gt.lo.ndf.male)
  d.reg.out[ reg.out.cond.gt.lo.ndf.male  & d.reg.out$mod.form == 2, vn.formula] <-  listify( mod.2.gt.lo.ndf.male)
  d.reg.out[ reg.out.cond.gt.lo.ndf.male  & d.reg.out$mod.form == 3, vn.formula] <-  listify( mod.3.gt.lo.ndf.male)
  d.reg.out[ reg.out.cond.gt.lo.ndf.male  & d.reg.out$mod.form == 4, vn.formula] <-  listify( mod.4.gt.lo.ndf.male)
  d.reg.out[ reg.out.cond.gt.lo.ndf.male  & d.reg.out$mod.form == 5, vn.formula] <- listify( mod.5.gt.lo.ndf.male)
  d.reg.out[ reg.out.cond.gt.lo.ndf.male  & d.reg.out$mod.form == 6, vn.formula] <-  listify( mod.6.gt.lo.ndf.male)
  d.reg.out[ reg.out.cond.gt.lo.ndf.male  & d.reg.out$mod.form == 7, vn.formula] <-  listify( mod.7.gt.lo.ndf.male)
  d.reg.out[ reg.out.cond.gt.lo.ndf.male  & d.reg.out$mod.form == 8, vn.formula] <-  listify( mod.8.gt.lo.ndf.male)
  d.reg.out[ reg.out.cond.gt.lo.ndf.male  & d.reg.out$mod.form == 9, vn.formula] <-  listify( mod.9.gt.lo.ndf.male)
  
  
  d.reg.out[ reg.out.cond.gt.lo.ndf.female & d.reg.out$mod.form == 1, vn.formula] <- listify( mod.1.gt.lo.ndf.female )
  d.reg.out[ reg.out.cond.gt.lo.ndf.female  & d.reg.out$mod.form == 2, vn.formula] <-  listify( mod.2.gt.lo.ndf.female )
  d.reg.out[ reg.out.cond.gt.lo.ndf.female  & d.reg.out$mod.form == 3, vn.formula] <-  listify( mod.3.gt.lo.ndf.female )
  d.reg.out[ reg.out.cond.gt.lo.ndf.female  & d.reg.out$mod.form == 4, vn.formula] <-  listify( mod.4.gt.lo.ndf.female )
  d.reg.out[ reg.out.cond.gt.lo.ndf.female  & d.reg.out$mod.form == 5, vn.formula] <- listify( mod.5.gt.lo.ndf.female )
  d.reg.out[ reg.out.cond.gt.lo.ndf.female  & d.reg.out$mod.form == 6, vn.formula] <-  listify( mod.6.gt.lo.ndf.female )
  d.reg.out[ reg.out.cond.gt.lo.ndf.female  & d.reg.out$mod.form == 7, vn.formula] <-  listify( mod.7.gt.lo.ndf.female )
  d.reg.out[ reg.out.cond.gt.lo.ndf.female  & d.reg.out$mod.form == 8, vn.formula] <-  listify( mod.8.gt.lo.ndf.female )
  d.reg.out[ reg.out.cond.gt.lo.ndf.female  & d.reg.out$mod.form == 9, vn.formula] <-  listify( mod.9.gt.lo.ndf.female )
  
  
  d.reg.out[ reg.out.cond.gt.hi.ndf.male  & d.reg.out$mod.form == 1, vn.formula] <-  listify( mod.1.gt.hi.ndf.male )
  d.reg.out[ reg.out.cond.gt.hi.ndf.male  & d.reg.out$mod.form == 2, vn.formula] <-  listify( mod.2.gt.hi.ndf.male )
  d.reg.out[ reg.out.cond.gt.hi.ndf.male  & d.reg.out$mod.form == 3, vn.formula] <-  listify( mod.3.gt.hi.ndf.male )
  d.reg.out[ reg.out.cond.gt.hi.ndf.male  & d.reg.out$mod.form == 4, vn.formula] <-  listify( mod.4.gt.hi.ndf.male )
  d.reg.out[ reg.out.cond.gt.hi.ndf.male  & d.reg.out$mod.form == 5, vn.formula] <-  listify( mod.5.gt.hi.ndf.male )
  d.reg.out[ reg.out.cond.gt.hi.ndf.male  & d.reg.out$mod.form == 6, vn.formula] <-  listify( mod.6.gt.hi.ndf.male )
  d.reg.out[ reg.out.cond.gt.hi.ndf.male  & d.reg.out$mod.form == 7, vn.formula] <-  listify( mod.7.gt.hi.ndf.male )
  d.reg.out[ reg.out.cond.gt.hi.ndf.male  & d.reg.out$mod.form == 8, vn.formula] <-  listify( mod.8.gt.hi.ndf.male )
  d.reg.out[ reg.out.cond.gt.hi.ndf.male  & d.reg.out$mod.form == 9, vn.formula] <-  listify( mod.9.gt.hi.ndf.male )
  
  
  d.reg.out[ reg.out.cond.gt.hi.ndf.female & d.reg.out$mod.form == 1, vn.formula] <- listify( mod.1.gt.hi.ndf.female )
  d.reg.out[ reg.out.cond.gt.hi.ndf.female  & d.reg.out$mod.form == 2, vn.formula] <-  listify( mod.2.gt.hi.ndf.female )
  d.reg.out[ reg.out.cond.gt.hi.ndf.female  & d.reg.out$mod.form == 3, vn.formula] <-  listify( mod.3.gt.hi.ndf.female )
  d.reg.out[ reg.out.cond.gt.hi.ndf.female  & d.reg.out$mod.form == 4, vn.formula] <-  listify( mod.4.gt.hi.ndf.female )
  d.reg.out[ reg.out.cond.gt.hi.ndf.female  & d.reg.out$mod.form == 5, vn.formula] <- listify( mod.5.gt.hi.ndf.female )
  d.reg.out[ reg.out.cond.gt.hi.ndf.female  & d.reg.out$mod.form == 6, vn.formula] <-  listify( mod.6.gt.hi.ndf.female )
  d.reg.out[ reg.out.cond.gt.hi.ndf.female  & d.reg.out$mod.form == 7, vn.formula] <-  listify( mod.7.gt.hi.ndf.female )
  d.reg.out[ reg.out.cond.gt.hi.ndf.female  & d.reg.out$mod.form == 8, vn.formula] <-  listify( mod.8.gt.hi.ndf.female )
  d.reg.out[ reg.out.cond.gt.hi.ndf.female  & d.reg.out$mod.form == 9, vn.formula] <-  listify( mod.9.gt.hi.ndf.female )
  
} # GOATS



if ( random.exp.int){ 
  for (r in 1:nrow(d.reg.out)){
    
    form <- de.listify( d.reg.out[r , vn.formula])
    
    form.with.ue.id <- listify( update(  form , . ~ . + ue.id) )
    
    d.reg.out[r , vn.formula] <- form.with.ue.id 
    
  }} # If including random intercepts, add 'ue.id' to all formulae



d.reg.out.null <- d.reg.out



