



# Conditions specification

# Goats
# Breed
cnd.gt.indg <- (reg.dat$Species == species.goat  & reg.dat$breed.type == 1 ) 
cnd.gt.cbred <- (reg.dat$Species == species.goat  & reg.dat$breed.type == 2 ) 
cnd.gt.breed.all <- (reg.dat$Species == species.goat  & reg.dat$breed.type %in% c(1,2) & !is.na(reg.dat$breed.type) ) 

# Low vs. high NDF
cnd.gt.hi.ndf <- (reg.dat$Species == 'Goat' & reg.dat$ndf.level == ndf.lev.hi & !is.na(reg.dat$ndf.level)) 
cnd.gt.lo.ndf <- (reg.dat$Species == 'Goat' & reg.dat$ndf.level == ndf.lev.lo & !is.na(reg.dat$ndf.level))
cnd.gt.ndf.all <-   !is.na(reg.dat$Species == 'Goat'  & !is.na(reg.dat$ndf.level)) 

# Growing vs. mature
cnd.gt.growing <- (reg.dat$Species == 'Goat' & !reg.dat$is.adult ) 
cnd.gt.mature <- (reg.dat$Species == 'Goat' &  reg.dat$is.adult) 
cnd.gt.age.all <-   (!is.na(reg.dat$is.adult) & reg.dat$Species == 'Goat' ) 

# Male vs. female
cnd.gt.male <- (reg.dat$is.male  ) 
cnd.gt.female <- ( !reg.dat$is.male  ) 
cnd.gt.sex.all <-   !is.na(reg.dat$is.male ) 


# Sheep
# Breed
cnd.sp.indg <- (reg.dat$Species == species.sheep  & reg.dat$breed.type == 1) 
cnd.sp.cbred <- (reg.dat$Species == species.sheep  & reg.dat$breed.type == 2) 
cnd.sp.breed.all <- (reg.dat$Species == species.sheep  & reg.dat$breed.type %in% c(1,2) & !is.na(reg.dat$breed.type) ) 


# Low vs. high NDF
cnd.sp.hi.ndf <- (reg.dat$species.ndf == 'Sheep - High NDF' ) 
cnd.sp.lo.ndf <- (reg.dat$species.ndf == 'Sheep - Low NDF' ) 
cnd.sp.ndf.all <-   !is.na(reg.dat$species.ndf  ) 

# Growing vs. mature
cnd.sp.growing <- (!reg.dat$is.adult & reg.dat$Species == "Sheep" ) 
cnd.sp.mature <- ( reg.dat$is.adult & reg.dat$Species == "Sheep") 
cnd.sp.age.all <-   !is.na(reg.dat$is.adult & reg.dat$Species == "Sheep") 

# Male vs. female
# Sheep
cnd.sp.male <- (reg.dat$is.male  & reg.dat$Species == "Sheep" & reg.dat$ndf.level == ndf.lev.hi ) 
cnd.sp.female <- ( !reg.dat$is.male  & reg.dat$Species == "Sheep" & reg.dat$ndf.level == ndf.lev.hi ) 
cnd.sp.sex.all <-   !is.na(reg.dat$is.male & reg.dat$Species == "Sheep" & reg.dat$ndf.level == ndf.lev.hi ) 



# Cattle
# Breed
cnd.bv.indg <- (reg.dat$Species == species.cattle & reg.dat$breed.type == 1) 
cnd.bv.cbred <- (reg.dat$Species == species.cattle  & reg.dat$breed.type == 2) 
cnd.bv.breed.all <- (reg.dat$Species == species.cattle  & reg.dat$breed.type %in% c(1,2) & !is.na(reg.dat$breed.type) ) 


# Low vs. high NDF
cnd.bv.hi.ndf <- ( !is.na(reg.dat$species.ndf) & reg.dat$species.ndf == 'Cattle - High NDF' ) 
cnd.bv.lo.ndf <- (!is.na(reg.dat$species.ndf) & reg.dat$species.ndf == 'Cattle - Low NDF' ) 
cnd.bv.ndf.all <-   !is.na(reg.dat$species.ndf  ) 

# Growing vs. mature
cnd.sp.growing <- (!reg.dat$is.adult & reg.dat$Species == species.cattle ) 
cnd.sp.mature <- ( reg.dat$is.adult & reg.dat$Species == species.cattle) 
cnd.sp.age.all <-   !is.na(reg.dat$is.adult & reg.dat$Species == species.cattle) 

# Male vs. female
cnd.bv.male <- (reg.dat$is.male  & reg.dat$Species == species.cattle & reg.dat$ndf.level == ndf.lev.hi & !is.na(reg.dat$ndf.level == ndf.lev.hi ) )  
cnd.bv.female <- ( !reg.dat$is.male  & reg.dat$Species == species.cattle & reg.dat$ndf.level == ndf.lev.hi & !is.na(reg.dat$ndf.level == ndf.lev.hi )) 
cnd.bv.sex.all <-   !is.na(reg.dat$is.male & reg.dat$Species == species.cattle & reg.dat$ndf.level == ndf.lev.hi & !is.na(reg.dat$ndf.level == ndf.lev.hi )) 



formula.me.all <-    as.formula(
  feed_intake_g_d
~
  bw_kg 
#+   bw_kg^2
+ adg_g_day
#+ adg_g_day ^2
#+ CP_nutrition
#+ NDF_nutrition
+ ( 1 | ue.id))


formula.gbr.all <-    as.formula(
  feed_intake_g_d
  ~
    bw_kg 
  #+   bw_kg^2
  + adg_g_day
  #+ adg_g_day ^2
  #+ CP_nutrition
  #+ NDF_nutrition
  + ( 1 | ue.id))





formula.all.fe <-    as.formula(
  feed_intake_g_d
  ~
    bw_kg
  #+   bw_kg^2
 # + adg_g_day
  #+ adg_g_day ^2
  #+ CP_nutrition
   + NDF_nutrition
  )

formula.all.sex <- as.formula(feed_intake_g_d
                             ~
                               bw_kg 
                             #+   bw_kg^2
                             + adg_g_day 
                             
                             + is.male
                             #+ adg_g_day ^2
                             #   + CP_nutrition
                             #+ NDF_nutrition
                             
                             + ( 1 | ue.id)
                             
)

formula.red <- as.formula(feed_intake_g_d
                          ~
                            bw_kg
                          #+   bw_kg^2
                          + adg_g_day
                          #+ adg_g_day ^2
                       #   + CP_nutrition
                          #+ NDF_nutrition
                         
                        + ( 1 | ue.id)
                       
                       )



# df = predictors + 2 = 6 + 2 = 8 
# significance = 0.05
# threshold for rejection: > 12.6


# ~~~~~~ BREED ~~~~~~~
# Sheep
{
  
{
  form.sp.me.indg.only <-  lm( 
    
    formula.me.all
    
    , data =  reg.dat[cnd.sp.indg,]
    
  )
  
  

  LL.sp.me.indg <- as.numeric(logLik(form.sp.me.indg.only))
  
  
  form.sp.me.cbred.only <-  lm( 
    
    formula.me.all
    
    , data =  reg.dat[cnd.sp.cbred,]
    
  )
  
  LL.sp.me.cbred <- as.numeric(logLik(  form.sp.me.cbred.only))
  
  
  form.sp.me.all <-  lm( 
    
    formula.me.all
    
    , data =  reg.dat[  cnd.sp.breed.all ,]
    
  )
  
  
  LL.sp.me.all <- as.numeric(logLik(form.sp.me.all))
  
  LR.sp.me.breed <- -2 * (LL.sp.me.all  - (LL.sp.me.indg + LL.sp.me.cbred ))
  LR.sp.me.breed
  
} # ME
  
{
  
form.gbr.sp.indg.only <-   glmboost( 

formula.all.fe

, data = reg.dat[cnd.sp.indg,]

# , family =  family
,  control = boost_control(mstop =   1000 , nu = 0.1)
, center = FALSE
)

    
LL.gbr.sp.indg <- as.numeric(logLik(form.gbr.sp.indg.only ))
  
    
form.gbr.sp.cb.only <-   glmboost( 
  
  formula.all.fe
  
  , data = reg.dat[cnd.sp.cbred,]
  
  # , family =  family
  ,  control = boost_control(mstop =   1000 , nu = 0.1)
  , center = FALSE
)


LL.gbr.sp.cb <- as.numeric(logLik(form.gbr.sp.cb.only ))
    


form.gbr.sp.breed.all <-  glmboost( 
  
  formula.all.fe
  
  , data = reg.dat[cnd.sp.breed.all,]
  
  # , family =  family
  ,  control = boost_control(mstop =   1000 , nu = 0.1)
  , center = FALSE
)


LL.gbr.sp.breed.all <- as.numeric(logLik(form.gbr.sp.breed.all ))

    
    LR.sp.gb.breed <- -2 * (LL.gbr.sp.breed.all  - ( LL.gbr.sp.indg + LL.gbr.sp.cb ))
    LR.sp.gb.breed
    
  } # GB
  
}

# Goats
{
  
{
form.gt.me.indg.only <-  lm( 

formula.me.all

, data =  reg.dat[cnd.gt.indg,]

)



LL.gt.me.indg <- as.numeric(logLik(form.gt.me.indg.only))


form.gt.me.cbred.only <-  lm( 

formula.me.all

, data =  reg.dat[cnd.gt.cbred,]

)

LL.gt.me.cbred <- as.numeric(logLik(  form.gt.me.cbred.only))


form.gt.me.all <-  lm( 

formula.me.all

, data =  reg.dat[  cnd.gt.breed.all ,]

)


LL.gt.me.all <- as.numeric(logLik(form.gt.me.all))

LR.gt.me.breed <- -2 * (LL.gt.me.all  - (LL.gt.me.indg + LL.gt.me.cbred ))
LR.gt.me.breed

} # ME

{

form.gbr.gt.indg.only <-   glmboost( 

formula.all.fe

, data = reg.dat[cnd.gt.indg,]

# , family =  family
,  control = boost_control(mstop =   1000 , nu = 0.1)
, center = FALSE
)


LL.gbr.gt.indg <- as.numeric(logLik(form.gbr.gt.indg.only ))


form.gbr.gt.cb.only <-   glmboost( 

formula.all.fe

, data = reg.dat[cnd.gt.cbred,]

# , family =  family
,  control = boost_control(mstop =   1000 , nu = 0.1)
, center = FALSE
)


LL.gbr.gt.cb <- as.numeric(logLik(form.gbr.gt.cb.only ))



form.gbr.gt.breed.all <-  glmboost( 

formula.all.fe

, data = reg.dat[cnd.gt.breed.all,]

# , family =  family
,  control = boost_control(mstop =   1000 , nu = 0.1)
, center = FALSE
)


LL.gbr.gt.breed.all <- as.numeric(logLik(form.gbr.gt.breed.all ))


LR.gt.gb.breed <- -2 * (LL.gbr.gt.breed.all  - ( LL.gbr.gt.indg + LL.gbr.gt.cb ))
LR.gt.gb.breed

} # GB

}

# Cattle
{
  
{
    form.bv.me.indg.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.bv.indg,]
      
    )
    
    
    
    LL.bv.me.indg <- as.numeric(logLik(form.bv.me.indg.only))
    
    
    form.bv.me.cbred.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.bv.cbred,]
      
    )
    
    LL.bv.me.cbred <- as.numeric(logLik(  form.bv.me.cbred.only))
    
    
    form.bv.me.all <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[  cnd.bv.breed.all ,]
      
    )
    
    
    LL.bv.me.all <- as.numeric(logLik(form.bv.me.all))
    
    LR.bv.me.breed <- -2 * (LL.bv.me.all  - (LL.bv.me.indg + LL.bv.me.cbred ))
    LR.bv.me.breed
    
  } # ME
  
{
    
    form.gbr.bv.indg.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.bv.indg,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.bv.indg <- as.numeric(logLik(form.gbr.bv.indg.only ))
    
    
    form.gbr.bv.cb.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.bv.cbred,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.bv.cb <- as.numeric(logLik(form.gbr.bv.cb.only ))
    
    
    
    form.gbr.bv.breed.all <-  glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.bv.breed.all,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.bv.breed.all <- as.numeric(logLik(form.gbr.bv.breed.all ))
    
    
    LR.bv.gb.breed <- -2 * (LL.gbr.bv.breed.all  - ( LL.gbr.bv.indg + LL.gbr.bv.cb ))
    LR.bv.gb.breed
    
  } # GB
  
}

# ~~~~~~ NDF ~~~~~~~

{
  
{
form.sp.me.lo.ndf.only <-  lm( 

formula.me.all

, data =  reg.dat[cnd.sp.lo.ndf,]

)



LL.sp.me.lo.ndf <- as.numeric(logLik(form.sp.me.lo.ndf.only))


form.sp.me.hi.ndf.only <-  lm( 

formula.me.all

, data =  reg.dat[cnd.sp.hi.ndf,]

)

LL.sp.me.hi.ndf <- as.numeric(logLik(  form.sp.me.hi.ndf.only))


form.sp.me.ndf.all <-  lm( 

formula.me.all

, data =  reg.dat[  cnd.sp.ndf.all ,]

)


LL.sp.me.ndf.all <- as.numeric(logLik(form.sp.me.ndf.all))

LR.sp.me.ndf <- -2 * (    LL.sp.me.ndf.all - (LL.sp.me.lo.ndf + LL.sp.me.hi.ndf ))
LR.sp.me.ndf

} # ME
  
{

form.gbr.sp.lo.ndf.only <-   glmboost( 

formula.all.fe

, data = reg.dat[cnd.sp.lo.ndf,]

# , family =  family
,  control = boost_control(mstop =   1000 , nu = 0.1)
, center = FALSE
)


LL.gbr.sp.lo.ndf <- as.numeric(logLik(form.gbr.sp.lo.ndf.only ))


form.gbr.sp.hi.ndf.only <-   glmboost( 

formula.all.fe

, data = reg.dat[cnd.sp.hi.ndf,]

# , family =  family
,  control = boost_control(mstop =   1000 , nu = 0.1)
, center = FALSE
)


LL.gbr.sp.hi.ndf <- as.numeric(logLik(form.gbr.sp.hi.ndf.only))



form.gbr.sp.ndf.all <-  glmboost( 

formula.all.fe

, data = reg.dat[cnd.sp.ndf.all,]

# , family =  family
,  control = boost_control(mstop =   1000 , nu = 0.1)
, center = FALSE
)


LL.gbr.sp.ndf.all <- as.numeric(logLik(form.gbr.sp.ndf.all ))


LR.sp.gb.ndf <- -2 * (  LL.gbr.sp.ndf.all - ( LL.gbr.sp.lo.ndf + LL.gbr.sp.hi.ndf  ))
LR.sp.gb.ndf

} # GB
  
} # Sheep

{
  
{
    form.gt.me.lo.ndf.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.gt.lo.ndf,]
      
    )
    
    
    
    LL.gt.me.lo.ndf <- as.numeric(logLik(form.gt.me.lo.ndf.only))
    
    
    form.gt.me.hi.ndf.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.gt.hi.ndf,]
      
    )
    
    LL.gt.me.hi.ndf <- as.numeric(logLik(  form.gt.me.hi.ndf.only))
    
    
    form.gt.me.ndf.all <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[  cnd.gt.ndf.all ,]
      
    )
    
    
    LL.gt.me.ndf.all <- as.numeric(logLik(form.gt.me.ndf.all))
    
    LR.gt.me.ndf <- -2 * (    LL.gt.me.ndf.all - (LL.gt.me.lo.ndf + LL.gt.me.hi.ndf ))
    LR.gt.me.ndf
    
  } # ME
  
{
    
    form.gbr.gt.lo.ndf.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.gt.lo.ndf,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.gt.lo.ndf <- as.numeric(logLik(form.gbr.gt.lo.ndf.only ))
    
    
    form.gbr.gt.hi.ndf.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.gt.hi.ndf,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.gt.hi.ndf <- as.numeric(logLik(form.gbr.gt.hi.ndf.only))
    
    
    
    form.gbr.gt.ndf.all <-  glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.gt.ndf.all,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.gt.ndf.all <- as.numeric(logLik(form.gbr.gt.ndf.all ))
    
    
    LR.gt.gb.ndf <- -2 * (  LL.gbr.gt.ndf.all - ( LL.gbr.gt.lo.ndf + LL.gbr.gt.hi.ndf  ))
    LR.gt.gb.ndf
    
  } # GB
  
} # Goats

{
  
{
    form.bv.me.lo.ndf.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.bv.lo.ndf,]
      
    )
    
    
    
    LL.bv.me.lo.ndf <- as.numeric(logLik(form.bv.me.lo.ndf.only))
    
    
    form.bv.me.hi.ndf.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.bv.hi.ndf,]
      
    )
    
    LL.bv.me.hi.ndf <- as.numeric(logLik(  form.bv.me.hi.ndf.only))
    
    
    form.bv.me.ndf.all <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[  cnd.bv.ndf.all ,]
      
    )
    
    
    LL.bv.me.ndf.all <- as.numeric(logLik(form.bv.me.ndf.all))
    
    LR.bv.me.ndf <- -2 * (    LL.bv.me.ndf.all - (LL.bv.me.lo.ndf + LL.bv.me.hi.ndf ))
    LR.bv.me.ndf
    
  } # ME
  
{
    
form.gbr.bv.lo.ndf.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.bv.lo.ndf,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.bv.lo.ndf <- as.numeric(logLik(form.gbr.bv.lo.ndf.only ))
    
    
    form.gbr.bv.hi.ndf.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.bv.hi.ndf,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.bv.hi.ndf <- as.numeric(logLik(form.gbr.bv.hi.ndf.only))
    
    
    
    form.gbr.bv.ndf.all <-  glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.bv.ndf.all,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.bv.ndf.all <- as.numeric(logLik(form.gbr.bv.ndf.all ))
    
    
    LR.bv.gb.ndf <- -2 * (  LL.gbr.bv.ndf.all - ( LL.gbr.bv.lo.ndf + LL.gbr.bv.hi.ndf  ))
    LR.bv.gb.ndf
    
  } # GB
  
} # Cattle

# ~~~~~ Age status  ~~~~~~~~~

# Sheep
{
  
{
    form.sp.me.growing.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.sp.growing,]
      
    )
    
    
    
    LL.sp.me.growing <- as.numeric(logLik(form.sp.me.growing.only))
    
    
    form.sp.me.mature.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.sp.mature,]
      
    )
    
    LL.sp.me.mature <- as.numeric(logLik(  form.sp.me.mature.only))
    
    
    form.sp.me.age.all <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[  cnd.sp.age.all ,]
      
    )
    
    
    LL.sp.me.age.all <- as.numeric(logLik(form.sp.me.age.all))
    
    LR.sp.me.age <- -2 * (    LL.sp.me.age.all - (LL.sp.me.growing + LL.sp.me.mature ))
    LR.sp.me.age
    
  } # ME
  
{
    
    form.gbr.sp.growing.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.sp.growing,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.sp.growing <- as.numeric(logLik(form.gbr.sp.growing.only ))
    
    
    form.gbr.sp.mature.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.sp.mature,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.sp.mature <- as.numeric(logLik(form.gbr.sp.mature.only))
    
    
    
    form.gbr.sp.age.all <-  glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.sp.age.all,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.sp.age.all <- as.numeric(logLik(form.gbr.sp.age.all ))
    
    
    LR.sp.gb.age <- -2 * (  LL.gbr.sp.age.all - ( LL.gbr.sp.growing + LL.gbr.sp.mature  ))
    LR.sp.gb.age
    
  } # GB
  
}

# Goats
{
  
{
form.gt.me.growing.only <-  lm( 

formula.me.all

, data =  reg.dat[cnd.gt.growing,]

)



LL.gt.me.growing <- as.numeric(logLik(form.gt.me.growing.only))


form.gt.me.mature.only <-  lm( 

formula.me.all

, data =  reg.dat[cnd.gt.mature,]

)

LL.gt.me.mature <- as.numeric(logLik(  form.gt.me.mature.only))


form.gt.me.age.all <-  lm( 

formula.me.all

, data =  reg.dat[  cnd.gt.age.all ,]

)


LL.gt.me.age.all <- as.numeric(logLik(form.gt.me.age.all))

LR.gt.me.age <- -2 * (    LL.gt.me.age.all - (LL.gt.me.growing + LL.gt.me.mature ))
LR.gt.me.age

} # ME
  
{
    
    form.gbr.gt.growing.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.gt.growing,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.gt.growing <- as.numeric(logLik(form.gbr.gt.growing.only ))
    
    
    form.gbr.gt.mature.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.gt.mature,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.gt.mature <- as.numeric(logLik(form.gbr.gt.mature.only))
    
    
    
    form.gbr.gt.age.all <-  glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.gt.age.all,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.gt.age.all <- as.numeric(logLik(form.gbr.gt.age.all ))
    
    
    LR.gt.gb.age <- -2 * (  LL.gbr.gt.age.all - ( LL.gbr.gt.growing + LL.gbr.gt.mature  ))
    LR.gt.gb.age
    
  } # GB
  
} # ~ not enough observations to conduct either ME and GB models


{
  
  {
    form.bv.me.growing.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.bv.growing,]
      
    )
    
    
    
    LL.bv.me.growing <- as.numeric(logLik(form.bv.me.growing.only))
    
    
    form.bv.me.mature.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.bv.mature,]
      
    )
    
    LL.bv.me.mature <- as.numeric(logLik(  form.bv.me.mature.only))
    
    
    form.bv.me.age.all <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[  cnd.bv.age.all ,]
      
    )
    
    
    LL.bv.me.age.all <- as.numeric(logLik(form.bv.me.age.all))
    
    LR.bv.me.age <- -2 * (    LL.bv.me.age.all - (LL.bv.me.growing + LL.bv.me.mature ))
    LR.bv.me.age
    
  } # ME
  
  {
    
    form.gbr.bv.growing.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.bv.growing,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.bv.growing <- as.numeric(logLik(form.gbr.bv.growing.only ))
    
    
    form.gbr.bv.mature.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.bv.mature,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.bv.mature <- as.numeric(logLik(form.gbr.bv.mature.only))
    
    
    
    form.gbr.bv.age.all <-  glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.bv.age.all,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.bv.age.all <- as.numeric(logLik(form.gbr.bv.age.all ))
    
    
    LR.bv.gb.age <- -2 * (  LL.gbr.bv.age.all - ( LL.gbr.bv.growing + LL.gbr.bv.mature  ))
    LR.bv.gb.age
    
  } # GB
  
} # Cattle #


# ~~~~~ Sex  ~~~~~~~~~

{
  
{
    form.sp.me.male.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.sp.male,]
      
    )
    
    
    
    LL.sp.me.male <- as.numeric(logLik(form.sp.me.male.only))
    
    
    form.sp.me.female.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.sp.female,]
      
    )
    
    LL.sp.me.female <- as.numeric(logLik(  form.sp.me.female.only))
    
    
    form.sp.me.sex.all <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[  cnd.sp.sex.all ,]
      
    )
    
    
    LL.sp.me.sex.all <- as.numeric(logLik(form.sp.me.sex.all))
    
    LR.sp.me.sex <- -2 * (    LL.sp.me.sex.all - (LL.sp.me.male + LL.sp.me.female ))
    LR.sp.me.sex
    
  } # ME
  
{
    
    form.gbr.sp.male.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.sp.male,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.sp.male <- as.numeric(logLik(form.gbr.sp.male.only ))
    
    
    form.gbr.sp.female.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.sp.female,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.sp.female <- as.numeric(logLik(form.gbr.sp.female.only))
    
    
    
    form.gbr.sp.sex.all <-  glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.sp.sex.all,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.sp.sex.all <- as.numeric(logLik(form.gbr.sp.sex.all ))
    
    
    LR.sp.gb.sex <- -2 * (  LL.gbr.sp.sex.all - ( LL.gbr.sp.male + LL.gbr.sp.female  ))
    LR.sp.gb.sex
    
  } # GB
  
}  # Sheep

{

{
form.gt.me.male.only <-  lm( 

formula.me.all

, data =  reg.dat[cnd.gt.male,]

)



LL.gt.me.male <- as.numeric(logLik(form.gt.me.male.only))


form.gt.me.female.only <-  lm( 

formula.me.all

, data =  reg.dat[cnd.gt.female,]

)

LL.gt.me.female <- as.numeric(logLik(  form.gt.me.female.only))


form.gt.me.sex.all <-  lm( 

formula.me.all

, data =  reg.dat[  cnd.gt.sex.all ,]

)


LL.gt.me.sex.all <- as.numeric(logLik(form.gt.me.sex.all))

LR.gt.me.sex <- -2 * (    LL.gt.me.sex.all - (LL.gt.me.male + LL.gt.me.female ))
LR.gt.me.sex

} # ME

{

form.gbr.gt.male.only <-   glmboost( 

formula.all.fe

, data = reg.dat[cnd.gt.male,]

# , family =  family
,  control = boost_control(mstop =   1000 , nu = 0.1)
, center = FALSE
)


LL.gbr.gt.male <- as.numeric(logLik(form.gbr.gt.male.only ))


form.gbr.gt.female.only <-   glmboost( 

formula.all.fe

, data = reg.dat[cnd.gt.female,]

# , family =  family
,  control = boost_control(mstop =   1000 , nu = 0.1)
, center = FALSE
)


LL.gbr.gt.female <- as.numeric(logLik(form.gbr.gt.female.only))



form.gbr.gt.sex.all <-  glmboost( 

formula.all.fe

, data = reg.dat[cnd.gt.sex.all,]

# , family =  family
,  control = boost_control(mstop =   1000 , nu = 0.1)
, center = FALSE
)


LL.gbr.gt.sex.all <- as.numeric(logLik(form.gbr.gt.sex.all ))


LR.gt.gb.sex <- -2 * (  LL.gbr.gt.sex.all - ( LL.gbr.gt.male + LL.gbr.gt.female  ))
LR.gt.gb.sex

} # GB

} # Goats

{
  
{
    form.bv.me.male.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.bv.male,]
      
    )
    
    
    
    LL.bv.me.male <- as.numeric(logLik(form.bv.me.male.only))
    
    
    form.bv.me.female.only <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[cnd.bv.female,]
      
    )
    
    LL.bv.me.female <- as.numeric(logLik(  form.bv.me.female.only))
    
    
    form.bv.me.sex.all <-  lm( 
      
      formula.me.all
      
      , data =  reg.dat[  cnd.bv.sex.all ,]
      
    )
    
    
    LL.bv.me.sex.all <- as.numeric(logLik(form.bv.me.sex.all))
    
    LR.bv.me.sex <- -2 * (    LL.bv.me.sex.all - (LL.bv.me.male + LL.bv.me.female ))
    LR.bv.me.sex
    
  } # ME
  
{
    
    form.gbr.bv.male.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.bv.male,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.bv.male <- as.numeric(logLik(form.gbr.bv.male.only ))
    
    
    form.gbr.bv.female.only <-   glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.bv.female,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.bv.female <- as.numeric(logLik(form.gbr.bv.female.only))
    
    
    
    form.gbr.bv.sex.all <-  glmboost( 
      
      formula.all.fe
      
      , data = reg.dat[cnd.bv.sex.all,]
      
      # , family =  family
      ,  control = boost_control(mstop =   1000 , nu = 0.1)
      , center = FALSE
    )
    
    
    LL.gbr.bv.sex.all <- as.numeric(logLik(form.gbr.bv.sex.all ))
    
    
    LR.bv.gb.sex <- -2 * (  LL.gbr.bv.sex.all - ( LL.gbr.bv.male + LL.gbr.bv.female  ))
    LR.bv.gb.sex
    
  } # GB
  
} # Cattle


lr.res  <-  data.frame(
  species = NA
  , mod = NA
  , nrow= 10
)

lr.res  <-  data.frame(matrix(NA,  nrow = n.species * 2 * 4, ncol = 1))


lr.res$species <- NA
lr.res$mod <- NA

r.cnt <- 1

lr.metrics <-  c('breed' , 'sys' , 'age' , 'sex') ; lr.models <- c('me','gbr')

for (t in lr.metrics){
for (s in species.srs){
for (m in lr.models){
  
  # test: s <- species.sheep ; m <- 'me'
   
  lr.res[ r.cnt  , 'species'] <- s 
  lr.res[ r.cnt  , 'model'] <- m
  lr.res[ r.cnt  , 'metric'] <- t
  
  r.cnt <- r.cnt + 1
  
}}}


# Breed
lr.res[ lr.res$species == species.sheep & lr.res$metric == lr.metrics[1] &  lr.res$model == lr.models[1]   , 'LR.stat']  <- LR.sp.me.breed
lr.res[ lr.res$species == species.sheep & lr.res$metric == lr.metrics[1] & lr.res$model == lr.models[2]  , 'LR.stat']  <- LR.sp.gb.breed

lr.res[ lr.res$species == species.goat & lr.res$metric == lr.metrics[1] &  lr.res$model == lr.models[1]   , 'LR.stat']  <- LR.gt.me.breed
lr.res[ lr.res$species == species.goat & lr.res$metric == lr.metrics[1] & lr.res$model == lr.models[2]  , 'LR.stat']  <- LR.gt.gb.breed

# NDF
lr.res[ lr.res$species == species.sheep & lr.res$metric == lr.metrics[2] &  lr.res$model == lr.models[1]   , 'LR.stat']  <- LR.sp.me.ndf
lr.res[ lr.res$species == species.sheep & lr.res$metric == lr.metrics[2] & lr.res$model == lr.models[2]  , 'LR.stat']  <- LR.sp.gb.ndf

lr.res[ lr.res$species == species.goat & lr.res$metric == lr.metrics[2] &  lr.res$model == lr.models[1]   , 'LR.stat']  <- LR.gt.me.ndf
lr.res[ lr.res$species == species.goat & lr.res$metric == lr.metrics[2] & lr.res$model == lr.models[2]  , 'LR.stat']  <- LR.gt.gb.ndf

# Age
lr.res[ lr.res$species == species.sheep & lr.res$metric == lr.metrics[3] &  lr.res$model == lr.models[1]   , 'LR.stat']  <- LR.sp.me.age
lr.res[ lr.res$species == species.sheep & lr.res$metric == lr.metrics[3] & lr.res$model == lr.models[2]  , 'LR.stat']  <- LR.sp.gb.age

lr.res[ lr.res$species == species.goat & lr.res$metric == lr.metrics[3] &  lr.res$model == lr.models[1]   , 'LR.stat']  <- LR.gt.me.age
lr.res[ lr.res$species == species.goat & lr.res$metric == lr.metrics[3] & lr.res$model == lr.models[2]  , 'LR.stat']  <- LR.gt.gb.age

# Sex
lr.res[ lr.res$species == species.sheep & lr.res$metric == lr.metrics[4] &  lr.res$model == lr.models[1]   , 'LR.stat']  <- LR.sp.me.sex
lr.res[ lr.res$species == species.sheep & lr.res$metric == lr.metrics[4] & lr.res$model == lr.models[2]  , 'LR.stat']  <- LR.sp.gb.sex

lr.res[ lr.res$species == species.goat & lr.res$metric == lr.metrics[4] &  lr.res$model == lr.models[1]   , 'LR.stat']  <- LR.gt.me.sex
lr.res[ lr.res$species == species.goat & lr.res$metric == lr.metrics[4] & lr.res$model == lr.models[2]  , 'LR.stat']  <- LR.gt.gb.sex

lr.res$Ho <- NA ; ho.reject <- 16.8

lr.res[  lr.res$LR.stat > ho.reject , 'Ho'] <- 'rejected'
lr.res[  lr.res$LR.stat <= ho.reject , 'Ho'] <- 'not rejected'


write.csv(  lr.res , file =  str_c(results.out.dir , 'lr.res.csv')  )


