
# source( str_c(settings.dir,'variables.R'))

# Global variable names


# Bovines
gg.x.label.cattle.lo.ndf <<- 'Low NDF'
gg.x.label.cattle.hi.ndf <<- 'High NDF'



# Labels for sample sizes
lab.total.experiment.sample <<- 'n\u1D31'
lab.total.treatment.sample <<- 'n\u1D40'
lab.total.animal.sample <<- 'n\u1D2C'



# Sex categories
sex.levels <<- c(1,2) # Male, female
sex.male <<- 1 ; sex.female <<- 2

sex.labels <<- c('Male', 'Female')


# Breed categories
breed.levels <<- c(1,2) # Indigenous, Exotic/cross
breed.indig <<- 1 ; breed.cbred <<- 2


breed.labels <<- c('Bos indicus', 'Exotic/cross')

# Model levels
model.class.GBR <<- 1 ; model.class.ME <<- 2



#if (merge.sexes) { sex.male <- c(sex.male , sex.female)}

stages.male <<- c(  "growing.male" ,  "adult.male"  , "growing.male.castrate" ,  "adult.male.castrate" )


mod.tier.1 <<- c(1:3)
mod.tier.2 <<- c(4:6)
mod.tier.3 <<- c(7:9)

tier.label.1 <<- 'Animal' ;   tier.label.2 <<- 'Animal +\nNutrition'  ;   tier.label.3 <<- 'Animal +\nNutrition ext.'


pred.var.BW <<-   'bw_kg'
pred.var.ADG <<-   'adg_kg_day'
pred.var.MY  <<-    'milk_prod_kg_d'
pred.var.GES <<-   'is_gestating'
pred.var.NDF <<-  'NDF_nutrition'
pred.var.ADF <<-   'ADF_nutrition'
pred.var.CP <<-   'CP_nutrition'
pred.var.Ash <<-   'Ash_nutrition'
pred.var.EE <<-   'EE_nutrition'
pred.var.NDF_Dig <<-  'NDF_digest'

pred.var.ADL <<-  'ADL_nutrition'
pred.var.OM_Dig <<-  'DM_digest'
pred.var.DM_Dig <<-  'OM_digest'
pred.var.DM <<-  'DM_nutrition'


pred.var.Offset <<-   'offset'
pred.var.Intercept <<-   '(Intercept)'


v.list.DV <<- c( y.var.reg )
v.list.BW<<- c('dummy' , 'bw_kg' )   
v.list.ADG <<- c('dummy' , pred.var.ADG) 
v.list.MY <<- c('dummy' , pred.var.MY ) 
v.list.GES <<- c('dummy' , pred.var.GES  ) 
v.list.CP <<- c('dummy' , pred.var.CP ) 
v.list.NDF<<- c('dummy' , pred.var.NDF  ) 
v.list.ADF<<- c('dummy' , pred.var.ADF ) 
v.list.EE<<- c('dummy' ,  pred.var.EE )  
v.list.Ash <<- c('dummy' , pred.var.Ash )  
v.list.NDF.dig<<- c('dummy' , pred.var.NDF_Dig ) 

v.list.ADL <<- c('dummy' , pred.var.ADL ) 
v.list.OM.dig <<- c('dummy' , pred.var.OM_Dig  ) 
v.list.DM.dig <<- c('dummy' , pred.var.DM_Dig   ) 
v.list.DM <<- c('dummy' , pred.var.DM  ) 



suffixes <<- c(
  
  '.e25'
  , '.sqt' 
  , '.e75'
  , '.sqd'
  , '.cbd' 
  , '.log'
  , '.recip'
  
 # , '.mean.s'
 # , '.min.max.s'
)

vn.unique.exp.id <<- 'ue.id'

vn.all.treatment.IDs <<- 'all.treatment.IDs'
vn.all.experiment.IDs <<- 'all.experiment.IDs'


vn.total.animal.sample.size <<- 'Total.animal.sample'
vn.total.exp.sample.size <<- 'Total.exp.sample'
vn.total.trt.sample.size <<- 'Total.trt.sample'


vn.weight.list <<- "weight.list"

vn.loop.iter <<- "loop.iter"
vn.iter.is.NULL <<- "is.NULL"

vn.species <<- "species"
vn.sex <<- 'sex'
vn.breed <<- 'breed'
vn.ndf <<- "ndf" 


vn.formula <<- "formula"
vn.tier <<- "tier"


vn.mod.vers <<- "mod.vers" 
vn.mod.class <<- "mod.class" 
vn.mod.form <<- "mod.form" 
vn.k <<- "fold" 

vn.nu <<- "nu"
vn.m.stop <<- "m.stop"

# Model objects
vn.mod.object <<- "mod.object" 
vn.mod.object.ws <<- "mod.object.ws" 



# Model rankings
vn.best.in.formula <<- 'is.best.model.in.formula'
vn.is.best.tier <<- 'is.best.model.in.tier'
vn.best.in.class <<-  'is.best.model.in.class'
vn.best.global <<-  'is.best.global.model'



# TEST dataset 
vn.test.r2 <<- "R2" 
vn.test.nrmse <<- "nRMSE" 
vn.test.aic <<- "AIC" 
vn.test.ccc <<- "CCC" 

vn.test.w.r2 <<- "R2.weighted" 
vn.test.w.nrmse <<- "nRMSE.weighted" 
vn.test.w.aic <<- "AIC.weighted" 
vn.test.w.ccc <<- "CCC.weighted" 

vn.coef.int <<- "coef.int" 
vn.coef.offset.plus.intercept <<- "test.coef.offset.plus.intercept" 

vn.coef.BW <<- "coef.BW" 
vn.coef.ADG <<- "coef.ADG" 
vn.coef.MY <<- "coef.MY" 
vn.coef.NDF <<- "coef.NDF" 
vn.coef.ADF <<- "coef.ADF" 
vn.coef.CP <<- "coef.CP" 

vn.coef.EE <<- "coef.EE" 
vn.coef.Ash <<- "coef.Ash" 

vn.coef.NDF_digest <<- "coef.NDF_digest" 

vn.coef.ADL <<- "coef.ADL"
vn.coef.OM_digest <<- "coef.OM_Digest"
vn.coef.DM_digest <<- "coef.DM_Digest"
vn.coef.DM <<- "coef.DM"


vn.best.hypar.w.R2 <<-'best.hypar.w.R2'
vn.best.hypar.w.nRMSE <<-'best.hypar.w.nRMSE'
vn.best.hypar.w.CCC <<-'best.hypar.w.CCC'
vn.best.hypar.w.AIC <<-'best.hypar.w.AIC'


vn.best.hypar.ucb.w.R2 <<-'best.hypar.ucb.w.R2'
vn.best.hypar.ucb.w.nRMSE <<-'best.hypar.ucb.w.nRMSE'
vn.best.hypar.ucb.w.CCC <<-'best.hypar.ucb.w.CCC'
vn.best.hypar.ucb.w.AIC <<-'best.hypar.ucb.w.AIC'


# Best in tier (all formulae in current tier (and class))
vn.tier.rank <<- 'rank.among.tier'


vn.best.tier.w.R2 <<-'best.tier.w.R2'
vn.best.tier.w.nRMSE <<-'best.tier.w.nRMSE'
vn.best.tier.w.CCC <<-'best.tier.w.CCC'
vn.best.tier.w.AIC <<-'best.tier.w.AIC'


vn.best.tier.w.R2.ucb <<-'best.tier.w.R2.ucb'
vn.best.tier.w.nRMSE.ucb <<-'best.tier.w.nRMSE.ucb'
vn.best.tier.w.CCC.ucb <<-'best.tier.w.CCC.ucb'
vn.best.tier.w.AIC.ucb <<-'best.tier.w.AIC.ucb'


# Global best - best among all hyperparameters, formulae, and classes
vn.best.global.w.R2 <<- 'best.global.w.R2'
vn.best.global.w.nRMSE <<- 'best.global.w.nRMSE'
vn.best.global.w.CCC <<- 'best.global.w.CCC'
vn.best.global.w.AIC <<- 'best.global.w.AIC'

vn.best.global.w.R2.ucb <<- 'best.global.w.R2.ucb'
vn.best.global.w.nRMSE.ucb <<-'best.global.w.nRMSE.ucb'
vn.best.global.w.CCC.ucb <<-'best.global.w.CCC.ucb'
vn.best.global.w.AIC.ucb <<- 'best.global.w.AIC.ucb'

vn.R2.mean <<-'R2.mean'
vn.nRMSE.mean <<-'nRMSE.mean'
vn.CCC.mean <<-'CCC.mean'
vn.AIC.mean <<-'AIC.mean'

vn.R2.sd <<-'R2.sd'
vn.nRMSE.sd <<-'nRMSE.sd'
vn.CCC.sd <<-'CCC.sd'
vn.AIC.sd <<- 'AIC.sd'

vn.R2.ucb <<-'R2.ucb'
vn.nRMSE.ucb <<-'nRMSE.ucb'
vn.CCC.ucb <<-'CCC.ucb'
vn.AIC.ucb <<-'AIC.ucb'

vn.w.R2.mean <<-'w.R2.mean'
vn.w.nRMSE.mean <<-'w.nRMSE.mean'
vn.w.CCC.mean <<-'w.CCC.mean'
vn.w.AIC.mean <<-'w.AIC.mean'

vn.w.R2.sd <<-'w.R2.sd'
vn.w.nRMSE.sd <<-'w.nRMSE.sd'
vn.w.CCC.sd <<-'w.CCC.sd'
vn.w.AIC.sd <<- 'w.AIC.sd'

vn.w.R2.ucb.ur <<-'w.R2.ucb.ur'
vn.w.R2.ucb <<-'w.R2.ucb'
vn.w.nRMSE.ucb <<-'w.nRMSE.ucb'
vn.w.CCC.ucb <<-'w.CCC.ucb'
vn.w.AIC.ucb <<-'w.AIC.ucb'

# Fold averaged metrics
vn.mn.coef.int <<- 'mean.coef.int'
vn.mn.coef.BW <<- 'mean.coef.BW'
vn.mn.coef.ADG <<- 'mean.coef.ADG'
vn.mn.coef.NDF <<- 'mean.coef.NDF'
vn.mn.coef.CP <<- 'mean.coef.CP'
vn.mn.coef.NDF_digest <<- 'mean.coef.NDF_digest'


vn.mn.coef.ADL <<- 'mean.coef.ADL'
vn.mn.coef.DM_digest <<- 'mean.coef.DM_digest'
vn.mn.coef.OM_digest <<- 'mean.coef.OM_digest'
vn.mn.coef.DM <<- 'mean.coef.DM'


vn.sd.coef.int <<- 'sd.coef.int'
vn.sd.coef.BW <<- 'sd.coef.BW'
vn.sd.coef.ADG <<- 'sd.coef.ADG'
vn.sd.coef.NDF <<- 'sd.coef.NDF'
vn.sd.coef.CP <<- 'sd.coef.CP'
vn.sd.coef.NDF_digest <<- 'sd.coef.NDF_digest'

vn.sd.coef.ADL <<- 'sd.coef.ADL'
vn.sd.coef.DM_digest <<- 'sd.coef.DM_digest'
vn.sd.coef.OM_digest <<- 'sd.coef.OM_digest'
vn.sd.coef.DM <<- 'sd.coef.DM'



# Whole sample performance metrics
vn.ws.R2 <<-'ws.R2'
vn.ws.nRMSE <<-'ws.nRMSE'
vn.ws.CCC <<-'ws.CCC'
vn.ws.AIC <<-'ws.AIC'

vn.ws.w.R2 <<-'ws.w.R2'
vn.ws.w.nRMSE<<-'ws.w.nRMSE'
vn.ws.w.CCC <<-'ws.w.CCC'
vn.ws.w.AIC <<-'ws.w.AIC'

# Variable importance (ML methods only)
vn.mn.var.imp.int <<- 'mean.var.imp.int'
vn.mn.var.imp.BW <<- 'mean.var.imp.BW'
vn.mn.var.imp.ADG <<- 'mean.var.imp.ADG'
vn.mn.var.imp.NDF <<- 'mean.var.imp.NDF'
vn.mn.var.imp.CP <<- 'mean.var.imp.CP'
vn.mn.var.imp.NDF_digest <<- 'mean.var.imp.NDF_digest'

vn.mn.var.imp.ADL <<- 'mean.var.imp.ADL'
vn.mn.var.imp.OM_digest <<- 'mean.var.imp.OM_digest '
vn.mn.var.imp.DM_digest <<- 'mean.var.imp.DM_digest '
vn.mn.var.imp.DM  <<- 'mean.var.imp.DM'

# Variable importance in relative terms 
vn.mn.var.rel.imp.int <<- 'mean.var.rel.imp.int'
vn.mn.var.rel.imp.BW <<- 'mean.var.rel.imp.BW'
vn.mn.var.rel.imp.ADG <<- 'mean.var.rel.imp.ADG'
vn.mn.var.rel.imp.NDF <<- 'mean.var.rel.imp.NDF'
vn.mn.var.rel.imp.CP <<- 'mean.var.rel.imp.CP'
vn.mn.var.rel.imp.NDF_digest <<- 'mean.var.rel.imp.NDF_digest'
vn.mn.var.rel.imp.ADL <<- 'mean.var.rel.imp.ADL'
vn.mn.var.rel.imp.OM_digest <<- 'mean.var.rel.imp.OM_digest '
vn.mn.var.rel.imp.DM_digest <<- 'mean.var.rel.imp.DM_digest '
vn.mn.var.rel.imp.DM  <<- 'mean.var.rel.imp.DM'



vn.sd.var.imp.rel.int <<- 'sd.rel.var.imp.int'
vn.sd.var.imp.rel.BW <<- 'sd.rel.var.imp.BW'
vn.sd.var.imp.rel.ADG <<- 'sd.rel.var.imp.ADG'
vn.sd.var.imp.rel.NDF <<- 'sd.rel.var.imp.NDF'
vn.sd.var.imp.rel.CP <<- 'sd.rel.var.imp.CP'
vn.sd.var.imp.rel.NDF_digest <<- 'sd.rel.var.imp.NDF_digest'
vn.sd.var.rel.imp.ADL <<- 'sd.var.rel.imp.ADL'
vn.sd.var.rel.imp.OM_digest <<- 'sd.var.rel.imp.OM_digest '
vn.sd.var.rel.imp.DM_digest <<- 'sd.var.rel.imp.DM_digest '
vn.sd.var.rel.imp.DM  <<- 'sd.var.rel.imp.DM'

vn.sd.var.imp.int <<- 'sd.var.imp.int'
vn.sd.var.imp.BW <<- 'sd.var.imp.BW'
vn.sd.var.imp.ADG <<- 'sd.var.imp.ADG'
vn.sd.var.imp.NDF <<- 'sd.var.imp.NDF'
vn.sd.var.imp.CP <<- 'sd.var.imp.CP'
vn.sd.var.imp.NDF_digest <<- 'sd.var.imp.NDF_digest'
vn.sd.var.imp.ADL <<- 'sd.var.imp.ADL'
vn.sd.var.imp.OM_digest <<- 'sd.var.imp.OM_digest '
vn.sd.var.imp.DM_digest <<- 'sd.var.imp.DM_digest '
vn.sd.var.imp.DM  <<- 'sd.var.imp.DM'


# Outlier control
ol.status.residual.method.var.name <<- 'ol.status.error.SDs'
ol.status.range.method.var.name <<- 'ol.status.absolute.range.method'


# Threshold values


vars.outlier.ranges <<- c(
  
    'bw_kg'
  , 'feed_intake_kg_d'
  
  
  
  , 'NDF_nutrition'
  , 'ADF_nutrition'
  
  , 'CP_nutrition'
  , 'EE_nutrition'
  , 'Ash_nutrition'
  
  , 'adg_kg_day'
  
  , 'ADL'
  , 'DM_digest'
  , 'OM_digest'
  , 'DM'
  
)




# Coefficient estimates - whole sample
vn.coef.ws.intercept <<- 'ws.coef.intercept'
vn.coef.ws.offset <<- 'ws.coef.offset'
vn.coef.ws.offset.plus.intercept <<- 'ws.coef.offset.plus.intercept'


vn.coef.ws.bw <<- 'ws.coef.bw_kg'
vn.coef.ws.bw.e25 <<- 'ws.coef.bw_kg.e25'
vn.coef.ws.bw.sqt <<- 'ws.coef.bw_kg.sqt'
vn.coef.ws.bw.e75 <<- 'ws.coef.bw_kg.e75'
vn.coef.ws.bw.sqd <<- 'ws.coef.bw_kg.sqd'
vn.coef.ws.bw.cbd <<- 'ws.coef.bw_kg.cbd'
vn.coef.ws.bw.log <<- 'ws.coef.bw_kg.log'

vn.coef.ws.ADG <<- 'ws.coef.adg_kg_day'
vn.coef.ws.ADG.e25 <<- 'ws.coef.adg_kg_day.e25'
vn.coef.ws.ADG.sqt <<- 'ws.coef.adg_kg_day.sqt'
vn.coef.ws.ADG.e75 <<- 'ws.coef.adg_kg_day.e75'
vn.coef.ws.ADG.sqd <<- 'ws.coef.adg_kg_day.sqd'
vn.coef.ws.ADG.cbd <<- 'ws.coef.adg_kg_day.cbd'
vn.coef.ws.ADG.log <<- 'ws.coef.adg_kg_day.log'

vn.coef.ws.MY <<- 'ws.coef.milk_prod_kg_d'
vn.coef.ws.MY.e25 <<- 'ws.coef.milk_prod_kg_d.e25'
vn.coef.ws.MY.sqt <<- 'ws.coef.milk_prod_kg_d.sqt'
vn.coef.ws.MY.e75 <<- 'ws.coef.milk_prod_kg_d.e75'
vn.coef.ws.MY.sqd <<- 'ws.coef.milk_prod_kg_d.sqd'
vn.coef.ws.MY.cbd <<- 'ws.coef.milk_prod_kg_d.cbd'
vn.coef.ws.MY.log <<- 'ws.coef.milk_prod_kg_d.log'

vn.coef.ws.GES <<- 'ws.coef.is_gestating'
vn.coef.ws.GES.e25 <<- 'ws.coef.is_gestating.e25'
vn.coef.ws.GES.sqt <<- 'ws.coef.is_gestating.sqt'
vn.coef.ws.GES.e75 <<- 'ws.coef.is_gestating.e75'
vn.coef.ws.GES.sqd <<- 'ws.coef.is_gestating.sqd'
vn.coef.ws.GES.cbd <<- 'ws.coef.is_gestating.cbd'
vn.coef.ws.GES.log <<- 'ws.coef.is_gestating.log'


vn.coef.ws.NDF <<- 'ws.coef.NDF'
vn.coef.ws.NDF.e25 <<- 'ws.coef.NDF.e25'
vn.coef.ws.NDF.sqt <<- 'ws.coef.NDF.sqt'
vn.coef.ws.NDF.e75 <<- 'ws.coef.NDF.e75'
vn.coef.ws.NDF.sqd <<- 'ws.coef.NDF.sqd'
vn.coef.ws.NDF.cbd <<- 'ws.coef.NDF.cbd'
vn.coef.ws.NDF.log <<- 'ws.coef.NDF.log'

vn.coef.ws.ADF <<- 'ws.coef.ADF'
vn.coef.ws.ADF.e25 <<- 'ws.coef.ADF.e25'
vn.coef.ws.ADF.sqt <<- 'ws.coef.ADF.sqt'
vn.coef.ws.ADF.e75 <<- 'ws.coef.ADF.e75'
vn.coef.ws.ADF.sqd <<- 'ws.coef.ADF.sqd'
vn.coef.ws.ADF.cbd <<- 'ws.coef.ADF.cbd'
vn.coef.ws.ADF.log <<- 'ws.coef.ADF.log'

vn.coef.ws.CP <<- 'ws.coef.CP'
vn.coef.ws.CP.e25 <<- 'ws.coef.CP_nutrition.e25'
vn.coef.ws.CP.sqt <<- 'ws.coef.CP_nutrition.sqt'
vn.coef.ws.CP.e75 <<- 'ws.coef.CP_nutrition.e75'
vn.coef.ws.CP.sqd <<- 'ws.coef.CP_nutrition.sqd'
vn.coef.ws.CP.cbd <<- 'ws.coef.CP_nutrition.cbd'
vn.coef.ws.CP.log <<- 'ws.coef.CP_nutrition.log'

vn.coef.ws.EE <<- 'ws.coef.EE'
vn.coef.ws.EE.e25 <<- 'ws.coef.EE_nutrition.e25'
vn.coef.ws.EE.sqt <<- 'ws.coef.EE_nutrition.sqt'
vn.coef.ws.EE.e75 <<- 'ws.coef.EE_nutrition.e75'
vn.coef.ws.EE.sqd <<- 'ws.coef.EE_nutrition.sqd'
vn.coef.ws.EE.cbd <<- 'ws.coef.EE_nutrition.cbd'
vn.coef.ws.EE.log <<- 'ws.coef.EE_nutrition.log'

vn.coef.ws.Ash <<- 'ws.coef.Ash'
vn.coef.ws.Ash.e25 <<- 'ws.coef.Ash_nutrition.e25'
vn.coef.ws.Ash.sqt <<- 'ws.coef.Ash_nutrition.sqt'
vn.coef.ws.Ash.e75 <<- 'ws.coef.Ash_nutrition.e75'
vn.coef.ws.Ash.sqd <<- 'ws.coef.Ash_nutrition.sqd'
vn.coef.ws.Ash.cbd <<- 'ws.coef.Ash_nutrition.cbd'
vn.coef.ws.Ash.log <<- 'ws.coef.Ash_nutrition.log'

vn.coef.ws.NDF_Dig <<- 'ws.coef.NDF_digest'
vn.coef.ws.NDF_Dig.e25 <<- 'ws.coef.NDF_digest.e25'
vn.coef.ws.NDF_Dig.sqt <<- 'ws.coef.NDF_digest.sqt'
vn.coef.ws.NDF_Dig.e75 <<- 'ws.coef.NDF_digest.e75'
vn.coef.ws.NDF_Dig.sqd <<- 'ws.coef.NDF_digest.sqd'
vn.coef.ws.NDF_Dig.cbd <<- 'ws.coef.NDF_digest.cbd'
vn.coef.ws.NDF_Dig.log <<- 'ws.coef.NDF_digest.log'

vn.coef.ws.ADL <<- 'ws.coef.ADL'
vn.coef.ws.ADL.e25 <<- 'ws.coef.ADL_nutrition.e25'
vn.coef.ws.ADL.sqt <<- 'ws.coef.ADL_nutrition.sqt'
vn.coef.ws.ADL.e75 <<- 'ws.coef.ADL_nutrition.e75'
vn.coef.ws.ADL.sqd <<- 'ws.coef.ADL_nutrition.sqd'
vn.coef.ws.ADL.cbd <<- 'ws.coef.ADL_nutrition.cbd'
vn.coef.ws.ADL.log <<- 'ws.coef.ADL_nutrition.log'

vn.coef.ws.DM_Dig <<- 'ws.coef.DM_digest'
vn.coef.ws.DM_Dig.e25 <<- 'ws.coef.DM_digest.e25'
vn.coef.ws.DM_Dig.sqt <<- 'ws.coef.DM_digest.sqt'
vn.coef.ws.DM_Dig.e75 <<- 'ws.coef.DM_digest.e75'
vn.coef.ws.DM_Dig.sqd <<- 'ws.coef.DM_digest.sqd'
vn.coef.ws.DM_Dig.cbd <<- 'ws.coef.DM_digest.cbd'
vn.coef.ws.DM_Dig.log <<- 'ws.coef.DM_digest.log'


vn.coef.ws.OM_Dig <<- 'ws.coef.OM_digest'
vn.coef.ws.OM_Dig.e25 <<- 'ws.coef.OM_digest.e25'
vn.coef.ws.OM_Dig.sqt <<- 'ws.coef.OM_digest.sqt'
vn.coef.ws.OM_Dig.e75 <<- 'ws.coef.OM_digest.e75'
vn.coef.ws.OM_Dig.sqd <<- 'ws.coef.OM_digest.sqd'
vn.coef.ws.OM_Dig.cbd <<- 'ws.coef.OM_digest.cbd'
vn.coef.ws.OM_Dig.log <<- 'ws.coef.OM_digest.log'

vn.coef.ws.DM <<- 'ws.coef.DM_digest'
vn.coef.ws.DM.e25 <<- 'ws.coef.DM_digest.e25'
vn.coef.ws.DM.sqt <<- 'ws.coef.DM_digest.sqt'
vn.coef.ws.DM.e75 <<- 'ws.coef.DM_digest.e75'
vn.coef.ws.DM.sqd <<- 'ws.coef.DM_digest.sqd'
vn.coef.ws.DM.cbd <<- 'ws.coef.DM_digest.cbd'
vn.coef.ws.DM.log <<- 'ws.coef.DM_digest.log'


# Variable names -- coefficient standard errors 
vn.coef.se.ws.intercept <<- 'ws.coef.se.intercept'
vn.coef.se.ws.offset <<- 'ws.coef.se.offset'

vn.coef.se.ws.bw <<- 'ws.coef.se.bw_kg'
vn.coef.se.ws.bw.e25 <<- 'ws.coef.se.bw_kg.e25'
vn.coef.se.ws.bw.sqt <<- 'ws.coef.se.bw_kg.sqt'
vn.coef.se.ws.bw.e75 <<- 'ws.coef.se.bw_kg.e75'
vn.coef.se.ws.bw.sqd <<- 'ws.coef.se.bw_kg.sqd'
vn.coef.se.ws.bw.cbd <<- 'ws.coef.se.bw_kg.cbd'
vn.coef.se.ws.bw.log <<- 'ws.coef.se.bw_kg.log'

vn.coef.se.ws.ADG <<- 'ws.coef.se.adg_kg_day'
vn.coef.se.ws.ADG.e25 <<- 'ws.coef.se.adg_kg_day.e25'
vn.coef.se.ws.ADG.sqt <<- 'ws.coef.se.adg_kg_day.sqt'
vn.coef.se.ws.ADG.e75 <<- 'ws.coef.se.adg_kg_day.e75'
vn.coef.se.ws.ADG.sqd <<- 'ws.coef.se.adg_kg_day.sqd'
vn.coef.se.ws.ADG.cbd <<- 'ws.coef.se.adg_kg_day.cbd'
vn.coef.se.ws.ADG.log <<- 'ws.coef.se.adg_kg_day.log'

vn.coef.se.ws.MY <<- 'ws.coef.se.milk_prod_kg_d'
vn.coef.se.ws.MY.e25 <<- 'ws.coef.se.milk_prod_kg_d.e25'
vn.coef.se.ws.MY.sqt <<- 'ws.coef.se.milk_prod_kg_d.sqt'
vn.coef.se.ws.MY.e75 <<- 'ws.coef.se.milk_prod_kg_d.e75'
vn.coef.se.ws.MY.sqd <<- 'ws.coef.se.milk_prod_kg_d.sqd'
vn.coef.se.ws.MY.cbd <<- 'ws.coef.se.milk_prod_kg_d.cbd'
vn.coef.se.ws.MY.log <<- 'ws.coef.se.milk_prod_kg_d.log'

vn.coef.se.ws.GES <<- 'ws.coef.se.is_gestating'
vn.coef.se.ws.GES.e25 <<- 'ws.coef.se.is_gestating.e25'
vn.coef.se.ws.GES.sqt <<- 'ws.coef.se.is_gestating.sqt'
vn.coef.se.ws.GES.e75 <<- 'ws.coef.se.is_gestating.e75'
vn.coef.se.ws.GES.sqd <<- 'ws.coef.se.is_gestating.sqd'
vn.coef.se.ws.GES.cbd <<- 'ws.coef.se.is_gestating.cbd'
vn.coef.se.ws.GES.log <<- 'ws.coef.se.is_gestating.log'

vn.coef.se.ws.NDF <<- 'ws.coef.se.NDF'
vn.coef.se.ws.NDF.e25 <<- 'ws.coef.se.NDF.e25'
vn.coef.se.ws.NDF.sqt <<- 'ws.coef.se.NDF.sqt'
vn.coef.se.ws.NDF.e75 <<- 'ws.coef.se.NDF.e75'
vn.coef.se.ws.NDF.sqd <<- 'ws.coef.se.NDF.sqd'
vn.coef.se.ws.NDF.cbd <<- 'ws.coef.se.NDF.cbd'
vn.coef.se.ws.NDF.log <<- 'ws.coef.se.NDF.log'

vn.coef.se.ws.ADF <<- 'ws.coef.se.ADF'
vn.coef.se.ws.ADF.e25 <<- 'ws.coef.se.ADF.e25'
vn.coef.se.ws.ADF.sqt <<- 'ws.coef.se.ADF.sqt'
vn.coef.se.ws.ADF.e75 <<- 'ws.coef.se.ADF.e75'
vn.coef.se.ws.ADF.sqd <<- 'ws.coef.se.ADF.sqd'
vn.coef.se.ws.ADF.cbd <<- 'ws.coef.se.ADF.cbd'
vn.coef.se.ws.ADF.log <<- 'ws.coef.se.ADF.log'

vn.coef.se.ws.CP <<- 'ws.coef.se.CP'
vn.coef.se.ws.CP.e25 <<- 'ws.coef.se.CP_nutrition.e25'
vn.coef.se.ws.CP.sqt <<- 'ws.coef.se.CP_nutrition.sqt'
vn.coef.se.ws.CP.e75 <<- 'ws.coef.se.CP_nutrition.e75'
vn.coef.se.ws.CP.sqd <<- 'ws.coef.se.CP_nutrition.sqd'
vn.coef.se.ws.CP.cbd <<- 'ws.coef.se.CP_nutrition.cbd'
vn.coef.se.ws.CP.log <<- 'ws.coef.se.CP_nutrition.log'

vn.coef.se.ws.EE <<- 'ws.coef.se.EE'
vn.coef.se.ws.EE.e25 <<- 'ws.coef.se.EE_nutrition.e25'
vn.coef.se.ws.EE.sqt <<- 'ws.coef.se.EE_nutrition.sqt'
vn.coef.se.ws.EE.e75 <<- 'ws.coef.se.EE_nutrition.e75'
vn.coef.se.ws.EE.sqd <<- 'ws.coef.se.EE_nutrition.sqd'
vn.coef.se.ws.EE.cbd <<- 'ws.coef.se.EE_nutrition.cbd'
vn.coef.se.ws.EE.log <<- 'ws.coef.se.EE_nutrition.log'

vn.coef.se.ws.Ash <<- 'ws.coef.se.Ash'
vn.coef.se.ws.Ash.e25 <<- 'ws.coef.se.Ash_nutrition.e25'
vn.coef.se.ws.Ash.sqt <<- 'ws.coef.se.Ash_nutrition.sqt'
vn.coef.se.ws.Ash.e75 <<- 'ws.coef.se.Ash_nutrition.e75'
vn.coef.se.ws.Ash.sqd <<- 'ws.coef.se.Ash_nutrition.sqd'
vn.coef.se.ws.Ash.cbd <<- 'ws.coef.se.Ash_nutrition.cbd'
vn.coef.se.ws.Ash.log <<- 'ws.coef.se.Ash_nutrition.log'

vn.coef.se.ws.NDF_Dig <<- 'ws.coef.se.NDF_digest'
vn.coef.se.ws.NDF_Dig.e25 <<- 'ws.coef.se.NDF_digest.e25'
vn.coef.se.ws.NDF_Dig.sqt <<- 'ws.coef.se.NDF_digest.sqt'
vn.coef.se.ws.NDF_Dig.e75 <<- 'ws.coef.se.NDF_digest.e75'
vn.coef.se.ws.NDF_Dig.sqd <<- 'ws.coef.se.NDF_digest.sqd'
vn.coef.se.ws.NDF_Dig.cbd <<- 'ws.coef.se.NDF_digest.cbd'
vn.coef.se.ws.NDF_Dig.log <<- 'ws.coef.se.NDF_digest.log'

vn.coef.se.ws.ADL <<- 'ws.coef.se.ADL'
vn.coef.se.ws.ADL.e25 <<- 'ws.coef.se.ADL_nutrition.e25'
vn.coef.se.ws.ADL.sqt <<- 'ws.coef.se.ADL_nutrition.sqt'
vn.coef.se.ws.ADL.e75 <<- 'ws.coef.se.ADL_nutrition.e75'
vn.coef.se.ws.ADL.sqd <<- 'ws.coef.se.ADL_nutrition.sqd'
vn.coef.se.ws.ADL.cbd <<- 'ws.coef.se.ADL_nutrition.cbd'
vn.coef.se.ws.ADL.log <<- 'ws.coef.se.ADL_nutrition.log'

vn.coef.se.ws.DM_Dig <<- 'ws.coef.se.DM_digest'
vn.coef.se.ws.DM_Dig.e25 <<- 'ws.coef.se.DM_digest.e25'
vn.coef.se.ws.DM_Dig.sqt <<- 'ws.coef.se.DM_digest.sqt'
vn.coef.se.ws.DM_Dig.e75 <<- 'ws.coef.se.DM_digest.e75'
vn.coef.se.ws.DM_Dig.sqd <<- 'ws.coef.se.DM_digest.sqd'
vn.coef.se.ws.DM_Dig.cbd <<- 'ws.coef.se.DM_digest.cbd'
vn.coef.se.ws.DM_Dig.log <<- 'ws.coef.se.DM_digest.log'


vn.coef.se.ws.OM_Dig <<- 'ws.coef.se.OM_digest'
vn.coef.se.ws.OM_Dig.e25 <<- 'ws.coef.se.OM_digest.e25'
vn.coef.se.ws.OM_Dig.sqt <<- 'ws.coef.se.OM_digest.sqt'
vn.coef.se.ws.OM_Dig.e75 <<- 'ws.coef.se.OM_digest.e75'
vn.coef.se.ws.OM_Dig.sqd <<- 'ws.coef.se.OM_digest.sqd'
vn.coef.se.ws.OM_Dig.cbd <<- 'ws.coef.se.OM_digest.cbd'
vn.coef.se.ws.OM_Dig.log <<- 'ws.coef.se.OM_digest.log'

vn.coef.se.ws.DM <<- 'ws.coef.se.DM_digest'
vn.coef.se.ws.DM_Dig.e25 <<- 'ws.coef.se.DM_digest.e25'
vn.coef.se.ws.DM_Dig.sqt <<- 'ws.coef.se.DM_digest.sqt'
vn.coef.se.ws.DM_Dig.e75 <<- 'ws.coef.se.DM_digest.e75'
vn.coef.se.ws.DM_Dig.sqd <<- 'ws.coef.se.DM_digest.sqd'
vn.coef.se.ws.DM_Dig.cbd <<- 'ws.coef.se.DM_digest.cbd'
vn.coef.se.ws.DM_Dig.log <<- 'ws.coef.se.DM_digest.log'


vn.coef.se.ws.DM <<- 'ws.coef.se.DM'
vn.coef.se.ws.DM.e25 <<- 'ws.coef.se.DM.e25'
vn.coef.se.ws.DM.sqt <<- 'ws.coef.se.DM.sqt'
vn.coef.se.ws.DM.e75 <<- 'ws.coef.se.DM.e75'
vn.coef.se.ws.DM.sqd <<- 'ws.coef.se.DM.sqd'
vn.coef.se.ws.DM.cbd <<- 'ws.coef.se.DM.cbd'
vn.coef.se.ws.DM.log <<- 'ws.coef.se.DM.log'






# Variable names -- coefficient statistical signifiance
vn.coef.sign.ws.bw <<- 'ws.coef.sign.bw_kg'
vn.coef.sign.ws.bw.e25 <<- 'ws.coef.sign.bw_kg.e25'
vn.coef.sign.ws.bw.sqt <<- 'ws.coef.sign.bw_kg.sqt'
vn.coef.sign.ws.bw.e75 <<- 'ws.coef.sign.bw_kg.e75'
vn.coef.sign.ws.bw.sqd <<- 'ws.coef.sign.bw_kg.sqd'
vn.coef.sign.ws.bw.cbd <<- 'ws.coef.sign.bw_kg.cbd'
vn.coef.sign.ws.bw.log <<- 'ws.coef.sign.bw_kg.log'

vn.coef.sign.ws.ADG <<- 'ws.coef.sign.adg_kg_day'
vn.coef.sign.ws.ADG.e25 <<- 'ws.coef.sign.adg_kg_day.e25'
vn.coef.sign.ws.ADG.sqt <<- 'ws.coef.sign.adg_kg_day.sqt'
vn.coef.sign.ws.ADG.e75 <<- 'ws.coef.sign.adg_kg_day.e75'
vn.coef.sign.ws.ADG.sqd <<- 'ws.coef.sign.adg_kg_day.sqd'
vn.coef.sign.ws.ADG.cbd <<- 'ws.coef.sign.adg_kg_day.cbd'
vn.coef.sign.ws.ADG.log <<- 'ws.coef.sign.adg_kg_day.log'

vn.coef.sign.ws.MY <<- 'ws.coef.sign.milk_prod_kg_d'
vn.coef.sign.ws.MY.e25 <<- 'ws.coef.sign.milk_prod_kg_d.e25'
vn.coef.sign.ws.MY.sqt <<- 'ws.coef.sign.milk_prod_kg_d.sqt'
vn.coef.sign.ws.MY.e75 <<- 'ws.coef.sign.milk_prod_kg_d.e75'
vn.coef.sign.ws.MY.sqd <<- 'ws.coef.sign.milk_prod_kg_d.sqd'
vn.coef.sign.ws.MY.cbd <<- 'ws.coef.sign.milk_prod_kg_d.cbd'
vn.coef.sign.ws.MY.log <<- 'ws.coef.sign.milk_prod_kg_d.log'

vn.coef.sign.ws.GES <<- 'ws.coef.sign.is_gestating'
vn.coef.sign.ws.GES.e25 <<- 'ws.coef.sign.is_gestating.e25'
vn.coef.sign.ws.GES.sqt <<- 'ws.coef.sign.is_gestating.sqt'
vn.coef.sign.ws.GES.e75 <<- 'ws.coef.sign.is_gestating.e75'
vn.coef.sign.ws.GES.sqd <<- 'ws.coef.sign.is_gestating.sqd'
vn.coef.sign.ws.GES.cbd <<- 'ws.coef.sign.is_gestating.cbd'
vn.coef.sign.ws.GES.log <<- 'ws.coef.sign.is_gestating.log'

vn.coef.sign.ws.NDF <<- 'ws.coef.sign.NDF'
vn.coef.sign.ws.NDF.e25 <<- 'ws.coef.sign.NDF.e25'
vn.coef.sign.ws.NDF.sqt <<- 'ws.coef.sign.NDF.sqt'
vn.coef.sign.ws.NDF.e75 <<- 'ws.coef.sign.NDF.e75'
vn.coef.sign.ws.NDF.sqd <<- 'ws.coef.sign.NDF.sqd'
vn.coef.sign.ws.NDF.cbd <<- 'ws.coef.sign.NDF.cbd'
vn.coef.sign.ws.NDF.log <<- 'ws.coef.sign.NDF.log'

vn.coef.sign.ws.ADF <<- 'ws.coef.sign.ADF'
vn.coef.sign.ws.ADF.e25 <<- 'ws.coef.sign.ADF.e25'
vn.coef.sign.ws.ADF.sqt <<- 'ws.coef.sign.ADF.sqt'
vn.coef.sign.ws.ADF.e75 <<- 'ws.coef.sign.ADF.e75'
vn.coef.sign.ws.ADF.sqd <<- 'ws.coef.sign.ADF.sqd'
vn.coef.sign.ws.ADF.cbd <<- 'ws.coef.sign.ADF.cbd'
vn.coef.sign.ws.ADF.log <<- 'ws.coef.sign.ADF.log'

vn.coef.sign.ws.CP <<- 'ws.coef.sign.CP'
vn.coef.sign.ws.CP.e25 <<- 'ws.coef.sign.CP_nutrition.e25'
vn.coef.sign.ws.CP.sqt <<- 'ws.coef.sign.CP_nutrition.sqt'
vn.coef.sign.ws.CP.e75 <<- 'ws.coef.sign.CP_nutrition.e75'
vn.coef.sign.ws.CP.sqd <<- 'ws.coef.sign.CP_nutrition.sqd'
vn.coef.sign.ws.CP.cbd <<- 'ws.coef.sign.CP_nutrition.cbd'
vn.coef.sign.ws.CP.log <<- 'ws.coef.sign.CP_nutrition.log'

vn.coef.sign.ws.EE <<- 'ws.coef.sign.EE'
vn.coef.sign.ws.EE.e25 <<- 'ws.coef.sign.EE_nutrition.e25'
vn.coef.sign.ws.EE.sqt <<- 'ws.coef.sign.EE_nutrition.sqt'
vn.coef.sign.ws.EE.e75 <<- 'ws.coef.sign.EE_nutrition.e75'
vn.coef.sign.ws.EE.sqd <<- 'ws.coef.sign.EE_nutrition.sqd'
vn.coef.sign.ws.EE.cbd <<- 'ws.coef.sign.EE_nutrition.cbd'
vn.coef.sign.ws.EE.log <<- 'ws.coef.sign.EE_nutrition.log'

vn.coef.sign.ws.Ash <<- 'ws.coef.sign.Ash'
vn.coef.sign.ws.Ash.e25 <<- 'ws.coef.sign.Ash_nutrition.e25'
vn.coef.sign.ws.Ash.sqt <<- 'ws.coef.sign.Ash_nutrition.sqt'
vn.coef.sign.ws.Ash.e75 <<- 'ws.coef.sign.Ash_nutrition.e75'
vn.coef.sign.ws.Ash.sqd <<- 'ws.coef.sign.Ash_nutrition.sqd'
vn.coef.sign.ws.Ash.cbd <<- 'ws.coef.sign.Ash_nutrition.cbd'
vn.coef.sign.ws.Ash.log <<- 'ws.coef.sign.Ash_nutrition.log'

vn.coef.sign.ws.NDF_Dig <<- 'ws.coef.sign.NDF_digest'
vn.coef.sign.ws.NDF_Dig.e25 <<- 'ws.coef.sign.NDF_digest.e25'
vn.coef.sign.ws.NDF_Dig.sqt <<- 'ws.coef.sign.NDF_digest.sqt'
vn.coef.sign.ws.NDF_Dig.e75 <<- 'ws.coef.sign.NDF_digest.e75'
vn.coef.sign.ws.NDF_Dig.sqd <<- 'ws.coef.sign.NDF_digest.sqd'
vn.coef.sign.ws.NDF_Dig.cbd <<- 'ws.coef.sign.NDF_digest.cbd'
vn.coef.sign.ws.NDF_Dig.log <<- 'ws.coef.sign.NDF_digest.log'

vn.coef.sign.ws.ADL <<- 'ws.coef.sign.ADL'
vn.coef.sign.ws.ADL.e25 <<- 'ws.coef.sign.ADL_nutrition.e25'
vn.coef.sign.ws.ADL.sqt <<- 'ws.coef.sign.ADL_nutrition.sqt'
vn.coef.sign.ws.ADL.e75 <<- 'ws.coef.sign.ADL_nutrition.e75'
vn.coef.sign.ws.ADL.sqd <<- 'ws.coef.sign.ADL_nutrition.sqd'
vn.coef.sign.ws.ADL.cbd <<- 'ws.coef.sign.ADL_nutrition.cbd'
vn.coef.sign.ws.ADL.log <<- 'ws.coef.sign.ADL_nutrition.log'

vn.coef.sign.ws.DM_Dig <<- 'ws.coef.sign.DM_digest'
vn.coef.sign.ws.DM_Dig.e25 <<- 'ws.coef.sign.DM_digest.e25'
vn.coef.sign.ws.DM_Dig.sqt <<- 'ws.coef.sign.DM_digest.sqt'
vn.coef.sign.ws.DM_Dig.e75 <<- 'ws.coef.sign.DM_digest.e75'
vn.coef.sign.ws.DM_Dig.sqd <<- 'ws.coef.sign.DM_digest.sqd'
vn.coef.sign.ws.DM_Dig.cbd <<- 'ws.coef.sign.DM_digest.cbd'
vn.coef.sign.ws.DM_Dig.log <<- 'ws.coef.sign.DM_digest.log'


vn.coef.sign.ws.OM_Dig <<- 'ws.coef.sign.OM_digest'
vn.coef.sign.ws.OM_Dig.e25 <<- 'ws.coef.sign.OM_digest.e25'
vn.coef.sign.ws.OM_Dig.sqt <<- 'ws.coef.sign.OM_digest.sqt'
vn.coef.sign.ws.OM_Dig.e75 <<- 'ws.coef.sign.OM_digest.e75'
vn.coef.sign.ws.OM_Dig.sqd <<- 'ws.coef.sign.OM_digest.sqd'
vn.coef.sign.ws.OM_Dig.cbd <<- 'ws.coef.sign.OM_digest.cbd'
vn.coef.sign.ws.OM_Dig.log <<- 'ws.coef.sign.OM_digest.log'

vn.coef.sign.ws.DM <<- 'ws.coef.sign.DM_digest'
vn.coef.sign.ws.DM_Dig.e25 <<- 'ws.coef.sign.DM_digest.e25'
vn.coef.sign.ws.DM_Dig.sqt <<- 'ws.coef.sign.DM_digest.sqt'
vn.coef.sign.ws.DM_Dig.e75 <<- 'ws.coef.sign.DM_digest.e75'
vn.coef.sign.ws.DM_Dig.sqd <<- 'ws.coef.sign.DM_digest.sqd'
vn.coef.sign.ws.DM_Dig.cbd <<- 'ws.coef.sign.DM_digest.cbd'
vn.coef.sign.ws.DM_Dig.log <<- 'ws.coef.sign.DM_digest.log'


vn.coef.sign.ws.DM <<- 'ws.coef.sign.DM'
vn.coef.sign.ws.DM.e25 <<- 'ws.coef.sign.DM.e25'
vn.coef.sign.ws.DM.sqt <<- 'ws.coef.sign.DM.sqt'
vn.coef.sign.ws.DM.e75 <<- 'ws.coef.sign.DM.e75'
vn.coef.sign.ws.DM.sqd <<- 'ws.coef.sign.DM.sqd'
vn.coef.sign.ws.DM.cbd <<- 'ws.coef.sign.DM.cbd'
vn.coef.sign.ws.DM.log <<- 'ws.coef.sign.DM.log'



# Variance inflation factors
vn.VIF.bw <<- 'vif.bw_kg'
vn.VIF.ADG <<- 'vif.ADG'
vn.VIF.MY <<- 'vif.MY'
vn.VIF.GES <<- 'vif.GES'
vn.VIF.NDF <<- 'vif.NDF_Nutrition'
vn.VIF.ADF <<- 'vif.ADF_Nutrition'
vn.VIF.CP <<- 'vif.CP_Nutrition'
vn.VIF.EE <<- 'vif.EE_Nutrition'
vn.VIF.Ash <<- 'vif.Ash_Nutrition'
vn.VIF.ADL <<- 'vif.ADL_Nutrition'
vn.VIF.NDF_Dig <<- 'vif.NDF_digest'
vn.VIF.DM_Dig <<- 'vif.DM_digest'
vn.VIF.OM_Dig <- 'vif.OM_digest'
vn.VIF.DM <- 'vif.DM'



# Coefficient estimates - boot strap
vn.coef.bs.intercept <<- 'bs.coef.intercept'

vn.coef.bs.bw <<- 'bs.coef.bw_kg'
vn.coef.bs.bw.e25 <<- 'bs.coef.bw_kg.e25'
vn.coef.bs.bw.sqt <<- 'bs.coef.bw_kg.sqt'
vn.coef.bs.bw.e75 <<- 'bs.coef.bw_kg.e75'
vn.coef.bs.bw.sqd <<- 'bs.coef.bw_kg.sqd'
vn.coef.bs.bw.cbd <<- 'bs.coef.bw_kg.cbd'
vn.coef.bs.bw.log <<- 'bs.coef.bw_kg.log'

vn.coef.bs.ADG <<- 'bs.coef.adg_kg_day'
vn.coef.bs.ADG.e25 <<- 'bs.coef.adg_kg_day.e25'
vn.coef.bs.ADG.sqt <<- 'bs.coef.adg_kg_day.sqt'
vn.coef.bs.ADG.e75 <<- 'bs.coef.adg_kg_day.e75'
vn.coef.bs.ADG.sqd <<- 'bs.coef.adg_kg_day.sqd'
vn.coef.bs.ADG.cbd <<- 'bs.coef.adg_kg_day.cbd'
vn.coef.bs.ADG.log <<- 'bs.coef.adg_kg_day.log'

vn.coef.bs.MY <<- 'bs.coef.milk_kg_day'  
vn.coef.bs.MY.e25 <<- 'bs.coef.milk_kg_day.e25'
vn.coef.bs.MY.sqt <<- 'bs.coef.milk_kg_day.sqt'
vn.coef.bs.MY.e75 <<- 'bs.coef.milk_kg_day.e75'
vn.coef.bs.MY.sqd <<- 'bs.coef.milk_kg_day.sqd'
vn.coef.bs.MY.cbd <<- 'bs.coef.milk_kg_day.cbd'
vn.coef.bs.MY.log <<- 'bs.coef.milk_kg_day.log'

vn.coef.bs.GES <<- 'bs.coef.is_gestating'  
vn.coef.bs.GES.e25 <<- 'bs.coef.is_gestating.e25'
vn.coef.bs.GES.sqt <<- 'bs.coef.is_gestating.sqt'
vn.coef.bs.GES.e75 <<- 'bs.coef.is_gestating.e75'
vn.coef.bs.GES.sqd <<- 'bs.coef.is_gestating.sqd'
vn.coef.bs.GES.cbd <<- 'bs.coef.is_gestating.cbd'
vn.coef.bs.GES.log <<- 'bs.coef.is_gestating.log'

vn.coef.bs.NDF <<- 'bs.coef.NDF'
vn.coef.bs.NDF.e25 <<- 'bs.coef.NDF.e25'
vn.coef.bs.NDF.sqt <<- 'bs.coef.NDF.sqt'
vn.coef.bs.NDF.e75 <<- 'bs.coef.NDF.e75'
vn.coef.bs.NDF.sqd <<- 'bs.coef.NDF.sqd'
vn.coef.bs.NDF.cbd <<- 'bs.coef.NDF.cbd'
vn.coef.bs.NDF.log <<- 'bs.coef.NDF.log'

vn.coef.bs.ADF <<- 'bs.coef.ADF'
vn.coef.bs.ADF.e25 <<- 'bs.coef.ADF.e25'
vn.coef.bs.ADF.sqt <<- 'bs.coef.ADF.sqt'
vn.coef.bs.ADF.e75 <<- 'bs.coef.ADF.e75'
vn.coef.bs.ADF.sqd <<- 'bs.coef.ADF.sqd'
vn.coef.bs.ADF.cbd <<- 'bs.coef.ADF.cbd'
vn.coef.bs.ADF.log <<- 'bs.coef.ADF.log'

vn.coef.bs.CP <<- 'bs.coef.CP'
vn.coef.bs.CP.e25 <<- 'bs.coef.CP_nutrition.e25'
vn.coef.bs.CP.sqt <<- 'bs.coef.CP_nutrition.sqt'
vn.coef.bs.CP.e75 <<- 'bs.coef.CP_nutrition.e75'
vn.coef.bs.CP.sqd <<- 'bs.coef.CP_nutrition.sqd'
vn.coef.bs.CP.cbd <<- 'bs.coef.CP_nutrition.cbd'
vn.coef.bs.CP.log <<- 'bs.coef.CP_nutrition.log'

vn.coef.bs.EE <<- 'bs.coef.EE'
vn.coef.bs.EE.e25 <<- 'bs.coef.EE_nutrition.e25'
vn.coef.bs.EE.sqt <<- 'bs.coef.EE_nutrition.sqt'
vn.coef.bs.EE.e75 <<- 'bs.coef.EE_nutrition.e75'
vn.coef.bs.EE.sqd <<- 'bs.coef.EE_nutrition.sqd'
vn.coef.bs.EE.cbd <<- 'bs.coef.EE_nutrition.cbd'
vn.coef.bs.EE.log <<- 'bs.coef.EE_nutrition.log'

vn.coef.bs.Ash <<- 'bs.coef.Ash'
vn.coef.bs.Ash.e25 <<- 'bs.coef.Ash_nutrition.e25'
vn.coef.bs.Ash.sqt <<- 'bs.coef.Ash_nutrition.sqt'
vn.coef.bs.Ash.e75 <<- 'bs.coef.Ash_nutrition.e75'
vn.coef.bs.Ash.sqd <<- 'bs.coef.Ash_nutrition.sqd'
vn.coef.bs.Ash.cbd <<- 'bs.coef.Ash_nutrition.cbd'
vn.coef.bs.Ash.log <<- 'bs.coef.Ash_nutrition.log'

vn.coef.bs.NDF_Dig <<- 'bs.coef.NDF_digest'
vn.coef.bs.NDF_Dig.e25 <<- 'bs.coef.NDF_digest.e25'
vn.coef.bs.NDF_Dig.sqt <<- 'bs.coef.NDF_digest.sqt'
vn.coef.bs.NDF_Dig.e75 <<- 'bs.coef.NDF_digest.e75'
vn.coef.bs.NDF_Dig.sqd <<- 'bs.coef.NDF_digest.sqd'
vn.coef.bs.NDF_Dig.cbd <<- 'bs.coef.NDF_digest.cbd'
vn.coef.bs.NDF_Dig.log <<- 'bs.coef.NDF_digest.log'


vn.coef.bs.ADL<<- 'bs.coef.Ash'
vn.coef.bs.Ash.e25 <<- 'bs.coef.Ash_nutrition.e25'
vn.coef.bs.Ash.sqt <<- 'bs.coef.Ash_nutrition.sqt'
vn.coef.bs.Ash.e75 <<- 'bs.coef.Ash_nutrition.e75'
vn.coef.bs.Ash.sqd <<- 'bs.coef.Ash_nutrition.sqd'
vn.coef.bs.Ash.cbd <<- 'bs.coef.Ash_nutrition.cbd'
vn.coef.bs.Ash.log <<- 'bs.coef.Ash_nutrition.log'

vn.coef.bs.DM_Dig <<- 'bs.coef.DM_digest'
vn.coef.bs.DM_Dig.e25 <<- 'bs.coef.DM_digest.e25'
vn.coef.bs.DM_Dig.sqt <<- 'bs.coef.DM_digest.sqt'
vn.coef.bs.DM_Dig.e75 <<- 'bs.coef.DM_digest.e75'
vn.coef.bs.DM_Dig.sqd <<- 'bs.coef.DM_digest.sqd'
vn.coef.bs.DM_Dig.cbd <<- 'bs.coef.DM_digest.cbd'
vn.coef.bs.DM_Dig.log <<- 'bs.coef.DM_digest.log'

vn.coef.bs.OM_Dig <<- 'bs.coef.OM_digest'
vn.coef.bs.OM_Dig.e25 <<- 'bs.coef.OM_digest.e25'
vn.coef.bs.OM_Dig.sqt <<- 'bs.coef.OM_digest.sqt'
vn.coef.bs.OM_Dig.e75 <<- 'bs.coef.OM_digest.e75'
vn.coef.bs.OM_Dig.sqd <<- 'bs.coef.OM_digest.sqd'
vn.coef.bs.OM_Dig.cbd <<- 'bs.coef.OM_digest.cbd'
vn.coef.bs.OM_Dig.log <<- 'bs.coef.OM_digest.log'

vn.coef.bs.DM <<- 'bs.coef.DM'
vn.coef.bs.DM.e25 <<- 'bs.coef.DM.e25'
vn.coef.bs.DM.sqt <<- 'bs.coef.DM.sqt'
vn.coef.bs.DM.e75 <<- 'bs.coef.DM.e75'
vn.coef.bs.DM.sqd <<- 'bs.coef.DM.sqd'
vn.coef.bs.DM.cbd <<- 'bs.coef.DM.cbd'
vn.coef.bs.DM.log <<- 'bs.coef.DM.log'


# Fold conditioning
vn.train.t.IDS  <<- 'train.t.IDs' 
vn.train.t.IDS.length   <<- 'train.t.IDs.length' 

vn.test.t.IDS <<- 'test.t.IDs' 
vn.test.t.IDS.length <<- 'test.t.IDs.length'

vn.t.IDS.CCs <<- 't.ID.CCs'
vn.t.IDS.CCs.remaining <<- 't.IDS.CCs.remaining'


vn.total.experimental.units <<- 'total.exp.units'

vn.all.data <<- "all.data"
vn.r.cond <<- 'r.cond'


# Predicted/observed data
vn.ws.observed <<- "ws.Observed"
vn.ws.predicted <<- "ws.Predicted"
vn.ws.predicted.base <<- "ws.Predicted.base"
vn.ws.residuals <<- "ws.Residuals"
vn.ws.rms.residuals <<- "ws.rms.Residuals"



