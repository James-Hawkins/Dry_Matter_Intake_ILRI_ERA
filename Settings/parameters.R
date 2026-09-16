

# coefficients and constants

base.nat.log.e <<- 2.71828
SE.scalar.90.pci <<- 1.65 ; SE.scalar.95.pci <<- 1.95 ; SE.scalar.99.pci <<- 2.65


separator <<- ' + '

# File paths
# Main folders
parent.dir <<- '../' 
helpers.dir <<- 'Helpers/'
results.dir <<- 'Results/'
data.dir <<- 'Data.prep/'
modules.dir <<- 'Modules/'
settings.dir <<- 'Settings/'
results.out.dir <<- 'Results/Results.out/'

results.out.dir.bv <<- 'Results/Results.out/Bovines/'
results.out.dir.sp <<- 'Results/Results.out/Sheep/'
results.out.dir.gt <<- 'Results/Results.out/Goats/'

results.out.dir.bv.data <<- 'Results/Results.out/Bovines/Data/'
results.out.dir.bv.figs <<- 'Results/Results.out/Bovines/Figures/'

ndf.lev.lo <<- 'low'
ndf.lev.hi <<- 'hi'

ndf.lev.lo.lab <<- 'Low NDF'
ndf.lev.hi.lab <<- 'High NDF'



ndf.thresh.lo <<- 400
ndf.thresh.hi <<- 400

ndf.thresh.sheep <<- 385
ndf.thresh.goat <<- 420
ndf.thresh.cattle <<- 450


# Sets
na.value <<- 'NA'


species.cattle <<- "Cattle"
species.goat <<-  "Goat" 
species.sheep <<- "Sheep"
species.srs <<- c( species.sheep ,   species.goat )




stage.growing.male <<- "growing.male"
stage.growing.female <<- "growing.female"
stage.growing.all <<-  "growing.all" 


stage.adult.male <<- "adult.male"
stage.adult.female <<- "adult.female"
stage.adult.all <<-  "adult.all"


stage.lactating <<- "lactating.adult.female"
stage.gestating <<- "gestating.adult.female"

unique.stages.cond <<- c( 'Growing' , 'Mature')


breed.types <<- c(1,2)
breed.type.labels.bv <<- c('Bos indicus' , 'Exotic/cross')

# Production systems
prod.systs <<- c(1,2)
prod.sys.ext <<- 1 ; prod.sys.sint <- 2


# Outlier variable ranges 
cutoff.ol.s.residual.method <<- 3.5 # Number of standard deviations in regression residual for which higher values get omitted from sample



max.adg.g.day  <<- 2000
min.adg.g.day  <<- -2000

max.ndf.g.kg <<- 0
min.ndf.g.kg <<- 0


suffx.mm.s <<- 'min.max.s'
suffx.mean.s <<- 'mean.s'



ndf.levs <- c( ndf.lev.lo ,ndf.lev.hi )
ndf.labs <- c( 'low NDF' ,'high NDF')


species.lc.plural.labs <- c('sheep' , 'goats')

   
gbr.reg.variables <<- c(
  
  'Species'
  , 'ndf.level'
  , 'species.ndf'
  , "T.Animals"  
  , 'Sample.size'
  
  , "B.Code" 
  ,   "A.Level.Name" 
  
  
  , 'feed_intake_g_d'
  , 'DM_nutrition'
  
  
  , "NDF_nutrition" 
  , "CP_nutrition" 
  , "bw_kg" 
  ,  "adg_g_day"
  ,  "NDF_digest"
  
  , "Ash_nutrition"
 , "EE_nutrition" 
  
)



# Specify which Y variable to use
y.var <<- 0
y.var[1] <- 'feed_intake_g_d'
y.var[2] <- 'feed_intake_kg_d'
y.var[3] <- 'feeding.level.g.d.g.bw'
y.var[4] <- 'feeding.level.g.d.kg.bw'

y.var.reg <<- y.var[1]


gg.freq.label.clr <<- "white"
gg.freq.label.sz  <<- 5.5
gg.freq.label.xc <<- 0.00725
gg.freq.label.yc <<- 0.975


color.grad.freq.hi <<- '#2F5F8A'
color.grad.freq.lo <<-  '#75A2BF'


gg.freq.group.fs.color <- gg.freq.label.clr

# Sample size equation to weight formulae


sample.weight.var <<- 'Sample.size'

sample.coef.weight <<- 0.75


# Outlier handling

ol.rm.gr.sheep.lo.ndf <<- FALSE
ol.rm.gr.sheep.hi.ndf <<- FALSE
ol.rm.gr.goat.lo.ndf <<- FALSE
ol.rm.gr.goat.hi.ndf <<- FALSE




rd.decs.R2 <<- 2
rd.decs.nRMSE <<- 1
rd.decs.CCC <<- 2
rd.decs.AIC <<- 0
rd.decs.VIF <<- 1

rd.decs.coefs <<- 1
rd.decs.SEs <<- 1
rd.decs.corr <<- 2
