
# source( str_c(settings.dir,'lists.R'))



pred.vars <<- c(
  
  pred.var.BW
  , pred.var.ADG
  , pred.var.MY 
  
  , pred.var.NDF
  , pred.var.ADF
  , pred.var.CP
  , pred.var.Ash
  , pred.var.EE 
  
  , pred.var.NDF_Dig
  
  , pred.var.ADL
  ,pred.var.OM_Dig 
  , pred.var.DM_Dig 
  , pred.var.DM 
)




all.vars <- c( pred.vars , c(y.var.reg)) 







# ~ ~  - - - - - - - Subsets of variables (for producing output tables)

model.variables.identifiers <<- c(
  
  vn.loop.iter 
  , vn.iter.is.NULL
  
  # Sample classifiers
  , vn.species
  , vn.ndf
  , vn.breed
  , vn.sex 
  
  
  # Model identifiers
  , vn.mod.class
  , vn.mod.vers
  , vn.mod.form
  , vn.tier
  
  , vn.formula
  
  , vn.k
)


{

model.variables.all <<- c(
  
  model.variables.identifiers
  
  , vn.weight.list 
  
  , vn.mod.object 
  , vn.mod.object.ws 
  
  # GBR hyperparameters
  #, vn.k 
  , vn.nu
  , vn.m.stop
  
  # Model ranking
  
  , vn.best.in.formula
  , vn.is.best.tier 
  , vn.tier.rank 
  , vn.best.in.class
  , vn.best.global
  
  
  
  # Fold - specific test results
  , vn.test.r2 
  , vn.test.w.r2 
  
  , vn.test.nrmse
  , vn.test.w.nrmse
  
  , vn.test.w.aic 
  , vn.test.aic 
  
  , vn.test.ccc
  , vn.test.w.ccc 
  
  
  # Fold specific coefficients
  , vn.coef.int 
  ,vn.coef.offset.plus.intercept 
  
  ,vn.coef.BW 
  ,vn.coef.ADG 
  , vn.coef.MY
  , vn.coef.NDF 
  , vn.coef.ADF 
  ,vn.coef.CP 
  ,vn.coef.EE
  ,vn.coef.Ash
  , vn.coef.NDF_digest 
  ,vn.coef.ADL
  , vn.coef.OM_digest 
  , vn.coef.DM_digest 
  , vn.coef.DM_digest 
  
  
  # Best among hyper parameters
  ,vn.best.hypar.w.R2 
  ,vn.best.hypar.w.nRMSE 
  ,vn.best.hypar.w.CCC 
  ,vn.best.hypar.w.AIC
  
  ,vn.best.hypar.ucb.w.R2 
  ,vn.best.hypar.ucb.w.nRMSE 
  ,vn.best.hypar.ucb.w.CCC 
  ,vn.best.hypar.ucb.w.AIC 
  
  
  # Best in tier (all formulae in current tier (and class))
  ,vn.best.tier.w.R2 
  ,vn.best.tier.w.nRMSE 
  ,vn.best.tier.w.CCC 
  ,vn.best.tier.w.AIC 
  
  ,vn.best.tier.w.R2.ucb 
  ,vn.best.tier.w.nRMSE.ucb 
  ,vn.best.tier.w.CCC.ucb 
  ,vn.best.tier.w.AIC.ucb 
  
  # Global best - best among all hyperparameters, formulae, and classes
  ,vn.best.global.w.R2 
  ,vn.best.global.w.nRMSE 
  ,vn.best.global.w.CCC 
  ,vn.best.global.w.AIC
  
  ,vn.best.global.w.R2.ucb
  ,vn.best.global.w.nRMSE.ucb 
  ,vn.best.global.w.CCC.ucb 
  ,vn.best.global.w.AIC.ucb
  
  
  # Mean performance metrics across all folds
  ,vn.R2.mean 
  ,vn.nRMSE.mean 
  ,vn.CCC.mean 
  ,vn.AIC.mean 
  
  ,vn.w.R2.mean
  ,vn.w.nRMSE.mean 
  ,vn.w.CCC.mean 
  ,vn.w.AIC.mean 
  
  # SD in performance metrics across all folds
  ,vn.R2.sd
  ,vn.nRMSE.sd 
  ,vn.CCC.sd 
  ,vn.AIC.sd 
  
  ,vn.w.R2.sd 
  ,vn.w.nRMSE.sd 
  ,vn.w.CCC.sd 
  ,vn.w.AIC.sd 
  
  # UCB in performance metrics across all folds
  ,vn.R2.ucb 
  ,vn.nRMSE.ucb 
  ,vn.CCC.ucb 
  ,vn.AIC.ucb 
  
  ,vn.w.R2.ucb 
  ,vn.w.R2.ucb.ur 
  ,vn.w.nRMSE.ucb 
  ,vn.w.CCC.ucb 
  ,vn.w.AIC.ucb 
  
  
  # Fold averaged metrics
  #  ,vn.mn.coef.int 
  # ,vn.mn.coef.BW 
  # ,vn.mn.coef.ADG 
  # ,vn.mn.coef.NDF 
  # ,vn.mn.coef.CP 
  # ,vn.mn.coef.NDF_digest 
  
  
  #  ,vn.sd.coef.int 
  # ,vn.sd.coef.BW 
  # ,vn.sd.coef.ADG 
  #,vn.sd.coef.NDF 
  #,vn.sd.coef.CP 
  #,vn.sd.coef.NDF_digest 
  
  
  # Whole sample performance metrics
  ,vn.ws.R2 
  ,vn.ws.nRMSE
  ,vn.ws.CCC 
  ,vn.ws.AIC 
  
  ,vn.ws.w.R2 
  ,vn.ws.w.nRMSE
  ,vn.ws.w.CCC 
  ,vn.ws.w.AIC 
  
  # Coefficient estimates
  , vn.coef.ws.intercept 
  , vn.coef.ws.offset 
  , vn.coef.ws.offset.plus.intercept 
  
  
  
  , vn.coef.ws.bw 
  , vn.coef.ws.bw.e25 
  , vn.coef.ws.bw.sqt 
  , vn.coef.ws.bw.e75 
  , vn.coef.ws.bw.sqd 
  , vn.coef.ws.bw.cbd 
  
  , vn.coef.ws.ADG 
  , vn.coef.ws.ADG.e25 
  , vn.coef.ws.ADG.sqt 
  , vn.coef.ws.ADG.e75 
  , vn.coef.ws.ADG.sqd 
  , vn.coef.ws.ADG.cbd 
  
  , vn.coef.ws.MY 
  , vn.coef.ws.MY.e25 
  , vn.coef.ws.MY.sqt 
  , vn.coef.ws.MY.e75 
  , vn.coef.ws.MY.sqd 
  , vn.coef.ws.MY.cbd 
  
  , vn.coef.ws.GES 
  , vn.coef.ws.GES.e25 
  , vn.coef.ws.GES.sqt 
  , vn.coef.ws.GES.e75 
  , vn.coef.ws.GES.sqd 
  , vn.coef.ws.GES.cbd 
  
  , vn.coef.ws.NDF 
  , vn.coef.ws.NDF.e25 
  , vn.coef.ws.NDF.sqt 
  , vn.coef.ws.NDF.e75 
  , vn.coef.ws.NDF.sqd 
  , vn.coef.ws.NDF.cbd 
  
  , vn.coef.ws.ADF 
  , vn.coef.ws.ADF.e25 
  , vn.coef.ws.ADF.sqt 
  , vn.coef.ws.ADF.e75 
  , vn.coef.ws.ADF.sqd 
  , vn.coef.ws.ADF.cbd 
  
  , vn.coef.ws.CP 
  , vn.coef.ws.CP.e25 
  , vn.coef.ws.CP.sqt
  , vn.coef.ws.CP.e75 
  , vn.coef.ws.CP.sqd 
  , vn.coef.ws.CP.cbd 
  
  , vn.coef.ws.EE 
  , vn.coef.ws.EE.e25 
  , vn.coef.ws.EE.sqt 
  , vn.coef.ws.EE.e75 
  , vn.coef.ws.EE.sqd 
  , vn.coef.ws.EE.cbd 
  
  , vn.coef.ws.Ash 
  , vn.coef.ws.Ash.e25 
  , vn.coef.ws.Ash.sqt 
  , vn.coef.ws.Ash.e75 
  , vn.coef.ws.Ash.sqd 
  , vn.coef.ws.Ash.cbd 
  
  , vn.coef.ws.NDF_Dig 
  , vn.coef.ws.NDF_Dig.e25 
  , vn.coef.ws.NDF_Dig.sqt 
  , vn.coef.ws.NDF_Dig.e75 
  , vn.coef.ws.NDF_Dig.sqd 
  , vn.coef.ws.NDF_Dig.cbd 
  
  , vn.coef.ws.ADL 
  , vn.coef.ws.ADL.e25 
  , vn.coef.ws.ADL.sqt 
  , vn.coef.ws.ADL.e75 
  , vn.coef.ws.ADL.sqd 
  , vn.coef.ws.ADL.cbd 
  
  , vn.coef.ws.DM_Dig 
  , vn.coef.ws.DM_Dig.e25 
  , vn.coef.ws.DM_Dig.sqt 
  , vn.coef.ws.DM_Dig.e75 
  , vn.coef.ws.DM_Dig.sqd 
  , vn.coef.ws.DM_Dig.cbd 
  
  , vn.coef.ws.OM_Dig 
  , vn.coef.ws.OM_Dig.e25 
  , vn.coef.ws.OM_Dig.sqt 
  , vn.coef.ws.OM_Dig.e75 
  , vn.coef.ws.OM_Dig.sqd 
  , vn.coef.ws.OM_Dig.cbd 
  
  , vn.coef.ws.DM 
  , vn.coef.ws.DM.e25 
  , vn.coef.ws.DM.sqt 
  , vn.coef.ws.DM.e75 
  , vn.coef.ws.DM.sqd 
  , vn.coef.ws.DM.cbd 
  
  
  # Standard errors
  , vn.coef.se.ws.intercept 
  , vn.coef.se.ws.offset
  
  
  , vn.coef.se.ws.bw 
  , vn.coef.se.ws.bw.e25 
  , vn.coef.se.ws.bw.sqt 
  , vn.coef.se.ws.bw.e75 
  , vn.coef.se.ws.bw.sqd 
  , vn.coef.se.ws.bw.cbd 
  
  
  , vn.coef.se.ws.ADG 
  , vn.coef.se.ws.ADG.e25 
  , vn.coef.se.ws.ADG.sqt 
  , vn.coef.se.ws.ADG.e75 
  , vn.coef.se.ws.ADG.sqd 
  , vn.coef.se.ws.ADG.cbd 
  
  , vn.coef.se.ws.MY 
  , vn.coef.se.ws.MY.e25 
  , vn.coef.se.ws.MY.sqt 
  , vn.coef.se.ws.MY.e75 
  , vn.coef.se.ws.MY.sqd 
  , vn.coef.se.ws.MY.cbd 
  
  , vn.coef.se.ws.GES 
  , vn.coef.se.ws.GES.e25 
  , vn.coef.se.ws.GES.sqt 
  , vn.coef.se.ws.GES.e75 
  , vn.coef.se.ws.GES.sqd 
  , vn.coef.se.ws.GES.cbd 
  
  
  , vn.coef.se.ws.NDF 
  , vn.coef.se.ws.NDF.e25 
  , vn.coef.se.ws.NDF.sqt 
  , vn.coef.se.ws.NDF.e75 
  , vn.coef.se.ws.NDF.sqd 
  , vn.coef.se.ws.NDF.cbd 
  
  , vn.coef.se.ws.ADF 
  , vn.coef.se.ws.ADF.e25 
  , vn.coef.se.ws.ADF.sqt 
  , vn.coef.se.ws.ADF.e75 
  , vn.coef.se.ws.ADF.sqd 
  , vn.coef.se.ws.ADF.cbd 
  
  , vn.coef.se.ws.CP 
  , vn.coef.se.ws.CP.e25 
  , vn.coef.se.ws.CP.sqt
  , vn.coef.se.ws.CP.e75 
  , vn.coef.se.ws.CP.sqd 
  , vn.coef.se.ws.CP.cbd 
  
  , vn.coef.se.ws.EE 
  , vn.coef.se.ws.EE.e25 
  , vn.coef.se.ws.EE.sqt 
  , vn.coef.se.ws.EE.e75 
  , vn.coef.se.ws.EE.sqd 
  , vn.coef.se.ws.EE.cbd 
  
  , vn.coef.se.ws.Ash 
  , vn.coef.se.ws.Ash.e25 
  , vn.coef.se.ws.Ash.sqt 
  , vn.coef.se.ws.Ash.e75 
  , vn.coef.se.ws.Ash.sqd 
  , vn.coef.se.ws.Ash.cbd 
  
  , vn.coef.se.ws.NDF_Dig 
  , vn.coef.se.ws.NDF_Dig.e25 
  , vn.coef.se.ws.NDF_Dig.sqt 
  , vn.coef.se.ws.NDF_Dig.e75 
  , vn.coef.se.ws.NDF_Dig.sqd 
  , vn.coef.se.ws.NDF_Dig.cbd 
  
  , vn.coef.se.ws.ADL 
  , vn.coef.se.ws.ADL.e25 
  , vn.coef.se.ws.ADL.sqt 
  , vn.coef.se.ws.ADL.e75 
  , vn.coef.se.ws.ADL.sqd 
  , vn.coef.se.ws.ADL.cbd 
  
  , vn.coef.se.ws.OM_Dig 
  , vn.coef.se.ws.OM_Dig.e25 
  , vn.coef.se.ws.OM_Dig.sqt 
  , vn.coef.se.ws.OM_Dig.e75 
  , vn.coef.se.ws.OM_Dig.sqd 
  , vn.coef.se.ws.OM_Dig.cbd 
  
  , vn.coef.se.ws.DM_Dig 
  , vn.coef.se.ws.DM_Dig.e25 
  , vn.coef.se.ws.DM_Dig.sqt 
  , vn.coef.se.ws.DM_Dig.e75 
  , vn.coef.se.ws.DM_Dig.sqd 
  , vn.coef.se.ws.DM_Dig.cbd 
  
  
  , vn.coef.se.ws.DM 
  , vn.coef.se.ws.DM.e25 
  , vn.coef.se.ws.DM.sqt 
  , vn.coef.se.ws.DM.e75 
  , vn.coef.se.ws.DM.sqd 
  , vn.coef.se.ws.DM.cbd 
  
  
  # Statistical significance
  , vn.coef.sign.ws.bw 
  , vn.coef.sign.ws.bw.e25 
  , vn.coef.sign.ws.bw.sqt 
  , vn.coef.sign.ws.bw.e75 
  , vn.coef.sign.ws.bw.sqd 
  , vn.coef.sign.ws.bw.cbd 
  
  
  , vn.coef.sign.ws.ADG 
  , vn.coef.sign.ws.ADG.e25 
  , vn.coef.sign.ws.ADG.sqt 
  , vn.coef.sign.ws.ADG.e75 
  , vn.coef.sign.ws.ADG.sqd 
  , vn.coef.sign.ws.ADG.cbd 
  
  , vn.coef.sign.ws.MY 
  , vn.coef.sign.ws.MY.e25 
  , vn.coef.sign.ws.MY.sqt 
  , vn.coef.sign.ws.MY.e75 
  , vn.coef.sign.ws.MY.sqd 
  , vn.coef.sign.ws.MY.cbd 
  
  , vn.coef.sign.ws.GES 
  , vn.coef.sign.ws.GES.e25 
  , vn.coef.sign.ws.GES.sqt 
  , vn.coef.sign.ws.GES.e75 
  , vn.coef.sign.ws.GES.sqd 
  , vn.coef.sign.ws.GES.cbd 
  
  
  , vn.coef.sign.ws.NDF 
  , vn.coef.sign.ws.NDF.e25 
  , vn.coef.sign.ws.NDF.sqt 
  , vn.coef.sign.ws.NDF.e75 
  , vn.coef.sign.ws.NDF.sqd 
  , vn.coef.sign.ws.NDF.cbd 
  
  , vn.coef.sign.ws.ADF 
  , vn.coef.sign.ws.ADF.e25 
  , vn.coef.sign.ws.ADF.sqt 
  , vn.coef.sign.ws.ADF.e75 
  , vn.coef.sign.ws.ADF.sqd 
  , vn.coef.sign.ws.ADF.cbd 
  
  , vn.coef.sign.ws.CP 
  , vn.coef.sign.ws.CP.e25 
  , vn.coef.sign.ws.CP.sqt
  , vn.coef.sign.ws.CP.e75 
  , vn.coef.sign.ws.CP.sqd 
  , vn.coef.sign.ws.CP.cbd 
  
  , vn.coef.sign.ws.EE 
  , vn.coef.sign.ws.EE.e25 
  , vn.coef.sign.ws.EE.sqt 
  , vn.coef.sign.ws.EE.e75 
  , vn.coef.sign.ws.EE.sqd 
  , vn.coef.sign.ws.EE.cbd 
  
  , vn.coef.sign.ws.Ash 
  , vn.coef.sign.ws.Ash.e25 
  , vn.coef.sign.ws.Ash.sqt 
  , vn.coef.sign.ws.Ash.e75 
  , vn.coef.sign.ws.Ash.sqd 
  , vn.coef.sign.ws.Ash.cbd 
  
  , vn.coef.sign.ws.NDF_Dig 
  , vn.coef.sign.ws.NDF_Dig.e25 
  , vn.coef.sign.ws.NDF_Dig.sqt 
  , vn.coef.sign.ws.NDF_Dig.e75 
  , vn.coef.sign.ws.NDF_Dig.sqd 
  , vn.coef.sign.ws.NDF_Dig.cbd 
  
  , vn.coef.sign.ws.ADL 
  , vn.coef.sign.ws.ADL.e25 
  , vn.coef.sign.ws.ADL.sqt 
  , vn.coef.sign.ws.ADL.e75 
  , vn.coef.sign.ws.ADL.sqd 
  , vn.coef.sign.ws.ADL.cbd 
  
  , vn.coef.sign.ws.OM_Dig 
  , vn.coef.sign.ws.OM_Dig.e25 
  , vn.coef.sign.ws.OM_Dig.sqt 
  , vn.coef.sign.ws.OM_Dig.e75 
  , vn.coef.sign.ws.OM_Dig.sqd 
  , vn.coef.sign.ws.OM_Dig.cbd 
  
  , vn.coef.sign.ws.DM_Dig 
  , vn.coef.sign.ws.DM_Dig.e25 
  , vn.coef.sign.ws.DM_Dig.sqt 
  , vn.coef.sign.ws.DM_Dig.e75 
  , vn.coef.sign.ws.DM_Dig.sqd 
  , vn.coef.sign.ws.DM_Dig.cbd 
  
  
  , vn.coef.sign.ws.DM 
  , vn.coef.sign.ws.DM.e25 
  , vn.coef.sign.ws.DM.sqt 
  , vn.coef.sign.ws.DM.e75 
  , vn.coef.sign.ws.DM.sqd 
  , vn.coef.sign.ws.DM.cbd 
  
  # Variance inflation factors
  , vn.VIF.bw 
  , vn.VIF.ADG 
  , vn.VIF.MY 
  , vn.VIF.GES 
  , vn.VIF.NDF 
  , vn.VIF.ADF 
  , vn.VIF.CP 
  , vn.VIF.EE 
  , vn.VIF.Ash 
  , vn.VIF.NDF_Dig 
  , vn.VIF.DM_Dig 
  , vn.VIF.OM_Dig 
  , vn.VIF.DM
  
  
  # Variable importance (ML methods only)
  ,vn.mn.var.imp.int 
  ,vn.mn.var.imp.BW 
  ,vn.mn.var.imp.ADG 
  ,vn.mn.var.imp.NDF 
  ,vn.mn.var.imp.CP 
  ,vn.mn.var.imp.NDF_digest 
  ,vn.mn.var.imp.ADL 
  ,vn.mn.var.imp.DM_digest
  ,vn.mn.var.imp.OM_digest 
  ,vn.mn.var.imp.DM
  
  # Variable importance in relative terms 
  ,vn.mn.var.rel.imp.int 
  ,vn.mn.var.rel.imp.BW 
  ,vn.mn.var.rel.imp.ADG 
  ,vn.mn.var.rel.imp.NDF 
  ,vn.mn.var.rel.imp.CP 
  ,vn.mn.var.rel.imp.NDF_digest 
  
  ,vn.mn.var.rel.imp.ADL 
  ,vn.mn.var.rel.imp.DM_digest
  ,vn.mn.var.rel.imp.OM_digest 
  ,vn.mn.var.rel.imp.DM
  
  ,vn.sd.var.imp.rel.int 
  ,vn.sd.var.imp.rel.BW 
  ,vn.sd.var.imp.rel.ADG 
  ,vn.sd.var.imp.rel.NDF 
  ,vn.sd.var.imp.rel.CP 
  ,vn.sd.var.imp.rel.NDF_digest 
  
  ,vn.sd.var.rel.imp.ADL 
  ,vn.sd.var.rel.imp.DM_digest
  ,vn.sd.var.rel.imp.OM_digest 
  ,vn.sd.var.rel.imp.DM
  
  
  ,vn.sd.var.imp.int 
  ,vn.sd.var.imp.BW 
  ,vn.sd.var.imp.ADG 
  ,vn.sd.var.imp.NDF 
  ,vn.sd.var.imp.CP 
  ,vn.sd.var.imp.NDF_digest 
  ,vn.sd.var.imp.ADL 
  ,vn.sd.var.imp.DM_digest
  ,vn.sd.var.imp.OM_digest 
  ,vn.sd.var.imp.DM
  
  
  # Validation plots
  , vn.ws.observed
  ,  vn.ws.predicted
  , vn.ws.predicted.base
  , vn.ws.residuals
  , vn.ws.rms.residuals
  
  
  
  
  # Fold conditioning
  
  ,  vn.all.treatment.IDs
  ,  vn.all.experiment.IDs
  
  
  , vn.train.t.IDS 
  , vn.train.t.IDS.length  
  
  , vn.test.t.IDS 
  , vn.test.t.IDS.length
  
  , vn.t.IDS.CCs 
  , vn.t.IDS.CCs.remaining 
  
  
  , vn.total.exp.sample.size
  , vn.total.trt.sample.size
  , vn.total.animal.sample.size
  
  
  , vn.all.data 
  , vn.r.cond 
  
)
  
} # All out variables

{
  
model.perf.summary.variables.all <<- c(
  
  
  model.variables.identifiers
  
  
  ,  vn.best.in.formula 
  , vn.is.best.tier 
  , vn.best.in.class 
  , vn.best.global 
  
  
  
  ,vn.test.r2
  , vn.test.w.r2
  
  ,vn.test.nrmse
  , vn.test.w.nrmse
  
  
  # Fold averaged performance metrics
  # Basic (unweighted) performance metrics
  , vn.R2.mean 
  , vn.nRMSE.mean 
  , vn.CCC.mean 
  , vn.AIC.mean 
  
  , vn.R2.sd 
  , vn.nRMSE.sd 
  , vn.CCC.sd 
  , vn.AIC.sd 
  
  
  # Basic (weighted) performance metrics
  , vn.w.R2.mean 
  , vn.w.nRMSE.mean 
  , vn.w.CCC.mean 
  #, vn.w.AIC.mean 
  
  
  , vn.w.R2.sd 
  , vn.w.nRMSE.sd 
  , vn.w.CCC.sd
  , vn.w.AIC.sd 
  
  
  # Whole sample performance metrics
  ,vn.ws.R2 
  ,vn.ws.nRMSE
  ,vn.ws.CCC 
  ,vn.ws.AIC 
  
  ,vn.ws.w.R2 
  ,vn.ws.w.nRMSE
  ,vn.ws.w.CCC 
  ,vn.ws.w.AIC 
  
  
)
  
} # Performance summary variables



fold.cond.variables.all <<- c(
  
  model.variables.identifiers
  
  , vn.train.t.IDS
  , vn.train.t.IDS.length
  
  , vn.test.t.IDS
  , vn.test.t.IDS.length
  
  , vn.t.IDS.CCs
  , vn.t.IDS.CCs.remaining
  
  
  
)

variable.names.sub.sample <- c(
  
  model.variables.identifiers
  ,  vn.all.treatment.IDs
  ,  vn.all.experiment.IDs
  
  , vn.total.exp.sample.size
  , vn.total.trt.sample.size
  , vn.total.animal.sample.size
  
)



coefficients.variables.all <<-c(
  
  model.variables.identifiers
  
  
  ,  vn.coef.ws.intercept 
  , vn.coef.ws.offset 
  , vn.coef.ws.offset.plus.intercept 
  
  
  , vn.coef.ws.bw 
  , vn.coef.se.ws.bw
  , vn.coef.sign.ws.bw
  
  
  , vn.coef.ws.ADG 
  , vn.coef.se.ws.ADG
  , vn.coef.sign.ws.ADG
  
  
  , vn.coef.ws.MY
  ,  vn.coef.se.ws.MY
  ,  vn.coef.sign.ws.MY
  
  , vn.coef.ws.NDF 
  , vn.coef.se.ws.NDF
  , vn.coef.sign.ws.NDF
  
  
  , vn.coef.ws.ADF 
  , vn.coef.se.ws.ADF
  , vn.coef.sign.ws.ADF
  
  , vn.coef.ws.CP 
  , vn.coef.se.ws.CP
  , vn.coef.sign.ws.CP
  
  
  ,vn.coef.ws.Ash 
  , vn.coef.se.ws.Ash
  , vn.coef.sign.ws.Ash
  
  ,vn.coef.ws.EE 
  ,  vn.coef.se.ws.EE
  ,  vn.coef.sign.ws.EE
  
  ,vn.coef.ws.NDF_Dig
  ,  vn.coef.se.ws.DM_Dig
  ,  vn.coef.sign.ws.DM_Dig
  
  
  , vn.coef.ws.ADL
  ,  vn.coef.se.ws.ADL
  ,  vn.coef.sign.ws.ADL
  
  
  , vn.coef.ws.DM_Dig
  ,  vn.coef.se.ws.DM_Dig
  ,  vn.coef.sign.ws.DM_Dig
  
  , vn.coef.ws.OM_Dig
  ,  vn.coef.se.ws.OM_Dig
  ,  vn.coef.sign.ws.OM_Dig
  
  , vn.coef.ws.DM
  ,  vn.coef.se.ws.DM
  ,  vn.coef.sign.ws.DM
  
  
  # Extended variable coefficients
  
  , vn.coef.ws.bw.sqt
  , vn.coef.se.ws.bw.sqt
  , vn.coef.ws.bw.sqd
  , vn.coef.se.ws.bw.sqd
  
  , vn.coef.ws.ADG.sqt  
  , vn.coef.se.ws.ADG.sqt
  , vn.coef.ws.ADG.sqd
  , vn.coef.se.ws.ADG.sqd
  
  , vn.coef.ws.NDF.sqt  
  , vn.coef.se.ws.NDF.sqt
  , vn.coef.ws.NDF.sqd
  , vn.coef.se.ws.NDF.sqd
  
  , vn.coef.ws.ADF.sqt  
  , vn.coef.se.ws.ADF.sqt
  , vn.coef.ws.ADF.sqd
  , vn.coef.se.ws.ADF.sqd
  
  , vn.coef.ws.CP.sqt  
  , vn.coef.se.ws.CP.sqt
  , vn.coef.ws.CP.sqd
  , vn.coef.se.ws.CP.sqd
  
  
  # Variance inflation factors
  , vn.VIF.bw 
  , vn.VIF.ADG 
  , vn.VIF.MY 
  , vn.VIF.NDF 
  , vn.VIF.ADF 
  , vn.VIF.CP 
  , vn.VIF.EE 
  , vn.VIF.Ash 
  , vn.VIF.NDF_Dig 
  , vn.VIF.DM_Dig 
  , vn.VIF.OM_Dig 
  , vn.VIF.DM
  
) 



# Groupings of coefficient variables

# Whole sample
vns.coef.ws.BW <<- c(
  vn.coef.ws.bw
  , vn.coef.ws.bw.e25
  
  , vn.coef.ws.bw.sqt
  , vn.coef.ws.bw.e75
  , vn.coef.ws.bw.sqd
  , vn.coef.ws.bw.cbd
  , vn.coef.ws.bw.log
)


vns.coef.ws.ADG <<- c(
  vn.coef.ws.ADG
  , vn.coef.ws.ADG.e25
  
  , vn.coef.ws.ADG.sqt
  , vn.coef.ws.ADG.e75
  , vn.coef.ws.ADG.sqd
  , vn.coef.ws.ADG.cbd
  , vn.coef.ws.ADG.log
)

vns.coef.ws.MY <<- c(
  vn.coef.ws.MY
  , vn.coef.ws.MY.e25
  
  , vn.coef.ws.MY.sqt
  , vn.coef.ws.MY.e75
  , vn.coef.ws.MY.sqd
  , vn.coef.ws.MY.cbd
  , vn.coef.ws.MY.log
)

vns.coef.ws.GES <<- c(
  vn.coef.ws.GES
  , vn.coef.ws.GES.e25
  
  , vn.coef.ws.GES.sqt
  , vn.coef.ws.GES.e75
  , vn.coef.ws.GES.sqd
  , vn.coef.ws.GES.cbd
  , vn.coef.ws.GES.log
)

vns.coef.ws.NDF <<- c(
  vn.coef.ws.NDF
  , vn.coef.ws.NDF.e25
  
  , vn.coef.ws.NDF.sqt
  , vn.coef.ws.NDF.e75
  , vn.coef.ws.NDF.sqd
  , vn.coef.ws.NDF.cbd
  , vn.coef.ws.NDF.log
)

vns.coef.ws.ADF <<- c(
  vn.coef.ws.ADF
  , vn.coef.ws.ADF.e25
  
  , vn.coef.ws.ADF.sqt
  , vn.coef.ws.ADF.e75
  , vn.coef.ws.ADF.sqd
  , vn.coef.ws.ADF.cbd
  , vn.coef.ws.ADF.log
)

vns.coef.ws.CP <<- c(
  vn.coef.ws.CP
  , vn.coef.ws.CP.e25
  
  , vn.coef.ws.CP.sqt
  , vn.coef.ws.CP.e75
  , vn.coef.ws.CP.sqd
  , vn.coef.ws.CP.cbd
  , vn.coef.ws.CP.log
)

vns.coef.ws.EE <<- c(
  vn.coef.ws.EE
  , vn.coef.ws.EE.e25
  
  , vn.coef.ws.EE.sqt
  , vn.coef.ws.EE.e75
  , vn.coef.ws.EE.sqd
  , vn.coef.ws.EE.cbd
  , vn.coef.ws.EE.log
)

vns.coef.ws.Ash <<- c(
  vn.coef.ws.Ash
  , vn.coef.ws.Ash.e25
  
  , vn.coef.ws.Ash.sqt
  , vn.coef.ws.Ash.e75
  , vn.coef.ws.Ash.sqd
  , vn.coef.ws.Ash.cbd
  , vn.coef.ws.Ash.log
)

vns.coef.ws.NDF_Dig <<- c(
  vn.coef.ws.NDF_Dig
  , vn.coef.ws.NDF_Dig.e25
  
  , vn.coef.ws.NDF_Dig.sqt
  , vn.coef.ws.NDF_Dig.e75
  , vn.coef.ws.NDF_Dig.sqd
  , vn.coef.ws.NDF_Dig.cbd
  , vn.coef.ws.NDF_Dig.log
)


vns.coef.ws.ADL <<- c(
  vn.coef.ws.ADL
  , vn.coef.ws.ADL.e25
  
  , vn.coef.ws.ADL.sqt
  , vn.coef.ws.ADL.e75
  , vn.coef.ws.ADL.sqd
  , vn.coef.ws.ADL.cbd
  , vn.coef.ws.ADL.log
)

vns.coef.ws.DM_Digest <<- c(
  vn.coef.ws.DM_Dig
  , vn.coef.ws.DM_Dig.e25
  
  , vn.coef.ws.DM_Dig.sqt
  , vn.coef.ws.DM_Dig.e75
  , vn.coef.ws.DM_Dig.sqd
  , vn.coef.ws.DM_Dig.cbd
  , vn.coef.ws.DM_Dig.log
)

vns.coef.ws.OM_Digest <<- c(
  vn.coef.ws.OM_Dig
  , vn.coef.ws.OM_Dig.e25
  
  , vn.coef.ws.OM_Dig.sqt
  , vn.coef.ws.OM_Dig.e75
  , vn.coef.ws.OM_Dig.sqd
  , vn.coef.ws.OM_Dig.cbd
  , vn.coef.ws.OM_Dig.log
)

vns.coef.ws.DM <<- c(
  vn.coef.ws.DM
  , vn.coef.ws.DM.e25
  
  , vn.coef.ws.DM.sqt
  , vn.coef.ws.DM.e75
  , vn.coef.ws.DM.sqd
  , vn.coef.ws.DM.cbd
  , vn.coef.ws.DM.log
)

# STANDARD ERRORS OF COEFFICIENTS

vns.coef.se.ws.BW <<- c(
  vn.coef.se.ws.bw
  , vn.coef.se.ws.bw.e25
  
  , vn.coef.se.ws.bw.sqt
  , vn.coef.se.ws.bw.e75
  , vn.coef.se.ws.bw.sqd
  , vn.coef.se.ws.bw.cbd
  , vn.coef.se.ws.bw.log
)


vns.coef.se.ws.ADG <<- c(
  vn.coef.se.ws.ADG
  , vn.coef.se.ws.ADG.e25
  
  , vn.coef.se.ws.ADG.sqt
  , vn.coef.se.ws.ADG.e75
  , vn.coef.se.ws.ADG.sqd
  , vn.coef.se.ws.ADG.cbd
  , vn.coef.se.ws.ADG.log
)

vns.coef.se.ws.MY <<- c(
  vn.coef.se.ws.MY
  , vn.coef.se.ws.MY.e25
  
  , vn.coef.se.ws.MY.sqt
  , vn.coef.se.ws.MY.e75
  , vn.coef.se.ws.MY.sqd
  , vn.coef.se.ws.MY.cbd
  , vn.coef.se.ws.MY.log
)

vns.coef.se.ws.GES <<- c(
  vn.coef.se.ws.GES
  , vn.coef.se.ws.GES.e25
  
  , vn.coef.se.ws.GES.sqt
  , vn.coef.se.ws.GES.e75
  , vn.coef.se.ws.GES.sqd
  , vn.coef.se.ws.GES.cbd
  , vn.coef.se.ws.GES.log
)


vns.coef.se.ws.NDF <<- c(
  vn.coef.se.ws.NDF
  , vn.coef.se.ws.NDF.e25
  
  , vn.coef.se.ws.NDF.sqt
  , vn.coef.se.ws.NDF.e75
  , vn.coef.se.ws.NDF.sqd
  , vn.coef.se.ws.NDF.cbd
  , vn.coef.se.ws.NDF.log
)

vns.coef.se.ws.ADF <<- c(
  vn.coef.se.ws.ADF
  , vn.coef.se.ws.ADF.e25
  
  , vn.coef.se.ws.ADF.sqt
  , vn.coef.se.ws.ADF.e75
  , vn.coef.se.ws.ADF.sqd
  , vn.coef.se.ws.ADF.cbd
  , vn.coef.se.ws.ADF.log
)

vns.coef.se.ws.CP <<- c(
  vn.coef.se.ws.CP
  , vn.coef.se.ws.CP.e25
  
  , vn.coef.se.ws.CP.sqt
  , vn.coef.se.ws.CP.e75
  , vn.coef.se.ws.CP.sqd
  , vn.coef.se.ws.CP.cbd
  , vn.coef.se.ws.CP.log
)

vns.coef.se.ws.EE <<- c(
  vn.coef.se.ws.EE
  , vn.coef.se.ws.EE.e25
  
  , vn.coef.se.ws.EE.sqt
  , vn.coef.se.ws.EE.e75
  , vn.coef.se.ws.EE.sqd
  , vn.coef.se.ws.EE.cbd
  , vn.coef.se.ws.EE.log
)

vns.coef.se.ws.Ash <<- c(
  vn.coef.se.ws.Ash
  , vn.coef.se.ws.Ash.e25
  
  , vn.coef.se.ws.Ash.sqt
  , vn.coef.se.ws.Ash.e75
  , vn.coef.se.ws.Ash.sqd
  , vn.coef.se.ws.Ash.cbd
  , vn.coef.se.ws.Ash.log
)

vns.coef.se.ws.NDF_Dig <<- c(
  vn.coef.se.ws.NDF_Dig
  , vn.coef.se.ws.NDF_Dig.e25
  
  , vn.coef.se.ws.NDF_Dig.sqt
  , vn.coef.se.ws.NDF_Dig.e75
  , vn.coef.se.ws.NDF_Dig.sqd
  , vn.coef.se.ws.NDF_Dig.cbd
  , vn.coef.se.ws.NDF_Dig.log
)

vns.coef.se.ws.ADL <<- c(
  vn.coef.se.ws.ADL
  , vn.coef.se.ws.ADL.e25
  
  , vn.coef.se.ws.ADL.sqt
  , vn.coef.se.ws.ADL.e75
  , vn.coef.se.ws.ADL.sqd
  , vn.coef.se.ws.ADL.cbd
  , vn.coef.se.ws.ADL.log
)

vns.coef.se.ws.OM_Dig <<- c(
  vn.coef.se.ws.OM_Dig
  , vn.coef.se.ws.OM_Dig.e25
  
  , vn.coef.se.ws.OM_Dig.sqt
  , vn.coef.se.ws.OM_Dig.e75
  , vn.coef.se.ws.OM_Dig.sqd
  , vn.coef.se.ws.OM_Dig.cbd
  , vn.coef.se.ws.OM_Dig.log
)

vns.coef.se.ws.DM_Dig <<- c(
  vn.coef.se.ws.DM_Dig
  , vn.coef.se.ws.DM_Dig.e25
  
  , vn.coef.se.ws.DM_Dig.sqt
  , vn.coef.se.ws.DM_Dig.e75
  , vn.coef.se.ws.DM_Dig.sqd
  , vn.coef.se.ws.DM_Dig.cbd
  , vn.coef.se.ws.DM_Dig.log
)

vns.coef.se.ws.DM <<- c(
  vn.coef.se.ws.DM
  , vn.coef.se.ws.DM.e25
  
  , vn.coef.se.ws.DM.sqt
  , vn.coef.se.ws.DM.e75
  , vn.coef.se.ws.DM.sqd
  , vn.coef.se.ws.DM.cbd
  , vn.coef.se.ws.DM.log
)


# SIGNIFICANCE OF COEFFICIENTS

vns.coef.sign.ws.BW <<- c(
  vn.coef.sign.ws.bw
  , vn.coef.sign.ws.bw.e25
  
  , vn.coef.sign.ws.bw.sqt
  , vn.coef.sign.ws.bw.e75
  , vn.coef.sign.ws.bw.sqd
  , vn.coef.sign.ws.bw.cbd
  , vn.coef.sign.ws.bw.log
)


vns.coef.sign.ws.ADG <<- c(
  vn.coef.sign.ws.ADG
  , vn.coef.sign.ws.ADG.e25
  
  , vn.coef.sign.ws.ADG.sqt
  , vn.coef.sign.ws.ADG.e75
  , vn.coef.sign.ws.ADG.sqd
  , vn.coef.sign.ws.ADG.cbd
  , vn.coef.sign.ws.ADG.log
)

vns.coef.sign.ws.MY <<- c(
  vn.coef.sign.ws.MY
  , vn.coef.sign.ws.MY.e25
  
  , vn.coef.sign.ws.MY.sqt
  , vn.coef.sign.ws.MY.e75
  , vn.coef.sign.ws.MY.sqd
  , vn.coef.sign.ws.MY.cbd
  , vn.coef.sign.ws.MY.log
)

vns.coef.sign.ws.GES <<- c(
  vn.coef.sign.ws.GES
  , vn.coef.sign.ws.GES.e25
  
  , vn.coef.sign.ws.GES.sqt
  , vn.coef.sign.ws.GES.e75
  , vn.coef.sign.ws.GES.sqd
  , vn.coef.sign.ws.GES.cbd
  , vn.coef.sign.ws.GES.log
)


vns.coef.sign.ws.NDF <<- c(
  vn.coef.sign.ws.NDF
  , vn.coef.sign.ws.NDF.e25
  
  , vn.coef.sign.ws.NDF.sqt
  , vn.coef.sign.ws.NDF.e75
  , vn.coef.sign.ws.NDF.sqd
  , vn.coef.sign.ws.NDF.cbd
  , vn.coef.sign.ws.NDF.log
)

vns.coef.sign.ws.ADF <<- c(
  vn.coef.sign.ws.ADF
  , vn.coef.sign.ws.ADF.e25
  
  , vn.coef.sign.ws.ADF.sqt
  , vn.coef.sign.ws.ADF.e75
  , vn.coef.sign.ws.ADF.sqd
  , vn.coef.sign.ws.ADF.cbd
  , vn.coef.sign.ws.ADF.log
)

vns.coef.sign.ws.CP <<- c(
  vn.coef.sign.ws.CP
  , vn.coef.sign.ws.CP.e25
  
  , vn.coef.sign.ws.CP.sqt
  , vn.coef.sign.ws.CP.e75
  , vn.coef.sign.ws.CP.sqd
  , vn.coef.sign.ws.CP.cbd
  , vn.coef.sign.ws.CP.log
)

vns.coef.sign.ws.EE <<- c(
  vn.coef.sign.ws.EE
  , vn.coef.sign.ws.EE.e25
  
  , vn.coef.sign.ws.EE.sqt
  , vn.coef.sign.ws.EE.e75
  , vn.coef.sign.ws.EE.sqd
  , vn.coef.sign.ws.EE.cbd
  , vn.coef.sign.ws.EE.log
)

vns.coef.sign.ws.Ash <<- c(
  vn.coef.sign.ws.Ash
  , vn.coef.sign.ws.Ash.e25
  
  , vn.coef.sign.ws.Ash.sqt
  , vn.coef.sign.ws.Ash.e75
  , vn.coef.sign.ws.Ash.sqd
  , vn.coef.sign.ws.Ash.cbd
  , vn.coef.sign.ws.Ash.log
)

vns.coef.sign.ws.NDF_Dig <<- c(
  vn.coef.sign.ws.NDF_Dig
  , vn.coef.sign.ws.NDF_Dig.e25
  
  , vn.coef.sign.ws.NDF_Dig.sqt
  , vn.coef.sign.ws.NDF_Dig.e75
  , vn.coef.sign.ws.NDF_Dig.sqd
  , vn.coef.sign.ws.NDF_Dig.cbd
  , vn.coef.sign.ws.NDF_Dig.log
)

vns.coef.sign.ws.ADL <<- c(
  vn.coef.sign.ws.ADL
  , vn.coef.sign.ws.ADL.e25
  
  , vn.coef.sign.ws.ADL.sqt
  , vn.coef.sign.ws.ADL.e75
  , vn.coef.sign.ws.ADL.sqd
  , vn.coef.sign.ws.ADL.cbd
  , vn.coef.sign.ws.ADL.log
)

vns.coef.sign.ws.OM_Dig <<- c(
  vn.coef.sign.ws.OM_Dig
  , vn.coef.sign.ws.OM_Dig.e25
  
  , vn.coef.sign.ws.OM_Dig.sqt
  , vn.coef.sign.ws.OM_Dig.e75
  , vn.coef.sign.ws.OM_Dig.sqd
  , vn.coef.sign.ws.OM_Dig.cbd
  , vn.coef.sign.ws.OM_Dig.log
)

vns.coef.sign.ws.DM_Dig <<- c(
  vn.coef.sign.ws.DM_Dig
  , vn.coef.sign.ws.DM_Dig.e25
  
  , vn.coef.sign.ws.DM_Dig.sqt
  , vn.coef.sign.ws.DM_Dig.e75
  , vn.coef.sign.ws.DM_Dig.sqd
  , vn.coef.sign.ws.DM_Dig.cbd
  , vn.coef.sign.ws.DM_Dig.log
)

vns.coef.sign.ws.DM <<- c(
  vn.coef.sign.ws.DM
  , vn.coef.sign.ws.DM.e25
  
  , vn.coef.sign.ws.DM.sqt
  , vn.coef.sign.ws.DM.e75
  , vn.coef.sign.ws.DM.sqd
  , vn.coef.sign.ws.DM.cbd
  , vn.coef.sign.ws.DM.log
)

# Bootstrap coefficients

# Whole sample
vns.coef.bs.BW <<- c(
  vn.coef.bs.bw
  , vn.coef.bs.bw.e25
  
  , vn.coef.bs.bw.sqt
  , vn.coef.bs.bw.e75
  , vn.coef.bs.bw.sqd
  , vn.coef.bs.bw.cbd
  , vn.coef.bs.bw.log
)


vns.coef.bs.ADG <<- c(
  vn.coef.bs.ADG
  , vn.coef.bs.ADG.e25
  
  , vn.coef.bs.ADG.sqt
  , vn.coef.bs.ADG.e75
  , vn.coef.bs.ADG.sqd
  , vn.coef.bs.ADG.cbd
  , vn.coef.bs.ADG.log
)


vns.coef.bs.MY <<- c(
  vn.coef.bs.MY
  , vn.coef.bs.MY.e25
  
  , vn.coef.bs.MY.sqt
  , vn.coef.bs.MY.e75
  , vn.coef.bs.MY.sqd
  , vn.coef.bs.MY.cbd
  , vn.coef.bs.MY.log
)


vns.coef.bs.GES <<- c(
  vn.coef.bs.GES
  , vn.coef.bs.GES.e25
  
  , vn.coef.bs.GES.sqt
  , vn.coef.bs.GES.e75
  , vn.coef.bs.GES.sqd
  , vn.coef.bs.GES.cbd
  , vn.coef.bs.GES.log
)

vns.coef.bs.NDF <<- c(
  vn.coef.bs.NDF
  , vn.coef.bs.NDF.e25
  
  , vn.coef.bs.NDF.sqt
  , vn.coef.bs.NDF.e75
  , vn.coef.bs.NDF.sqd
  , vn.coef.bs.NDF.cbd
  , vn.coef.bs.NDF.log
)

vns.coef.bs.ADF <<- c(
  vn.coef.bs.ADF
  , vn.coef.bs.ADF.e25
  
  , vn.coef.bs.ADF.sqt
  , vn.coef.bs.ADF.e75
  , vn.coef.bs.ADF.sqd
  , vn.coef.bs.ADF.cbd
  , vn.coef.bs.ADF.log
)

vns.coef.bs.CP <<- c(
  vn.coef.bs.CP
  , vn.coef.bs.CP.e25
  
  , vn.coef.bs.CP.sqt
  , vn.coef.bs.CP.e75
  , vn.coef.bs.CP.sqd
  , vn.coef.bs.CP.cbd
  , vn.coef.bs.CP.log
)

vns.coef.bs.EE <<- c(
  vn.coef.bs.EE
  , vn.coef.bs.EE.e25
  
  , vn.coef.bs.EE.sqt
  , vn.coef.bs.EE.e75
  , vn.coef.bs.EE.sqd
  , vn.coef.bs.EE.cbd
  , vn.coef.bs.EE.log
)

vns.coef.bs.Ash <<- c(
  vn.coef.bs.Ash
  , vn.coef.bs.Ash.e25
  
  , vn.coef.bs.Ash.sqt
  , vn.coef.bs.Ash.e75
  , vn.coef.bs.Ash.sqd
  , vn.coef.bs.Ash.cbd
  , vn.coef.bs.Ash.log
)

vns.coef.bs.NDF_Dig <<- c(
  vn.coef.bs.NDF_Dig
  , vn.coef.bs.NDF_Dig.e25
  
  , vn.coef.bs.NDF_Dig.sqt
  , vn.coef.bs.NDF_Dig.e75
  , vn.coef.bs.NDF_Dig.sqd
  , vn.coef.bs.NDF_Dig.cbd
  , vn.coef.bs.NDF_Dig.log
)


vns.coef.bs.ADL<<- c(
  vn.coef.bs.Ash
  , vn.coef.bs.Ash.e25
  
  , vn.coef.bs.Ash.sqt
  , vn.coef.bs.Ash.e75
  , vn.coef.bs.Ash.sqd
  , vn.coef.bs.Ash.cbd
  , vn.coef.bs.Ash.log
)

vns.coef.bs.OM_Dig <<- c(
  vn.coef.bs.OM_Dig
  , vn.coef.bs.OM_Dig.e25
  
  , vn.coef.bs.OM_Dig.sqt
  , vn.coef.bs.OM_Dig.e75
  , vn.coef.bs.OM_Dig.sqd
  , vn.coef.bs.OM_Dig.cbd
  , vn.coef.bs.OM_Dig.log
)

vns.coef.bs.DM_Dig <<- c(
  vn.coef.bs.DM_Dig
  , vn.coef.bs.DM_Dig.e25
  
  , vn.coef.bs.DM_Dig.sqt
  , vn.coef.bs.DM_Dig.e75
  , vn.coef.bs.DM_Dig.sqd
  , vn.coef.bs.DM_Dig.cbd
  , vn.coef.bs.DM_Dig.log
)

vns.coef.bs.DM <<- c(
  vn.coef.bs.DM
  , vn.coef.bs.DM.e25
  
  , vn.coef.bs.DM.sqt
  , vn.coef.bs.DM.e75
  , vn.coef.bs.DM.sqd
  , vn.coef.bs.DM.cbd
  , vn.coef.bs.DM.log
)

vns.coef.bs.All <<- c(
  vns.coef.bs.BW
  ,vns.coef.bs.ADG
  
  
  ,vns.coef.bs.NDF
  ,vns.coef.bs.ADF
  
  ,vns.coef.bs.CP
  
  ,vns.coef.bs.EE
  ,vns.coef.bs.Ash
  ,vns.coef.bs.NDF_Dig
  
  ,vns.coef.bs.ADL
  ,vns.coef.bs.OM_Dig
  ,vns.coef.bs.DM_Dig
  ,vns.coef.bs.DM
)





all.x.vars <<- c(  
  
  
  "bw_kg" 
  #, 'BW_frac_Mat_BW'
  #, 'Met_bw_kg'
  , "bw_kg.e25" 
  , "bw_kg.sqt" 
  , "bw_kg.e75"  
  , "bw_kg.sqd" 
  , "bw_kg.cbd"  
  
  , "bw_kg.log" 
  
  , "adg_kg_day"
  , "adg_kg_day.e25"
  , "adg_kg_day.sqt"
  , "adg_kg_day.e75"
  , "adg_kg_day.sqd"
  , "adg_kg_day.cbd"
  , "adg_kg_day.log"
  
  , "milk_prod_kg_d"  
  , "milk_prod_kg_d.e25"
  , "milk_prod_kg_d.sqt"
  , "milk_prod_kg_d.e75"
  , "milk_prod_kg_d.sqd"
  , "milk_prod_kg_d.cbd"
  , "milk_prod_kg_d.log"
  
  , "is_gestating"  
  , "is_gestating.e25"
  , "is_gestating.sqt"
  , "is_gestating.e75"
  , "is_gestating.sqd"
  , "is_gestating.cbd"
  , "is_gestating.log"
  
  , "NDF_nutrition"
  , "NDF_nutrition.e25"
  , "NDF_nutrition.sqt"
  , "NDF_nutrition.e75"
  , "NDF_nutrition.sqd"
  , "NDF_nutrition.cbd"
  , "NDF_nutrition.log"
  
  
  , "ADF_nutrition"
  , "ADF_nutrition.e25"
  , "ADF_nutrition.sqt"
  , "ADF_nutrition.e75"
  , "ADF_nutrition.sqd"
  , "ADF_nutrition.cbd"
  , "ADF_nutrition.log"
  
  , "CP_nutrition"
  , "CP_nutrition.e25"
  , "CP_nutrition.sqt"
  , "CP_nutrition.e75"
  , "CP_nutrition.sqd"
  , "CP_nutrition.cbd"
  , "CP_nutrition.log"
  
  , "NDF_digest"
  , "NDF_digest.e25"
  , "NDF_digest.sqt"
  , "NDF_digest.e75"
  , "NDF_digest.sqd"
  , "NDF_digest.cbd"
  , "NDF_digest.log"
  
  
  
  , "EE_nutrition"
  , "EE_nutrition.e25"
  , "EE_nutrition.sqt"
  , "EE_nutrition.e75"
  , "EE_nutrition.sqd"
  , "EE_nutrition.cbd"
  , "EE_nutrition.log"
  
  
  , "Ash_nutrition"
  , "Ash_nutrition.e25"
  , "Ash_nutrition.sqt"
  , "Ash_nutrition.e75"
  , "Ash_nutrition.sqd"
  , "Ash_nutrition.cbd"
  , "Ash_nutrition.log"
  
  , "ADL_nutrition"
  , "ADL_nutrition.e25"
  , "ADL_nutrition.sqt"
  , "ADL_nutrition.e75"
  , "ADL_nutrition.sqd"
  , "ADL_nutrition.cbd"
  , "ADL_nutrition.log"
  
  , "OM_digest"
  , "OM_digest.e25"
  , "OM_digest.sqt"
  , "OM_digest.e75"
  , "OM_digest.sqd"
  , "OM_digest.cbd"
  , "OM_digest.log"
  
  , "DM_digest"
  , "DM_digest.e25"
  , "DM_digest.sqt"
  , "DM_digest.e75"
  , "DM_digest.sqd"
  , "DM_digest.cbd"
  , "DM_digest.log"
  
  
  #, "DM_Nutrition"
  #, "DM_Nutrition.e25"
  #, "DM_Nutrition.sqt"
  #, "DM_Nutrition.e75"
  #, "DM_Nutrition.sqd"
  #, "DM_Nutrition.cbd"
  #, "DM_Nutrition.log"
  
  # Interaction terms
  
  #  , 'NDF_x_NDF_digest'
  
  # , 'is.adult'
  #, 'is.male'
  # ,  "breed.type" 
  ,   'ue.id'
)  




main.x.vars <<- all.x.vars[ which( 
  
  !(
    str_detect( all.x.vars ,c(suffixes)[1] ) |
      str_detect( all.x.vars ,c(suffixes)[2] )  |
      str_detect( all.x.vars ,c(suffixes)[3] )  |
      str_detect( all.x.vars ,c(suffixes)[4] )  |
      str_detect( all.x.vars ,c(suffixes)[5] )  |
      str_detect( all.x.vars ,c(suffixes)[6] )  |
      str_detect( all.x.vars ,c(suffixes)[7] )  
  )
)   
]


all.BW.vars <<- all.x.vars[which(  str_detect(  all.x.vars ,'bw' ) ) ] 
all.ADG.vars <<- all.x.vars[which(  str_detect(  all.x.vars ,'adg' ) ) ] 

all.NDF.vars <<- all.x.vars[which(  str_detect(  all.x.vars ,'NDF' ) ) ] 
all.ADF.vars <<- all.x.vars[which(  str_detect(  all.x.vars ,'ADF' ) ) ] 
all.CP.vars <<- all.x.vars[which(  str_detect(  all.x.vars ,'CP' ) ) ] 
all.EE.vars <<- all.x.vars[which(  str_detect(  all.x.vars ,'EE' ) ) ] 
all.Ash.vars <<- all.x.vars[which(  str_detect(  all.x.vars ,'Ash' ) ) ] 
all.NDF_digest.vars <<- all.x.vars[which(  str_detect(  all.x.vars ,'NDF_digest' ) ) ] 

all.DM_digest.vars <<- all.x.vars[which(  str_detect(  all.x.vars ,'DM_digest' ) ) ] 
all.OM_digest.vars <<- all.x.vars[which(  str_detect(  all.x.vars ,'OM_digest' ) ) ] 
all.DM.vars <<- all.x.vars[which(  str_detect(  all.x.vars ,'DM_Nutrition' ) ) ] 
all.ADL.vars <<- all.x.vars[which(  str_detect(  all.x.vars ,'ADL_Nutrition' ) ) ] 


suffixes.labs <<-  c(  
  # subsc('i')
  supsc("1/4") 
  , supsc("1/2") 
  , supsc("3/4") 
  , supsc("2") 
  , supsc("3") 
  # , subsc("MS") 
  # , subsc("MMS") 
)



form.aliases <<- rep(NA, length(all.x.vars) )

label.alias.BW <<- 'BW'
label.alias.MBW <<- 'Met_BW'
label.alias.BWFM <<- 'BW_Frac_MBW'
label.alias.ADG <<-'ADG'

label.alias.MY <<-'MY'
label.alias.GES <<-'GES'

label.alias.NDF <<- 'NDF'
label.alias.ADF <<- 'ADF'
label.alias.EE <<- 'EE'
label.alias.Ash <<- 'Ash'
label.alias.CP <<- 'CP'
label.alias.NDFD <<- 'NDFD'
label.alias.ADL <<- 'ADL'
label.alias.OMD <<- 'OMD'
label.alias.DMD <<- 'DMD'
label.alias.DM <<- 'DM'

label.alias.NDFxNDFD <<- 'NDF*NDFD'

for (e in 1:length(all.x.vars)){
  
  # test c.var <- x.vars[1]
  
  c.var <- all.x.vars[e]
  
  if ( str_detect( c.var , 'bw_kg' )   ){  lab.2.add <- label.alias.BW }
  if ( str_detect( c.var , 'bw_kg.e75' )   ){  lab.2.add <- label.alias.MBW }
  if ( str_detect( c.var , 'BW_frac_Mat_BW' )   ){  lab.2.add <- label.alias.BWFM  }
  
  if ( str_detect( c.var , 'adg' )   ){  lab.2.add <- label.alias.ADG  } 
  if ( str_detect( c.var , 'milk' )   ){  lab.2.add <- label.alias.MY } 
  if ( str_detect( c.var , 'gest' )   ){  lab.2.add <- label.alias.GES  } 
  
  if ( str_detect( c.var , 'CP' )   ){  lab.2.add <- label.alias.CP  } 
  
  
  
  if ( str_detect( c.var , 'NDF' )   ){  lab.2.add <- label.alias.NDF } 
  if ( str_detect( c.var , 'ADF' )   ){  lab.2.add <- label.alias.ADF } 
  if ( str_detect( c.var , 'EE' )   ){  lab.2.add <- label.alias.EE } 
  if ( str_detect( c.var , 'Ash' )   ){  lab.2.add <- label.alias.Ash } 
  
  if ( str_detect( c.var , 'ADL' )   ){  lab.2.add <- label.alias.ADL } 
  
  if ( str_detect( c.var , 'DM_Dig' )   ){  lab.2.add <- label.alias.DMD } 
  if ( str_detect( c.var , 'OM_Dig' )   ){  lab.2.add <- label.alias.OMD } 
  if ( str_detect( c.var , 'DM_N' )   ){  lab.2.add <- label.alias.DM  } 
  
  if ( str_detect( c.var , 'NDF_x_NDF_digest' )   ){  lab.2.add <- label.alias.NDFxNDFD } 
  
  
  
  form.aliases[e] <- lab.2.add
  
}  


form.aliases <<-   form.aliases


