

source.all <- function(){ 
  
  rm(list = ls())
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  source(  str_c(  'Settings/' ,'libraries.R') )  
  
  source(  str_c(  'Settings/'    ,'parameters.R') )
  
  source(  str_c( settings.dir    ,'variables.R') )
  
  source(  str_c( settings.dir    ,'lists.R') )
  
  source(  str_c(  helpers.dir   ,'functions.R') )
  
  source(  str_c(  settings.dir   ,'model.specs.R') )  
  
    source('data.prep.R')
  
  
  # Data prep
  CV.prep <<-  function() { source(   str_c(  modules.dir  ,'cv.dat.prep.R')  ) }
  
  formulae.defs <<-  function() { source(   str_c( modules.dir,'formulae.R')  ) }
  
  # Results output
  coefs.table.out <<-  function() { source(   str_c(  results.dir  ,'table.coefs.R')  ) }
  
  box.fig.out <<-  function() { source(   str_c(  results.dir  ,'box.plot.R')  ) }
  
  valid.plots.out <<-  function() { source(   str_c(  results.dir  ,'valid.plots.R')  ) }
  

}
