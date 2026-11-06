



cond.bov.all <-  (d.reg.in$Species == species.cattle & d.reg.in$min.data)



# After outlier removing
cond.bov.final.all <-  ( 
  
  cnd.bv.lo.ndf.Bind.male
  |    cnd.bv.lo.ndf.Bind.female
  
  | cnd.bv.hi.ndf.Bind.male
  |    cnd.bv.hi.ndf.Bind.female
  
  | cnd.bv.lo.ndf.Btau.male
  |    cnd.bv.lo.ndf.Btau.female
  
  | cnd.bv.hi.ndf.Btau.male
  |    cnd.bv.hi.ndf.Btau.female
  
  )


length( unique( d.reg.in[ cond.bov.final.all,  'ue.id' ]  ))
length( unique( d.reg.in[ cond.bov.final.all,  'ut.id' ]  ))

# STRATA 2

# BREED GROUPINGS
# NUMBER OF EXPERIMENTS ( k = ...)
length( unique( d.reg.in[  cnd.bv.hi.ndf.Bind.female |   cnd.bv.hi.ndf.Bind.male |  cnd.bv.lo.ndf.Bind.female |   cnd.bv.lo.ndf.Bind.male  ,  'ue.id' ]  ))
length( unique( d.reg.in[  cnd.bv.hi.ndf.Btau.female |   cnd.bv.hi.ndf.Btau.male |  cnd.bv.lo.ndf.Btau.female |   cnd.bv.lo.ndf.Btau.male  ,  'ue.id' ]  ))

length( unique( d.reg.in[  cnd.bv.hi.ndf.Bind.female |   cnd.bv.hi.ndf.Bind.male |  cnd.bv.lo.ndf.Bind.female |   cnd.bv.lo.ndf.Bind.male  ,  'ut.id' ]  ))
length( unique( d.reg.in[  cnd.bv.hi.ndf.Btau.female |   cnd.bv.hi.ndf.Btau.male |  cnd.bv.lo.ndf.Btau.female |   cnd.bv.lo.ndf.Btau.male  ,  'ut.id' ]  ))


# NDF GROUPINGS
# NUMBER OF EXPERIMENTS ( k = ...)
length( unique( d.reg.in[  cnd.bv.lo.ndf.Bind.female |   cnd.bv.lo.ndf.Bind.male |  cnd.bv.lo.ndf.Btau.female |  cnd.bv.lo.ndf.Btau.male ,  'ue.id' ]  ))
length( unique( d.reg.in[  cnd.bv.hi.ndf.Bind.female |   cnd.bv.hi.ndf.Bind.male |  cnd.bv.hi.ndf.Btau.female |  cnd.bv.hi.ndf.Btau.male ,  'ue.id' ]  ))

length( unique( d.reg.in[  cnd.bv.lo.ndf.Bind.female |   cnd.bv.lo.ndf.Bind.male |  cnd.bv.lo.ndf.Btau.female |  cnd.bv.lo.ndf.Btau.male ,  'ut.id' ]  ))
length( unique( d.reg.in[  cnd.bv.hi.ndf.Bind.female |   cnd.bv.hi.ndf.Bind.male |  cnd.bv.hi.ndf.Btau.female |  cnd.bv.hi.ndf.Btau.male ,  'ut.id' ]  ))



# SEX GROUPINGS
# NUMBER OF EXPERIMENTS ( k = ...)
length( unique( d.reg.in[  cnd.bv.lo.ndf.Bind.female |   cnd.bv.hi.ndf.Bind.female |  cnd.bv.lo.ndf.Btau.female |  cnd.bv.hi.ndf.Btau.female ,  'ue.id' ]  ))
length( unique( d.reg.in[  cnd.bv.lo.ndf.Bind.male |   cnd.bv.hi.ndf.Bind.male |  cnd.bv.lo.ndf.Btau.male |  cnd.bv.hi.ndf.Btau.male ,  'ue.id' ]  ))

length( unique( d.reg.in[  cnd.bv.lo.ndf.Bind.female |   cnd.bv.hi.ndf.Bind.female |  cnd.bv.lo.ndf.Btau.female |  cnd.bv.hi.ndf.Btau.female ,  'ut.id' ]  ))
length( unique( d.reg.in[  cnd.bv.lo.ndf.Bind.male |   cnd.bv.hi.ndf.Bind.male |  cnd.bv.lo.ndf.Btau.male |  cnd.bv.hi.ndf.Btau.male ,  'ut.id' ]  ))


length( unique( d.reg.in[    ,  'ue.id' ]  ))





# STRATA 1
# NUMBER OF EXPERIMENTS ( k = ...)
length( unique( d.reg.in[  cnd.bv.hi.ndf.Bind.female,  'ue.id' ]  ))
length( unique( d.reg.in[  cnd.bv.hi.ndf.Bind.male,  'ue.id' ]  ))

length( unique( d.reg.in[  cnd.bv.lo.ndf.Bind.female,  'ue.id' ]  ))
length( unique( d.reg.in[  cnd.bv.lo.ndf.Bind.male,  'ue.id' ]  ))

length( unique( d.reg.in[  cnd.bv.hi.ndf.Btau.female,  'ue.id' ]  ))
length( unique( d.reg.in[  cnd.bv.hi.ndf.Btau.male,  'ue.id' ]  ))

length( unique( d.reg.in[  cnd.bv.lo.ndf.Btau.female,  'ue.id' ]  ))
length( unique( d.reg.in[  cnd.bv.lo.ndf.Btau.male,  'ue.id' ]  ))



# NUMBER OF TREATMENTS ( n = ...)
length( unique( d.reg.in[  cnd.bv.hi.ndf.Bind.female,  'ut.id' ]  ))
length( unique( d.reg.in[  cnd.bv.hi.ndf.Bind.male,  'ut.id' ]  ))

length( unique( d.reg.in[  cnd.bv.lo.ndf.Bind.female,  'ut.id' ]  ))
length( unique( d.reg.in[  cnd.bv.lo.ndf.Bind.male,  'ut.id' ]  ))

length( unique( d.reg.in[  cnd.bv.hi.ndf.Btau.female,  'ut.id' ]  ))
length( unique( d.reg.in[  cnd.bv.hi.ndf.Btau.male,  'ut.id' ]  ))

length( unique( d.reg.in[  cnd.bv.lo.ndf.Btau.female,  'ut.id' ]  ))
length( unique( d.reg.in[  cnd.bv.lo.ndf.Btau.male,  'ut.id' ]  ))




length(unique(d[ d$Species == species.cattle & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.cattle & d$is.growing == TRUE & !is.na(d$ndf.level),'B.Code']))


# By breed

# Sheep
length(unique(d[ d$Species == species.sheep  & d$breed.type == 1 & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.sheep & d$breed.type == 1 & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))

length(unique(d[ d$Species == species.sheep  & d$breed.type == 2 & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.sheep & d$breed.type == 2 & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))

# Goats
length(unique(d[ d$Species == species.goat  & d$breed.type == 1 & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.goat & d$breed.type == 1 & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))

length(unique(d[ d$Species == species.goat  & d$breed.type == 2 & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.goat  & d$breed.type == 2 & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))

# By sex
# Sheep
length(unique(d[ d$Species == species.sheep  & d$is.male  & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.sheep & d$is.male  & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))

length(unique(d[ d$Species == species.sheep  & !d$is.male & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.sheep & !d$is.male& d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))

# Goats
length(unique(d[ d$Species == species.goat  & d$is.male & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.goat & d$is.male & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))

length(unique(d[ d$Species == species.goat  & !d$is.male & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.goat  & !d$is.male & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))





length(unique(d[ d$Species == species.sheep & d$is.growing == TRUE & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.sheep & d$is.growing == TRUE & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))

length(unique(d[ d$Species == species.sheep & d$is.growing == FALSE & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.sheep & d$is.growing == FALSE & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))




length(unique(d[ d$Species == species.goat & d$is.growing == TRUE & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.goat & d$is.growing == TRUE & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))


length(unique(d[ d$Species == species.sheep & d$is.adult == TRUE & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.sheep & d$is.adult == TRUE & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))

length(unique(d[ d$Species == species.goat & d$is.adult == TRUE & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
length(unique(d[ d$Species == species.goat & d$is.adult == TRUE & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))


# Species tallies

length(unique(d[ d$Species == species.sheep & d$min.data   ,'B.Code']))
length(unique(d[ d$Species == species.sheep & d$min.data   ,'A.Level.Name']))

length(unique(d[ d$Species == species.goat & d$min.data  ,'B.Code']))
length(unique(d[ d$Species == species.goat & d$min.data  ,'A.Level.Name']))


# Growing vs. adult
length(unique(d[ d$Species == species.sheep & d$is.growing & d$min.data ,'B.Code']))
length(unique(d[ d$Species == species.sheep & !d$is.growing & d$min.data ,'B.Code']))

length(unique(d[ d$Species == species.goat & d$is.growing & d$min.data ,'B.Code']))
length(unique(d[ d$Species == species.goat & !d$is.growing & d$min.data ,'B.Code']))

# Goat
length(unique(d[ d$Species == species.goat & d$is.growing & d$min.data ,'B.Code']))
length(unique(d[ d$Species == species.goat & !d$is.growing & d$min.data ,'B.Code']))

length(unique(d[ d$Species == species.goat & d$is.growing & d$min.data ,'B.Code']))
length(unique(d[ d$Species == species.goat & !d$is.growing & d$min.data ,'B.Code']))



#Adult females
length(unique(d[ d$Species == species.sheep & d$is_lactating & d$min.data ,'B.Code']))
length(unique(d[ d$Species == species.sheep & d$is_gestating & d$min.data ,'B.Code']))

length(unique(d[ d$Species == species.goat & d$is_lactating & d$min.data ,'B.Code']))
length(unique(d[ d$Species == species.goat & d$is_gestating & d$min.data,'B.Code']))
