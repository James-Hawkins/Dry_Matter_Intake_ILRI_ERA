


table(reg.dat$Sample.size)






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
