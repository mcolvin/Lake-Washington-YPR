

agedat<- read.csv("./dat/Lake-Washington-2015-Age-Data.csv")
agedat<-subset(agedat,
    !(Weight..g.< 180 &
    Length..mm.>250))
agedat$Species<- as.character(agedat$Species)
agedat[agedat$Species=="bc ",]$Species<-"bc"   
agedat$Species<- as.factor(agedat$Species)
    agedat$lnL<-log(agedat$Length..mm.)
    agedat$lnW<-log(agedat$Weight..g.)
dat<- read.csv("./dat/Lake-Washington-2015.csv")
dat<- subset(dat,Spp..Code %in% c(1408, 1409, 1410))
    dat$lnL<-log(dat$Length)
    dat$lnW<-log(dat$Weight)


## EGG DATA
egg<- read.csv("dat/eggs lake washington.csv")


load(file="output/ypr.Rdata")

