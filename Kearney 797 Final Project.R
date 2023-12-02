library(foreign)
#Original CCCES2014 dataset
x<-read.delim("CCES14_Common_Content_Validated.tab")
View(x)

#Coastal Counties list:
install.packages("readxl")
library("readxl")
y<-read_excel("CoastalCounties.xlsx")
View(y)
y$coastal<-1

#merging
x<-merge(x,y, by.x="countyfips", by.y="fips", all.x=TRUE)
x$coastal[is.na(x$coastal)]<-0
View(x)


##State Majorities coding; Use for 2 of the polknowledge questions
x$shousemaj[x$StateAbbr=="AL"]<-"Republican"
x$shousemaj[x$StateAbbr=="AK"]<-"Republican"
x$shousemaj[x$StateAbbr=="AZ"]<-"Republican"
x$shousemaj[x$StateAbbr=="AR"]<-"Republican"
x$shousemaj[x$StateAbbr=="CA"]<-"Democrat"
x$shousemaj[x$StateAbbr=="CO"]<-"Democrat"
x$shousemaj[x$StateAbbr=="CT"]<-"Democrat"
x$shousemaj[x$StateAbbr=="DE"]<-"Democrat"
x$shousemaj[x$StateAbbr=="FL"]<-"Republican"
x$shousemaj[x$StateAbbr=="GA"]<-"Republican"
x$shousemaj[x$StateAbbr=="HI"]<-"Democrat"
x$shousemaj[x$StateAbbr=="ID"]<-"Republican"
x$shousemaj[x$StateAbbr=="IL"]<-"Democrat"
x$shousemaj[x$StateAbbr=="IN"]<-"Republican"
x$shousemaj[x$StateAbbr=="IA"]<-"Republican"
x$shousemaj[x$StateAbbr=="KS"]<-"Republican"
x$shousemaj[x$StateAbbr=="KY"]<-"Democrat"
x$shousemaj[x$StateAbbr=="LA"]<-"Republican"
x$shousemaj[x$StateAbbr=="ME"]<-"Democrat"
x$shousemaj[x$StateAbbr=="MD"]<-"Democrat"
x$shousemaj[x$StateAbbr=="MA"]<-"Democrat"
x$shousemaj[x$StateAbbr=="MI"]<-"Republican"
x$shousemaj[x$StateAbbr=="MN"]<-"Democrat"
x$shousemaj[x$StateAbbr=="MS"]<-"Republican"
x$shousemaj[x$StateAbbr=="MO"]<-"Republican"
x$shousemaj[x$StateAbbr=="MT"]<-"Republican"
x$shousemaj[x$StateAbbr=="NE"]<-"NA"
x$shousemaj[x$StateAbbr=="NV"]<-"Republican"
x$shousemaj[x$StateAbbr=="NH"]<-"Democrat"
x$shousemaj[x$StateAbbr=="NJ"]<-"Democrat"
x$shousemaj[x$StateAbbr=="NM"]<-"Democrat"
x$shousemaj[x$StateAbbr=="NY"]<-"Democrat"
x$shousemaj[x$StateAbbr=="NC"]<-"Republican"
x$shousemaj[x$StateAbbr=="ND"]<-"Republican"
x$shousemaj[x$StateAbbr=="OH"]<-"Republican"
x$shousemaj[x$StateAbbr=="OK"]<-"Republican"
x$shousemaj[x$StateAbbr=="OR"]<-"Democrat"
x$shousemaj[x$StateAbbr=="PA"]<-"Republican"
x$shousemaj[x$StateAbbr=="RI"]<-"Democrat"
x$shousemaj[x$StateAbbr=="SC"]<-"Republican"
x$shousemaj[x$StateAbbr=="SD"]<-"Republican"
x$shousemaj[x$StateAbbr=="TN"]<-"Republican"
x$shousemaj[x$StateAbbr=="TX"]<-"Republican"
x$shousemaj[x$StateAbbr=="UT"]<-"Republican"
x$shousemaj[x$StateAbbr=="VT"]<-"Democrat"
x$shousemaj[x$StateAbbr=="VA"]<-"Democrat"
x$shousemaj[x$StateAbbr=="WA"]<-"Republican"
x$shousemaj[x$StateAbbr=="WV"]<-"Republican"
x$shousemaj[x$StateAbbr=="WI"]<-"Democrat"
x$shousemaj[x$StateAbbr=="WY"]<-"Republican"

x$ssenatemaj[x$StateAbbr=="AL"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="AK"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="AZ"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="AR"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="CA"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="CO"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="CT"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="DE"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="FL"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="GA"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="HI"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="ID"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="IL"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="IN"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="IA"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="KS"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="KY"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="LA"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="ME"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="MD"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="MA"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="MI"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="MN"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="MS"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="MO"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="MT"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="NE"]<-"NA"
x$ssenatemaj[x$StateAbbr=="NV"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="NH"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="NJ"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="NM"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="NY"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="NC"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="ND"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="OH"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="OK"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="OR"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="PA"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="RI"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="SC"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="SD"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="TN"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="TX"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="UT"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="VT"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="VA"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="WA"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="WV"]<-"Republican"
x$ssenatemaj[x$StateAbbr=="WI"]<-"Democrat"
x$ssenatemaj[x$StateAbbr=="WY"]<-"Republican"

View(x$ssenatemaj)
View(x$shousemaj)


#Coding the policy questions:
colnames(x)[colnames(x)=="CC14_326_1"] <- "CEmissions"
colnames(x)[colnames(x)=="CC14_326_2"] <- "FuelEff"
colnames(x)[colnames(x)=="CC14_326_3"] <- "Renewable"
colnames(x)[colnames(x)=="CC14_326_4"] <- "CleanAir"
x$CEmissions[x$CEmissions==2] <- 0
x$FuelEff[x$FuelEff==2] <- 0
x$Renewable[x$Renewable==2] <- 0
x$CleanAir[x$CleanAir==2] <- 0

#Income
x$faminc[x$faminc==31]<-NA
x$faminc[x$faminc==18]<-NA
x$faminc[x$faminc==32]<-NA
x$faminc[x$faminc==97]<-NA
x$faminc[x$faminc==98]<-NA



#Political Knowledge:
x$College[x$educ==3]<-1
x$College[x$educ==4]<-1
x$College[x$educ==5]<-1
x$College[x$educ==6]<-1
x$College[x$educ==1]<-0
x$College[x$educ==2]<-0
x$College[x$educ==8]<-NA
x$College[x$educ==9]<-NA
x$HouseMaj[x$CC14_309a==1]<-1
x$HouseMaj[x$CC14_309a==2]<-0
x$HouseMaj[x$CC14_309a==3]<-0
x$HouseMaj[x$CC14_309a==4]<-0
x$SenateMaj[x$CC14_309b==1]<-0
x$SenateMaj[x$CC14_309b==2]<-1
x$SenateMaj[x$CC14_309b==3]<-0
x$SenateMaj[x$CC14_309b==4]<-0

x$govcorrect<-0
x$govcorrect[x$CC14_310a==2 & x$CurrentGovParty=="Republican"]<-1
x$govcorrect[x$CC14_310a==3 & x$CurrentGovParty=="Democratic"]<-1

x$sen1correct<-0
x$sen1correct[x$CC14_310b==2 & x$CurrentSen1Party=="Republican"]<-1
x$sen1correct[x$CC14_310b==3 & x$CurrentSen1Party=="Democratic"]<-1

x$sen2correct<-0
x$sen2correct[x$CC14_310c==2 & x$CurrentSen2Party=="Republican"]<-1
x$sen2correct[x$CC14_310c==3 & x$CurrentSen2Party=="Democratic"]<-1


x$mccorrect<-0
x$mccorrect[x$CC14_310d==2 & x$CurrentHouseParty=="Republican"]<-1
x$mccorrect[x$CC14_310d==3 & x$CurrentHouseParty=="Democratic"]<-1

x$ssencorrect<-0
x$ssencorrect[x$CC14_309c==1 & x$ssenatemaj=="Republican"]<-1
x$ssencorrect[x$CC14_309c==2 & x$ssenatemaj=="Democrat"]<-1
x$ssencorrect[x$CC14_309c==3 & x$StateAbbr=="NE"]<-1

x$shousemajcorrect<-0
x$shousemajcorrect[x$CC14_309d==1 & x$shousemaj=="Republican"]<-1
x$shousemajcorrect[x$CC14_309d==2 & x$shousemaj=="Democrat"]<-1
x$shousemajcorrect[x$CC14_309d==3 & x$StateAbbr=="NE"]<-1


#News Interest
x$newsint[x$newsint==7]<-NA
x$newsint[x$newsint==8]<-NA
x$newsint[x$newsint==9]<-NA

View(x$race)
#Race
x$white[x$race==1]<-1
x$white[x$race!=1]<-0

#Male
x$male[x$gender==1]<-1
x$male[x$gender==2]<-0

#Republican
x$repub<-0
x$repub[x$pid3==2]<-1
x$rep[x$pid3==3]<-0
x$rep[x$pid3==4]<-0
x$rep[x$pid3==5]<-0
x$rep[x$pid3==8]<-0

x$age<-2014-x$birthyr
View(x$age)

sophis<-c("govcorrect","sen1correct","sen2correct","mccorrect","HouseMaj","SenateMaj", "ssencorrect", "shousemajcorrect")
sophis<-x[sophis]


#Saving the dataset with edits
write.csv(x, "FinalDATASET797.csv")

setwd("~/Desktop/R")
x<-read.csv("FinalDATASET797.csv")


View(x$sophis)
###FACTOR ANALYSIS
library(psych)
alpha(sophis) #raw alpha 0.84
fit<-factor.pa(sophis)
fit #1 factor sufficient

###IRT MODEL
library(ltm)
ltm(sophis~z1)
irt<-ltm(sophis~z1)
plot(irt) ###Not great...only 2 positive abilities, and they aren't far from 0. 
sophisfs<- factor.scores(irt, resp.patterns=sophis)
View(sophisfs)

#Creating the factor scores
y<-sophisfs$score.dat
z<-y$z1
View(sophisfs$score.dat)
x<-cbind(x,z)

summary(x$z)
x$SophisScore<-x$z
summary(x$SophisScore)


##Models for each policy question:


##Probit models
model1<-glm(FuelEff~SophisScore*repub+College+newsint+male+white, data=x, family=binomial(link="probit"))
model2<-glm(CEmissions~SophisScore*repub+College+newsint+male+white, data=x, family=binomial(link="probit"))
model3<-glm(Renewable~SophisScore*repub+College+newsint+male+white, data=x, family=binomial(link="probit"))
model4<-glm(CleanAir~SophisScore*repub+College+newsint+male+white, data=x, family=binomial(link="probit"))


#data subsets:
dat<- with(x, data.frame(repub = 0, newsint=mean(newsint, na.rm=TRUE), College=mean(College, na.rm=TRUE), male=mean(male, na.rm=TRUE), 
                         white=mean(white, na.rm = TRUE), SophisScore=c((-1.3:1.1), 0.1)))
dat1<- with(x, data.frame(repub = 1, newsint=mean(newsint, na.rm=TRUE), College=mean(College, na.rm=TRUE), male=mean(male, na.rm=TRUE), 
                          white=mean(white, na.rm = TRUE), SophisScore=c((-1.3:1.1), 0.1)))

##probabilities:
dat$fuelP <- predict(model1, newdata = dat, type = "response")
dat1$fuelP <- predict(model1, newdata = dat1, type = "response")
dat$CEmP <- predict(model2, newdata = dat, type = "response")
dat1$CEmP <- predict(model2, newdata = dat1, type = "response")
dat$RenewP <- predict(model3, newdata = dat, type = "response")
dat1$RenewP <- predict(model3, newdata = dat1, type = "response")
dat$CAAP <- predict(model4, newdata = dat, type = "response")
dat1$CAAP <- predict(model4, newdata = dat1, type = "response")



##graphs (didn't end up including in paper)

library(ggplot2)
p1 <- ggplot(dat, aes(x = SophisScore, y = fuelP))
p1 + geom_point() + geom_line() + xlab("Political Knowledge") + ylab("Prob.  of supporting Fuel Policy") + 
  geom_line(data = dat1, aes(x=SophisScore, y=fuelP), show.legend=FALSE, colour="blue") + geom_point(data=dat1) + ggtitle("Probability of Approval by partisanship: Increase in Fuel Efficiency")


p2 <- ggplot(dat, aes(x = SophisScore, y = CEmP))
p2 + geom_point() + geom_line() + xlab("Political Knowledge") + ylab("Prob.  of supporting EPA Policy") + 
  geom_line(data = dat1, aes(x=SophisScore, y=CEmP), show.legend=FALSE, colour="blue") + geom_point(data=dat1)+ggtitle("Probability of Approval by partisanship: Expansion of the EPA")


p3 <- ggplot(dat, aes(x = SophisScore, y = RenewP))
p3 + geom_point() + geom_line() + xlab("Political Knowledge") + ylab("Prob.  of supporting Renewables Policy") + 
  geom_line(data = dat1, aes(x=SophisScore, y=RenewP), show.legend=FALSE, colour="blue") + geom_point(data=dat1)+ggtitle("Probability of Approval by partisanship: Renewable Energies")


p4 <- ggplot(dat, aes(x = SophisScore, y = CAAP))
p4 + geom_point() + geom_line() + xlab("Political Knowledge") + ylab("Prob.  of supporting Clean Air Act") + 
  geom_line(data = da1t, aes(x=SophisScore, y=CAAP), show.legend=FALSE, colour="blue") + geom_point(data=dat1)+ ggtitle("Probability of Approval by partisanship: Clean Air Act")



####################################################
#####Now working in the coastal counties information:

coastal<-subset(x,coastal==1)
noncoastal<-subset(x,coastal==0)

###non coastal Dems
da<- with(noncoastal, data.frame(newsint=mean(newsint, na.rm=TRUE), repub=0, College=mean(College, na.rm=TRUE), male=mean(male, na.rm=TRUE), 
                        white=mean(white, na.rm = TRUE), SophisScore=c((-1.3:1.1), 0.1)))
###non coastal Republicans
da1<- with(noncoastal, data.frame(newsint=mean(newsint, na.rm=TRUE), repub=1, College=mean(College, na.rm=TRUE), male=mean(male, na.rm=TRUE), 
                         white=mean(white, na.rm = TRUE), SophisScore=c((-1.3:1.1), 0.1)))
#coastal dems
da2<-with(coastal, data.frame(newsint=mean(newsint, na.rm=TRUE), repub=0, College=mean(College, na.rm=TRUE), male=mean(male, na.rm=TRUE), 
                        white=mean(white, na.rm = TRUE), SophisScore=c((-1.3:1.1), 0.1)))
#coastal reps
da3<-with(coastal, data.frame(newsint=mean(newsint, na.rm=TRUE), repub=1, College=mean(College, na.rm=TRUE), male=mean(male, na.rm=TRUE), 
                        white=mean(white, na.rm = TRUE), SophisScore=c((-1.3:1.1), 0.1)))

##Probit regressions to make probabilities:
mod<-glm(CleanAir~SophisScore*repub+College+newsint+male+white, data=noncoastal, family=binomial(link="probit"))
modA<-glm(CleanAir~SophisScore*repub+College+newsint+male+white, data=coastal, family=binomial(link="probit"))
mod1<-glm(CEmissions~SophisScore*repub+College+newsint+male+white, data=noncoastal, family=binomial(link="probit"))
mod1A<-glm(CEmissions~SophisScore*repub+College+newsint+male+white, data=coastal, family=binomial(link="probit"))
mod2<-glm(Renewable~SophisScore*repub+College+newsint+male+white, data=noncoastal, family=binomial(link="probit"))
mod2A<-glm(Renewable~SophisScore*repub+College+newsint+male+white, data=coastal, family=binomial(link="probit"))
mod3<-glm(FuelEff~SophisScore*repub+College+newsint+male+white, data=noncoastal, family=binomial(link="probit"))
mod3A<-glm(FuelEff~SophisScore*repub+College+newsint+male+white, data=coastal, family=binomial(link="probit"))

###Attaching the probabilities to each dataset
da$CAAP <- predict(mod, newdata = da, type = "response")
da$CEmiss <- predict(mod1, newdata = da, type = "response")
da$RenewP <- predict(mod2, newdata = da, type = "response")
da$FeP <- predict(mod3, newdata = da, type = "response")

da1$CAAP <- predict(mod, newdata = da1, type = "response")
da1$CEmiss <- predict(mod1, newdata = da1, type = "response")
da1$RenewP <- predict(mod2, newdata = da1, type = "response")
da1$FeP <- predict(mod3, newdata = da1, type = "response")

da2$CAAP <- predict(modA, newdata = da2, type = "response")
da2$CEmiss <- predict(mod1A, newdata = da2, type = "response")
da2$RenewP <- predict(mod2A, newdata = da2, type = "response")
da2$FeP <- predict(mod3A, newdata = da2, type = "response")

da3$CAAP <- predict(modA, newdata = da3, type = "response")
da3$CEmiss <- predict(mod1A, newdata = da3, type = "response")
da3$RenewP <- predict(mod2A, newdata = da3, type = "response")
da3$FeP <- predict(mod3A, newdata = da3, type = "response")


###graphs (didn't end up putting into paper):
p1 <- ggplot(da, aes(x = SophisScore, y = CAAP))
p1 + geom_point() + geom_line() + xlab("Political Knowledge") + ylab("Prob.  of supporting Clean Air Act") +
  ggtitle("Non Coastal Counties") + geom_line(data = da1, aes(x=SophisScore, y=CAAP), 
                                              show.legend=FALSE, colour="blue") + geom_point(data=da1)

p2 <- ggplot(da2, aes(x = SophisScore, y = CAAP))
p2 + geom_point() + geom_line() + xlab("Political Knowledge") + ylab("Prob.  of supporting Clean Air Act") +
  ggtitle("Coastal Counties") + geom_line(data = da3, aes(x=SophisScore, y=CAAP), 
                                          show.legend=FALSE, colour="blue") + geom_point(data=da3)

p3 <- ggplot(da, aes(x = SophisScore, y = CEmiss))
p3 + geom_point() + geom_line() + xlab("Political Knowledge") + ylab("Prob.  of supporting Carbon Emiss") + 
  ggtitle("Non-Coastline Counties")+ geom_line(data = da1, aes(x=SophisScore, y=CEmiss), 
                                               show.legend=FALSE, colour="blue") + geom_point(data=da1) 

p4 <- ggplot(da2, aes(x = SophisScore, y = CEmiss))
p4 + geom_point() + geom_line() + xlab("Political Knowledge") + ylab("Prob.  of supporting Carbon Emiss") + 
  ggtitle("Coastline Counties")+ geom_line(data = da3, aes(x=SophisScore, y=CEmiss), 
                                           show.legend=FALSE, colour="blue") + geom_point(data=da3)


p5 <- ggplot(da, aes(x = SophisScore, y = RenewP))
p5 + geom_point() + geom_line() + xlab("Political Knowledge") + ylab("Prob.  of supporting Renewable Energy Funding") + 
  ggtitle("Non-Coastline Counties")+ geom_line(data = da1, aes(x=SophisScore, y=RenewP), 
                                               show.legend=FALSE, colour="blue") + geom_point(data=da1)

p6 <- ggplot(da2, aes(x = SophisScore, y = RenewP))
p6 + geom_point() + geom_line() + xlab("Political Knowledge") + ylab("Prob.  of supporting Renewable Energy Funding") + 
  ggtitle("Coastline Counties")+ geom_line(data = da3, aes(x=SophisScore, y=RenewP), 
                                           show.legend=FALSE, colour="blue") + geom_point(data=da3)


library(stargazer)

#all data
stargazer(model1, model2, model3, model4, dep.var.labels=c("Clean Air Act", "Carbon Emission Regulation", "Increase Fuel Efficiency", 
                                                                          "Funding for Renewable Energies"), omit.stat=c("f"), type="html", out="797final.table.html")


##non coastal
stargazer(mod, mod1, mod2, mod3, dep.var.labels=c("Clean Air Act", "Carbon Emission Regulation", "Increase Fuel Efficiency", 
                                                                          "Funding for Renewable Energies"), omit.stat=c("f"), type="html", out="797final.table.NONCOASTAL.html")
#coastal
stargazer(modA, mod1A, mod2A, mod3A, dep.var.labels=c("Clean Air Act", "Carbon Emission Regulation", "Increase Fuel Efficiency", 
                                                  "Funding for Renewable Energies"), omit.stat=c("f"), type="html", out="797final.table.COASTAL.html")





