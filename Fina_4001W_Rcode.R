#Capital Strucure

#S&P500 current market (2023)

spcap = data.frame(S_P500_Capital_Structure_2023)
spcap = spcap[-c(1:6),]
#change to numeric
for (i in 3:5){
  spcap[,i] = as.numeric(spcap[,i])
}

colnames(spcap) = c("Ticker","Name","D/E","LT-D/E","D/A","Sector")
rownames(spcap) = NULL

#Omit based on insolvency
issues = na.omit(spcap[,5])[na.omit(spcap[,5])>100]
issueINDEX = match(issues,spcap[,5])
spcap[issueINDEX,3:5] <- NA

match(max(na.omit(spcap[,3])),spcap[,3])


#arbitrary omit (reminder!!! values multiplied by 100 in raw data)
length(na.omit(spcap[,3])[na.omit(spcap[,3])>500])
#length(na.omit(spcap[,3]))
DEomit = (na.omit(spcap[,3])[na.omit(spcap[,3])>500])
LTEomit = (na.omit(spcap[,4])[na.omit(spcap[,4])>500])
DEoindex = match(DEomit,spcap[,3])
LTEoindex = match(LTEomit,spcap[,4])

spcap[DEoindex,3:5] <- NA
spcap[LTEoindex,3:5] <- NA

rownames(spcap) = NULL

full = data.frame(x=c(spcap[,3]+spcap[,4]+spcap[,5]),y=c(rep(1,length(spcap[,3])),rep(2,length(spcap[,4])),rep(3,length(spcap[,5]))))
boxplot(full$x~full$y,rm.na=T, ylab = "Values",xlab = "Ratios")

plot(density(na.omit(spcap[,3])))

#Outlier Removal
#phold = na.omit(spcap)
#outexist = T
#while (outexist == T){
#  IQR = (quantile(na.omit(phold[,i]),prob=0.75)-quantile(na.omit(phold[,i]),prob=0.25))
#  thresh = quantile(na.omit(phold[,i]),prob=0.75) + (1.5*IQR)
#  if (max(na.omit(phold[,i]) > thresh)){
#    na.omit(phold[,i])[max(na.omit(phold[,i]))] = phold[,i][phold[,i] < max(phold[,i])]
#  } else{
#    outexist = F
#  }
#}

#stack exchange solution... didn't work
#phold[!phold %in% boxplot.stats(phold)$out]

#Omit based on insolvency (notes/first try) (no longer necessary. Real code above)
length(na.omit(spcap[,5])[na.omit(spcap[,5])>100])
(na.omit(spcap[,5])[na.omit(spcap[,5])>100])

#removing "0" values for strictly positive tests
spcap[spcap<=0] <- NA

#checking normality
normname = c("Total Debt to Equity","Long-Term Debt to Equity","Total Debt to Total Assets")
for (i in 1:3){
  print(normname[i])
  print(shapiro.test(na.omit(spcap[,i+2])))
}

par(mfrow=c(1,3))
for (i in 1:3){
  qqnorm(na.omit(spcap[,i+2]))
  qqline(na.omit(spcap[,i+2]))
}
par(mfrow=c(1,1))

#density plots
par(mfrow=c(1,3))
for (i in 1:3){
  plot(density(na.omit(spcap[,i+2])))
}
par(mfrow=c(1,1))

#check homoscedasticity
library(car)
for (i in 3:5){
  print(normname[i-2])
  print(leveneTest(spcap[,i]~spcap$Sector))
  print(fligner.test(spcap[,i]~spcap$Sector))
}

aggregate(spcap[,5],by=list(spcap$Sector),FUN = var,na.rm=T)


#boxcox
library(MASS)
greek = data.frame(index = c(1:499))
par(mfrow=c(1,3))
for (i in 3:5){
  boxCox(lm(na.omit(spcap[,i])~1))
  lambda = boxcox(lm(spcap[,i]~1))$x[which.max(boxcox(lm(spcap[,i]~1))$y)]
  greek = cbind(greek,((spcap[,i]^lambda-1)/lambda))
}
par(mfrow=c(1,1))

#size
dim(na.omit(greek))
head(greek)

#for aggregate data where 



#checking normality after transformation
normname = c("Total Debt to Equity","Long-Term Debt to Equity","Total Debt to Total Assets")
for (i in 2:4){
  print(normname[i-1])
  print(shapiro.test(na.omit(greek[,i])))
}

par(mfrow=c(1,3))
for (i in 2:4){
  qqnorm(na.omit(greek[,i]))
  qqline(na.omit(greek[,i]))
}
par(mfrow=c(1,1))


#check homoscedasticity on transformation
library(car)
for (i in 2:4){
  print(normname[i-1])
  print(leveneTest(greek[,i]~spcap$Sector))
  print(fligner.test(greek[,i]~spcap$Sector))
}

aggregate(spcap[,5],by=list(spcap$Sector),FUN = var,na.rm=T)


#for length aggregate graph
sigh1 = na.omit(data.frame(index = c(1:499),TDE = greek[,2],Sectors = spcap$Sector))
sigh2 = na.omit(data.frame(index = c(1:499),TLTE = greek[,3],Sectors = spcap$Sector))
sigh3 = na.omit(data.frame(index = c(1:499),TDA = greek[,4],Sectors = spcap$Sector))

aggregate(sigh1$TDE,by=list(sigh1$Sectors),FUN = var,na.rm=T)
aggregate(sigh1$TDE,by=list(sigh1$Sectors),FUN = length)

aggregate(sigh2$TLTE,by=list(sigh2$Sectors),FUN = var,na.rm=T)
aggregate(sigh2$TLTE,by=list(sigh2$Sectors),FUN = length)

aggregate(sigh3$TDA,by=list(sigh3$Sectors),FUN = var,na.rm=T)
aggregate(sigh3$TDA,by=list(sigh3$Sectors),FUN = length)

#ANOVA on transformed data
DEaov = aov(TDE ~ Sectors,data = sigh1)
LTDEaov = aov(TLTE ~ Sectors,data = sigh2)
DAaov = aov(TDA ~ Sectors,data = sigh3)

summary(DEaov)
summary(LTDEaov)
summary(DAaov)

#Tukey on transformed data
TukeyHSD(DEaov)
TukeyHSD(LTDEaov)
TukeyHSD(DAaov)


aggregate(na.omit(spcap[,3]),by=list(sigh1$Sectors),FUN = mean,na.rm=T)

aggregate(na.omit(spcap[,4]),by=list(sigh2$Sectors),FUN = mean,na.rm=T)

aggregate(na.omit(spcap[,5]),by=list(sigh3$Sectors),FUN = mean,na.rm=T)


#Total Debt to Equity

library(ggplot2)

TDE = data.frame(Year=1999:2023,DE = as.double(t(rev(S_P500_Leverage_25yearhistory_to2023[12,2:26]))))

plot(1999:2023,TDE$DE,main = "Total Debt to Equity",xlab = "Year",ylab = "D/E")
for (i in 1:length(TDE$DE)){
  print(i)
  segments(x0=i+1998,y0=TDE$DE[i],x1=i+1999,y1=TDE$DE[i+1],col="black")
}
  

#LT Debt to Equity

LTDE = data.frame(Year=1999:2023,LTDtE=as.double(t(rev(S_P500_Leverage_25yearhistory_to2023[8,2:26]))))

plot(1999:2023,LTDE$LTDtE,main = "LT Debt to Equity",xlab = "Year",ylab = "LTD/E")
for (i in 1:length(LTDE$LTDtE)){
  print(i)
  segments(x0=i+1998,y0=LTDE$LTDtE[i],x1=i+1999,y1=LTDE$LTDtE[i+1],col="black")
}

par(mfrow=c(1,2))
par(mfrow=c(1,1))



TDE = data.frame(S_P500_Tot_Debt_to_Equity_1999_to_2023)
Master = data.frame(DY$FactSet.Universal.Screening, DY$...3, DY$Sector)
colnames(Master) = c('Tickers', 'Names','Sector')

hold = c(TDE$...2,TDE$...6,TDE$...10,TDE$...14,TDE$...18,TDE$...22,TDE$...26,TDE$...30,TDE$...34,TDE$...38,TDE$...42,TDE$...46,TDE$...50,TDE$...54,TDE$...58,TDE$...62,TDE$...66,TDE$...70,TDE$...74,TDE$...78,TDE$...82,TDE$...86,TDE$...90,TDE$...94,TDE$...98)

hmm = data.frame(a=TDE$...2,b=TDE$...6,c=TDE$...10,d=TDE$...14,e=TDE$...18,f=TDE$...22,g=TDE$...26,h=TDE$...30,i=TDE$...34,j=TDE$...38,k=TDE$...42,l=TDE$...46,m=TDE$...50,n=TDE$...54,o=TDE$...58,p=TDE$...62,q=TDE$...66,r=TDE$...70,s=TDE$...74,t=TDE$...78,u=TDE$...82,v=TDE$...86,w=TDE$...90,x=TDE$...94,y=TDE$...98)
hmm[is.na(hmm)]=50

nam = hmm[1]
for (i in 2:25){
  for (j in hmm[,i]){
    if (j %in% nam){
      
    }else{
      nam = c(nam,j)
      print(j)
    }
  }
}

check = 0
for (i in hmm[,2]){
  if (i == "Abbott Laboratories"){
    check = 1
  }
}
if (check == 0){
  print("new")
}else{
  print("old")
}


tester = c()
for (j in c("hate","die","why","pain","broken","tired")){
  check = 0
  #print(paste("====",j))
  for (i in c("love","working","why","pain","die","tired")){
    #print(i)
    if (i == j){
      check = 1
    }
  }
  if (check == 0){
    print(paste("+++++",j))
    tester = c(tester, j)
  }
}

nam1 = hmm$a
nam2 = hmm$a
for (j in hmm$x){
  check = 0
  #print(paste("====",j))
  for (i in hmm$a){
    #print(i)
    if (i == j){
      check = 1
    }
  }
  if (check == 0){
    print(j)
    nam2 = c(nam2, j)
  }
}



place = 0
for (i in unique(hold)){
  place = place + 1
  if (i %in% Master$Names){
  } else{
    Master = rbind(Master,data.frame('Tickers'=TDE$S.P.500..SP50.SPX...AGGREGATE.Mode..Constituent.count..503.with.Data..316..Data.in.USD..millions..as.of..31.Dec..99[place],'Names'=i,'Sector'=NA))  
    print(i)
  }
  if (place > 504){print(place)}
}

length(Master$Tickers)

#import Capital Structure data
CapStr = data.frame(NYSE_Capital_Structure_2023_to_1999)
dim(CapStr)

#change to numeric
for (i in 4:28){
  CapStr[,i] = as.numeric(CapStr[,i])
}

###############
#to find number of companies in each sector (Dumb at simple math)

#cuts should exclude first cut and add (last poistion +1) to last position for the calculation
cuts3 = c(70,147,327,459,514,584,743,797,817,866,964)

#save should be the first cut position
save = 6
for (i in cuts3){
  x = 0
  for (j in (save+1):(i-1)){
    x = x+1
  }
  print(x)
  save = i
}
################

CapStr = CapStr[-c(1:6,70,147,327,459,514,584,743,797,817,866,964:9373),]
CapStr$Sector = c(rep("Energy",63),rep("Materials",76),rep("Industrials",179),rep("Consumer Discretionary",131),rep("Consumer Staples",54),rep("Health Care",69),rep("Financials",158),rep("Information Technology",53),rep("Communication Services",19),rep("Utilities",48),rep("Real Estate",97))


#arbitrary omit (reminder!!! values multiplied by 100 in raw data)
length(na.omit(CapStr[,6])[na.omit(CapStr[,6])>500])

for (i in 4:28){
  capDEomit = (na.omit(CapStr[,i])[na.omit(CapStr[,i])>500])
  capDEoindex = match(capDEomit,CapStr[,i])
  CapStr[capDEoindex,i] <- NA
}



#generate heat map values
library(dunn.test)
compval = data.frame(Comparisons = dunn.test(c(1:11),c("Energy","Materials","Industrials","Consumer Discretionary","Consumer Staples","Health Care","Financials","Information Technology","communication Services","Utilities","Real Estate"))$comparisons, values =c(rep(0,55)))
sigkey = data.frame(Comparisons = compval$Comparisons)
for (i in 1:25){
  dummy = data.frame(values = CapStr[i+3],sector = CapStr$Sector)
  colnames(dummy) = c('values','sector')
  dummy = na.omit(dummy)
  dunnval = dunn.test(dummy$values,dummy$sector)$P
  dunnval = ifelse(dunnval < 0.025,1,0)
  compval$values = compval$values + dunnval
  sigkey = cbind(sigkey,dunnval)
}
compval$values

#generate TUKEY heat map values
#hmm not working
compval = data.frame(Comparisons = dunn.test(c(1:11),c("Energy","Materials","Industrials","Consumer Discretionary","Consumer Staples","Health Care","Financials","Information Technology","communication Services","Utilities","Real Estate"))$comparisons, values =c(rep(0,55)))
sigkey = data.frame(Comparisons = compval$Comparisons)
for (i in 1:25){
  dummy = data.frame(values = CapStr[i+3],sector = CapStr$Sector)
  colnames(dummy) = c('values','sector')
  dummy = na.omit(dummy)
  dunnval = dunn.test(dummy$values,dummy$sector)$P
  #replace line above with below
  #dunnval = TukeyHSD(aov(dummy$values~dummy$sector))
  #can't extract p values + not sure if NA are being omitted
  dunnval = ifelse(dunnval < 0.025,1,0)
  compval$values = compval$values + dunnval
  sigkey = cbind(sigkey,dunnval)
}
compval$values


compval$x_coor = c(1,1:2,1:3,1:4,1:5,1:6,1:7,1:8,1:9,1:10)
compval$y_coor = c(10,rep(9,2),rep(8,3),rep(7,4),rep(6,5),rep(5,6),rep(4,7),rep(3,8),rep(2,9),rep(1,10))

library(reshape2)
heatmat = acast(compval,y_coor~x_coor,value.var = "values")
heatmat
melt(heatmat)

#heatmap
library(ggplot2)
ggplot(melt(heatmat), aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "X Coordinate", y = "Y Coordinate", title = "Example Heat Map")

dim(sigkey)
colnames(sigkey) = c('Comparisons',as.character(rev(1999:2023)))

#transpose rows for a vector to generate sig years
t(sigkey[1,])

#means
Capmean = aggregate(CapStr$...4 , by = list(CapStr$Sector), FUN = mean, na.rm=T)
Capmean$x = c(rep(0,11))
for (i in 1:25){
  Capmean$x = Capmean$x + aggregate(CapStr[,i+3], by = list(CapStr$Sector), FUN = mean, na.rm=T)$x

}
Capmean$x = Capmean$x / 25
colnames(Capmean) = c("Sectors","Total Debt to Equity")
Capmean

#Adding sig levels
Capmean$Significance = c()


#Sector mean overlay
CapmeanSec = data.frame(Communication_Services = c(),Consumer_Discretionary = c(),Consumer_Staples = c(),Energy = c(),Financials = c(),Health_Care = c(),Industrials = c(),Information_Technology = c(),Materials = c(),Real_Estate = c(),Utilities = c())
for (i in 1:25){
  CapmeanSec = rbind(CapmeanSec,t(aggregate(CapStr[,i+3], by = list(CapStr$Sector), FUN = mean, na.rm=T)$x))
}

LTCapmeanSec = data.frame(Communication_Services = c(),Consumer_Discretionary = c(),Consumer_Staples = c(),Energy = c(),Financials = c(),Health_Care = c(),Industrials = c(),Information_Technology = c(),Materials = c(),Real_Estate = c(),Utilities = c())
for (i in 1:25){
  LTCapmeanSec = rbind(LTCapmeanSec,t(aggregate(CapStrLT[,i+3], by = list(CapStrLT$Sector), FUN = mean, na.rm=T)$x))
}

DACapmeanSec = data.frame(Communication_Services = c(),Consumer_Discretionary = c(),Consumer_Staples = c(),Energy = c(),Financials = c(),Health_Care = c(),Industrials = c(),Information_Technology = c(),Materials = c(),Real_Estate = c(),Utilities = c())
for (i in 1:25){
  DACapmeanSec = rbind(DACapmeanSec,t(aggregate(CapLev[,i+3], by = list(CapLev$Sector), FUN = mean, na.rm=T)$x))
}



ooo = c()
for (i in 1:11){
  ooo = append(ooo,CapmeanSec[,i])
  print("hi")
}

eee = c()
for (i in 1:11){
  eee = append(eee,LTCapmeanSec[,i])
  print("hello")
}


iii = c()
for (i in 1:11){
  iii = append(iii,DACapmeanSec[,i])
  print("bye")
}


colcode = c("red","orange","yellow","green","blue","purple","cyan","brown","black","blue2","orange2")
plot(rep(1:25,11),ooo)
for (j in 1:11){
  for (i in 1:(length(CapmeanSec[,j])-1)){
    segments((i+1),CapmeanSec[i+1,j],x1=i,y1=CapmeanSec[i,j],col = colcode[j])
    points(i,CapmeanSec[i,j],col=colcode[j])
  }
}

plot(rep(1:25,11),eee)
for (j in 1:11){
  for (i in 1:(length(LTCapmeanSec[,j])-1)){
    segments((i+1),LTCapmeanSec[i+1,j],x1=i,y1=LTCapmeanSec[i,j],col = colcode[j])
    points(i,LTCapmeanSec[i,j],col=colcode[j])
  }
}

plot(rep(1:25,11),iii)
for (j in 1:11){
  for (i in 1:(length(DACapmeanSec[,j])-1)){
    segments((i+1),DACapmeanSec[i+1,j],x1=i,y1=DACapmeanSec[i,j],col = colcode[j])
    points(i,DACapmeanSec[i,j],col=colcode[j])
  }
}


###

library(readxl)
# specifying the path name 
path <- "C:/Users/tmccr/Downloads/NYSE_Capital_Structure_2023_to_1999.xlsx"

CapStrLT = data.frame(read_excel(path,"Tab2"))
dim(CapStrLT)

#change to numeric
for (i in 4:28){
  CapStrLT[,i] = as.numeric(CapStrLT[,i])
}

CapStrLT = CapStrLT[-c(1:6,70,147,327,459,514,584,743,797,817,866,964:9373),]
CapStrLT$Sector = c(rep("Energy",63),rep("Materials",76),rep("Industrials",179),rep("Consumer Discretionary",131),rep("Consumer Staples",54),rep("Health Care",69),rep("Financials",158),rep("Information Technology",53),rep("Communication Services",19),rep("Utilities",48),rep("Real Estate",97))


#omit arbitrary
for (i in 4:28){
  capLTDEomit = (na.omit(CapStrLT[,i])[na.omit(CapStrLT[,i])>500])
  capLTDEoindex = match(capLTDEomit,CapStrLT[,i])
  CapStrLT[capLTDEoindex,i] <- NA
}


#generate heat map values
library(dunn.test)
compval2 = data.frame(Comparisons = dunn.test(c(1:11),c("Energy","Materials","Industrials","Consumer Discretionary","Consumer Staples","Health Care","Financials","Information Technology","communication Services","Utilities","Real Estate"))$comparisons, values =c(rep(0,55)))
sigkey2 = data.frame(Comparisons = compval2$Comparisons)
for (i in 1:25){
  dummy2 = data.frame(values = CapStrLT[i+3],sector = CapStrLT$Sector)
  colnames(dummy2) = c('values','sector')
  dummy2 = na.omit(dummy2)
  dunnval2 = dunn.test(dummy2$values,dummy2$sector)$P
  dunnval2 = ifelse(dunnval2 < 0.025,1,0)
  compval2$values = compval2$values + dunnval2
  sigkey2 = cbind(sigkey2,dunnval2)
}
compval2$values


compval2$x_coor = c(1,1:2,1:3,1:4,1:5,1:6,1:7,1:8,1:9,1:10)
compval2$y_coor = c(10,rep(9,2),rep(8,3),rep(7,4),rep(6,5),rep(5,6),rep(4,7),rep(3,8),rep(2,9),rep(1,10))

library(reshape2)
heatmat2 = acast(compval2,y_coor~x_coor,value.var = "values")
heatmat2
melt(heatmat)

#heatmap
library(ggplot2)
ggplot(melt(heatmat2), aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "X Coordinate", y = "Y Coordinate", title = "Example Heat Map")

dim(sigkey2)
colnames(sigkey2) = c('Comparisons',as.character(rev(1999:2023)))

#transpose rows for a vector to generate sig years
t(sigkey2[1,])

#means
CapmeanLT = aggregate(CapStrLT$...4 , by = list(CapStrLT$Sector), FUN = mean, na.rm=T)
CapmeanLT$x = c(rep(0,11))
for (i in 1:25){
  CapmeanLT$x = CapmeanLT$x + aggregate(CapStrLT[,i+3], by = list(CapStrLT$Sector), FUN = mean, na.rm=T)$x
  
}
CapmeanLT$x = CapmeanLT$x / 25
colnames(CapmeanLT) = c("Sectors","Means")
CapmeanLT

###

CapLev = data.frame(read_excel(path,"Tab3"))
dim(CapLev)

#change to numeric
for (i in 4:28){
  CapLev[,i] = as.numeric(CapLev[,i])
}

CapLev = CapLev[-c(1:6,70,147,327,459,514,584,743,797,817,866,964:9373),]
CapLev$Sector = c(rep("Energy",63),rep("Materials",76),rep("Industrials",179),rep("Consumer Discretionary",131),rep("Consumer Staples",54),rep("Health Care",69),rep("Financials",158),rep("Information Technology",53),rep("Communication Services",19),rep("Utilities",48),rep("Real Estate",97))

#Omit based on insolvency
for (i in 4:28){
  capLevissues = na.omit(CapLev[,i])[na.omit(CapLev[,i])>100]
  capLevissuesINDEX - match(capLevissues,CapStr[,i])
  CapLev[capLevissusINDEX,i] <- NA
  #match(max(na.omit(CapStr[,i])),CapStr[,i])
}


#generate heat map values
library(dunn.test)
compval3 = data.frame(Comparisons = dunn.test(c(1:11),c("Energy","Materials","Industrials","Consumer Discretionary","Consumer Staples","Health Care","Financials","Information Technology","communication Services","Utilities","Real Estate"))$comparisons, values =c(rep(0,55)))
sigkey3 = data.frame(Comparisons = compval3$Comparisons)
for (i in 1:25){
  dummy3 = data.frame(values = CapLev[i+3],sector = CapLev$Sector)
  colnames(dummy3) = c('values','sector')
  dummy3 = na.omit(dummy3)
  dunnval3 = dunn.test(dummy3$values,dummy3$sector)$P
  dunnval3 = ifelse(dunnval3 < 0.025,1,0)
  compval3$values = compval3$values + dunnval3
  sigkey3 = cbind(sigkey3,dunnval3)
}
compval3$values


compval3$x_coor = c(1,1:2,1:3,1:4,1:5,1:6,1:7,1:8,1:9,1:10)
compval3$y_coor = c(10,rep(9,2),rep(8,3),rep(7,4),rep(6,5),rep(5,6),rep(4,7),rep(3,8),rep(2,9),rep(1,10))

library(reshape2)
heatmat3 = acast(compval2,y_coor~x_coor,value.var = "values")
heatmat3
melt(heatmat3)

#heatmap
library(ggplot2)
ggplot(melt(heatmat3), aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "green") +
  labs(x = "X Coordinate", y = "Y Coordinate", title = "Example Heat Map")

dim(sigkey3)
colnames(sigkey3) = c('Comparisons',as.character(rev(1999:2023)))

#transpose rows for a vector to generate sig years
t(sigkey3[1,])

#means
CapmeanLev = aggregate(CapLev$...4 , by = list(CapLev$Sector), FUN = mean, na.rm=T)
CapmeanLev$x = c(rep(0,11))
for (i in 1:25){
  CapmeanLev$x = CapmeanLev$x + aggregate(CapLev[,i+3], by = list(CapLev$Sector), FUN = mean, na.rm=T)$x
  
}
CapmeanLev$x = CapmeanLev$x / 25
colnames(CapmeanLev) = c("Sectors","Total Debt to Assets")
CapmeanLev




##########

#Create df
DY = data.frame(S_P500_DivYield_1999_2023)
DY = DY[-c(1:6,30,59,138,192,231,296,369,434,457,488),]
DY$Sector = c(rep("Energy",23),rep("Materials",28),rep("Industrials",78),rep("Consumer Discretionary",53),rep("Consumer Staples",38),rep("Health Care",64),rep("Financials",72),rep("Information Technology",64),rep("Communication Services",22),rep("Utilities",30),rep("Real Estate",31))

#rename col
new_names = c()
for (i in 1998:2023){
  new_names = append(new_names,as.character(i))
}
new_names = rev(new_names)

colnames(DY)[4:29] = new_names

#change to numeric
for (i in 4:29){
  DY[,i] = as.numeric(DY[,i])
}

#Get rid of NA values
DY[is.na(DY)] = 0

#Reset row numbers
rownames(DY) = NULL

#Check
DY[4]

###############
#to find number of companies in each sector (Dumb at simple math)

#cuts should exclude first cut and add (last poistion +1) to last position for the calculation
cuts = c(30,59,138,192,231,296,369,434,457,488,520)

#save should be the first cut position
save = 6
for (i in cuts){
  x = 0
  for (j in (save+1):(i-1)){
    x = x+1
  }
  print(x)
  save = i
}
################

#Check Sample Size
aggregate(DY$`2023`, by = list(DY$Sector), FUN = length)

#Check mean
aggregate(DY$`2023`, by = list(DY$Sector), FUN = mean)

divanova = aov(DY$`2023`~DY$Sector)
summary(divanova)

xDY = DY[1:4]
xDY$Sector = DY$Sector
xDY = xDY[xDY[4]>0,]
rownames(xDY) = NULL


#Check Sample Size
aggregate(xDY$`2023`, by = list(xDY$Sector), FUN = length)

#Check mean
aggregate(xDY$`2023`, by = list(xDY$Sector), FUN = mean)


#Transform Data for ~N and homogeneity of var
library(AID)
AID::boxcoxfr(xDY$`2023`,xDY$Sector,option = "var",lambda = seq(-3,3,0.1),lambda2=NULL,tau=0.05,alpha=0.05,verbose=TRUE)


xdivanova = aov(xDY$`2023`~xDY$Sector)
summary(xdivanova)



#note: levene's test proves homogenity of variance within groups is voilated (not similar variances) meaning one of the assumptions of the anova test doesn't hold, so I can't use those results
#cont. meaning I need a weaker test such as the Kruskal-Wallis with less strignant requirements. 
#cont. alternatively, I could also examine a larger sample size that might level out the variance such as the Russell 3000
library(car)
leveneTest(DY$`2023`~DY$Sector)
fligner.test(DY$`2023`~DY$Sector)


#Test from normality
qqnorm(xDY$`2023`)
qqline(xDY$`2023`)
shapiro.test(xDY$`2023`)


#Group Distributions
xDYmeans = aggregate(xDY$`2023`, by = list(xDY$Sector), FUN = mean)


library(ggplot2)
#clean distribution
ggplot(xDY, aes(`2023`, colour = Sector, group = Sector)) + geom_density(fill=NA)

#with means
ggplot(xDY, aes(`2023`, colour = Sector, group = Sector)) + geom_density(fill=NA) + geom_vline(data = xDYmeans, aes(xintercept = x,colour = Group.1))

leveneTest(xDY$`2023`~xDY$Sector)


#log 
qqnorm(log(xDY$`2023`))
qqline(log(xDY$`2023`))

leveneTest(log(xDY$`2023`)~xDY$Sector)

library(MASS)
boxcox(lm(xDY$`2023`~1))
lambda = boxcox(lm(xDY$`2023`~1))$x[which.max(boxcox(lm(xDY$`2023`~1))$y)]
xDY$'2023bc'= (xDY$`2023`^lambda-1)/lambda

qqnorm(xDY$`2023bc`)
qqline(xDY$`2023bc`)
shapiro.test(xDY$`2023bc`)

leveneTest(xDY$`2023bc`~xDY$Sector)
fligner.test(xDY$`2023bc`~xDY$Sector)

#Kruskal-Wallis
kruskal.test(xDY$`2023`~xDY$Sector)

#post hoc
library(dunn.test)
dunn.test(xDY$`2023`,xDY$Sector,wrap=T)

#Percentage of companies with no dividend
divperc = c()
for (i in 4:29){
  yn = 0
  for (j in DY[,i]){
    if (j != 0){
      yn = yn + 1
    }
  }  
  divperc = append(divperc, (yn/length(DY[,i])))    
}
print(divperc)


DY$cat = DY$`2023`
DY["cat"][DY["cat"] > 0] = 1

dunn.test(DY$cat,DY$Sector,wrap = T)

SPdivperc = aggregate(DY$cat,by=list(DY$Sector),FUN = mean)
colnames(SPdivperc) = c("Sectors","Means")
SPdivperc$Significnace = c("C","C","AB","AB","AB","C","B","C","A","AB","A")
sort(SPdivperc$Means)
SPdivperc

##########

rus3k = data.frame(iShares_Russell_3000_Div_Yield_DEC_2023)
rus3k = rus3k[-c(1:6,78,165,412,614,699,867,1120,1285,1339,1397,1510:2706),]

nrow(rus3k)

#rename col
colnames(rus3k)[1:4] = c("Ticker","GICS","Names","Dividend Yields")

#create sector categorical variable
rus3k$Sector = c(rep("Energy",71),rep("Materials",86),rep("Industrials",246),rep("Consumer Discretionary",201),rep("Consumer Staples",84),rep("Health Care",167),rep("Financials",252),rep("Information Technology",164),rep("Communication Services",53),rep("Utilities",57),rep("Real Estate",112))


#change to numeric
rus3k[,4] = as.numeric(rus3k[,4])

#Get rid of NA values
rus3k[is.na(rus3k)] = 0

#Reset row numbers
rownames(rus3k) = NULL


rus3k[4]

##############
#to find number of companies in each sector (Dumb at simple math)

#cuts should exclude first cut and add (last poistion +1) to last position for the calculation
cuts2 = c(78,165,412,614,699,867,1120,1285,1339,1397,1510)

#save should be the first cut position
save2 = 6
for (i in cuts2){
  y = 0
  for (j in (save2+1):(i-1)){
    y = y+1
  }
  print(y)
  save2 = i
}
##############

#remove non-dividend issuing companies
xrus3k = rus3k[rus3k$`Dividend Yields`>0,]

#Check
rownames(xrus3k) = NULL

xrus3k[4]

#Check Sample Size
aggregate(xrus3k$`Dividend Yields`, by = list(xrus3k$Sector), FUN = length)

#Check mean
aggregate(xrus3k$`Dividend Yields`, by = list(xrus3k$Sector), FUN = mean)


qqnorm(xrus3k$`Dividend Yields`)
qqline(xrus3k$`Dividend Yields`)

library(car)
shapiro.test(xrus3k$`Dividend Yields`)
hist(xrus3k$`Dividend Yields`)

library(ggplot2)
ggplot(xrus3k, aes(`Dividend Yields`, colour = Sector, group = Sector)) + geom_density(fill=NA)

#with means
xrus3kmeans = aggregate(xrus3k$`Dividend Yields`, by = list(xrus3k$Sector), mean)
ggplot(xrus3k, aes(`Dividend Yields`, colour = Sector, group = Sector)) + geom_density(fill=NA) + geom_vline(data = xrus3kmeans, aes(xintercept = x,colour = Group.1))


leveneTest(xrus3k$`Dividend Yields`~xrus3k$Sector)
fligner.test(xrus3k$`Dividend Yields`~xrus3k$Sector)

rusdivanova = aov(rus3k$`Dividend Yields` ~ rus3k$Sector)
summary(rusdivanova)



library(MASS)
boxcox(lm(xrus3k$`Dividend Yields`~1))
lambda2 = boxcox(lm(xrus3k$`Dividend Yields`~1))$x[which.max(boxcox(lm(xrus3k$`Dividend Yields`~1))$y)]
xrus3k$`Dividend Yields boxcox`= (xrus3k$`Dividend Yields`^lambda-1)/lambda

#check normality
qqnorm(xrus3k$`Dividend Yields boxcox`)
qqline(xrus3k$`Dividend Yields boxcox`)
shapiro.test(xrus3k$`Dividend Yields boxcox`)

#check homoscedasticity
leveneTest(xDY$`2023bc`~xDY$Sector)
fligner.test(xDY$`2023bc`~xDY$Sector)

#Kruskal-Wallis
kruskal.test(xrus3k$`Dividend Yields`~xrus3k$Sector)

#post hoc
library(dunn.test)
dunn.test(xrus3k$`Dividend Yields`,xrus3k$Sector,wrap=T)



rus3k$cat = rus3k$`Dividend Yields`
rus3k["cat"][rus3k["cat"] > 0] = 1

dunn.test(rus3k$cat,rus3k$Sector,wrap = T)

rusdivperc = aggregate(rus3k$cat, by = list(rus3k$Sector), FUN = mean)
colnames(rusdivperc) = c("sectors","Means")
rusdivperc$Significance = c("CD","C","B","B","A","D","B","D","AB","A","A")
rusdivperc

########









