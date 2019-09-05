

library(devtools)
#install.packages("sf")
library("sf")
install_github("nickmckay/varveR")
library(varveR)
install.packages("units")
library(units)
install.packages("ggplot2")
library(ggplot2)
install.packages("Rcpp")
install.packages("yaml")
install.packages("colorspace")
library(colorspace)
library(varveR)
install.packages("ggthemes") 
library(ggthemes)
install.packages("classInt")
library(classInt)

slideannie <- readVarveShapefile("annie_0-8shp.shp", varveTop  = "left")

#reading in the data 
slide1new <- readVarveShapefile("8cm.shp", varveTop  = "left")
best <- readVarveShapefile("Export8t.shp", varveTop  = "left")

total <- rbind(slide1, slide2, slide3, slide4, slide5, slide6, slide7, slide8, slide9, slide10, slide11, slide12, slide13, slide14, slide15, slide16, slide17, slide18, slide19, slide20)

totalthick <- sum(total$thick)

totalthickness <- sum(total$thick)
total$allCount = 2018-seq_len(nrow(total))

varveplot= ggplot(varves) + geom_line(aes(x= varves$count, y = varves$thick))



allvarves <- read.csv("allvarves.csv")

varves <- read.csv("Columbine.csv")

varveplot= ggplot(varves) + 
  geom_line(aes(x= varves$year, y = varves$adjussted.thickness)) + 
  ylab("Varve Thickness (mm)") + 
  xlab("Varve Year AD") + 
  ggtitle("Varve Thickness Plot") + 
  theme_bw() +
  geom_line(aes(x= varves$year, y = varves$Moving), colour = "red")

varveageplot= ggplot(varves) + 
  geom_line(aes(x= varves$year, y = varves$count)) + 
  ylab("Depth") + 
  xlab("Varve Year AD") + 
  ggtitle("Varve Thickness Plot") + 
  theme_bw() 

movingavg <- ggplot(varves) +
  geom_line(aes(x = varves$year, y = varves$Moving)) +
  ylab("Varve Thickness (mm)") + 
  xlab("Varve Year AD") + 
  ggtitle("Moving average Plot") + 
  theme_bw()

newvarves <- read.csv("FINALvarvecounts.csv")

varveplot2 = ggplot(newvarves) + geom_line(aes(x= newvarves$year, y = newvarves$thick)) + 
  ylab("Varve Thickness (mm)") + xlab("Varve Year AD") + ggtitle("Varve Thickness Plot") + theme_bw()

#total varve plot with all varve thickness
varveplot= ggplot(newvarves) + geom_line(aes(x= newvarves$year, y = newvarves$thick)) + 
  ylab("Varve Thickness (mm)") + xlab("Varve Year AD") + ggtitle("Varve Thickness Plot") + theme_bw() + geom_line(aes(x= averagevarves$Years.AD, y = averagevarves$Movingaverage), colour = "red")


newvarveplot= ggplot(averagevarves) + geom_line(aes(x= averagevarves$Years.AD, y = averagevarves$Thickness)) + 
  ylab("Varve Thickness (mm)") + xlab("Varve Year AD") + ggtitle("Varve Thickness Plot") + theme_bw() 


weatherdata <- read.csv("weatherdata.csv")

ggplot(weatherdata, aes(weatherdata$Date)) +                    # basic graphical object
  geom_line(aes(y=weatherdata$thickness), colour="red") +  # first layer
  geom_line(aes(y=weatherdata$ppt..inches.), colour="green")

averagevarves <- read.csv("allvarves.csv")


#looking at a 20 year moving window of the varves
movingplot <- ggplot(averagevarves) +geom_line(aes(x = averagevarves$Years.AD, y = averagevarves$Movingaverage)) + 
  ylab("Varve Thickness") + xlab("Varve Year") + ggtitle("Varve Thickness 20 year Moving Average") + theme_bw()

treeringplot2 <- ggplot(trees, aes(x = trees$Year)) +                  
  geom_line(aes(y = trees$Columbine, colour="black"), show.legend = TRUE) +
  geom_line(aes(y = trees$Summitville, colour="red"), show.legend = TRUE) +
  ylab("Depth (cm)")+  
  xlab("Age (years AD)") + 
  ggtitle("Columbine Lake and Summitville Tree Rings") + 
  theme_bw()  +
  scale_color_manual(name = "Data", values = c(black = "black", red = "red"), labels = c('Columbine','Summitville'))







#making a plot of varve thickness by slide
varvecodes <- read.csv("varvecodes.csv")
sections <- read.csv("Columbine.csv")

varvecodeplot <- ggplot(sections) + geom_line(aes(y = sections$adjussted.thickness, x = sections$year),color= sections$section) + 
  ylab("Varve Thickness (mm)")  +
  xlab("Varve Year") + 
  ggtitle("Varve Thickness by Slide") + 
  theme_bw() 

varvecodeplot2 <- ggplot(sequence) + geom_line(aes(y = sequence$thick, x = sequence$count),color= sequence$section) + 
  ylab("Varve Thickness (mm)")  +
  xlab("Varve Year") + 
  ggtitle("Varve Thickness by Slide") + 
  theme_bw()

Thinsection <- varvecodes$Code

varveplot <- ggplot(varvecodes, aes(x = varvecodes$Years.AD, y = varvecodes$Thickness.mm, color = "Thinsection")) +geom_line()

snowdata2 <- read.csv("1984_data.csv")



#comparing tree ring data to varve thickness


#gaussianize data
install.packages("geochronr")
library(geoChronR)

thickmatrix <- as.matrix(varvethickness)
varvethickness <- read.csv("thickness.csv")


gaussianize <- function(X,jitter=FALSE){ 
  #   Transform each column of data matrix X to normality using the inverse
  #   Rosenblatt transform.
  #
  # inspired by split.m in normal.m by Van Albada, S.J., Robinson P.A. (2006)
  # Transformation of arbitrary distributions to the normal distribution with application to EEG
  # test-retest reliability. J Neurosci Meth, doi:10.1016/j.jneumeth.2006.11.004
  #
  #  Written 26/06/2015 by Julien Emile-Geay (USC)
  #translated to R and added jitter option by 29/06/2015 by Nick McKay (NAU) 
  
  if(!is.matrix(X)){
    X=as.matrix(X)
  }
  
  p=NCOL(X)
  n=NROW(X) 
  
  if(jitter){
    #add tiny random numbers to avoid ties
    X=array(rnorm(p*n,mean=0,sd=sd(as.vector(X))/1e6),c(n,p))+X
  }
  
  Xn    = matrix(data = 0,nrow = n,ncol = p)
  for (j in 1:p){
    # Sort the data in ascending order and retain permutation indices
    R = rank(X[,j])
    # The cumulative distribution function
    CDF = R/n - 1/(2*n)
    # Apply the inverse Rosenblatt transformation
    Xn[,j] = qnorm(CDF)  # Xn is now normally distributed
  }
  
  return(Xn)
}


newnormal <- read.csv("normalnewvarves1.csv")


 x <- gaussianize(varvestrees)
 y <- gaussianize(newnormal)
 
 varves<- read.csv("varvecount.csv")

 v <- gaussianize(varves)
 
write.csv(y, file = "newgausvarves.csv")
 
new <- read.csv("newthickness.csv")
new2 <- gaussianize(new)

write.csv(new2, file = "normalnewvarves.csv")


plot(x)

write.csv(x, file = "gaussianizedata.csv")

treeringplot <- ggplot(trees, aes(x =trees$year)) +                   
  geom_line(aes(y = trees$V2, colour="black"), show.legend = TRUE) +
  geom_line(aes(y = trees$V4, colour="red"), show.legend = TRUE) +
  ylab("Depth (cm)")+  
  xlab("Age (years AD)") + 
  ggtitle("Columbine Lake and Summitville Tree Rings") + 
  theme_bw() + 
  scale_y_reverse() +
  scale_color_manual(name = "Data", values = c(black = "black", red = "red"), labels = c('Columbine','Summitville'))


  
  


trees <- read.csv("gaussianizedata.csv")

#correlating trees and varves
res2 <- cor.test(trees$V2, trees$V3, method = "pearson")
res2$p.value


#El Nino comparisons

library(scales)
elnino <- read.csv("elnino.csv")
e <- elnino$El.Nino
th <- elnino$Thickness
elninonorm <- rescale(e, to = c(0, 1), from = range(e, na.rm = TRUE, finite = TRUE))
elninothicknorm <- rescale(th, to = c(0, 1), from = range(th, na.rm = TRUE, finite = TRUE))


newplot <- ggplot(elnino, aes(elnino$Date))+ geom_line(aes(x = elnino$Date, y = elninothicknorm)) + 
  geom_line(aes(x = elnino$Date, y = elninonorm, colour = "red")) 

correlationelnino <- cor.test(elnino$Thickness, elnino$El.Nino, method = "pearson")



#HIS and varves
HISvarves <- read.csv("HIS_varves.csv")
h <- HISvarves$RABD
thi <- HISvarves$adjussted.thickness
HISnorm <- rescale(h, to = c(0, 1), from = range(h, na.rm = TRUE, finite = TRUE))
thicknessnorm <- rescale(thi, to = c(0, 1), from = range(thi, na.rm = TRUE, finite = TRUE))+1
thnorm <- rescale(thi, to = c(0, 1), from = range(thi, na.rm = TRUE, finite = TRUE))



HIS <- ggplot() +geom_line(aes(x = HISvarves$Depth.mm, y = HISnorm, color = "green"), show.legend = TRUE)  +
  ylab("Thickness/RABD Concentration")+geom_line(aes(x = HISvarves$Depth.mm, y = thicknessnorm, color = "black"), show.legend = TRUE) +
  xlab("Depth (mm)") +
  ggtitle("Varve Thickness and RABD") + 
  theme_bw()+
  scale_color_manual(name = "Data", values = c(black = "black", green = "green"), labels = c('Varve Thickness','RABD'))
  



HISvarves
#add a column and cbind it to the data and then do a forloop where if the rabd is between 0-1mm and 1-2mm ect it should be equal to the data for 1mm

HISvarves [ ,6] = newcolumn

newcolumn = c(1:2612)


for(i in 1:2612){
  HISvarves[i,6] <- if( HISvarves$Depth.mm = data[,i]
  precipcor[6,i] <- 
}

install.packages("smooth")
library("smooth")

averagethick <- sma(thnorm, h=10, silent=FALSE)
averageHIS <- sma(HISnorm, h=10, silent=FALSE)






