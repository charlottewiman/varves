library(devtools)
library(geoChronR)
library(readr)
library(dplyr)
library(stats)
library(ggplot2)
library(varveR)


#load in a directory:
varveSequence <- readVarveDirectory(varveTop = "left")


#determine the order
secOrder <- determineMarkerLayerOrder(varveSequence)

#sort by the order
sorted <- varveSequence[secOrder]

#combine into a single sequence
sequence <- combineSectionsByMarkerLayer(sorted)

#scale sequence by thickness (1230 mm here)
sequence$thick <- sequence$thick*(1230/sum(sequence$thick))

#plot
plotCompositeSequence(sequence)


#create a table to turn codes to probabilities
translationTable <- data.frame(codes = c(1,2,3,4), overcount = c(.01,.05,.1,.5), undercount = c(.02,.1,.2,0))

#convert codes to probabilities
compositeSequence <- translateCodesToProbabilities(sequence,translationTable)

#simulate ensembles
varveEnsemble <- generateThicknessEnsemble(compositeSequence)

#count vector
count <- seq_len(nrow(varveEnsemble))

#plot ensemble
geoChronR::plotTimeseriesEnsRibbons(X = count, Y = varveEnsemble) %>%
  geoChronR::plotTimeseriesEnsLines(X = count, Y = varveEnsemble,maxPlotN = 2,color = "red")

#calculate an age/depth matrix
ageEns <- createEnsembleAgeDepthModel(varveEnsemble)

#find central ageEns
cv <- ageEns$ageDepthEns[,-1]
endRow <- cv[nrow(cv),]
centEns <- which(near(endRow,median(endRow),tol = .6))
centEns <- sample(centEns,size = 1)

#plot with central
geoChronR::plotTimeseriesEnsRibbons(X = 2018-count, Y = varveEnsemble) +
  geom_line(aes(x = 2018-count, y = varveEnsemble[,centEns], color = "Red"), size = .2)



#age model plot
plotTable <- as.data.frame(ageEns$summaryTable)

names(plotTable) <- c("depth", "c2.5", "c25", "c50", "c75", "c97.5")

newplot <- ggplot(plotTable)+
  geom_ribbon(aes(x = depth,ymin = c2.5  ,ymax = c97.5),fill = "gray80")+
  geom_ribbon(aes(x = depth,ymin = c25  ,ymax = c75), fill = "gray50")+ 
  geom_line(aes(x = depth, y = c50))+
  scale_x_reverse()+
  theme_bw()+
  coord_flip()


#create age model
ageModel <- createDepth2AgeFunction(ageEns$ageDepthEns)

#use age model function to estimate depth
someDepthSequence <- c(1,10,51,456,510,700,1100)

varveAges <- ageModel(someDepthSequence)

#plot some results
plot(varveAges$depth,varveAges$medianAge)
hist(varveAges$ageEnsemble[5,])

#plotting correlations for the past 40 years
plotty <-geoChronR::plotTimeseriesEnsRibbons(X = 2018-count, Y = varveEnsemble, x.bin = 1981:2017) +xlim(c(1981,2017))+ ylim(c(0,0.25))+
  xlab("Year AD") + 
  ylab("Varve thickness/SWE") +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size =20))




