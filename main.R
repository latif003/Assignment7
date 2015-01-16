# Name: Nikoula, Latifah & Nikos
# Date: 13 January 2015
# Assignment 7 : Advance Raster

rm(list=ls()) # clear the workspace

getwd()# make sure the data directory

# Load functions:
source("R/rmse.R")

# load libraries
library(raster)

# load the data
load("data/GewataB1.rda")
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load("data/vcfGewata.rda")
load ('data/trainingPoly.rda')

# process the input data for further analyses
landsat <- brick(GewataB1,GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)
landsat <- calc(landsat, fun=function(x) x / 10000)

# the max value for tree cover is 100 (http://glcf.umd.edu/data/landsatTreecover/), remove the value above 100
vcfGewata[vcfGewata > 100] <- NA
landsatvcf <-  addLayer(landsat,vcfGewata)
names(landsatvcf) <- c("band1", "band2", "band3", "band4",  "band5", "band7", "vcf")
landsatvcf

# creating data frame from landsat vcf raster
value <- getValues(landsatvcf)
value <- na.omit (value)
valuedf <- as.data.frame (value)

# Creating linear regression model
model <- lm(vcf ~ band1 + band2+ band3 + band4 + band5 + band7, valuedf)
summary(model) # show results

# new model by eliminating band 7 (because it does'nt have significant influence in vcf)
model <- lm(vcf ~ band1 + band2+ band3 + band4 + band5, valuedf)
summary(model) # show results

# predict vcf values based on the developed regression model
vcfPredR <- predict(landsatvcf, model)
names(vcfPredR) <- "vcfPred"

# make data frame
vcfPred <- predict(model, valuedf)
vcfPreddf <- as.data.frame (vcfPred)
head (vcfPred) # inspect data
head (vcfPreddf) # inspect data

# add the predicted value data frame
# allvcfdf <- cbind(valuedf, vcfPreddf) # if we want to combine all the df
# head (allvcfdf)

# plot the predicted tree cover raster and compare with the original VCF raster.
p1=spplot(landsatvcf$vcf, zcol = 'vcf',col.regions = colorRampPalette(c("lightblue","green", "yellow","red"))(100), main= "Original VCF tree cover")
p2=spplot(vcfPredR, zcol = 'vcfPred',col.regions = colorRampPalette(c("lightblue","green", "yellow","red"))(100), main= "Predicted VCF tree cover")
print(p1, position = c(0,.5,.5,1),more=T)
print(p2, position = c(.5,.5,1,1),more = T)

# Compute the root mean squared error (data frames)
rmse <- rmse(valuedf$vcf, vcfPreddf)
print(paste("The rmse is equal to",rmse ))

# calculate rmse per layer using trainingPoly
## combine vcf and vcf predict raster data
vcfbrick<-brick(landsatvcf$vcf,vcfPredR)

## rasterize the training area polygon
## assign classes based on training area 
classes <- rasterize(trainingPoly, vcfbrick, field='Class')
plot (classes) #inspect the result 

# calculate the mean for each classes
zonal<-zonal(vcfbrick,classes, mean)
zonaldf<-as.data.frame(zonal)
head(zonaldf) # inspect the data

# call function
source("R/rmse.R")

# zone correspons to: cropland, forest, wetland
rmseCrop <- rmse(zonaldf[1,2], zonaldf[1,3])
rmseForest <- rmse(zonaldf [2,2], zonaldf[2,3])
rmseWetland <- rmse(zonaldf [3,2], zonaldf[3,3])

print(paste("The rmse is", rmseCrop, "for crop, ", rmseForest, "for Forest, and", rmseWetland, "for wetland."))
