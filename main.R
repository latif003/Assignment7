# Name: Nikoula, Latifah & Nikos
# Date: 13 January 2015

rm(list=ls()) # clear the workspace

getwd()# make sure the data directory

# Load functions:
source("R/rmse.R")

# load libraries
library(raster)

# load the data
list<-list.files ('data/', pattern = glob2rx('*.rda'), full.names = TRUE)
load ('data/trainingPoly.rda')

# process the input data for further analyses
landsat <- brick(GewataB1,GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)
landsat <- calc(landsat, fun=function(x) x / 10000)

# the max value for tree cover is 100 (http://glcf.umd.edu/data/landsatTreecover/), remove the value above 100
vcfGewata[vcfGewata > 100] <- NA
landsatvcf <-  addLayer(landsat,vcfGewata)
names(landsatvcf) <- c("band1", "band2", "band3", "band4",  "band5", "band7", "vcf")
landsatvcf

# extract the values into a data frame
valuetable <- getValues(landsatvcf)
valuetable <- na.omit(valuetable)
valuetable <- as.data.frame(valuetable)

# Creating linear regression model
model <- lm(vcf ~ band1 + band2 + band3 + band4 + band5 + band7, valuetable)
summary(model) # show results
coef(model) # extract the model's coefficients
layout(matrix(1:4,2,2)) 
plot(model)

##layout(matrix(1:4,2,2)) 
##plot(model)

# predict vdf values based on the developed regression model
landsatvcfPred <- predict(landsatvcf, model)
names(vcf.predict.raster) <- "vcf"

#make the data frame
vcf.predict <- predict(model, valuetable)
vcf.predict.df<-as.data.frame(vcf.predict)

summary(vcf.predict.df)

# plot the predicted tree cover raster and compare with the original VCF raster.
p1=spplot(landsatvcf$vcf, zcol = 'vcf',col.regions = colorRampPalette(c("lightblue","green", "yellow","red"))(100), main= "Original VCF tree cover")
p2=spplot(vcf.predict.raster, zcol = 'vcf',col.regions = colorRampPalette(c("lightblue","green", "yellow","red"))(100), main= "Predicted VCF tree cover")
print(p1, position = c(0,.5,.5,1),more=T)
print(p2, position = c(.5,.5,1,1),more = T)

# Compute the root mean squared error (data frames)
rmse <- rmse(valuetable$vcf,vcf.predict)
print(paste("The rmse is equal to",rmse ))

# assign class in trainingPoly
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)

# assign 'Code' values to raster cells (where they overlap)
classActual <- rasterize(trainingPoly, landsatvcf, field='Code')
classPred <- rasterize(trainingPoly, landsatvcfPred, field='Code')

# mask the raster
actual <- mask(landsatvcf, )

# calculate the RMSE separately for each of the classes and compare
# provide classification using random forest
plot(trainingPoly)
str(trainingPoly)


classes <- rasterize(trainingPoly, vcf.predict.raster, field='Code')

zonal(vcf.predict.raster, trainingPoly)
