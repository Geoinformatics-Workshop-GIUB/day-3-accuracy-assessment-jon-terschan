#Validation: Creating Samples ---------------------------------------------------

#Import required packages
library(raster)

#Import RF-Classification
img.classified <- raster("RF_classification.tif")

smp.test <- sampleStratified(x = img.classified,
                             size = 50,
                             na.rm = TRUE,
                             sp = TRUE)

#Check out result
smp.test$RF_classification

#Sample smp.test
smp.test <- smp.test[sample(nrow(smp.test)), ]

#Check out resampled smp.test-df
smp.test$RF_classification

#Delete variables (cell, RF_classification) of smp.test and replace them by "ID"
smp.test <- smp.test[, -c(1, 2)]
smp.test$ID <- 1:nrow(smp.test)

#Check out attributes of smp.test
smp.test

#Plot distribution of sampling on top of classification map
plot(img.classified, 
     axes = FALSE, 
     box = FALSE,
     col = mycolors
)
points(smp.test)

#Save smp.test as df 
shapefile(smp.test,
          filename = "RF_validation.shp",
          overwrite = TRUE
)
