#Validation: Accuracy Statistics ------------------------------------------------

#Import files
img.classified <- raster("RF_classification.tif")
shp.train <- shapefile("training_data.shp")
shp.valid <- shapefile("RF_validation.shp")

#Access validclass-column of shp.valid, transfer it to factors
reference <- as.factor(shp.valid$validclass)
reference

#Access shp.valid of RF-classification, transfer it to factors
predicted <- as.factor(extract(img.classified, shp.valid))
predicted

#Generate table of predicted and reference
accmat <- table("pred" = predicted, "ref" = reference)
accmat

#Generate user's accuracy
UA <- diag(accmat) / rowSums(accmat) * 100
UA

#Generate producer's accuracy
PA <- diag(accmat) / colSums(accmat) * 100
PA

#Generate overall accuracy
OA <- sum(diag(accmat)) / sum(accmat) * 100
OA

#Generate nicely looking matrix
accmat.ext <- addmargins(accmat)
accmat.ext <- rbind(accmat.ext, "Users" = c(PA, NA))
accmat.ext <- cbind(accmat.ext, "Producers" = c(UA, NA, OA))
colnames(accmat.ext) <- c(levels(as.factor(shp.train$class)), "Sum", "PA")
rownames(accmat.ext) <- c(levels(as.factor(shp.train$class)), "Sum", "UA")
accmat.ext <- round(accmat.ext, digits = 1)
dimnames(accmat.ext) <- list("Prediction" = colnames(accmat.ext),
                             "Reference" = rownames(accmat.ext))
class(accmat.ext) <- "table"
accmat.ext

#Validation: Significance Test --------------------------------------------------

sign <- binom.test(x = sum(diag(accmat)),
                   n = sum(accmat),
                   alternative = c("two.sided"),
                   conf.level = 0.95
)

pvalue <- sign$p.value
pvalue

CI95 <- sign$conf.int[1:2]
CI95

#Validation: Kappa-Coefficient --------------------------------------------------

#Write Kappa-Coefficient function
kappa <- function(m) {
  N <- sum(m)
  No <- sum(diag(m))
  Ne <- 1 / N * sum(colSums(m) * rowSums(m))
  return( (No - Ne) / (N - Ne) )
}

#Use accmat as arguments for kappa
kappa(accmat)
