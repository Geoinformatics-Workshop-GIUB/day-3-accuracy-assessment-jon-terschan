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

#Validation: Area Adjusted Accuracies -------------------------------------------
#According to Olofsson et al. 2014

#Import files
img.classified <- raster("RF_classification.tif")
shp.train <- shapefile("training_data.shp")
shp.valid <- shapefile("RF_validation.shp")

#Create regular accuracy matrix 
confmat <- table(as.factor(extract(img.classified, shp.valid)), as.factor(shp.valid$validclass))

#Get number of pixels per class and convert in km²
imgVal <- as.factor(getValues(img.classified))
nclass <- length(unique(shp.train$class))
maparea <- sapply(1:nclass, function(x) sum(imgVal == x))
maparea
maparea <- maparea * res(img.classified)[1] ^ 2 / 1000000
maparea

#Set confidence interval
conf <- 1.96

#Total map area
A <- sum(maparea)

#Proportion of area mapped as class i
W_i <- maparea / A

#Number of reference points per class
n_i <- rowSums(confmat)

#Population error matrix 
p <- W_i * confmat / n_i
p[is.na(p)] <- 0
round(p, digits = 4)
p

#Area estimation
p_area <- colSums(p) * A

#Area estimation confidence interval 
p_area_CI <- conf * A * sqrt(colSums((W_i * p - p ^ 2) / (n_i - 1)))

#Overall accuracy (Eq.1)
OA <- sum(diag(p))

#Producers accuracy (Eq.3)
PA <- diag(p) / colSums(p)

#Users accuracy (Eq.2)
UA <- diag(p) / rowSums(p)

#Overall accuracy confidence interval (Eq.5)
OA_CI <- conf * sqrt(sum(W_i ^ 2 * UA * (1 - UA) / (n_i - 1)))

#User accuracy confidence interval (Eq.6)
UA_CI <- conf * sqrt(UA * (1 - UA) / (n_i - 1))

#Producer accuracy confidence interval (Eq.7)
N_j <- sapply(1:nclass, function(x) sum(maparea / n_i * confmat[ , x]) )
tmp <- sapply(1:nclass, function(x) sum(maparea[-x] ^ 2 * confmat[-x, x] / n_i[-x] * ( 1 - confmat[-x, x] / n_i[-x]) / (n_i[-x] - 1)) )
PA_CI <- conf * sqrt(1 / N_j ^ 2 * (maparea ^ 2 * ( 1 - PA ) ^ 2 * UA * (1 - UA) / (n_i - 1) + PA ^ 2 * tmp))

#Gather results
result <- matrix(c(p_area, p_area_CI, PA * 100, PA_CI * 100, UA * 100, UA_CI * 100, c(OA * 100, rep(NA, nclass-1)), c(OA_CI * 100, rep(NA, nclass-1))), nrow = nclass)
result <- round(result, digits = 2) 
rownames(result) <- levels(as.factor(shp.train$class))
colnames(result) <- c("km²", "km²±", "PA", "PA±", "UA", "UA±", "OA", "OA±")
class(result) <- "table"
result
