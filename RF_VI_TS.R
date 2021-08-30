# load required libraries

library(randomForest)
library(rgdal)
library(raster)
library(caret)

# working directory definition
vstupni_adresar <- choose.dir(getwd(), "Select working directory")
prac_adresar <- setwd(vstupni_adresar)

# selection of input raster data to be classified
r <- brick(choose.files(caption = "Select raster for classification"))

# set the proper names of input raster bands
names(r) <- c("August_28_2020", "June_30_2019", "August_13_2020")

# input traininig shapefile points must be called ,,roi"
# input shapefile must have two columns- the first ,,ID"; the second called ,,Class"
vyber_shp <- choose.dir(getwd(), "Select directory, where training shp is located")

# start time
start <- Sys.time()

# set the number of iterations
Pocet_iteraci <- 30

# for loop cycle start
for (i in 1:Pocet_iteraci){
  cat("Classifying", i)

# reading training data with gdal library  
shp <- readOGR(dsn = vyber_shp, layer = "roi")

# training and validation datasets separation - each 50 %
dataSeparation <- createDataPartition(shp$Class, p = .5)[[1]]
shpTrain <- shp[dataSeparation,]
shpTest <- shp[-dataSeparation,]

# data frame creation for training data
shpTrain1 <- na.omit(as.data.frame(cbind(shpTrain, extract(r, shpTrain))))
sloupecky <- ncol(shpTrain1)-2


# Random Forest Model itself
rf.mdl <- randomForest(x=shpTrain1[,3:sloupecky], y=as.factor(shpTrain1[,"Class"]),
                         ntree=1000, proximity=TRUE, importance=TRUE, confusion=TRUE, do.trace=TRUE, err.rate=TRUE,
                       mtry=sqrt(nlayers(r)))

# raster data prediction in parallel mode
beginCluster()
predikce <- clusterR(r, raster::predict, args=list(model=rf.mdl))
predikce2 <- clusterR(r, raster::predict, args=list(model=rf.mdl, type="prob"))
endCluster()

# export classification result
jmeno_rastru <- paste("rf_classification", i, ".img", sep="_")
klasifikace <- writeRaster(predikce, filename=jmeno_rastru, format='HFA', options='INTERLEAVE=BSQ', datatype='INT2S', overwrite=TRUE)
varImpPlot(rf.mdl, main ="Variable importance")
rf.mdl$confusion

# export probabilities of classes
jmeno_rastru2 <- paste("rf_prob", i, ".tif", sep="_")
klasifikace2 <- writeRaster(predikce2, filename=jmeno_rastru2, format='GTiff',  overwrite=TRUE)

# export Variable Imporatances Plot ,,jpg" format
jmeno_promenne <- paste("predictors_", i, ".jpeg")
export_promennych <- jpeg(filename=jmeno_promenne, units="px", width=10000, height=10000, res=600)
varImpPlot(rf.mdl, main ="Variable importance")
dev.off()

# confusion matrix export (based on OOB)
jmeno_matice <- paste("cm_OOB_", i, ".txt", sep="_")
presnost_klasifikatoru <- capture.output(print(rf.mdl))
export_presnosti <- cat(presnost_klasifikatoru, file=jmeno_matice, sep="\n", append=FALSE)

# export Variable Imporatnces in ,,csv" format
tabulka_prediktory <- paste("variable_importance_", i, ".csv")
prediktory <- importance(rf.mdl)
export_prediktoru <- write.csv(prediktory, file=tabulka_prediktory)

########################################################################################################################
# Acuracy Asssessment

# validation dataset extraction
shpTest1 <- as.data.frame(cbind(shpTest, extract(predikce, shpTest)))
shpTest1

# define columns for confusion matrix
pred <- as.factor(shpTest1[,3])
val  <- as.factor(shpTest$Class)
hodnoty <- data.frame(pred, val)

# confusion matrix extraction
cm <- confusionMatrix(data=hodnoty$pred, 
                      reference=hodnoty$val)
cm

# confusion matrix export in text format
cm_jmeno <- paste("confusion_matrix_", i, ".txt")
zaznam <- capture.output(cm)
presnost <- cat(zaznam, file=cm_jmeno, sep="\n", append=FALSE)

# OOB versus Overall Accuracy Comparision in the form of Table
tabulka <- cbind(cm$overall['Accuracy'], (sum(diag(as.matrix(rf.mdl$confusion)/nrow(shpTrain)))))
tabulka

# OOB versus OA table export in text format
tb_jmeno <- paste("OA_versus_OOB", i, ".txt")
zaznam1 <- capture.output(tabulka)
presnost1 <- cat(zaznam1, file=tb_jmeno, sep="\n", append=FALSE)


# confusion matrix into matrix class conversion
M <- as.matrix(cm)

# Producer's accuracy
Z <- diag(M)/colSums(M)
Z

# User's accuracy
U <- diag(M)/rowSums(M)
U

# Producer's accuracy export 
z2_jmeno <- paste("Producer's accuracy_RF", i, ".txt")
zaznam2 <- capture.output(Z)
presnost2 <- cat(zaznam2, file=z2_jmeno, sep="\n", append=FALSE)

# User's accuracy export
z3_jmeno <- paste("User's accuracy_RF", i, ".txt")
zaznam3 <- capture.output(U)
presnost3 <- cat(zaznam3, file=z3_jmeno, sep="\n", append=FALSE)

# Overall Accuracy
kombinace <- cm$overall['Accuracy']

# Overall accuracy export
jmeno_tb <- paste("Overall_Accuracy", i, ".txt")
zaznam4 <- capture.output(kombinace)
tabulka <- cat(zaznam4, file=jmeno_tb, sep="\n", append=FALSE)

#############################################################################################################################
# Training and Validation data Export in shapefile 

# defintion of trainig and validation datasets names
tr_jmeno <- paste("trianing_points", i)
val_jmeno <- paste("validation_points", i)

# training and validation points export
tr <-  writeOGR(obj=shpTrain, dsn=getwd(), layer=tr_jmeno, driver="ESRI Shapefile")
val <- writeOGR(obj=shpTest, dsn=getwd(), layer=val_jmeno, driver="ESRI Shapefile")

# information about how many points were used for training and validation
stats <- cbind(nrow(shpTrain), nrow(shpTest))

# export information statistics into disk
stats_jmeno <- paste("Train_Validation_Ratio_", i, ".txt")
zaznam1 <- capture.output(stats)
presnost1 <- cat(zaznam1, file=stats_jmeno, sep="\n", append=FALSE)
}

# the end of for loop

######################################################################################################################

# raster stack creation from classifications 
klasifikace_seznam <- list.files(vstupni_adresar, pattern='\\.img$')
klasifikace_stack <- stack(klasifikace_seznam)
ulozeni_klasifikace <- writeRaster(klasifikace_stack, filename="RandomForest_stacked.img", format='HFA', 
                                    options='INTERLEAVE=BSQ', datatype='INT2S', overwrite=TRUE)

# the overall time needed for calculations
konec <- Sys.time()
cas <- konec - start
cas

# export the overall time to disk
z_cas <- capture.output(cas)
u_cas <- cat(z_cas, file="Overall_Time.txt", sep="\n", append=FALSE)


