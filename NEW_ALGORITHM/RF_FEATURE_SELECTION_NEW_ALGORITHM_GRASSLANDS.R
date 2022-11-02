library(randomForest)
library(rgdal)
library(raster)
library(caret)
library(EBImage)
library(xlsx)
library(cvequality)

# set a path to working directory
vstupni_adresar <- choose.dir(getwd(), "Set a path to working directory")
prac_adresar <- setwd(vstupni_adresar)

# raster selection to be classified
r <- brick(choose.files(caption = "Set path to a raster to be classified"))

# create a vector of names of raster time series
names(r) <- c("August_30_2015", "August_4_2016", "June_20_2017", "September_28_2017", "April_6_2018",
              "April_21_2018", "May_31_2018", "August_29_2018", "September_18_2018", "September_28_2018",
              "October_13_2018", "April_1_2019", "April_16_2019", "June_30_2019", "August_29_2019", "April_5_2020",
              "April_20_2020", "July_14_2020", "July_24_2020", "August_8_2020", "August_13_2020", "August_28_2020",
              "September_12_2020")

# locate training point shapefile exactly named ,,trenink"
vyber_shp <- choose.dir(getwd(), "Select training shp location")

# start a measure of processing time
start <- Sys.time()

# set a number of required iterations
Pocet_iteraci <- 10

# definition of for cycle for iterated random forest classification
for (i in 1:Pocet_iteraci){
  cat("Executing classification", i)

# read training data  
shp <- readOGR(dsn = vyber_shp, layer = "roi")

# cut training data into training and validation parts - 50 % for each part
dataSeparation <- createDataPartition(shp$Class, p = .5)[[1]]
shpTrain <- shp[dataSeparation,]
shpTest <- shp[-dataSeparation,]

# create a data frame from training data
shpTrain1 <- na.omit(as.data.frame(cbind(shpTrain, extract(r, shpTrain))))
sloupecky <- ncol(shpTrain1)-2


# create own random forest model for classification
rf.mdl <- randomForest(x=shpTrain1[,3:sloupecky], y=as.factor(shpTrain1[,"Class"]),
                         ntree=1000, proximity=TRUE, importance=TRUE, confusion=TRUE, do.trace=TRUE, err.rate=TRUE,
                       mtry=sqrt(nlayers(r)))

# create a background for parallel processing in order to predict results for random forest classification
beginCluster()
predikce <- clusterR(r, raster::predict, args=list(model=rf.mdl))
predikce2 <- clusterR(r, raster::predict, args=list(model=rf.mdl, type="prob"))
endCluster()

# save random forest classification results into hard drive
jmeno_rastru <- paste("Change_Classification_RF", i, ".img", sep="_")
klasifikace <- writeRaster(predikce, filename=jmeno_rastru, format='HFA', options='INTERLEAVE=BSQ', datatype='INT2S', overwrite=TRUE)
varImpPlot(rf.mdl, main ="Variable importance")
rf.mdl$confusion

# save class probabilties to hard drive
jmeno_rastru2 <- paste("class_probabilities", i, ".tif", sep="_")
klasifikace2 <- writeRaster(predikce2, filename=jmeno_rastru2, format='GTiff',  overwrite=TRUE)

# variable importance plot a save to hard drive
jmeno_promenne <- paste("important_predictors_", i, ".jpeg")
export_promennych <- jpeg(filename=jmeno_promenne, units="px", width=10000, height=10000, res=600)
varImpPlot(rf.mdl, main ="Variable importance")
dev.off()

# save confusion matrix into csv file
jmeno_matice <- paste("confusion_matrix", i, ".txt", sep="_")
presnost_klasifikatoru <- capture.output(print(rf.mdl))
export_presnosti <- cat(presnost_klasifikatoru, file=jmeno_matice, sep="\n", append=FALSE)

# save variable important predictors into disk into csv 
tabulka_prediktory <- paste("variable_importance", i, ".csv")
prediktory <- importance(rf.mdl)
export_prediktoru <- write.csv(prediktory, file=tabulka_prediktory)

########################################################################################################################
# Acuracy Asssessment

# creating data frame for accuracy assessment
shpTest1 <- as.data.frame(cbind(shpTest, extract(predikce, shpTest)))
shpTest1

pred <- as.factor(shpTest1[,3])
val  <- as.factor(shpTest$Class)
hodnoty <- data.frame(pred, val)

cm <- confusionMatrix(data=hodnoty$pred, 
                      reference=hodnoty$val)
cm

# export confusion matrix into disk - txt file
cm_jmeno <- paste("confusion_matrix", i, ".txt")
zaznam <- capture.output(cm)
presnost <- cat(zaznam, file=cm_jmeno, sep="\n", append=FALSE)

# result of out of the bag record and confusion matrix
tabulka <- cbind(cm$overall['Accuracy'], (sum(diag(as.matrix(rf.mdl$confusion)/nrow(shpTrain)))))
tabulka

# table export to disk- txt file
tb_jmeno <- paste("OA_versus_OOB", i, ".txt")
zaznam1 <- capture.output(tabulka)
presnost1 <- cat(zaznam1, file=tb_jmeno, sep="\n", append=FALSE)


# confusion matrix - create a confusion matrix class object in R environment
M <- as.matrix(cm)

# Producer's accuracy
Z <- diag(M)/colSums(M)
Z

# User's accuracy
U <- diag(M)/rowSums(M)
U

# Export of producer's accuracy into hdd
z2_jmeno <- paste("Producer's_accuracy_RF", i, ".txt")
zaznam2 <- capture.output(Z)
presnost2 <- cat(zaznam2, file=z2_jmeno, sep="\n", append=FALSE)

# Export of user's accuracy into hdd
z3_jmeno <- paste("User's_accuracy_RF", i, ".txt")
zaznam3 <- capture.output(U)
presnost3 <- cat(zaznam3, file=z3_jmeno, sep="\n", append=FALSE)

# overall accuracy
kombinace <- cm$overall['Accuracy']

# Export overall accuracy into hdd
jmeno_tb <- paste("Overall_Accuracy", i, ".txt")
zaznam4 <- capture.output(kombinace)
tabulka <- cat(zaznam4, file=jmeno_tb, sep="\n", append=FALSE)

#############################################################################################################################
# Sampling strategy Accuracy Assessment

# set names for training and validation points datasets
tr_jmeno <- paste("training_points", i)
val_jmeno <- paste("validation_points", i)

# save training and validation point datasets into hard drive for each iteration
tr <-  writeOGR(obj=shpTrain, dsn=getwd(), layer=tr_jmeno, driver="ESRI Shapefile")
val <- writeOGR(obj=shpTest, dsn=getwd(), layer=val_jmeno, driver="ESRI Shapefile")

# statistics how many training and validation points was used
stats <- cbind(nrow(shpTrain), nrow(shpTest))

# export statistics to hdd
stats_jmeno <- paste("Training_Validation_Points_Ratio", i, ".txt")
zaznam1 <- capture.output(stats)
presnost1 <- cat(zaznam1, file=stats_jmeno, sep="\n", append=FALSE)

# F1 Score Definition
F1 <- 2*((U*Z)/(U+Z))
F1

# Export F1 Score to hdd in the form of txt file
F1_jmeno <- paste("F1_Score", i, ".txt")
F1_zaznam <- capture.output(F1)
F1_presnost1 <- cat(F1_zaznam, file=F1_jmeno, sep="\n", append=FALSE)
}
#######################################################################################################################

# total time definition
konec <- Sys.time()
cas <- konec - start
cas

# total time export
z_cas <- capture.output(cas)
u_cas <- cat(z_cas, file="Time_Elapsed_RF.txt", sep="\n", append=FALSE)

#######################################################################################################################
#######################################################################################################################

# Visualize average values of MDA and MDG metrics

# Create dedicated working directory
output_dir <- file.path(getwd(), "Importance_Plots")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

# Move necessary .csv files into newly created directory from previous step
move <- list.files(path=getwd(), pattern="variable_importance", full.names=TRUE)
move_folder <- file.copy(from=move, to=output_dir)

# Created folder "Importance Plots is now set as new working directory
new_wd <- setwd(output_dir)

## MDA PLOT
# create background for MDA plot
csv_list <- list.files(path=new_wd, pattern="*.csv", full.names=TRUE)
csv_files_df <- lapply(csv_list, function(x){read.csv(file=x, header=TRUE, sep=",")[,2]})
csv_combined <- do.call("cbind", lapply(csv_files_df, as.data.frame))

# name of x axis
radky <- read.csv(list.files()[c(1)])
radky2 <- as.character(radky[,1])

# export values of MDA metric in the form of excel file to hdd
vysledky_kl <- write.xlsx(csv_combined, file="Average_Variable_Importance_MDA.xlsx")

# average MDA values
vi <- rowMeans(csv_combined)
vi2 <- cbind(csv_combined, vi)
rownames(vi2) <- names(r)

# MDA mean
M_MDA <- vi

# plot MDA values with help of ggplot library
d <- as.data.frame(cbind(rownames(vi2),vi2$vi))
colnames(d) <- c("MDA", "Value")
d$Value <- as.numeric(as.character(d$Value))
f1 <- d[order(-d$Value),]

ggplot(f1, mapping= aes(x = reorder(MDA, Value), y = Value)) + theme_bw() +
  geom_bar(stat="identity", fill = "#FF6666") + coord_flip() + xlab(NULL) + theme_grey(base_size = 22)+
  ggtitle("Mean Decrease Accuracy")

# export MDA plot to hdd
png(filename="RF_MDA_GGPLOT.png", units="px", width=7000, height=5000, res=600)

ggplot(f1, mapping= aes(x = reorder(MDA, Value), y = Value)) + theme_bw() +
  geom_bar(stat="identity", fill = "#FF6666") + coord_flip() + xlab(NULL) + theme_grey(base_size = 22)+
  ggtitle("Mean Decrease Accuracy")

dev.off()
###########################################################################################################################

## MDG PLOT

# merge MDG .csv files into a single one
csv_list <- list.files(path=getwd(), pattern="*.csv", full.names=TRUE)
csv_files_df <- lapply(csv_list, function(x){read.csv(file=x, header=TRUE, sep=",")[,"MeanDecreaseGini"]})
csv_combined <- do.call("cbind", lapply(csv_files_df, as.data.frame))

# name of x axis
radky <- read.csv(list.files()[c(1)])
radky2 <- as.character(radky[,1])

# save merged MDG .csv files into hard drive as file of MS Excel
vysledky_kl <- write.xlsx(csv_combined, file="Average_Variable_Importance_MDG.xlsx")

# calculate average MDG values
vi <- rowMeans(csv_combined)
vi2 <- cbind(csv_combined, vi)
rownames(vi2) <- names(r)

# MDG mean
M_MDG <- vi

# plot MDG with help of ggplot library
d <- as.data.frame(cbind(rownames(vi2),vi2$vi))
colnames(d) <- c("MDA", "Value")
d$Value <- as.numeric(as.character(d$Value))
f2 <- d[order(-d$Value),]

ggplot(f2, mapping= aes(x = reorder(MDA, Value), y = Value)) + theme_bw() +
  geom_bar(stat="identity", fill = "#FF6666") + coord_flip() + xlab(NULL) + theme_grey(base_size = 22)+
  ggtitle("Mean Decrease Gini")

# export MDG plot to disk
png(filename="RF_MDG_GGPLOT.png", units="px", width=7000, height=5000, res=600)

ggplot(f2, mapping= aes(x = reorder(MDA, Value), y = Value)) + theme_bw() +
  geom_bar(stat="identity", fill = "#00abff") + coord_flip() + xlab(NULL) + theme_grey(base_size = 22)+
  ggtitle("Mean Decrease Gini")

dev.off()

################################################################################################################################

#################################################################################################################################

# read exported plots from MDA and MDG from hard drive in a working directory

a <- readImage(files="RF_MDA_GGPLOT.png")
b <- readImage(files="RF_MDG_GGPLOT.png")

# merge plot into a single file
c <- abind(a, b, along=1)
display(c)

# Ulozeni slouceneho obrazku na pevny disk
d1 <- writeImage(c, files="MDA_and_MDG_Together.jpeg")

#################################################################################################################################

# MDA and MDG correlation

# MDA and MDG dataframe in oreder to scale values between 0 a 1
dat <- as.data.frame(cbind(f1$Value, f2$Value))
colnames(dat) <- c("MDA", "MDG")
f <- function(x){(x-min(x))/(max(x)-min(x))}
dat2 <- f(dat)

# linear plot between scaled values of MDA a MDG
g <- plot(dat2, main="Relationship between MDA and MDG (Scaled)", xlab="MDA", ylab="MDG", col="firebrick",
          las = 1, cex = 1.5, bty = "l", pch=16)
abline(lm(MDG~MDA, data=dat2), col="black")

# Ulozeni vytahu MDG a MDA na pevny disk
png(filename="MDG_and_MDA_linear_regression.png", units="px", width=7000, height=5000, res=600)
plot(dat2, main="Relationship between MDA and MDG (Scaled)", xlab="MDA", ylab="MDG", col="firebrick",
     las = 1, cex = 1.5, bty = "l", pch=16)
abline(lm(MDG~MDA, data=dat2), col="black")

dev.off()

# linear regression between MDA a MDG
lin <- lm(MDG~MDA, data=dat2)
lin

# export of parameters of linear model
z <- capture.output(lin)
lm <- cat(z, file="Linear_Model_Coefficients.txt", sep="\n", append=FALSE)

# Jaccard index calculation MDA a DMG
a <- rownames(f1)
b <- rownames(f2)

jacard <- function(a, b){
  intersection = length(intersect(a, b))
  union = length(a) + length(b)
  return = (intersection/union)
}

ji <- jacard(a, b)
ji

# Export results of Jaccard index into hdd
jiz <- capture.output(ji)
jie <- cat(jiz, file="Jaccard_Index.txt", sep="\n", append=FALSE)

# Correct band numbers for MDA rastr
com <- f1
com

# Export ordered MDA to hdd
com_e <- capture.output(com)
com_u <- cat(com_e, file="MDA_Ordered.txt", sep="\n", append=FALSE)

# Correct band numbers for MDG
com2 <- f2
com2

# Export ordered MDA to hdd
com_e2 <- capture.output(com2)
com_u2 <- cat(com_e2, file="MDG_Ordered.txt", sep="\n", append=FALSE)
################################################################################################################################
################################################################################################################################

# Coefficient of variation calculation

# Coefficient of variation calculation for MDA

vi_df <- dat
MDA_CV <- sd(vi_df$MDA)/mean(vi_df$MDA)
MDA_CV

# Coefficient of variation calculation for MDG

MDG_CV <- sd(vi_df$MDG)/mean(vi_df$MDG)
MDG_CV

# MDA Mean
MDA_Mean <- mean(vi_df$MDA)

# MDA Standard Deviation
MDA_SD <- sd(vi_df$MDA)

# MDG Mean
MDG_Mean <- mean(vi_df$MDG)

# MDG Standard Deviation
MDG_SD <- sd(vi_df$MDG)

# Dedicated statistical test of significance for coefficient of variation comparison - Krisnamorty (2014)
A = c(Pocet_iteraci, Pocet_iteraci)
B = c(MDA_Mean, MDG_Mean)
C = c(MDA_SD, MDG_SD)

# Statistical test calculation MDA a MDG
cv_test <- mslr_test2(nr=1e3, n=A, x=c(B), s=c(C))
cv_test

# Export of results for statistical test of coefficients of variation
cv_cap <- capture.output(cv_test)
cv_exp <- cat(cv_cap, file="P_Value_CV_Test.txt", sep="\n", append=FALSE)

# Condition definition, that lower value of CV will be used
V = as.data.frame(c(MDA_CV, MDG_CV))
cv_CV = min(V)

if(MDA_CV==cv_CV){

con1=f1
x1=print("MDA has been used!")

cx1 <- capture.output(x1)
cx1_exp <- cat(cx1, file="MDA_USED.txt", sep="\n", append=FALSE)

}else{

con1=f2
x2=print("MDG has been used!")

cx2 <- capture.output(x2)
cx2_exp <- cat(cx2, file="MDG_USED.txt", sep="\n", append=FALSE)
  
}

# Select three best variable importances based on MDA or MDG for further analysis
rn <- as.numeric(rownames(con1))
sel1 <- c(rn[1:3])
sel1

# Create dedicated rastr dataset from the best variable importances
rs1 <- raster(r, layer=sel1[1])
rs2 <- raster(r, layer=sel1[2])
rs3 <- raster(r, layer=sel1[3])

# rasterbrick creation from the most important three variables
r_reduced <- brick(rs1, rs2, rs3)
r_reduced

#########################################################################################################################
#########################################################################################################################

# Create new working directory in order to produce reduced Time Series

output_dir <- file.path(getwd(), "Reduced_Time_Series")
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

setwd(output_dir)

for (i in 1:Pocet_iteraci){
  cat("Executing classification", i)
  
  # load training data 
  shp <- readOGR(dsn = vyber_shp, layer = "roi", stringsAsFactors=FALSE)
  
  # create training and validation datasets
  dataSeparation <- createDataPartition(shp$Class, p = .5)[[1]]
  shpTrain <- shp[dataSeparation,]
  shpTest <- shp[-dataSeparation,]
  
  # extrakce dat
  shpTrain1 <- na.omit(as.data.frame(cbind(shpTrain, extract(r_reduced, shpTrain))))
  sloupecky <- ncol(shpTrain1)-2
  
  
  # random forest model for reduced time series
  rf.mdl <- randomForest(x=shpTrain1[,3:sloupecky], y=as.factor(shpTrain1[,"Class"]),
                         ntree=1000, proximity=TRUE, importance=TRUE, confusion=TRUE, do.trace=TRUE, err.rate=TRUE,
                         mtry=sqrt(nlayers(r)))
  
  # start parallel processing of random forest classification
  beginCluster()
  predikce <- clusterR(r_reduced, raster::predict, args=list(model=rf.mdl))
  predikce2 <- clusterR(r_reduced, raster::predict, args=list(model=rf.mdl, type="prob"))
  endCluster()
  
  # save results of random forest classification into hdd
  jmeno_rastru <- paste("classification_reduced_RF", i, ".img", sep="_")
  klasifikace <- writeRaster(predikce, filename=jmeno_rastru, format='HFA', options='INTERLEAVE=BSQ', datatype='INT2S', overwrite=TRUE)
  varImpPlot(rf.mdl, main ="Variable importance")
  rf.mdl$confusion
  
  # save class probabilities into hard drive
  jmeno_rastru2 <- paste("classification_reduced_RF_P", i, ".tif", sep="_")
  klasifikace2 <- writeRaster(predikce2, filename=jmeno_rastru2, format='GTiff',  overwrite=TRUE)
  
  # export varible importances in .jpeg file
  jmeno_promenne <- paste("predictors_", i, ".jpeg")
  export_promennych <- jpeg(filename=jmeno_promenne, units="px", width=10000, height=10000, res=600)
  varImpPlot(rf.mdl, main ="Variable importance")
  dev.off()
  
  # save confusion matrix of reduced time series 
  jmeno_matice <- paste("confusion_matrix_r", i, ".txt", sep="_")
  presnost_klasifikatoru <- capture.output(print(rf.mdl))
  export_presnosti <- cat(presnost_klasifikatoru, file=jmeno_matice, sep="\n", append=FALSE)
  
  # save variable importances as .csv file
  tabulka_prediktory <- paste("variable_importance_reduced", i, ".csv")
  prediktory <- importance(rf.mdl)
  export_prediktoru <- write.csv(prediktory, file=tabulka_prediktory)
  
  ########################################################################################################################
  # Acuracy Asssessment for reduced time series
  
  # test data extraction for reduced time series
  shpTest1 <- as.data.frame(cbind(shpTest, extract(predikce, shpTest)))
  shpTest1
  
  pred <- as.factor(shpTest1[,3])
  val  <- as.factor(shpTest$Class)
  hodnoty <- data.frame(pred, val)
  
  cm <- confusionMatrix(data=hodnoty$pred, 
                        reference=hodnoty$val)
  cm
  
  # export of confusion matrix into hard drive
  cm_jmeno <- paste("confusion_matrix_reduced", i, ".txt")
  zaznam <- capture.output(cm)
  presnost <- cat(zaznam, file=cm_jmeno, sep="\n", append=FALSE)
  
  # out of the bag record
  tabulka <- cbind(cm$overall['Accuracy'], (sum(diag(as.matrix(rf.mdl$confusion)/nrow(shpTrain)))))
  tabulka
  
  # export tabulky na pevny disk
  tb_jmeno <- paste("OA_versus_OOB", i, ".txt")
  zaznam1 <- capture.output(tabulka)
  presnost1 <- cat(zaznam1, file=tb_jmeno, sep="\n", append=FALSE)
  
  
  # confusion matrix as class obejt matrix in R...reduced time series
  M <- as.matrix(cm)
  
  # Producer's accuracy for reduced time series
  Z <- diag(M)/colSums(M)
  Z
  
  # User's accuracy for reduced time series
  U <- diag(M)/rowSums(M)
  U
  
  # export of producer's accuracy to hdd for reduced time series
  z2_jmeno <- paste("Producers_Accuracy_RF_Reduced", i, ".txt")
  zaznam2 <- capture.output(Z)
  presnost2 <- cat(zaznam2, file=z2_jmeno, sep="\n", append=FALSE)
  
  # export of user's accuracy to hdd for reduced time series
  z3_jmeno <- paste("Users_Accuracy_RF_Reduced", i, ".txt")
  zaznam3 <- capture.output(U)
  presnost3 <- cat(zaznam3, file=z3_jmeno, sep="\n", append=FALSE)
  
  # overall accuracy calculation for reduced time series
  kombinace <- cm$overall['Accuracy']
  
  # Zapis celkove presnosti na pevny disk
  jmeno_tb <- paste("Overall_Accuracy_RF_Reduced", i, ".txt")
  zaznam4 <- capture.output(kombinace)
  tabulka <- cat(zaznam4, file=jmeno_tb, sep="\n", append=FALSE)
  
  #############################################################################################################################
  # Sampling strategy Accuracy Assessment for reduced time series
  
  # names definition for training and validation datasets of reduced time series
  tr_jmeno <- paste("training_points_reduced", i)
  val_jmeno <- paste("validation_points_reduced", i)
  
  # save training and validation datasets into hard drive as ESRI shapefile
  tr <-  writeOGR(obj=shpTrain, dsn=getwd(), layer=tr_jmeno, driver="ESRI Shapefile")
  val <- writeOGR(obj=shpTest, dsn=getwd(), layer=val_jmeno, driver="ESRI Shapefile")
  
  # statistics calculation for reduced time series
  stats <- cbind(nrow(shpTrain), nrow(shpTest))
  
  # export resuts to disk
  stats_jmeno <- paste("Train_Validation_Ammount", i, ".txt")
  zaznam1 <- capture.output(stats)
  presnost1 <- cat(zaznam1, file=stats_jmeno, sep="\n", append=FALSE)
  
  # F1 score calculation for reduced time series
  F1 <- 2*((U*Z)/(U+Z))
  F1
  
  # Save results of F1 score to hdd
  F1_jmeno <- paste("F1_Score_Reduced", i, ".txt")
  F1_zaznam <- capture.output(F1)
  F1_presnost1 <- cat(F1_zaznam, file=F1_jmeno, sep="\n", append=FALSE)
}

# final time of overall computation
konec <- Sys.time()
cas <- konec - start
cas

# export of result of final time 
z_cas <- capture.output(cas)
u_cas <- cat(z_cas, file="Time_Elapsed_RF_CV.txt", sep="\n", append=FALSE)

##############################################################################################################################
##############################################################################################################################




