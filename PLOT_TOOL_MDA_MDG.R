# skript plots results of avarage MDA and MDG of iterative classification
# load required libraries

library(rJava)
library(xlsx)
library(ggplot2)
library(cvequality)
library(EBImage)


# Set folder of necessary .csv files
vstupni_adresar <- choose.dir(getwd(), "Choose Correct Folder Location")
prac_adresar <- setwd(vstupni_adresar)

## MDA PLOT
# function, that combines of all .csv files into a sigle one
csv_list <- list.files(path=getwd(), pattern="*.csv", full.names=TRUE)
csv_files_df <- lapply(csv_list, function(x){read.csv(file=x, header=TRUE, sep=",")[,"MeanDecreaseAccuracy"]})
csv_combined <- do.call("cbind", lapply(csv_files_df, as.data.frame))

# label of axis x
radky <- read.csv(list.files()[c(1)])
radky2 <- as.character(radky[,1])

# conversion of merged single .csv file in previous step into do .xlsx (MS Excel native format)
vysledky_kl <- write.xlsx(csv_combined, file="MEAN_MDA.xlsx")

#  mean calculation of MDA
vi <- rowMeans(csv_combined)
vi2 <- cbind(csv_combined, vi)
rownames(vi2) <- c(radky2)

# MDA meanm
M_MDA <- vi

#  setting ggplot parameters for MDA
d <- as.data.frame(cbind(rownames(vi2),vi2$vi))
colnames(d) <- c("MDA", "Value")
d$Value <- as.numeric(as.character(d$Value))
f1 <- d[order(-d$Value),]

ggplot(f1, mapping= aes(x = reorder(MDA, Value), y = Value)) + theme_bw() +
  geom_bar(stat="identity", fill = "#FF6666") + coord_flip() + xlab(NULL) + theme_grey(base_size = 22)+
  ggtitle("Mean Decrease Accuracy")

# export plot to hard drive in .png format
png(filename="RF_MDA_GGPLOT.png", units="px", width=7000, height=5000, res=600)

ggplot(f1, mapping= aes(x = reorder(MDA, Value), y = Value)) + theme_bw() +
  geom_bar(stat="identity", fill = "#FF6666") + coord_flip() + xlab(NULL) + theme_grey(base_size = 22)+
  ggtitle("Mean Decrease Accuracy")

dev.off()
###########################################################################################################################

## MDG PLOT

# function, that combines of all .csv files into a sigle one
csv_list <- list.files(path=getwd(), pattern="*.csv", full.names=TRUE)
csv_files_df <- lapply(csv_list, function(x){read.csv(file=x, header=TRUE, sep=",")[,"MeanDecreaseGini"]})
csv_combined <- do.call("cbind", lapply(csv_files_df, as.data.frame))

# label of axis x
radky <- read.csv(list.files()[c(1)])
radky2 <- as.character(radky[,1])

# conversion of merged single .csv file in previous step into do .xlsx (MS Excel native format)
vysledky_kl <- write.xlsx(csv_combined, file="MEAN_MDG.xlsx")

# mean calculation of MDG
vi <- rowMeans(csv_combined)
vi2 <- cbind(csv_combined, vi)
rownames(vi2) <- c(radky2)

# MDG mean
M_MDG <- vi

# setting ggplot parameters
d <- as.data.frame(cbind(rownames(vi2),vi2$vi))
colnames(d) <- c("MDA", "Value")
d$Value <- as.numeric(as.character(d$Value))
f2 <- d[order(-d$Value),]

ggplot(f2, mapping= aes(x = reorder(MDA, Value), y = Value)) + theme_bw() +
  geom_bar(stat="identity", fill = "#FF6666") + coord_flip() + xlab(NULL) + theme_grey(base_size = 22)+
  ggtitle("Mean Decrease Gini")

# export vizualizace na pevny disk
png(filename="RF_MDG_GGPLOT.png", units="px", width=7000, height=5000, res=600)

ggplot(f2, mapping= aes(x = reorder(MDA, Value), y = Value)) + theme_bw() +
  geom_bar(stat="identity", fill = "#00abff") + coord_flip() + xlab(NULL) + theme_grey(base_size = 22)+
  ggtitle("Mean Decrease Gini")

dev.off()

################################################################################################################################

# combina MDA nad MDG results into a single data frame
df <- as.data.frame(cbind(M_MDA, M_MDG))
df

# mean calculation of MDA and MDG data frame created in previous steps
p <- colMeans(df)
p

# standart deviation claculation of MDA a MDG
std <- apply(df, 2, sd)
std

# coefficient of variation calculation for MDA and MDG
kk <- std/p
kk

# export results of coefficient of variation to hard drive in .txt format
zkk <- capture.output(kk)
ekk <- cat(zkk, file="Coefficient_of_Variationt.txt", sep="\n", append=FALSE)

# statistical test of significance for coefficient of variation between MDA and MDG
test <- mslr_test2(nr=nrow(df), n=c(df$M_MDA, df$M_MDG), x=p, s=std)
test

# export results of statistical test for coefficient of variation to hdd
z <- capture.output(test)
exp <- cat(z, file="Statistical_Test.txt", sep="\n", append=FALSE)
#################################################################################################################################

# Reading plotted results of MDA and MDG in working directory

a <- readImage(files="RF_MDA_GGPLOT.png")
b <- readImage(files="RF_MDG_GGPLOT.png")

# Merging of exported images together
c <- abind(a, b, along=1)
display(c)

# save merged images to hard drive
d1 <- writeImage(c, files="MDA_a_MDG_Together.jpeg")

#################################################################################################################################

# corellation between MDA a MDG

# Scale MDA and MDG values between 0 and 1
dat <- as.data.frame(cbind(f1$Value, f2$Value))
colnames(dat) <- c("MDA", "MDG")
f <- function(x){(x-min(x))/(max(x)-min(x))}
dat2 <- f(dat)

# linear relationship between scaled MDA a MDG
g <- plot(dat2, main="Relationship between MDA and MDG (Scaled)", xlab="MDA", ylab="MDG", col="firebrick",
          las = 1, cex = 1.5, bty = "l", pch=16)
abline(lm(MDG~MDA, data=dat2), col="black")

# save results to hdd
png(filename="MDG_and_MDA_linear_regression.png", units="px", width=7000, height=5000, res=600)
plot(dat2, main="Relationship between MDA and MDG (Scaled)", xlab="MDA", ylab="MDG", col="firebrick",
     las = 1, cex = 1.5, bty = "l", pch=16)
abline(lm(MDG~MDA, data=dat2), col="black")

dev.off()

# linear regression between MDA a MDG
lin <- lm(MDG~MDA, data=dat2)
lin

# export results of linear models to hdd (.txt format)
z <- capture.output(lin)
lm <- cat(z, file="Model_Coefficients.txt", sep="\n", append=FALSE)

# Jaccard index calculation
a <- rownames(f1)
b <- rownames(f2)

jacard <- function(a, b){
  intersection = length(intersect(a, b))
  union = length(a) + length(b)
  return = (intersection/union)
}

ji <- jacard(a, b)
ji

# export results of Jaccard index to hdd (.txt format)
jiz <- capture.output(ji)
jie <- cat(jiz, file="Jaccard_Index.txt", sep="\n", append=FALSE)

# set names of satellite band order for MDA 
com <- f1
com

# export ordered (based on the most important values) MDA results to hdd
com_e <- capture.output(com)
com_u <- cat(com_e, file="MDA_Ordered.txt", sep="\n", append=FALSE)

# set names of satellite band order for MDA 
com2 <- f2
com2

# export ordered (based on the most important values) MDG results to hdd
com_e2 <- capture.output(com2)
com_u2 <- cat(com_e2, file="MDG_Ordered.txt", sep="\n", append=FALSE)



