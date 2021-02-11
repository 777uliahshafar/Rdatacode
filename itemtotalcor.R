setwd("~/Documents/Magister/project files/proposalTesis/dataset")

getwd()

data <- read.table("dataset_tes1.csv", sep = ";", header = TRUE)

head(data)
library(colorout)
sum(is.na(data))
data[!complete.cases(data), ]

FiturBin <- data[, c(1:8)]
Aktiv <- data[, c(9:14)]

library(psych)

head(FiturBin)
str(FiturBin)

head(Aktiv)
str(Aktiv)

# Validitas dan Relibilitas Model 1
# Round 1
out1.1 <- alpha(FiturBin)
out1.1

fut2.1 <- alpha(Aktiv)
out2.1

# Round 2
## Eliminasi kolom 10(akt. fisik)
Aktiv2 <- data[, c(9, 11:14)]

out2.2 <- alpha(Aktiv2)
out2.2

# Round 3
Aktiv3 <- data[, c(11:14)]

out2.3 <- alpha(Aktiv3)
out2.3

# Round 4

Aktiv4 <- data[, c(12, 13, 14)]

out2.4 <- alpha(Aktiv4)
out2.4

# Validitas Model 2
rowSums(FiturBin)

## Tambah Kolom Total
FiturBin2 <- cbind(FiturBin, Total = rowSums(FiturBin))

head(FiturBin2)

VElemenJalan <- cor.test(FiturBin2$ElemenJalan, FiturBin2$Total)
VElemenJalan

VKualitasJalan <- cor.test(FiturBin2$KualitasJalan, FiturBin2$Total)
VKualitasJalan

## r(cor) > 0.3 atau alpha(p-value) < 0.05 artinya valid

# Relibilitas Model 2

head(FiturBin)
RelFit <- data.matrix(FiturBin)
install.packages("umx")
library(umx)
reliability(cov(RelFit))
