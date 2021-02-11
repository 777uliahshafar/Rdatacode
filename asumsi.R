library(lmtest)
library(car)
library(colorout)

setwd("~/Documents/Magister/project files/proposalTesis/dataset")
getwd()

data <- read.table("dataset_tes1.csv", sep = ";", header = TRUE)

reg1 <- lm(data$AktivitasRelaksasi ~ data$ElemenJalan + data$KualitasJalan + data$ElemenDuduk, data = data)

# Uji Asumsi Klasik
# Uji Normalitas Residual
shapiro.test(reg1$residuals)
ks.test(reg1$residuals, ecdf(reg1$residuals))

## nilai p-value > a(0.05) artinya normal


# Uji Autokorelasi
dwtest(reg1)
## nilai p-value > a(0.05) artinya tidak ada autokorelasi.

# Uji Homogenitas
bptest(reg1, studentize = F, data = data)
bptest(reg1)

## nilai p-value lebih dari alpha artinya sudah homogen.

# Multikolinearitas
vif(reg1)

library(olsrr)
ols_vif_tol(reg1)

## Nilai VIF dibawah 10 artinya tidak ada masalah multikolineritas
## Nilai tolerance diatas 0.1 artinya tidak ada masalah multi...
