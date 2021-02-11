setwd("~/Documents/Magister/project files/proposalTesis/dataset")
getwd()

data <- read.table("dataset_tes1.csv", sep = ";", header = TRUE)

head(data)
str(data)
library(colorout)

head(data$ElemenJalan)
tail(data$ElemenJalan)
str(data$ElemenJalan)

head(data$AktivitasRelaksasi)
str(data$AktivitasRelaksasi)

# Cara 1
## Lanjutkan ke cara berikutnya jika belum yakin
hist(data$ElemenJalan, freq = FALSE, xlim = c(min(data$ElemenJalan), max(data$ElemenJalan)), ylim = c(0, .2), col = 2)

hist(data$ElemenJalan, freq = F, ylim = c(0, 1.4), xlim = c(min(data$ElemenJalan), max(data$ElemenJalan)), las = 1)
curve(dnorm(x, mean = mean(data$ElemenJalan), sd = sd(data$ElemenJalan)), add = T, lwd = 2, col = 4)

qqnorm(data$ElemenJalan)
qqline(data$ElemenJalan, col = 2)

hist(data$AktivitasRelaksasi, freq = F, ylim = c(0, .4), xlim = c(min(data$AktivitasRelaksasi), max(data$AktivitasRelaksasi)), las = 1)
curve(dnorm(x, mean = mean(data$AktivitasRelaksasi), sd = sd(data$AktivitasRelaksasi)), add = T, lwd = 2, col = 4)

qqnorm(data$AktivitasRelaksasi)
qqline(data$AktivitasRelaksasi, col = 2)
## apabila item berada di sekitar garis qqline, aritnya normal

# Cara 2
shapiro.test(data$ElemenJalan)

shapiro.test(data$AktivitasRelaksasi)
## Hasil shapiro p-value 1.026e-06
## Angka 6 adalah 1 atau p-value lebih kecil dari 0.05
## 0.000001026 < 0.05

## Kolmogorov-Smirnov
ks.test(data$ElemenJalan, "pnorm")
ks.test(data$AktivitasRelaksasi, "pnorm")
## Hasil ks test p-value 2.2e-16

# Cara 3
library(psych)
describe(data$ElemenJalan)
describe(data$AktivitasRelaksasi)
## semakin angka mean dan median sama, artinya normal

# Cara 4 (Recommended)
library(moments)
skewness(data$ElemenJalan)
kurtosis(data$ElemenJalan)

skewness(data$AktivitasRelaksasi)
kurtosis(data$AktivitasRelaksasi)

## skewness -1.012961
## if skewness is between -0.5 and 0.5, data are fairly symmetrical
## -1 and -0.5 or 0.5 and 1, data are moderately skewed
## kurang -1 atau lebih 1, data are highly skewed
## secara umum, skew antara -2 and 2 adalah normal
