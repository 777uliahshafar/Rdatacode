setwd("~/Documents/Magister/project files/proposalTesis/dataset")
getwd()

data <- read.table("dataset_tes1.csv", sep = ";", header = TRUE)

library(colorout)
head(data)
str(data)



# Regresi Linear Sederhana
df <- data[, c(1, 9)]
head(df)
out <- lm(df$AktivitasRelaksasi ~ df$ElemenJalan, data = df)

summary(out)$r.square
summary(out)
anova(out)

fitted(out)
round(residuals(out), 0)

df$predicted_y <- fitted(out)
df$residual <- round(residuals(out), 0)

head(df)

plot(df$ElemenJalan, df$AktivitasRelaksasi, col = 4, xlab = "Elemen Jalan", ylab = "Aktivitas Relaksasi")
abline(lm(df$AktivitasRelaksasi ~ df$ElemenJalan))

head(data)

# Regresi Linear Berganda
reg1 <- lm(data$AktivitasRelaksasi ~ data$ElemenJalan + data$KualitasJalan + data$ElemenDuduk, data = data)

summary(reg1)
anova(reg1)
## p-value(Pr(>|t|)(uji-t) kurang dari 0.05 artinya berpengaruh signifikan
## p-value dari F-Statistic(uji-f) artinya berpengaruh sigfinikan menyeluruh
## Besar pengaruh di dari R-squared, 0.22 artinya berpengaruh sebesar 22%

# Mencari Koefesien Korelasi

cor.test(data$ElemenJalan, data$AktivitasRelaksasi)
cor(data$ElemenJalan, data$AktivitasRelaksasi, method = "pearson")
## koefesien korelasi (R) semakin dekat dngn -1 dan 1 artinya sngat berkorelasi, sdngkan mendekati 0 artinya tidk ada korelasi atau hubungan.

plot(data$ElemenJalan, data$AktivitasRelaksasi)
plot(data$KualitasJalan, data$AktivitasRelaksasi)
plot(data$ElemenDuduk, data$AktivitasRelaksasi)
