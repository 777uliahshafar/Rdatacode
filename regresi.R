setwd("~/Documents/Rdatacode")
getwd()

data <- read.table("dataset_tes2.csv", sep = ";", header = TRUE, stringsAsFactors = T)

library(colorout)
head(data)
str(data)


df <- data[, c(1:7)]
df
head(df)
str(df)

# Change value for regression
## Label ordered factor to numerical value
df$ElemenJalan <- factor(df$ElemenJalan, levels = c("buruk", "baik", "sangat baik"), labels = c(1, 2, 3), ordered = T)
unclass(df$ElemenJalan)
head(df$ElemenJalan)
table(df$ElemenJalan)

str(df$ElemenJalan)

df$ElemenAlami <- factor(df$ElemenAlami, levels = c("buruk", "baik", "sangat baik"), labels = c(1, 2, 3), ordered = T)
str(df$ElemenAlami)


## Convert Qualitative Ordered Factor to Numerical
df$ElemenJalan <- as.numeric(as.character(df$ElemenJalan))
df$ElemenAlami <- as.numeric(as.character(df$ElemenAlami))

head(df$ElemenAlami)
str(df$ElemenAlami)

# Regresi Linear Sederhana
out <- lm(df$ElemenAlami ~ df$ElemenJalan, data = df)
summary(out)$r.square
summary(out)
anova(out)

fitted(out)
round(residuals(out), 0)

df$predicted_y <- fitted(out)
df$residual <- round(residuals(out), 0)

head(df)

plot(df$ElemenJalan, df$ElemenAlami, col = 4, xlab = "Elemen Jalan", ylab = "Elemen Alami")
abline(lm(df$ElemenAlami ~ df$ElemenJalan))

head(data)

# Regresi Linear Berganda
reg1 <- lm(data$ElemenAlami ~ data$ElemenJalan + data$KualitasJalan + data$ElemenDuduk, data = data)

summary(reg1)
anova(reg1)
## p-value(Pr(>|t|)(uji-t) kurang dari 0.05 artinya berpengaruh signifikan
## p-value dari F-Statistic(uji-f) artinya berpengaruh sigfinikan menyeluruh
## Besar pengaruh di dari R-squared, 0.22 artinya berpengaruh sebesar 22%

# Mencari Koefesien Korelasi

cor.test(data$ElemenJalan, data$ElemenAlami)
cor(data$ElemenJalan, data$ElemenAlami, method = "pearson")
## koefesien korelasi (R) semakin dekat dngn -1 dan 1 artinya sngat berkorelasi, sdngkan mendekati 0 artinya tidk ada korelasi atau hubungan.

plot(data$ElemenJalan, data$ElemenAlami)
plot(data$KualitasJalan, data$ElemenAlami)
plot(data$ElemenDuduk, data$ElemenAlami)
