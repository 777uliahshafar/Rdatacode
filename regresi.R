setwd("~/Documents/Rdatacode")
getwd()

df <- read.table("dataset_tes2.csv", sep = ";", header = TRUE, stringsAsFactors = F)

library(data.table)

data <- data.table(df)
class(data)

library(colorout)
head(data)
str(data)

x <- data[, c(1:8)]
head(x)
str(x)

y <- data[, c(9:14)]
head(y)
str(y)

# Change value for regression
## Recode multiple categorical variable
## data type must not a factor
library(dplyr)
x <- x %>%
  mutate_at(c("ElemenJalan", "KualitasJalan", "ElemenDuduk", "KualitasDuduk", "ElemenAlami", "KualitasAlami", "FasilitasAminities", "Estetika"), list(~ recode(., "buruk" = 1, "baik" = 2, "sangat baik" = 3)))

y <- y %>%
  mutate_at(
    c("AktivitasRelaksasi", "AktivitasFisik", "AktivitasTravel", "InteraksiAlam", "InteraksiSosial", "PartisiGrup"),
    list(~ recode(., "tidak pernah" = 1, "tidak bulan lalu" = 2, "sekali sebulan" = 3, "dua kali sebulan" = 4, "sekali seminggu" = 5, "dua kali seminggu" = 6, "setiap hari" = 7))
  )

## //Or
## Convert to ordered factor with labels
df$ElemenJalan <- factor(df$ElemenJalan, levels = c("buruk", "baik", "sangat baik"), labels = c(1, 2, 3), ordered = T)
unclass(df$ElemenJalan)
head(df$ElemenJalan)
table(df$ElemenJalan)

str(df$ElemenJalan)

df$ElemenAlami <- factor(df$ElemenAlami, levels = c("buruk", "baik", "sangat baik"), labels = c(1, 2, 3), ordered = T)
str(df$ElemenAlami)

### Convert Qualitative Ordered Factor to Numerical
df$ElemenJalan <- as.numeric(as.character(df$ElemenJalan))
df$ElemenAlami <- as.numeric(as.character(df$ElemenAlami))

head(df$ElemenAlami)
str(df$ElemenAlami)

# Add total column
## Sum up across columns
x <- x %>%
  replace(is.na(.), 0) %>%
  mutate(total_fit = rowSums(across(where(is.numeric))))
head(y)

y <- y %>%
  replace(is.na(.), 0) %>%
  mutate(total_akt = rowSums(across(where(is.numeric))))

data2 <- cbind(x, y)
head(data2)

## Sum up sequence of column
x %>% mutate(total_fit = rowSums(.[2:4]))

#

# Regresi Linear Sederhana
out <- lm(data2$total_akt ~ data2$total_fit, data = data2)
summary(out)$r.square
summary(out)
anova(out)

fitted(out)
round(residuals(out), 0)

df$predicted_y <- fitted(out)
df$residual <- round(residuals(out), 0)

head(data2)

plot(data2$total_fit, data2$total_akt, col = 4, xlab = "Fitur Binaan", ylab = "Aktivitas")
abline(lm(data2$total_akt ~ data2$total_fit))

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
