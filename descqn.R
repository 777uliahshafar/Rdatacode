setwd("~/Documents/Rdatacode")
getwd()
df <- read.table("dataset_tes2.csv", sep = ";", header = TRUE)

library(ggplot2)
library(colorout)
head(df)

Jalan <- df$ElemenJalan
head(Jalan)

Duduk <- df$ElemenDuduk
head(Duduk)

Jalan <- factor(Jalan)
unclass(Jalan)
nlevels(Jalan)
table(Jalan)

head(Jalan)

## Explain order of factor
Jalan <- factor(df$ElemenJalan, ordered = T)
Jalan
unclass(Jalan)
table(Jalan)
Jalan[3]
Jalan[3] > Jalan[2]

## Convert data to factor
Jalan <- factor(df$ElemenJalan, levels = c("buruk", "baik", "sangat baik"))
Jalan
unclass(Jalan)
table(Jalan)
plot(Jalan)

Duduk <- factor(df$ElemenDuduk, levels = c("buruk", "baik", "sangat baik"))
Duduk

## Barchart
ggplot(
  data = df,
  aes(x = Jalan)
) +
  geom_bar() +
  ggtitle("Distribusi Kualitas Elemen Jalan")


ggplot(
  data = df,
  aes(x = Duduk)
) +
  geom_bar() +
  ggtitle("Distribusi Kualitas Elemen Duduk")

## Combine the plots on one page
library(gridExtra)
plot1 <- ggplot(
  data = df,
  aes(x = Jalan)
) +
  geom_bar() +
  ggtitle("Distribusi Kualitas Elemen Jalan")


plot2 <- ggplot(
  data = df,
  aes(x = Duduk)
) +
  geom_bar() +
  ggtitle("Distribusi Kualitas Elemen Duduk")

grid.arrange(plot1, plot2, ncol = 2)
