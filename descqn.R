setwd("~/Documents/Rdatacode")
getwd()
df <- read.table("dataset_tes2.csv", sep = ";", header = TRUE, stringsAsFactors = T)

library(ggplot2)
library(colorout)
head(df)

Jalan <- df$ElemenJalan
head(Jalan)

Duduk <- df$ElemenDuduk
head(Duduk)
str(df)


## Convert to factor
Jalan <- factor(Jalan)
unclass(Jalan)
nlevels(Jalan)
table(Jalan)

head(Jalan)

## Order Factor
Jalan <- factor(df$ElemenJalan, ordered = T)
Jalan
unclass(Jalan)
table(Jalan)
Jalan[3]
Jalan[3] > Jalan[2]

## Convert to  categorical
Jalan <- factor(df$ElemenJalan, levels = c("buruk", "baik", "sangat baik"), ordered = T)
Jalan
unclass(Jalan)
table(Jalan)
plot(Jalan)

Duduk <- factor(df$ElemenDuduk, levels = c("buruk", "baik", "sangat baik"))
Duduk

## Simple Barchart Factor Scale
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


## Add Color to  Barchart Factor Scale
library(RColorBrewer)
ggplot(
  data = df,
  aes(x = Jalan)
) +
  geom_bar(
    aes(fill = Jalan)
  ) +
  #  scale_fill_manual(values = c("buruk" = "#353436",
  #                               "baik" = "#1b98e0",
  #                               "sangat baik" = "red"))+
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Distribusi Kualitas Elemen Jalan")


ggplot(
  data = df,
  aes(x = Duduk)
) +
  geom_bar(
    aes(fill = Duduk)
  ) +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Distribusi Kualitas Elemen Duduk")


## Combine the plots to Barchart Factor Scale
library(gridExtra)
plot1 <- ggplot(
  data = df,
  aes(x = Jalan)
) +
  geom_bar(
    aes(fill = Jalan)
  ) +
  #  scale_fill_manual(values = c("buruk" = "#353436",
  #                               "baik" = "#1b98e0",
  #                               "sangat baik" = "red"))+
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Distribusi Kualitas Elemen Jalan") +
  theme(legend.position = "FALSE") # remove legend

plot2 <- ggplot(
  data = df,
  aes(x = Duduk)
) +
  geom_bar(
    aes(fill = Duduk)
  ) +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Distribusi Kualitas Elemen Duduk") +
  theme(legend.position = "FALSE") # remove legend



grid.arrange(plot1, plot2, ncol = 2)
