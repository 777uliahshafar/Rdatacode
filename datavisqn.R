setwd("~/Documents/Magister/project files/proposalTesis/dataset")
getwd()
df <- read.table("dataset_tes2.csv", sep = ";", header = TRUE)

library(ggplot2)
library(colorout)
head(df)

Jalan <- df$ElemenJalan
head(Jalan)

Jalan <- factor(Jalan)
unclass(Jalan)
nlevels(Jalan)
table(Jalan)

head(Jalan)

## Mengetahui urutan faktor
Jalan <- factor(df$ElemenJalan, ordered = T)
Jalan
unclass(Jalan)
table(Jalan)
Jalan[3]
Jalan[3] > Jalan[2]

Jalan <- factor(df$ElemenJalan, levels = c("buruk", "baik", "sangat baik"))
Jalan
unclass(Jalan)
table(Jalan)
plot(Jalan)

ggplot(
  data = df,
  aes(x = Jalan)
) +
  geom_histogram(binwidth = .5) +
  ggtitle("Distribusi Kualitas Elemen Jalan") +
  xlab("Kualitas Elemen Jalan")
