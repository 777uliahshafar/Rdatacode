setwd("~/Documents/Magister/project files/proposalTesis/dataset")
getwd()
df <- read.table("descriptiveCSV.csv", sep = ";", header = TRUE)

head(df)
attach(df)
names(df)
Usia[1:10]

## Membuat kategori usia: anak-kanak (5-11 tahun), remaja  (12-25 tahun), dewasa (26-45 tahun), dan lansia  (46-65 tahun)

CatUsia <- cut(Usia, breaks = c(5, 11, 25, 45, 65), labels = c("anak-anak", "remaja", "dewasa", "lansia"), right = T)
CatUsia
