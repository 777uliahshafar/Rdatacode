setwd("/Volumes/hdd/analisis")
getwd()
df <- read.table("descriptiveCSV.csv", sep = ";", header = TRUE)

install.packages("xlsx")

require(colorout)
library(conjoint)
install.packages("readxl")
library(xlsx)

# Install main package with dependencies
# install.packages("conjoint", dependencies = TRUE)
# less than 5 attribute
experiment0 <- expand.grid(
  jumlahphn = c("sedikit", "beberapa", "banyak"),
  bentukphn = c("cukup", "sangat"),
  permukaanjln = c("paving", "aspal", "tanah"),
  jeniskrs = c("kursipiknik", "gazebo", "kursilengkung"),
  cahayajln = c("kurang", "sedang", "tinggi")
)

ortho <- caFactorialDesign(data = experiment0, type = "orthogonal")
print(ortho)

code <- caEncodedDesign(ortho)
print(code)

write.xlsx(ortho, file = "orto.xlsv")
write.xlsx(code, file = "encoded.xlsv")

# more than 5 attribute

experiment <- expand.grid(
  jumlahphn = c("sedikit", "beberapa", "banyak"),
  bentukphn = c("cukup", "sangat"),
  warnabunga = c("1-2warna", "3-4warna", ">5warna"),
  lebarjln = c("<1.5m", "2-3m", ">3m"),
  permukaanjln = c("paving", "aspal", "tanah"),
  jeniskrs = c("kursipiknik", "gazebo", "kursilengkung"),
  cahayajln = c("kurang", "sedang", "tinggi")
)

library(AlgDesign)
set.seed(69)
optimal_design <- optFederov(~., data = experiment, nTrials = 32)
print(optimal_design)
str(optimal_design)

# write.xlsx(optimal_design, file = "optimal.xlsx")

data <- read.xlsx("optimal.xlsx", 1, header = TRUE)
head(data)
code <- caEncodedDesign(data)
print(code)

# write.xlsx(code, file = "encoded.xlsx")

# Create sample survey data
sd <- data.frame(replicate(32, sample(1:100, 100, rep = TRUE)))
head(sd)

write.xlsx(sd, file = "survey.xlsx")

## Continue
mysurvey <- read.xlsx("conjoint data survey.xlsx", 1)

myconj <- read.xlsx("conjoint data survey.xlsx", 2)

lev <- c("banyak", "beberapa", "sedikit", "cukup", "sangat", ">5warna", "3-4warna", "1-2warna", "<1.5m", ">3m", "2-3m", "aspal", "paving", "tanah", "gazebo", "kursilengkung", "kursipiknik", "kurang", "sedang", "tinggi")


lev_df <- data.frame(lev)

head(lev_df)

caModel(y = mysurvey[2, 2:33], x = myconj[, 9:15])
caUtilities(y = mysurvey[, 2:33], x = myconj[, 9:15], z = lev_df)

Conjoint(y = mysurvey[, 2:33], x = myconj[, 9:15], z = lev_df)


data(tea)
head(tprefm)

head(tprof)
head(tlevn)


# Contoh
write.xlsx(tprefm, file = "tprefm.xlsx")
write.xlsx(tprof, file = "tprof.xlsx")
write.xlsx(tlevn, file = "tlevn.xlsx")
caUtilities(tprefm, tprof, tlevn)
