setwd("/Volumes/hdd/analisis")
getwd()
df <- read.table("descriptiveCSV.csv", sep = ";", header = TRUE)

require(colorout)
library(conjoint)
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
optimal_design <- optFederov(~., data = experiment, nTrials = 22)
print(optimal_design)
str(optimal_design)

# write.xlsx(optimal_design, file = "optimal.xlsx")

data <- read.xlsx("optimal.xlsx", 1, header = TRUE)
head(data)
code <- caEncodedDesign(data)
print(cor(code))
head(code)

# write.xlsx(code, file = "encoded.xlsx")

# Create sample survey data
sd <- data.frame(replicate(32, sample(1:10, 100, rep = TRUE)))
head(sd)

write.xlsx(sd, file = "survey.xlsx")

## Continue
mysurvey <- read.xlsx("conjoint data survey.xlsx", 1)

myconj <- read.xlsx("conjoint data survey.xlsx", 2)

lev <- c("sedikit", "beberapa", "banyak", "cukup", "sangat", "1-2warna", "3-4warna", ">5warna", "<1.5m", "2-3m", ">3m", "paving", "aspal", "tanah", "kursipiknik", "gazebo", "kursilengkung", "kurang", "sedang", "tinggi")


levdf <- data.frame(lev)

head(levdf)

caModel(y = mysurvey[1, 2:33], x = myconj[, 9:15])
caUtilities(y = mysurvey[, 2:33], x = myconj[, 9:15], z = levdf)

Conjoint(y = mysurvey[, 2:33], x = myconj[, 9:15], z = levdf)


data(tea)
head(tprefm)

head(tprof)
head(tlevn)
