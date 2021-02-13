setwd("~/Documents/Rdatacode")


data <- read.table("kuesioner-awal.csv", sep = ",", header = TRUE)

head(data)
library(colorout)
library(data.table)
library(dplyr)

mydt <- data.table(data)

class(mydt)
head(mydt)

class(mydt$nomorHp)

## Remove columns
mydt[, c("ip", "user_agent", "referrer", "created_at") := NULL]

mydt <- mutate(mydt, id = nomorHp * 2)


## Create csv file
fwrite(mydt, "kuesioner2.csv")
