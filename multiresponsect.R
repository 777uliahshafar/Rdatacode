setwd("/Volumes/hdd/analisis")
getwd()
df <- read.table("descriptiveCSV.csv", sep = ";", header = TRUE)
require(colorout)
head(df)


# library(janitor)
# library(dplyr) mutate function
# First load some data:
# myData<-read.csv("F:/Statistics by Peter/Rstudio files/Cochran's Q v2.csv", sep=";", na.strings=c("", "NA"))

# THE MULTIPLE RESPONSE TABLE
# Create multiple response preffered aspect table:
mrpa <- data.frame(
  Frek = colSums(df[6:9]),
  Pct.Resp = (colSums(df[6:9]) / sum(df[6:9])) * 100,
  Pct.Kasus = (colSums(df[6:9]) / nrow(df[6:9])) * 100
)

mrpa <- cbind(Aspek = rownames(mrpa), mrpa)
rownames(mrpa) <- NULL

mrpa <- mrpa %>% mutate(across(where(is.numeric), round, 2))

total <- c(size = "total", apply(mrpa[, -1], FUN = sum, MAR = 2))
mrpa_pro <- rbind(mrpa, total)


# THE MULTIPLE RESPONSE CROSSTAB TABLE EACH CATEGORY

attach(df)

# Extract cols from file
x <- data.frame(aksesibilitas, keamanan, estetika, fasilitas)
nrow(x)

# Get the rows having at least 1 response
xx <- subset(x, aksesibilitas >= 0 | keamanan >= 0 | estetika >= 0 | fasilitas >= 0)

c <- colSums(x)
s <- sum(x)
n <- nrow(xx)
n

gender <- subset(df$gender, aksesibilitas >= 0 | keamanan >= 0 | estetika >= 0 | fasilitas >= 0)
table(gender, xx$aksesibilitas)
require(summarytools)
stak <- summarytools::ctable(gender, xx$aksesibilitas, dnn = c("Gender", "Aksesibilitas"), round.digits = 0, prop = "t")

# THE BAR CHART
# bar chart of multiple response set
# The bars only (note the double [[]])
barplot(mrpa[[3]])

# add in the labels for each bar:
barplot(mrpa[[3]],
  names.arg = row.names(mrpa)
)

# add in the a main title, and axis titles:

barplot(mrpa[[3]],
  names.arg = row.names(mrpa),
  main = "Visited Cinemas",
  xlab = "Cinema",
  ylab = "Percent of cases"
)
