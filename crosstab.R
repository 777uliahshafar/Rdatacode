setwd("/Volumes/hdd/analisis")
getwd()
df <- read.table("descriptiveCSV.csv", sep = ";", header = TRUE)
head(df)
require(colorout)


str(df)
df$gender
df$ruang

ct <- table(df$gender, df$ruang)
chisq.test(ct)

# crosstab sederhana
crosstabs <- xtabs(~ ruang + gender, data = df)

## get total proportion
prop.table(crosstabs, margin = 1)
## get proportion accros row
prop.table(crosstabs, margin = 1)

## get proportion accros columns
prop.table(crosstabs, margin = 2)

## cross tabulation countr to percentages by column
100 * prop.table(crosstabs, 2)

## round to 2 places after the decimal
ctps_gen <- round(100 * prop.table(crosstabs, 2), 2)

## Chi-squared test: tell us the difference between two variable
chisq.test(crosstabs)

# Crosstab gmodel
# install.packages("gmodels")
library(gmodels)
library(Hmisc)

# Crosstab descr
library(descr)
str(df$ruang)
str(df$pekerjaan)
str(df$gender)
crosstab(df$ruang, df$gender, xlab = "Ruang", ylab = "Gender")

ctpsG <- crosstab(df$ruang, df$gender, dnn = c("Ruang", "Gender"), user.missing.dep = "Don't know", expected = FALSE, prop.c = TRUE, prop.r = FALSE, total.r = FALSE, plot = FALSE)

plot(ctpsG, inv.y = TRUE)

tab <- ctpsG$tab
class(tab)
tab

complete_tab <- descr:::CreateNewTab(ctps_gen)
class(complete_tab)
complete_tab

## Print pretty table (requiredments)
# Add to the preamble of your Rnoweb document:
# \usepackage{booktabs}
# \usepackage{multirow}
# \usepackage{dcolumn}
# \newcolumntype{d}{D{.}{.}{-1}}

#### Sample from https://rdrr.io/cran/descr/man/crosstab.html
educ <- sample(c(1, 2), 200, replace = TRUE, prob = c(0.3, 0.7))
educ <- factor(educ, levels = c(1, 2), labels = c("Low", "High"))
opinion <- sample(c(1, 2, 9), 200,
  replace = TRUE,
  prob = c(0.4, 0.55, 0.05)
)
opinion <- factor(opinion,
  levels = c(1, 2, 9),
  labels = c("Disagree", "Agree", "Don't know")
)
attr(educ, "label") <- "Education level"
attr(opinion, "label") <- "Opinion"
weight <- sample(c(10, 15, 19), 200, replace = TRUE)

crosstab(opinion, educ, xlab = "Education", ylab = "Opinion")
ct <- crosstab(opinion, educ, weight,
  dnn = c("Opinion", "Education"),
  user.missing.dep = "Don't know",
  expected = TRUE, prop.c = TRUE, prop.r = TRUE,
  plot = FALSE
)
