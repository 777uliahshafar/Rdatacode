head(airmiles)
airmiles

head(mtcars)
sum(mtcars$wt)

# Working with a data.frame

attach(mtcars)
sum(mpg)
detach(mtcars)
sum(mtcars$mpg)

# Indexing Operator [row, column]
mtcars[2, 6]
mtcars[c(2, 5, 8), 6]

# Alternative to a Data.Frame

library(data.table)
library(dplyr)

class(iris)

irisdt <- data.table(iris)
class(irisdt)
head(irisdt)

iristibble <- as_tibble(iris)
class(iristibble)
head(iristibble)
attach(iristibble)

# Converting Data Types

is.numeric(Sepal.Width)
Sepal.WidthInt <- as.integer(round(Sepal.Width), 0)
is.integer(Sepal.WidthInt)
head(Sepal.WidthInt)

summary(iris)
is.factor(Species)

myspecies <- as.character(Species)
is.character(myspecies)
myspecies <- as.factor(myspecies)
levels(myspecies)

# Conditional Queries(Logical Test)
subset(mtcars, cyl < 6)
subset(mtcars, cyl == 6)
subset(mtcars, !cyl == 6)
subset(mtcars, cyl == 6 & gear > 3)
subset(mtcars, cyl == 6 | gear == 3)
subset(mtcars, cyl == 6 | gear == 3, select = c("mpg", "cyl", "gear"))


triple <- subset(mtcars, cyl == 6, select = c("mpg", "cyl", "gear"))
triple
mean(triple$mpg)

# Data Table
mydt <- data.table(mtcars)
class(mydt)
head(mydt)

mydt[2, ]
mydt[, 2]

mydt[cyl == 6, ]
mydt[cyl == 6 & gear > 3, ]

## Subsetting (i) and aggregating (j)
mydt[cyl == 6, sum(mpg)]

## Query: j, by
mydt[, by = cyl, .N]

## Query: i, j
mydt[cyl == 6, .N]

## Query: i, by, j
mydt[cyl == 6, by = gear, .N]

## Query by interval
mydt[between(cyl, 4, 7)]
mydt[cyl %between% c(4, 7)]
mydt[cyl %in% c(4, 7)]


## Sort
mydt[order(-mpg)]

# Filtering with Stringr
mydata <- cbind(mtcars, names = rownames(mtcars))
summary(mydata)
mydata$names <- as.character(mydata$names)
mydata$names

library(stringr)

str_detect(mydata$names, pattern = "Mazda")
mydata$Mazda <- str_detect(mydata$names, pattern = "Mazda")

mydata


# Tibble
library(dplyr)
mytbl <- as_tibble(mtcars)

select(mytbl, cyl)
select(mtcars, cyl)

## Subsetting - observation
filter(mytbl, cyl == 6 & gear == 5)
filter(mytbl, cyl < 6)
slice(mytbl, c(1, 3, 6))

## Reorder
arrange(mytbl, cyl)
arrange(mytbl, -cyl)
arrange(mytbl, desc(cyl))

## Subsetting - variables
select(mytbl, cyl, gear)
select(mytbl, -cyl)

## Rename headers
rename(mytbl, Cylinders = cyl)

## Find Unique Value
distinct(select(mytbl, cyl))

## Add new variable
mutate(mytbl, consAdj = mpg / hp)

## Collapse table
summarise(mytbl, mean = mean(mpg))
