setwd("~/Documents/Rdatacode")
getwd()
df <- read.table("descriptiveCSV.csv", sep = ";", header = TRUE)

library(colorout)
head(df$Pekerjaan)
head(df$Usia)

class(df$Pekerjaan)
class(df$Usia)

df$Pekerjaan <- as.factor(df$Pekerjaan)

barplot(
  names = df$Pekerjaan,
  height = df$Usia,
  col = "skyblue",
  main = "Hello World",
  xlab = "Pekerjaan",
  ylab = "Usia"
)

library(lattice)
dotplot(
  x = Usia ~ Pekerjaan,
  data = df,
  main = "Hello World",
  xlab = "Pekerjaan",
  ylab = "Usia"
)

library(ggplot2)
ggplot(
  data = df,
  aes(x = Pekerjaan, y = Usia)
) +
  geom_point() +
  ggtitle("Hello World") +
  xlab("Pekerjaan") +
  ylab("Usia")

ggplot(
  data = df,
  aes(x = Pekerjaan, y = Usia)
) +
  geom_bar(
    stat = "identity",
    fill = "skyblue"
  ) +
  ggtitle("Hello World") +
  xlab("Pekerjaan") +
  ylab("Usia")

# Univariate Qualitative Bar Charts
ggplot(
  data = df,
  aes(x = Pekerjaan)
) +
  geom_bar(
    fill = "skyblue"
  ) +
  ggtitle("Pekerjaan")

# Univariate Qualitative Bar Charts Horizontal
ggplot(
  data = df,
  aes(x = Pekerjaan)
) +
  geom_bar(
    fill = "skyblue"
  ) +
  coord_flip() +
  ggtitle("Pekerjaan")

# Create cleveland dot plot
ggplot(
  data = df,
  aes(x = Pekerjaan)
) +
  geom_point(stat = "count") +
  coord_flip() +
  ggtitle("Pekerjaan")

# Piechart univariate categorical
## Simple PieChart
ggplot(
  data = df,
  aes(x = "", fill = Pekerjaan)
) +
  geom_bar() +
  coord_polar(theta = "y") +
  ggtitle("Pekerjaan") +
  ylab("")

## Fair Piechart
library(scales)
library(dplyr)
df
plotdata <- df %>%
  count(Pekerjaan) %>%
  arrange(desc(Pekerjaan)) %>%
  mutate(
    prop = round(n * 100 / sum(n), 1),
    lab.ypos = cumsum(prop) - 0.5 * prop
  )
head(plotdata)

ggplot(
  data = plotdata,
  aes(x = "", y = prop, fill = Pekerjaan)
) +
  ggtitle("Pekerjaan") +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(
    aes(
      y = lab.ypos, label = prop
    ),
    color = "white"
  ) +
  theme_void()


## Adding Percent to Piechart Pekerjaan
plotdata <- df %>%
  count(Pekerjaan) %>%
  arrange(desc(Pekerjaan)) %>%
  mutate(
    prop = round(n * 100 / sum(n), 1),
    lab.ypos = cumsum(prop) - 0.5 * prop
  )
head(plotdata)

plotdata$percent <- paste0(
  plotdata$Pekerjaan, "\n",
  round(plotdata$prop), "%"
)

ggplot(
  data = plotdata,
  aes(x = "", y = prop, fill = Pekerjaan)
) +
  ggtitle("Pekerjaan") +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(
    aes(
      y = lab.ypos, label = percent
    ),
    color = "white"
  ) +
  theme_void()


## Adding Color to Piechart Pekerjaan
### change color manually
# myclrs <- c("#CC6666", "#9999CC", "#66CC99", "#0073C2FF")

library(RColorBrewer)

plotdata <- df %>%
  count(Pekerjaan) %>%
  arrange(desc(Pekerjaan)) %>%
  mutate(
    prop = round(n * 100 / sum(n), 1),
    lab.ypos = cumsum(prop) - 0.5 * prop
  )
head(plotdata)

plotdata$percent <- paste0(
  plotdata$Pekerjaan, "\n",
  round(plotdata$prop), "%"
)

ggplot(
  data = plotdata,
  aes(x = "", y = prop, fill = Pekerjaan)
) +
  ggtitle("Pekerjaan") +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(
    aes(
      y = lab.ypos, label = percent
    ),
    color = "white"
  ) +
  #  scale_fill_manual(values = myclrs) + # add color manually
  scale_fill_brewer(palette = "Set2") +
  theme_void() +
  theme(legend.position = "FALSE") # remove legend

# PieChart Usia
## Changing Numeric Variabel to Categorical(factor)

CatUsia <- cut(df$Usia, breaks = c(5, 11, 25, 45, 65), labels = c("anak-anak", "remaja", "dewasa", "lansia"), right = T)
CatUsia

class(CatUsia)

df$CatUsia <- CatUsia

head(df)
plotusia <- df %>%
  count(CatUsia) %>%
  arrange(desc(CatUsia)) %>%
  mutate(
    prop = round(n * 100 / sum(n), 1),
    lab.ypos = cumsum(prop) - 0.5 * prop
  )
head(plotusia)

plotusia$percent <- paste0(
  plotusia$CatUsia, "\n",
  round(plotusia$prop), "%"
)

ggplot(
  data = plotusia,
  aes(x = "", y = prop, fill = CatUsia)
) +
  ggtitle("Kelompok Usia") +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(
    aes(
      y = lab.ypos, label = percent
    ),
    color = "white"
  ) +
  scale_fill_brewer(palette = "Set2") + # add color
  theme_void()
