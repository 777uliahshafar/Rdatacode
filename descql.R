setwd("/Volumes/hdd/analisis")
getwd()
df <- read.table("descriptiveCSV.csv", sep = ";", header = TRUE)
require(colorout)
head(df$pekerjaan)
head(df$usia)
head(df$ruang)
head(df$y1)
class(df$Pekerjaan)
class(df$Usia)

# table for article tesis responden freq
library(dplyr)
## breakdown age group
df$usia <- cut(df$usia, breaks = c(15, 25, 40, 80), labels = c("18-25", "26-40", "> 41"), right = T)

g <- df %>%
  group_by(suku) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 2)) %>%
  arrange(desc(freq))

head(as.data.frame(g))


spr <- split(df, df$suku)
str(spr)
df$pekerjaan <- as.factor(df$pekerjaan)
df$ruang <- as.factor(df$ruang)
library(ggplot2)

df$pendidikan
table(df$pendidikan)


# gruped bar with percentage label
ggplot(data = df, aes(
  x = pekerjaan,
  y = prop.table(stat(count)),
  fill = ruang,
  label = scales::percent(prop.table(stat(count)))
)) +
  geom_bar(position = "dodge") +
  geom_text(
    stat = "count",
    position = position_dodge(.9),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "pekerjaan", y = "pct", fill = "ruang")

# groupped bar without pct label
ggplot(data = df, aes(
  x = pekerjaan,
  y = prop.table(stat(count)),
  fill = ruang,
  label = scales::percent(prop.table(stat(count)))
)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = mycols) +
  labs(x = "pekerjaan", y = "pct", fill = "ruang")

# other form of groupped bar
ggplot(df, aes(x = pekerjaan, fill = ruang)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent)

# bar
ggplot(data = df, aes(
  x = ruang,
  y = prop.table(stat(count)),
  fill = ruang,
  label = scales::percent(prop.table(stat(count)))
)) +
  geom_bar(position = "dodge", width = 0.5) +
  scale_fill_manual(values = mycols) +
  geom_text(
    stat = "count",
    position = position_dodge(.9),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ruang", y = "pct") +
  theme(legend.position = "none") +
  theme_minimal()

barplot(
  names = df$pekerjaan,
  height = df$Usia,
  col = "skyblue",
  main = "Hello World",
  xlab = "pekerjaan",
  ylab = "Usia"
)

library(lattice)
dotplot(
  x = Usia ~ pekerjaan,
  data = df,
  main = "Hello World",
  xlab = "pekerjaan",
  ylab = "Usia"
)

library(ggplot2)
ggplot(
  data = df,
  aes(x = pekerjaan, y = Usia)
) +
  geom_point() +
  ggtitle("Hello World") +
  xlab("pekerjaan") +
  ylab("Usia")

ggplot(
  data = df,
  aes(x = pekerjaan, y = Usia)
) +
  geom_bar(
    stat = "identity",
    fill = "skyblue"
  ) +
  ggtitle("Hello World") +
  xlab("pekerjaan") +
  ylab("Usia")

# Univariate Qualitative Bar Charts
ggplot(
  data = df,
  aes(x = pekerjaan)
) +
  geom_bar(
    fill = "skyblue"
  ) +
  ggtitle("pekerjaan")

# Univariate Qualitative Bar Charts Horizontal
ggplot(
  data = df,
  aes(x = pekerjaan)
) +
  geom_bar(
    fill = "skyblue"
  ) +
  coord_flip() +
  ggtitle("pekerjaan")

# Create cleveland dot plot
ggplot(
  data = df,
  aes(x = pekerjaan)
) +
  geom_point(stat = "count") +
  coord_flip() +
  ggtitle("pekerjaan")

# Piechart univariate categorical

## Simple PieChart
ggplot(
  data = df,
  aes(x = "", fill = pekerjaan)
) +
  geom_bar() +
  coord_polar(theta = "y") +
  ggtitle("pekerjaan") +
  ylab("")

## Moderate Piechart
library(scales)
library(dplyr)

plotdata <- df %>%
  count(pekerjaan) %>%
  arrange(desc(pekerjaan)) %>%
  mutate(
    prop = round(n * 100 / sum(n), 1),
    lab.ypos = cumsum(prop) - 0.5 * prop
  )
head(plotdata)

### Change color fill
mycols <- c("#003f5c", "#007274", "#649f68", "#e1be6a")

ggplot(
  data = plotdata,
  aes(x = "", y = prop, fill = pekerjaan)
) +
  ggtitle("pekerjaan") +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(
    aes(
      y = lab.ypos, label = prop
    ),
    color = "white"
  ) +
  scale_fill_manual(values = mycols) +
  theme_void()


## Adding Percent to Piechart pekerjaan
plotdata <- df %>%
  count(pekerjaan) %>%
  arrange(desc(pekerjaan)) %>%
  mutate(
    prop = round(n * 100 / sum(n), 1),
    lab.ypos = cumsum(prop) - 0.5 * prop
  )
head(plotdata)

plotdata$percent <- paste0(
  plotdata$pekerjaan, "\n",
  round(plotdata$prop), "%"
)

ggplot(
  data = plotdata,
  aes(x = "", y = prop, fill = pekerjaan)
) +
  ggtitle("pekerjaan") +
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
  count(pekerjaan) %>%
  arrange(desc(pekerjaan)) %>%
  mutate(
    prop = round(n * 100 / sum(n), 1),
    lab.ypos = cumsum(prop) - 0.5 * prop
  )
head(plotdata)

plotdata$percent <- paste0(
  plotdata$pekerjaan, "\n",
  round(plotdata$prop), "%"
)

ggplot(
  data = plotdata,
  aes(x = "", y = prop, fill = pekerjaan)
) +
  ggtitle("pekerjaan") +
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

catUsia <- cut(df$usia, breaks = c(5, 11, 25, 45, 65), labels = c("anak-anak", "remaja", "dewasa", "lansia"), right = T)
catUsia

class(catUsia)

df$catUsia <- catUsia

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

# PieChart Ruang

df$ruang <- as.factor(df$ruang)

table(df$ruang)


plotdata <- df %>%
  count(ruang) %>%
  arrange(desc(ruang)) %>%
  mutate(
    prop = round(n * 100 / sum(n), 1),
    lab.ypos = cumsum(prop) - 0.5 * prop
  )

plotdata$percent <- paste0(
  plotdata$ruang, "\n",
  round(plotdata$prop), "%"
)

ggplot(
  data = plotdata,
  aes(x = "", y = prop, fill = ruang)
) +
  ggtitle("Ruang") +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(
    aes(
      y = lab.ypos, label = percent
    ),
    color = "white"
  ) +
  scale_fill_manual(values = mycols) +
  theme_void()
