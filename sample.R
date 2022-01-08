sample1 <- sample(c("laki-laki", "perempuan"), 80, replace = TRUE, prob = c(.62, .38))

data.frame(sample1)
head(sample1)

sample2 <- sample(c("bugis", "toraja", "makasssar", "lainnya"), 80, replace = TRUE, prob = c(.71, .09, .08, .12))

data.frame(sample2)
str(sample2)

sample3 <- sample(c("karyawan", "wiraswasta", "pelajar", "belum bekerja", "lainnya"), 80, replace = TRUE, prob = c(.39, .18, .22, .07, .14))

sample4 <- sample(c("ruang a", "ruang b"), 80, replace = TRUE, prob = c(.59, .41))


samplea <- sample(c("estetika", "fasilitas", "aksessibilitas", "keamanan"), 47, replace = TRUE, prob = c(.41, .25, .12, .22))


sampleb <- sample(c("estetika", "fasilitas", "aksessibilitas", "keamanan"), 47, replace = TRUE, prob = c(.32, .43, .06, .19))

data.frame(samplea)

data.frame(sampleb)


usia_pelajar <- data.frame(floor(runif(17, min = 18, max = 22)))

usia_kw <- data.frame(floor(runif(40, min = 25, max = 49)))

usia_la <- data.frame(floor(runif(9, min = 37, max = 50)))
