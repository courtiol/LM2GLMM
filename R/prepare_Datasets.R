my_data <- read.table("./sources/hse98ai.tab", header = T, sep = "\t")
dataG <- subset(my_data, sex == "2")
period <- dataG$period
age <- dataG$age
period[period < 0] <- NA
period <- period == "1"
tt <- table(period, age)
plot(tt[1,] / (tt[1,] + tt[2,]))
drunk <- dataG$dxdrunkm == 1
tt <- table(drunk, age)
plot(tt[1,] / (tt[1,] + tt[2,]))
drink_pb <- dataG$cagetot
drink_pb[drink_pb < 0] <- NA
cigst1 <- dataG$cigst1
cigst1[cigst1 < 0] <- NA
cigst1 <- cigst1 == "1"
ever_smoked <- !cigst1
bmi <- dataG$bmival
bmi[bmi < 12] <- NA
education <- as.character(dataG$educend)
education[education < 1] <- NA
education[education == 1] <- "not finished"
education[education == 2] <- "14 or less"
education[education == 3] <- "15"
education[education == 4] <- "16"
education[education == 5] <- "17"
education[education == 6] <- "18"
education[education == 7] <- "19 or more"
education <- factor(education)

HSE98women <- data.frame(
  period = period,
  age = age,
  smoked = ever_smoked,
  drunkLast3M = drunk,
  drinkPb = drink_pb,
  education = education,
  bmi = bmi
)

save(HSE98women, file = "./data/HSE98women.rda", compress = "xz")
