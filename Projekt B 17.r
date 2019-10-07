
full_data <- read.table("data2.txt", header = TRUE)
full_data

control_data <- subset(full_data, full_data$Behandl == "Kontroll")
control_data

pairs(control_data, pch c= 10)



HSP90 <- control_data$HSP90
aktin <- control_data$aktin
behallare <- control_data$Behållare
lokal <- control_data$Lokal
omr <- control_data$Omr

y <- HSP90/aktin

plot(omr, y)

model1 <- aov(lm(formula = y ~ lokal))

summary(model1)



model2 <- aov(lm(y~behallare))

summary(model2)

model3 <- aov(lm(y~omr))

summary(model3)

library(PerformanceAnalytics)

chart.Correlation(control_data, histogram = TRUE, pch = 19)
