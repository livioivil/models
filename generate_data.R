
############################################################
# generate_data.R
# Reproducible data generator for confounding lab
############################################################

## Age as Continous variable

set.seed(1)
n <- 220

age  <- runif(n, 20, 80)
sex  <- rbinom(n, 1, 0.5)
education_years <- round(rnorm(n, 15, 2))
handedness <- rbinom(n, 1, 0.1)

sleep_hours  <- rnorm(n, 7, 1)
stress_score <- rnorm(n, 50, 10)
motion <- abs(rnorm(n, 0.15, 0.08))

ICV <- rnorm(n, 1500 + 120*sex, 100)

hippocampal_volume <- 7.8 - 0.035*age + 0.002*ICV + rnorm(n, 0, 0.25)

memory_score <- 110 - 0.85*age + 0.4*education_years + rnorm(n, 0, 6)

cont_data <- data.frame(
  memory_score, hippocampal_volume, age, sex, education_years,
  ICV, motion, sleep_hours, stress_score, handedness
)

write.csv(cont_data, "./data/hippocampus_memory_continuous_age.csv", row.names=FALSE)

cat("Datasets written.\n")


## Age as factor

cat_data <- cont_data
age  <- c(runif(n/2, 35, 40),runif(n/2, 50, 55))

cat_data$age <- age
cat_data$hippocampal_volume <- 7.8 - 0.035*age + 0.002*ICV + rnorm(n, 0, 0.25)

cat_data$memory_score <- 110 - 0.85*age + 0.6*education_years + rnorm(n, 0, 6)

cat_data$age_group <- ifelse(cat_data$age < 42, "1Young", "2Older")
cat_data$age <- NULL


write.csv(cat_data, "./data/hippocampus_memory_age_group.csv", row.names=FALSE)

cat("Datasets written.\n")
