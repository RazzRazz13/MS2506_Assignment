# Load necessary libraries
library(rstanarm)
library(ggplot2)

data <- read.csv("Affairs.csv")

str(data)
head(data)

data$had_affair <- ifelse(data$affairs > 0, 1, 0)
data$had_affair <- as.factor(data$had_affair)

table(data$had_affair)

data <- data[, c(
  "gender",
  "age",
  "yearsmarried",
  "children",
  "religiousness",
  "education",
  "occupation",
  "rating",
  "had_affair"
)]

data$gender <- as.numeric(factor(data$gender))
data$children <- ifelse(data$children == "yes", 1, 0)

data$gender <- factor(data$gender)
data$age <- scale(data$age)
data$yearsmarried <- scale(data$yearsmarried)
data$children <- factor(data$children)
data$religiousness <- factor(data$religiousness)
data$education <- factor(data$education)
data$occupation <- factor(data$occupation)
data$rating <- factor(data$rating)

fit <- stan_glm(
  had_affair ~ gender + age + yearsmarried + religiousness + children +
    religiousness +education + occupation + rating+ had_affair,
  data = data,
  family = binomial(link = "logit"),
  prior = normal(0, 2.5),       # prior for coefficients
  prior_intercept = normal(0, 5), # prior for intercept
)

# Posterior predictive check
pp_check(fit, plotfun = "hist", bins = 2) +
  ggtitle("Posterior Predictive Check: Had Affair (0 = No, 1 = Yes)")
