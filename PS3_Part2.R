# Load packages once
library(dplyr)
library(ggplot2)

# 1. Read the data and create age
therm <- read.csv("thermometers.csv")
therm$age <- 2017 - therm$birth_year

# 2. Describe ft_muslim by party_id
summary(therm$ft_muslim)
sd(therm$ft_muslim, na.rm = TRUE)

therm %>%
  group_by(party_id) %>%
  summarize(
    mean   = mean(ft_muslim, na.rm = TRUE),
    median = median(ft_muslim, na.rm = TRUE),
    sd     = sd(ft_muslim, na.rm = TRUE),
    n      = n()
  )

ggplot(therm, aes(ft_muslim)) +
  geom_histogram(na.rm = TRUE) +
  labs(x = "Feeling thermometer toward Muslims",
       y = "Number of respondents")

ggplot(therm, aes(ft_muslim)) +
  geom_histogram(na.rm = TRUE) +
  facet_wrap(~ party_id) +
  labs(x = "Feeling thermometer toward Muslims",
       y = "Number of respondents")

ggplot(therm, aes(ft_muslim, color = party_id)) +
  geom_density(na.rm = TRUE) +
  labs(x = "Feeling thermometer toward Muslims",
       y = "Density")

# 3. Regression: conditional mean of ft_muslim by party_id
therm$party_id <- factor(therm$party_id)
model_ft <- lm(ft_muslim ~ party_id, data = therm)
summary(model_ft)

# 4. Keep only Democrats and Republicans, create binary party
therm_dr <- subset(therm, party_id %in% c("Democrat", "Republican"))
therm_dr$party_bin <- ifelse(therm_dr$party_id == "Republican", 1, 0)
table(therm_dr$party_id, therm_dr$party_bin)

therm_dr$sex <- factor(therm_dr$sex)

# 5. Linear probability model
model_party <- lm(party_bin ~ ft_muslim * sex + age, data = therm_dr)
summary(model_party)

# 7. Predicted values as ft_muslim changes
mean_age <- mean(therm_dr$age, na.rm = TRUE)

newdata <- expand.grid(
  ft_muslim = seq(
    from = min(therm_dr$ft_muslim, na.rm = TRUE),
    to   = max(therm_dr$ft_muslim, na.rm = TRUE),
    length.out = 100
  ),
  sex = c("Female", "Male"),
  age = mean_age
)

newdata$pred <- predict(model_party, newdata = newdata)

ggplot(newdata, aes(ft_muslim, pred, color = sex)) +
  geom_line() +
  labs(
    x = "Feeling thermometer toward Muslims",
    y = "Predicted probability of being Republican",
    color = "Sex"
  )
