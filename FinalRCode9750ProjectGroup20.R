#install.packages('regclass')
data.mental <- read.csv("teenmentalhealth.csv")
data.mental$male <- ifelse(data.mental$gender == "male", 1, 0)
data.mental$no_of_social_media <- ifelse(data.mental$platform_usage == "Both", 2, 1)
print(head(data.mental, 20))
library(regclass)

#Model to see what affects addiction level
#y <- data.mental$addiction_level


# Daily Social Media Hours vs Addiction Level
plot(
  addiction_level ~ daily_social_media_hours,
  data = data.mental,
  xlab = "Daily Social Media Hours",
  ylab = "Addiction Level",
  main = "Addiction Level vs Daily Social Media Hours"
)

associate(
  addiction_level ~ daily_social_media_hours,
  data = data.mental,
  permutations = 2000,
  seed = 123
)


# Screen Time Before Sleep vs Addiction Level
plot(
  addiction_level ~ screen_time_before_sleep,
  data = data.mental,
  xlab = "Screen Time Before Sleep",
  ylab = "Addiction Level",
  main = "Addiction Level vs Screen Time Before Sleep"
)

associate(
  addiction_level ~ screen_time_before_sleep,
  data = data.mental,
  permutations = 2000,
  seed = 123
)


# Sleep Hours vs Addiction Level
plot(
  addiction_level ~ sleep_hours,
  data = data.mental,
  xlab = "Sleep Hours",
  ylab = "Addiction Level",
  main = "Addiction Level vs Sleep Hours"
)

associate(
  addiction_level ~ sleep_hours,
  data = data.mental,
  permutations = 7000,
  seed = 123
)


# Anxiety Level vs Addiction Level
plot(
  addiction_level ~ anxiety_level,
  data = data.mental,
  xlab = "Anxiety Level",
  ylab = "Addiction Level",
  main = "Addiction Level vs Anxiety Level"
)

associate(
  addiction_level ~ anxiety_level,
  data = data.mental,
  permutations = 2000,
  seed = 123
)


# Stress Level vs Addiction Level
plot(
  addiction_level ~ stress_level,
  data = data.mental,
  xlab = "Stress Level",
  ylab = "Addiction Level",
  main = "Addiction Level vs Stress Level"
)

associate(
  addiction_level ~ stress_level,
  data = data.mental,
  permutations = 2000,
  seed = 123
)

# Academic Performance vs Addiction Level
plot(
  addiction_level ~ academic_performance,
  data = data.mental,
  xlab = "Academic Performance",
  ylab = "Addiction Level",
  main = "Addiction Level vs Academic Performance"
)

associate(
  addiction_level ~ academic_performance,
  data = data.mental,
  permutations = 2000,
  seed = 123
)

# Physical Activity vs Addiction Level
plot(
  addiction_level ~ physical_activity,
  data = data.mental,
  xlab = "Physical Activity",
  ylab = "Addiction Level",
  main = "Addiction Level vs Physical Activity"
)

associate(
  addiction_level ~ physical_activity,
  data = data.mental,
  permutations = 2000,
  seed = 123
)

# No. of social media vs Addiction Level
plot(
  addiction_level ~ no_of_social_media,
  data = data.mental,
  xlab = "No. of social media",
  ylab = "Addiction Level",
  main = "Addiction Level vs No. of social media"
)

associate(
  addiction_level ~ no_of_social_media,
  data = data.mental,
  permutations = 2000,
  seed = 123
)

# Depression Label vs Addiction Level
plot(
  addiction_level ~ depression_label,
  data = data.mental,
  xlab = "Depression Label",
  ylab = "Addiction Level",
  main = "Addiction Level vs Depression Label"
)

associate(
  addiction_level ~ depression_label,
  data = data.mental,
  permutations = 2000,
  seed = 123
)

#Qualitative & Quantitative
associate(
  addiction_level ~ gender,
  data = data.mental,
  permutations = 2000,
  seed = 123
)

#Qualitative & Qualitative
associate(
  social_interaction_level ~ gender,
  data = data.mental,
  permutations = 2000,
  seed = 123
)

#LRM Stress Multiple Regressor Model
M <- lm(stress_level~daily_social_media_hours + academic_performance + physical_activity + sleep_hours + no_of_social_media, data = data.mental)
M

summary(M)

confint(M, level = 0.95)

#LRM Anxiety Multiple Regressor Model ***one stat. sign. value
M <- lm(anxiety_level~daily_social_media_hours + academic_performance + physical_activity + sleep_hours + no_of_social_media, data = data.mental)
M

summary(M)

confint(M, level = 0.95)

#LRM Addiction Multiple Regressor Model
M <- lm(addiction_level~daily_social_media_hours + academic_performance + physical_activity + sleep_hours+ no_of_social_media, data = data.mental)
M

summary(M)

confint(M, level = 0.95)

#LRM Anxiety v Hours on Social Media
M <- lm(anxiety_level~daily_social_media_hours, data = data.mental)
M

summary(M)

confint(M, level = 0.95)

#LRM Addiction Level v Hours on Social Media
M <- lm(addiction_level~daily_social_media_hours, data = data.mental)
M

summary(M)

confint(M, level = 0.95)

#LRM Academic Performance v Hours on Social Media
M <- lm(academic_performance~daily_social_media_hours, data = data.mental)
M

summary(M)

confint(M, level = 0.95)

#LRM Academic Performance v Hours on Social Media
M <- lm(stress_level~daily_social_media_hours, data = data.mental)
M

summary(M)

confint(M, level = 0.95)

#Logit Depression Multiple Regressor Model ***2 statistically significant values
M <- glm(depression_label~daily_social_media_hours + academic_performance + physical_activity + sleep_hours + male + screen_time_before_sleep, data = data.mental, family = binomial)
M

summary(M)


#Logit Depression Multiple Regressor Model 5 regressors ***2 statistically significant values***
M <- glm(depression_label~daily_social_media_hours + academic_performance + physical_activity + sleep_hours + no_of_social_media, data = data.mental, family = binomial)
M

summary(M)

#Random Forest Analysis through a Variable Importance Plot

install.packages('randomForest')

library(randomForest)


data.mental$no_of_social_media <- ifelse(data.mental$platform_usage == "Both", 2, 1)

set.seed(123)

rf_model <- randomForest(
  anxiety_level ~ daily_social_media_hours +
    sleep_hours +
    academic_performance +
    physical_activity +
    no_of_social_media,
  data = data.mental,
  ntree = 500,
  importance = TRUE
)

print(rf_model)

importance(rf_model)

varImpPlot(rf_model)

