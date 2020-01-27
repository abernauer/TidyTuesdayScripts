# load in packages for tidy evaluation
library(ggplot2)
library(dplyr)
library(rpart)
library(randomForest)
library(caret)

# read password data set into R
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# get an idea of the data
summary(passwords)

# peek at the beginning of the data
head(passwords)
tail(passwords)
any(is.na(passwords))
colSums(is.na(passwords))
#removes the na's from the tibble 
passwords_tidied <- passwords[1:500, 1:9]
# confirms we have removed all the nas  
any(is.na(passwords_tidied))


#plots of bar chart of 
ggplot(passwords_tidied, aes(rank, value, col = category)) +
  geom_point()

ggplot(passwords_tidied, aes(rank, strength, col = category)) +
  geom_point()

ggplot(passwords_tidied, aes(strength, rank, col = category)) +
  geom_point()

ggplot(passwords_tidied, aes(font_size, strength, col = category)) +
  geom_point()
#covariance heatmap



df <- as_tibble(passwords_tidied)
df$password <- as.factor(df$password)
category_levels <- c("name", "cool-macho", "simple-alphanumeric", "fluffy", "sport", "nerdy-pop", "animal", "password-related", "rebellious-rude", "food")
df$category <- factor(df$category, levels = category_levels, ordered = TRUE)
time_unit_levels <- c("years", "months", "weeks", "days", "minutes", "seconds")
df$time_unit <- factor(df$time_unit, levels = time_unit_levels, ordered = TRUE)


# set the seed for reproducibile results
set.seed(385)
# split the data in to training and tests data sets
trainIndex <- createDataPartition(df$category, times = 1, p = 0.8, list = FALSE)

dfTrain <- df[ trainIndex, ]
dfTest <- df[-trainIndex, ]

control <- trainControl(method = "repeatedcv",
                       number = 3,
                        repeats = 10)
set.seed(432)
classificationTree <- train(category ~ .,
                            data = dfTrain,
                            method = "rpart",
                           trControl = control,
                           na.action = na.omit)

classificationTree

plot(classificationTree)

classificationTreePredicts <- predict(classificationTree, newdata = dfTest)

table(classificationTreePredicts, dfTest$category[1:88])

set.seed(7)

rForest <- train(category ~.,
                 data = dfTrain,
                 method = "ranger",
                 trControl = control,
                 na.action = na.omit)

rForest

rForrestPredicts <- predict(rForest, newdata = dfTest)

table(rForrestPredicts, dfTest$category[1:88])

set.seed(10)

gbm <- train(category ~.,
                  data = dfTrain,
                  method = "gbm",
                  trControl = control,
                  na.action = na.omit)

plot(gbm)

gbm_predicts <- predict(gbm, newdata = dfTest)
table(gbm_predicts, dfTest$category[1:88])


# set.seed(99)
# tree_dt <- rpart(category ~ ., data = dfTrain)

scaled <- scale(df[, -c(2, 3, 5)], center = TRUE, scale = TRUE)


 
pca <- prcomp(scaled)

project <- predict(pca, scaled) [, 1:2]
project_plus <- cbind(as.data.frame(project),
                      category = df$category,
                      time_unit = df$time_unit,
                      password = df$password)

# ggplot(project_plus, aes(x = PC1, y = PC2)) +
#  geom_point(data = as.data.frame(project), color ="darkgrey") +
#  geom_jitter() +
#  geom_text(aes(label = password),
#            hjust = 0, vjust = , size = 3) +
#  facet_wrap( ~category, ncol = 3)
