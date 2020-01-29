# load in packages for tidy evaluation
library(ggplot2)
library(dplyr)
library(rpart)
library(randomForest)
library(caret)
library(FeatureHashing)
library(xgboost)

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
#covariance plots to do 
#Also add preprocessing here 
df <- as_tibble(passwords_tidied)
scaled_df <- as.data.frame(scale(df[, -c(2, 3, 5), drop = FALSE], center = TRUE, scale = TRUE))

# insert a call to cbind in the morning need to construct by columns
scaled_df_with_cat <- scaled_df
scaled_df_with_cat$password <- with(scaled_df, as.factor(df$password))
category_levels <- c("food", "rebellious-rude", "password-related", "animal", "nerdy-pop", "sport", "fluffy", "simple-alphanumeric", "cool-macho", "name")
scaled_df_with_cat$category <- with(scaled_df, factor(df$category, levels = category_levels, ordered = TRUE))
time_unit_levels <- c("seconds", "minutes", "days", "weeks", "months", "years")
scaled_df_with_cat$time_unit <- with(scaled_df, factor(df$time_unit, levels = time_unit_levels, ordered = TRUE))


# set the seed for reproducibile results
set.seed(385)
# split the data in to training and tests data sets
trainIndex <- createDataPartition(scaled_df_with_cat$category, times = 1, p = 0.8, list = FALSE)

dfTrain <- scaled_df_with_cat[trainIndex, ]
dfTest <- scaled_df_with_cat[-trainIndex, ]

control <- trainControl(method = "repeatedcv",
                       number = 3,
                        repeats = 10)
set.seed(385)
classificationTree <- train(category ~ .,
                            data = dfTrain,
                            method = "rpart",
                           trControl = control,
                           na.action = na.omit)

classificationTree

plot(classificationTree)

classificationTreePredicts <- predict(classificationTree, newdata = dfTest)

table(classificationTreePredicts, dfTest$category[1:86])

set.seed(385)

rForest <- train(category ~.,
                 data = dfTrain,
                 method = "ranger",
                 trControl = control,
                 na.action = na.omit)

rForest

rForrestPredicts <- predict(rForest, newdata = dfTest)

table(rForrestPredicts, dfTest$category[1:86])

set.seed(385)

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

naive_bayes <- e1071::naiveBayes(category~. , data = dfTrain)

 
pca <- prcomp(scaled_df)

project <- predict(pca, scaled_df) [, 1:2]
project_plus <- cbind(as.data.frame(project),
                      category = df$category,
                      time_unit = df$time_unit,
                      password = df$password)
set.seed(385)
 ggplot(project_plus, aes(x = PC1, y = PC2)) +
  geom_point(data = as.data.frame(project), color ="darkgrey") +
  geom_point() +
  geom_text_repel(aes(label = password), point.padding = NA, hjust = 1, segment.size = 0.2, segment.alpha = 0.25) +
  facet_wrap( ~category, ncol = 2)

# consider using naiveBayes
 f <- category ~ rank + password + category + time_unit + offline_crack_sec + rank_alt + strength + font_size
 
hashed_modelMat_train <- hashed.model.matrix(f, data = dfTrain, create.mapping = TRUE)
hashed_modelMat_test <- hashed.model.matrix(f, data = dfTest, create.mapping = TRUE)
cv.xgb <- xgboost(hashed_modelMat_train, dfTrain$category, max.depth=7, eta=0.1,
                  nround = 100)
p.lm <- predict(cv.xgb, hashed_modelMat_test)

round(p.lm, digits = 1) == as.integer(dfTest$category)
