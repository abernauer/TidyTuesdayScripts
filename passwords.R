# load in packages for tidy evaluation
library(ggplot2)
library(dplyr)
library(mlr)

# read password data set into R
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# get an idea of the data
summary(passwords)

# peek at the beginning of the data
head(passwords)
tail(passwords)
any(is.na(passwords))
colSums(is.na(passwords))
#removvs the na's from the tibble 
passwords_tidied <- passwords[1:500, 1:9]
# confirms we have removed all the nas  
any(is.na(passwords_tidied))


ggplot(passwords_tidied, aes(rank, value, col = category)) +
  geom_point()

ggplot(passwords_tidied, aes(rank, strength, col = category)) +
  geom_point()

ggplot(passwords_tidied, aes(strength, rank, col = category)) +
  geom_point()

ggplot(passwords_tidied, aes(font_size, strength, col = category)) +
  geom_point()
df <- as_tibble(passwords_tidied)
df$password <- as.factor(df$password)
df$time_unit <- as.factor(df$time_unit)
df$category <- as.factor(df$value)

passwordTask <- makeClassifTask(data = df, target = "category")

tree <- makeLearner("classif.rpart")

getParamSet(tree)

# makeParamSet()
