#Importing Libraries
library(dplyr)
library(ggplot2)
library(rattle)
library(caret)
library(maptree)
library(randomForest)

# Setting Seed for replication of results
set.seed(2806)

#Importing Data
car <- read.csv("C:/Users/Fahim Usman/Documents/Edu/Data Science/R/data/car_evaluation.csv", 
                col.names = c("buying", "maint", "doors", "persons", "lugboot", "safety", "CAR"),
                colClasses = c(rep("factor",7)))

##Attribute Information (variables in all small letters)
# CAR                      car acceptability
# . PRICE                  overall price
# . . buying               buying price
# . . maint                price of the maintenance
# . TECH                   technical characteristics
# . . COMFORT              comfort
# . . . doors              number of doors
# . . . persons            capacity in terms of persons to carry
# . . . lug_boot           the size of luggage boot
# . . safety               estimated safety of the car

head(car)

##Checking levels of all variables
# Attribute Values:
# buying       v-high, high, med, low
# maint        v-high, high, med, low
# doors        2, 3, 4, 5-more
# persons      2, 4, more
# lug_boot     small, med, big
# safety       low, med, high

levels(car$buying) # needs correction
car$buying <- factor(car$buying, levels = c("vhigh", "high", "med", "low"))

levels(car$maint) # needs correction
car$maint <- factor(car$maint, levels = c("vhigh", "high", "med", "low"))

levels(car$doors) # fine as is
levels(car$persons) # fine as is

levels(car$lugboot) # needs correction
car$lugboot <- factor(car$lugboot, levels = c("small", "med", "big"))

levels(car$safety) # needs correction
car$safety <- factor(car$safety, levels = c("low", "med", "high"))

levels(car$CAR) # needs correction
car$CAR <- factor(car$CAR, levels = c("unacc", "acc", "good", "vgood"))

# Verifying that all levels are in order
summary(car)
str(car)

#EDA
### Is the data balanced?
lapply(car[, -7], table)

# The input data is evenly distributed

plot(car$CAR)
prop.table(table(car$CAR))
# The output variable has uneven distribution with only a combined 8% being good or very good
# The data is dominated by unacceptable which occupies 70% of the predictor's distribution
# acceptable takes the remaining 22% of the predictor's distribution

### Data Visualization
names(car)
# How does each variable affect CAR?

# Theme variable - To append to ggplot statements so as to prevent repetition
gg_theme <- theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                  axis.title = element_text(size = 10),
                  axis.text = element_text(size = 8),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())

ggplot(data = car, aes(x = CAR, fill = CAR)) +
  geom_bar() +
  facet_grid(~car$buying) +
  labs(x = "Car Quality", y = "", title = "Effect of Buying Cost on Car Quality") +
  gg_theme
# Quality of the car increases with a reduction in buying price
nrow(filter(car, CAR %in% c("vgood", "good") && buying %in% c("high", "vhigh")))
# No car makes the cut for being very good or even good in terms of car quality when the price is 
# high or even very high

ggplot(car, aes(x = CAR, fill = CAR)) +
  geom_bar() +
  facet_grid(~car$maint) +
  labs(x = "Car Quality", y = "", title = "Effect of Maintenance Cost on Car Quality") +
  gg_theme
# Quality of the car increases with a decrease in maintenance cost

ggplot(car, aes(x = CAR, fill = CAR)) +
  geom_bar() +
  facet_grid(~car$doors) +
  labs(x = "Car Quality", y = "", title = "Effect of No. of Doors on Car Quality") +
  gg_theme
# Quality of the car improves when the number of doors increase

ggplot(car, aes(x = CAR, fill = CAR)) +
  geom_bar() +
  facet_grid(~car$persons) +
  labs(x = "Car Quality", y = "", title = "Effect of Seating Capacity on Car Quality") +
  gg_theme
# Quality of the car increases with an increase in seating capacity
nrow(filter(car, CAR %in% c("acc", "good", "vgood") && persons == "2"))
# All the cars with 2 seats are termed unacceptable

ggplot(car, aes(x = CAR, fill = CAR)) +
  geom_bar() +
  facet_grid(~car$lugboot) +
  labs(x = "Car Quality", y = "", title = "Effect of Luggage Boot Size on Car Quality") +
  gg_theme
# Quality of the car improves with increment to the luggage boot size capacity

ggplot(car, aes(x = CAR, fill = CAR)) +
  geom_bar() +
  facet_grid(~car$safety) + 
  labs(x = "Car Quality", y = "", title = "Effect of Safety Rating on Car Quality") +
  gg_theme
# Quality of the car is higher for a higher safety rating

### Base Model - Using Decision Tree
# Before building our model, we will address the issue of our prediction being skewed.
# Since we have an extremely smaller count of records having predictions as "good" and "vgood" (only about 130 out of 1700), our model might face an issue predicting them as there will simply not be enough data to learn and hence, generalize for the model and will probably classify many, or even all, of the cars into only two classes - "unacceptable" or "acceptable". So, without wasting time on splitting data into training and testing, we will first apply our model on the entire data set to see if "good" and "vgood" values are predicted. If they are predicted with a good precision, we will do the fundamental process of splitting the data and move ahead with modeling, and if not, feature transformation may be needed.

mod_base_tree <- train(CAR ~ ., data = car, method = "rpart")
print(mod_base_tree$finalModel)
draw.tree(mod_base_tree$finalModel, cex = 1)

# As can be seen, our fears have come true. The cars have been classified only as "acceptable" or "unacceptable". 

# Now let's see if we can achieve some sort of accuracy using Random Forest 
mod_base_rf <- randomForest(CAR ~ ., data = car, ntree = 500)
plot(mod_base_rf, main = "Error Reduction")
# As can be seen from the error reduction plot, the error rate has been significantly reduced for 500 trees

# Let's see how the model has performed
predictions <- predict(mod_base_rf, car[, -c(7)])
summary(predictions)
levels(predictions)
conf_matrix <- confusionMatrix(data = predictions, reference = car$CAR)
conf_matrix

# This is a total surprise. Only two predictions from a total of over 1700 were wrong. This has to be a case of overfitting, probably. So, let's try and see if we can consolidate this model performanceon on a split data set

# Creating a training and testing dataset using a 8:2 ratio
split <- createDataPartition(y = car$CAR, p = 0.8, list = F)
training <- car[split, ]
testing  <- car[-split, ]
dim(training)
dim(testing)

# Let's build a model on the training dataset
model_train <- randomForest(CAR ~ ., data = training, ntree = 500)
plot(model_train, main = "Error Reduction")
# As can be seen from the error reduction plot, the error rate has been significantly reduced for 500 trees

# Predictions for the training dataset
pred_train <- predict(model_train, training[, -7])
conf_matrix <- confusionMatrix(data = pred_train, reference = training$CAR)
conf_matrix

# An accuracy of 99% on the training dataset. Let's see how it performs on the testing dataset although thumb rule suggests that the model has probably overfit

# Predictions for the testing dataset
pred_test <- predict(model_train, testing[, -7])
conf_matrix <- confusionMatrix(data = pred_test, reference = testing$CAR)
conf_matrix

# And, as we can see, surprisingly, the model has NOT overfit with an accuracy of 98%
# Thus, Random Forest has proven beneficial and saved us a lot of time in feature engineering and feature transformations.

# As a bonus, we can also see which variables proved to be the more critical than others using Random Forest
imp <- as.data.frame(model_train$importance)
imp[order(imp, decreasing = T), , drop = F]
# As seen from the table of importance, "safety" is the most vital feature that decides it's quality where as "doors" i.e. number of doors is the least vital feature. Makes sense.

# Let's now see if we can fine tune the model for performance using the error reduction plot
# The error reduction plot for out final model is given below
plot(model_train, main = "Error Reduction")

# We can see that the error rate has flattened as number of trees go beyond 200
# Let's check for ntree = 200

model_train_tune1 <- randomForest(CAR ~ ., data = training, ntree = 200)
plot(model_train_tune1, main = "Error Reduction")

# Predictions and performance for the training dataset
pred_train <- predict(model_train_tune1, training[, -7])
conf_matrix <- confusionMatrix(data = pred_train, reference = training$CAR)
conf_matrix

# Predictions and performance for the testing dataset
pred_test <- predict(model_train_tune1, testing[, -7])
conf_matrix <- confusionMatrix(data = pred_test, reference = testing$CAR)
conf_matrix

# The model does peforms upto the task but looks like the error rate can be reduced for a few classes
# Let's try ntree = 250
model_train_tune2 <- randomForest(CAR ~ ., data = training, ntree = 250)
plot(model_train_tune2, main = "Error Reduction")

# Predictions and performance for the training dataset
pred_train <- predict(model_train_tune2, training[, -7])
conf_matrix <- confusionMatrix(data = pred_train, reference = training$CAR)
conf_matrix

# Predictions and performance for the testing dataset
pred_test <- predict(model_train_tune2, testing[, -7])
conf_matrix <- confusionMatrix(data = pred_test, reference = testing$CAR)
conf_matrix

# This seems satisfying and close to the original result we obtained for 500 trees. Doubling the performance cost for a small gain is not necessary so we will stop at 250 trees.
# We will consolidate model_train_tune2 as our final model
final_model <- model_train_tune2
final_model$call
conf_matrix
