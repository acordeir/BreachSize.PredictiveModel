#Import necessary libraries
library(tidyverse)
library(caret)
library(rpart)
library(dslabs)
library(ggplot2)
library(readxl)
library(plot.matrix)

#Import data
master_dat <- read_excel("C:\\Users\\adamh\\Downloads\\MasterEvents20052019.xls")
company_dat <- read_excel("C:\\Users\\adamh\\Downloads\\Compustat20002019AllFirms.xls")

#Remove unnecessary columns
clean_master <- master_dat %>% filter(is.na(master_dat$notes)) %>% select(ngvkey, permno, cusip, cusip8, ticker, fyear, eventdate, name, breach_size, records_known, breach_type, event_state, hq_state)
clean_company <- company_dat[-c(78, 79, 80, 81, 82)]

#Create a master dataframe
breach <- left_join(clean_master, clean_company, by =c("ngvkey" = "ngvkey", "fyear" = "Data Year - Fiscal")) %>% filter(!is.na(breach_size))

#Continue cleaning up master dataframe
trim_breach <- breach[c(1,6,7,8,9,11,29,103,109)]
trim_breach <- trim_breach %>% rename("assets" ='Assets - Total')
#Logarithmic scale of breach size
log_breach_size <- log10(trim_breach$breach_size)
#Create a new column that startifies the breach size based on quantile
breach_level <- unlist(map(trim_breach$breach_size, function(x){if(x > quantile(trim_breach$breach_size)[4]){
  "large"
} else if(x < quantile(trim_breach$breach_size)[2]){
  "small"
} else {"medium"}}))
trim_breach <- cbind(trim_breach, log_breach_size, breach_level)  
trim_breach$breach_type <- as.factor(trim_breach$breach_type)
trim_breach <- trim_breach[complete.cases(trim_breach),]
trim_breach <- trim_breach[which(trim_breach$log_breach_size != "-Inf"),]

#Preliminiary visualizations:
#Number of breaches per year
trim_breach %>% ggplot(aes(x=fyear)) +
  geom_histogram()
  
#Size of breach sizes over time
trim_breach %>% ggplot(aes(x=fyear, y=log_breach_size)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x)

#Ploting proportions of breach types, to breach sizes
breach_level_by_type <- trim_breach %>% group_by(breach_level) %>%  select(fyear, breach_level, breach_type)

large_by_type <- filter(breach_level_by_type, breach_level == "large")

medium_by_type <- filter(breach_level_by_type, breach_level == "medium")

small_by_type <- filter(breach_level_by_type, breach_level == "small")


ggplot(breach_level_by_type, aes(x = breach_type)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  ylab("Proportion of breaches")

ggplot(large_by_type, aes(x = breach_type)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  ylab("Proportion of large breaches")

ggplot(medium_by_type, aes(x = breach_type)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  ylab("Proportion of medium breaches")

ggplot(small_by_type, aes(x = breach_type)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  ylab("Proportion of small breaches")
#This gives us the ratio of breach_types based on size. We can note that the majority of large data breaches are caused by hacks.


#Testing to see if there is linear correlation between breach_size and the predictors. There does not appear to be one.
temp <- scale(trim_breach[7:9])
cor(temp)
plot(as.matrix(cor(temp)))
#This tells us that there is no overlap in our predictive variables


#Lookout at important rows
trim_breach[which.max(trim_breach$breach_size),]

ggplot(trim_breach, aes(x=log_breach_size)) +
  geom_boxplot()
#Shows the greatest breach size and which breaches are greater than 3 standard deviations away from the mean.


#Principal Component Analysis
scaled_dat <- scale(trim_breach[,7:9])
pca_mat <- prcomp(scaled_dat)
pca_plot <- summary(pca_mat)$importance
pca_plot
#Shows us that assets contribute the most variance to the model


#Split into train and test
set.seed(1)
train_ind <- createDataPartition(1:nrow(trim_breach), p = 0.8, list = F)
train <- trim_breach[train_ind,]
test <- trim_breach[-train_ind,]

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


train <- train[complete.cases(train),]
test <- test[complete.cases(test),]
train[,7:9] <- scale(train[,7:9])
test[,7:9] <- scale(test[,7:9])

#GLM model
set.seed(1)
glm_mod <- train(log_breach_size ~ assets + roa + salegrowth, method = "glm", data = train)
glm_pred <- predict(glm_mod, test)
glm_rmse <- RMSE(test$log_breach_size, glm_pred)
glm_rmse
glm_graph <- data.frame(cbind(glm_pred, test$log_breach_size)) %>% rename("test" = "V2")
ggplot(glm_graph, aes(x=glm_pred, y=test)) +
  geom_point()

#KNN model
set.seed(2)
knn_mod <- train(breach_level ~ breach_type + fyear, method = "knn", tuneGrid = data.frame(k = seq(2,10,1)), data = train)
knn_results <- knn_mod$results
knn_results
ggplot(knn_results, aes(x=k, y=Accuracy)) +
  geom_point() +
  geom_line()


