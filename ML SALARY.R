# LIBRARIES
{

library(rio)
library(dplyr)
library(tidyr)
library(caret)
library(kernlab)
library(randomForest)
library(ggplot2)
library(janitor)
library(glue)

}

# DATA
{

df <- import("./salaries.csv")
df <- df %>% select(-V1)

}

# QUICK PLOT TO SEE IF DATA IS BIASED
{

aa <- df %>% select(work_year , salary_in_usd , experience_level)

p <- ggplot(data = aa , mapping = aes(x = as.factor(work_year) , y = salary_in_usd , color = experience_level)) +
    geom_jitter()

p

}

# MODIFY VARIABLES FOR ML
{

aa <- df

for(ii in c(aa$experience_level)) {
    zz <- aa %>% filter(experience_level == ii)
    zz[paste0("el_" , tolower(ii))] <- 1
    zz2 <- aa %>% filter(experience_level != ii)
    zz2[paste0("el_" , tolower(ii))] <- 0
    aa <- rbind(zz , zz2)
}

for(ii in c(aa$employment_type)) {
    zz <- aa %>% filter(employment_type == ii)
    zz[paste0("et_" , tolower(ii))] <- 1
    zz2 <- aa %>% filter(employment_type != ii)
    zz2[paste0("et_" , tolower(ii))] <- 0
    aa <- rbind(zz , zz2)
}

for(ii in c(aa$job_title)) {
    zz <- aa %>% filter(job_title == ii)
    zz[paste0("jt_" , tolower(ii))] <- 1
    zz2 <- aa %>% filter(job_title != ii)
    zz2[paste0("jt_" , tolower(ii))] <- 0
    aa <- rbind(zz , zz2)
}

aa <- clean_names(aa)

for(ii in c(aa$employee_residence)) {
    zz <- aa %>% filter(employee_residence == ii)
    zz[paste0("er_" , tolower(ii))] <- 1
    zz2 <- aa %>% filter(employee_residence != ii)
    zz2[paste0("er_" , tolower(ii))] <- 0
    aa <- rbind(zz , zz2)
}

for(ii in c(aa$company_location)) {
    zz <- aa %>% filter(company_location == ii)
    zz[paste0("cl_" , tolower(ii))] <- 1
    zz2 <- aa %>% filter(company_location != ii)
    zz2[paste0("cl_" , tolower(ii))] <- 0
    aa <- rbind(zz , zz2)
}

for(ii in c(aa$company_size)) {
    zz <- aa %>% filter(company_size == ii)
    zz[paste0("cs_" , tolower(ii))] <- 1
    zz2 <- aa %>% filter(company_size != ii)
    zz2[paste0("cs_" , tolower(ii))] <- 0
    aa <- rbind(zz , zz2)
}

aa <- aa %>% select(7 , 1 , 9 , 12:179)

}

# CREATING DATASET TO TRAIN AND THEN TEST THE MODELS
{
#create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(aa$salary , p = 0.8 , list = FALSE)

#select 20% of the data for validation
validation <- aa[-validation_index,]

#use the remaining 80% of data to training and testing the models
dataset <- aa[validation_index,]
}

# RUN SOME ALGORITHMS
{
# Run algorithms using 10-fold cross validation
control <- trainControl(method = "cv" , number = 10)
metric <- "RMSE"

# Build Models
# a) linear algorithms
        #set.seed(7)
        #fit.lda <- train(salary_in_usd~. , data = dataset , method = "lda" , metric = metric , trControl = control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(salary_in_usd~. , data = dataset , method = "rpart" , metric = metric , trControl = control)
# kNN
set.seed(7)
fit.knn <- train(salary_in_usd~. , data = dataset , method = "knn" , metric = metric , trControl = control)

# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(salary_in_usd~. , data = dataset , method = "svmRadial" , metric = metric , trControl = control)
# Random Forest
set.seed(7)
fit.rf <- train(salary_in_usd~. , data = dataset , method = "rf" , metric = metric , trControl = control)
}

# COMPARING THE MODELS
{
# Summarize accuracy of models
results <- resamples(list(cart = fit.cart , knn=fit.knn , svn = fit.svm , rf = fit.rf))
summary(results)

# Compare accuracy of models
dotplot(results)

# Summarize the Best Model (in this case it is "svm")
print(fit.svm)
}

# MAKE PREDICTIONS
{
predictions <- predict(fit.rf , validation)

print(predictions)

aa <- data.frame(values = validation$salary_in_usd , prediction = predictions)
zz <- data.frame(x = 1:200000 , y = 1:200000)


p <- ggplot() +
    geom_jitter(data = aa , mapping = aes(x = values , y = prediction)) +
    geom_line(data = zz , mapping = aes(x = x , y = y , color = "red"))

p
}

# RE-DOING THE VALIDATION SET:
{
andreu <- data.frame(work_year = 2022 ,
                     experience_level = "EN" ,
                     employment_type = "FT" ,
                     job_title = "Data Scientist" ,
                     salary = 30000 ,
                     salary_currency = "EUR" ,
                     salary_in_usd = 999999 ,
                     employee_residence = "ES" ,
                     remote_ratio = 100 ,
                     company_location = "ES" ,
                     company_size = "M")

}

# MODIFY VARIABLES FOR ML
{

aa <- rbind(df , andreu)

for(ii in c(aa$experience_level)) {
    zz <- aa %>% filter(experience_level == ii)
    zz[paste0("el_" , tolower(ii))] <- 1
    zz2 <- aa %>% filter(experience_level != ii)
    zz2[paste0("el_" , tolower(ii))] <- 0
    aa <- rbind(zz , zz2)
}

for(ii in c(aa$employment_type)) {
    zz <- aa %>% filter(employment_type == ii)
    zz[paste0("et_" , tolower(ii))] <- 1
    zz2 <- aa %>% filter(employment_type != ii)
    zz2[paste0("et_" , tolower(ii))] <- 0
    aa <- rbind(zz , zz2)
}

for(ii in c(aa$job_title)) {
    zz <- aa %>% filter(job_title == ii)
    zz[paste0("jt_" , tolower(ii))] <- 1
    zz2 <- aa %>% filter(job_title != ii)
    zz2[paste0("jt_" , tolower(ii))] <- 0
    aa <- rbind(zz , zz2)
}

aa <- clean_names(aa)

for(ii in c(aa$employee_residence)) {
    zz <- aa %>% filter(employee_residence == ii)
    zz[paste0("er_" , tolower(ii))] <- 1
    zz2 <- aa %>% filter(employee_residence != ii)
    zz2[paste0("er_" , tolower(ii))] <- 0
    aa <- rbind(zz , zz2)
}

for(ii in c(aa$company_location)) {
    zz <- aa %>% filter(company_location == ii)
    zz[paste0("cl_" , tolower(ii))] <- 1
    zz2 <- aa %>% filter(company_location != ii)
    zz2[paste0("cl_" , tolower(ii))] <- 0
    aa <- rbind(zz , zz2)
}

for(ii in c(aa$company_size)) {
    zz <- aa %>% filter(company_size == ii)
    zz[paste0("cs_" , tolower(ii))] <- 1
    zz2 <- aa %>% filter(company_size != ii)
    zz2[paste0("cs_" , tolower(ii))] <- 0
    aa <- rbind(zz , zz2)
}

aa <- aa %>% select(7 , 1 , 9 , 12:179)

}

# MAKE A PREDICTION OVER ANDREU
{
andreu <- aa %>% filter(salary_in_usd == 999999)

predictions <- predict(fit.rf , andreu)

print(glue("The expected salary for me is: " , as.numeric(predictions) , " €"))
}


