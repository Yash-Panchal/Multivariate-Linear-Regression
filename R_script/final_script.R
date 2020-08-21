library(car)
library(corrplot)
library(psych)
library(Hmisc)
library(tidyverse)
library(caret)
library(ggthemes)
library(plotly)
library(lmtest)
library(VIF)

getwd()
setwd("E:\\IIT Kanpur\\IME 2nd Sem Courses\\MBA652-SMBA\\Projects\\R script")
d1 <- read.table("student-mat.csv", sep = ";", header = TRUE)
summary(d1)
attach(d1)

d1$school <- as.numeric(d1$school)
class(d1$school)

d1$sex <- as.numeric(d1$sex) # male = 2, female = 1
class(d1$sex)

d1$address <- as.numeric(d1$address) # R(rural) = 1, U(urban) = 2
class(d1$address)

d1$famsize <- as.numeric(d1$famsize) # LE3 = 1, GT3 = 2
class(d1$famsize)

d1$Pstatus <- as.numeric(d1$Pstatus) #A=1,T=2
class(d1$Pstatus)

d1$Mjob <- as.numeric(d1$Mjob) #mother's job (nominal: "teacher"=5, "health"=2 care related, civil "services"=4 (e.g. administrative or police), "at_home"=1 or "other"=3)
class(d1$Mjob)

d1$Fjob <- as.numeric(d1$Fjob) #father's job (nominal: "teacher"=5, "health" care related, civil "services"=4 (e.g. administrative or police), "at_home"=1 or "other"=3)
class(d1$Fjob)

d1$reason <- as.numeric(d1$reason) #reason to choose this school (nominal: close to "home"=2, school "reputation"=4, "course"=1 preference or "other"=3)
class(d1$reason)

d1$guardian <- as.numeric(d1$guardian) #student's guardian (nominal: "mother"=2, "father"=1 or "other"=3)
class(d1$guardian)

d1$schoolsup <- as.numeric(d1$schoolsup) #student's guardian (nominal: "mother"=2, "father"=1 or "other"=3)
class(d1$schoolsup)

d1$famsup <- as.numeric(d1$famsup) #family educational support (binary: yes=2 or no=1)
class(d1$famsup)

d1$paid <- as.numeric(d1$paid) #extra paid classes within the course subject (Math) NO=1 , YES=2
class(d1$paid)

d1$activities <- as.numeric(d1$activities) #extra-curricular activities (binary: yes=2 or no=1)
class(d1$activities)

d1$nursery <- as.numeric(d1$nursery) #attended nursery school (binary: yes=2 or no=1)
class(d1$nursery)

d1$higher <- as.numeric(d1$higher) #wants to take higher education (binary: yes=2 or no=1)
class(d1$higher)

d1$internet <- as.numeric(d1$internet) #Internet access at home (binary: yes=2 or no=1)
class(d1$internet)

d1$romantic <- as.numeric(d1$romantic) #with a romantic relationship (binary: yes=2 or no=1)
class(d1$romantic)

summary(d1)

# write.csv(d1, "dataset.csv")
# ?write.table

data_train_1 <- d1[1:243, 1:31]
data_train_2 <- d1[350:381, 1:31]
data_train <- rbind(data_train_2, data_train_1)

data_val_1 <- d1[244:278, 1:31]
data_val_2 <- d1[382:386, 1:31]
data_val <- rbind(data_val_1, data_val_2)

data_test_1 <- d1[279:349, 1:31]
data_test_2 <- d1[387:395, 1:31]
data_test <- rbind(data_test_1, data_test_2)
#rm(data_train, data_val, data_test)
rm(data_test_1, data_test_2, data_val_1, data_val_2, data_train_1, data_train_2)


regressor <- lm(formula = G1 ~ .- G1, data = data_train)
data_val$y_pred <- predict(regressor, newdata = data_val)
data_val$diff <- data_val$y_pred - data_val$G1
summary(regressor)

class(data_train$school)

correlation <- cor(data_train)
correlation_1 <- round(correlation, 2)


corrplot(correlation_1, method = "number")
?corrplot

summary(data_train)
class(school)

########################
# scatter plots

ggplot(data_train, aes(x = G1, y = failures)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs failures", x = "Marks", y = "Failures")

ggplot(data_train, aes(x = G1, y = school)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs School", x = "Marks", y = "School")

ggplot(data_train, aes(x = G1, y = sex)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs Sex", x = "Marks", y = "Sex")

ggplot(data_train, aes(x = G1, y = age)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs Age", x = "Marks", y = "Age")

ggplot(data_train, aes(x = G1, y = address)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs Address", x = "Marks", y = "Address")

ggplot(data_train, aes(x = G1, y = famsize)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs family size", x = "Marks", y = "family size")

ggplot(data_train, aes(x = G1, y = Pstatus)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs Pstatus", x = "Marks", y = "Pstatus")

ggplot(data_train, aes(x = G1, y = Medu)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs Mother's Education", x = "Marks", y = "Mother's Education")

ggplot(data_train, aes(x = G1, y = Fedu)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs Father's Education", x = "Marks", y = "Father's Education")

ggplot(data_train, aes(x = G1, y = Mjob)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs Mother's job type", x = "Marks", y = "Mother's job type")

ggplot(data_train, aes(x = G1, y = Fjob)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs Father's job type", x = "Marks", y = "Father's job type")

ggplot(data_train, aes(x = G1, y = reason)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs Reason", x = "Marks", y = "Reason")

ggplot(data_train, aes(x = G1, y = guardian)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs guardian", x = "Marks", y = "guardian")

ggplot(data_train, aes(x = G1, y = traveltime)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs traveltime", x = "Marks", y = "traveltime")

ggplot(data_train, aes(x = G1, y = studytime)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs studytime", x = "Marks", y = "studytime")

ggplot(data_train, aes(x = G1, y = schoolsup)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs schoolsup", x = "Marks", y = "schoolsup")

ggplot(data_train, aes(x = G1, y = famsup)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs famsup", x = "Marks", y = "famsup")

ggplot(data_train, aes(x = G1, y = paid)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs paid", x = "Marks", y = "paid")

ggplot(data_train, aes(x = G1, y = activities)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs activities", x = "Marks", y = "activities")

ggplot(data_train, aes(x = G1, y = nursery)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs nursery", x = "Marks", y = "nursery")

ggplot(data_train, aes(x = G1, y = higher)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs higher", x = "Marks", y = "higher")

ggplot(data_train, aes(x = G1, y = internet)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs internet", x = "Marks", y = "internet")

ggplot(data_train, aes(x = G1, y = romantic)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs romantic", x = "Marks", y = "romantic")

ggplot(data_train, aes(x = G1, y = famrel)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs famrel", x = "Marks", y = "famrel")

ggplot(data_train, aes(x = G1, y = freetime)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs freetime", x = "Marks", y = "freetime")

ggplot(data_train, aes(x = G1, y = goout)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs goout", x = "Marks", y = "goout")

ggplot(data_train, aes(x = G1, y = Dalc)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs Dalc", x = "Marks", y = "Dalc")

ggplot(data_train, aes(x = G1, y = Walc)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs Walc", x = "Marks", y = "Walc")

ggplot(data_train, aes(x = G1, y = health)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs health", x = "Marks", y = "health")

ggplot(data_train, aes(x = G1, y = absences)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Marks vs absences", x = "Marks", y = "absences")




#model 15 - failures
model_15 <- lm(G1 ~ failures, data_train)
summary(model_15)
plot(model_15)
abline(model_15)
bptest(model_15)

# ggplot(full_data[1:3000,], aes(x = budget, y = revenue, color = budget)) +
#   geom_point() +
#   
#   scale_color_viridis(begin = 0, end = .95, option = 'D') + 
#   geom_smooth(method = 'lm', color = 'red3', fill = 'red3') +
#   scale_y_continuous(labels = c('$0', '$500', '$1000', '$1500')) +
#   labs(title = 'Revenue by budget', x = 'Budget', y = 'Revenue (Millions)')

#model A5 - failures + schoolsup
model_A5 <- lm(G1 ~ failures + schoolsup, data_train)
summary(model_A5)
plot(model_A5)
abline(model_A5)
bptest(model_A5)

#model A2 - failures + Medu + schoolsup
model_A2 <- lm(G1 ~ failures + Medu + schoolsup, data_train)
summary(model_A2)
plot(model_A2)
abline(model_A2)
bptest(model_A2)

# model C6 - failures + schoolsup + Medu + famsup
model_C6 <- lm(G1 ~ failures + schoolsup + Medu + famsup, data_train)
summary(model_C6)
plot(model_C6)
abline(model_C6)
bptest(model_C6)

# model D8 - failures + schoolsup + Medu + famsup + goout
model_D8 <- lm(G1 ~ failures + schoolsup + Medu + famsup + goout, data_train)
summary(model_D8)
plot(model_D8)
abline(model_D8)
bptest(model_D8)

# model E2 - failures + schoolsup + Medu + famsup + goout + sex
model_E2 <- lm(G1 ~ failures + schoolsup + Medu + famsup + goout + sex, data_train)
summary(model_E2)
plot(model_E2)
abline(model_E2)
bptest(model_E2)

# model F1 - failures + schoolsup + Medu + famsup + goout + sex + school
model_F1 <- lm(G1 ~ failures + schoolsup + Medu + famsup + goout + sex + school, data_train)
summary(model_F1)
plot(model_F1)
abline(model_F1)
bptest(model_F1)

# model G1 - failures + schoolsup + Medu + famsup + goout + sex + school + age
model_G1 <- lm(G1 ~ failures + schoolsup + Medu + famsup + goout + sex + school + age, data_train)
summary(model_G1)
plot(model_G1)
abline(model_G1)
bptest(model_G1)

### check for multicollinearity
car::vif(model_E2)
car::vif(regressor)

### heteroskedasticity check
bptest(model_G1)
bptest(regressor)

###########
library(olsrr)
test_model <- lm(d1$G1~.,data = d1)
olsrr::ols_step_all_possible(test_model)

ols_step_both_p(test_model, pent = 0.1, prem = 0.1, details = FALSE)

# d1$G1_pred <- predict(model_G1, d1)
# d1$G1_pred_round <- round(d1$G1_pred)
# d1$G1_diff <- d1$G1 - d1$G1_pred_round
# 
# max(d1$G1_diff)
