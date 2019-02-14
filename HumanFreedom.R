library(MASS)
library(tidyverse)
library(DataExplorer)
library(polycor)
library(caret)
library(recipes)
library(rsample)
library(ROCR)
library(car)
library(caret)
library(broom)

data_raw = read.csv(file="/home/neo/Downloads/Human_Freedom.csv", header=TRUE, sep=",")
glimpse(data_raw)

data_raw = data_raw %>% select(hf_score,everything())

plot_missing(data_raw)

data_raw2 = data_raw %>% 
  select(hf_score,
         pf_ss_disappearances_disap,
         pf_ss_disappearances_violent,
         pf_ss_disappearances_organized,
         pf_ss_disappearances_fatalities,
         pf_score,
         ef_legal_courts,
         ef_legal_protection,
         ef_legal_military,
         ef_legal_integrity,
         ef_legal_enforcement,
         ef_legal_restrictions,
         ef_legal_police,
         ef_legal_crime,
         ef_legal_gender,
         ef_score)

glimpse(data_raw2)
plot_missing(data_raw2)

set.seed(2020)
train_test_split = initial_split(data_raw2,prop=0.8)
train_test_split

train_tbl = training(train_test_split)
test_tbl = testing(train_test_split)

eclair = recipe(hf_score ~., data=train_tbl) %>% 
  #step_dummy(all_nominal(),-all_outcomes()) %>% 
  step_BoxCox(all_predictors(),-all_outcomes()) %>%
  prep(data=train_tbl)

eclair
  
train_clean = bake(eclair,new_data=train_tbl)
test_clean = bake(eclair,new_data=test_tbl)

plot_missing(train_clean)
plot_missing(test_clean)

train_clean = na.omit(train_clean)
test_clean = na.omit(test_clean)

glimpse(train_clean)
glimpse(test_clean)

lm.fit = lm(hf_score ~ ., data=train_clean)
summary(lm.fit)

train_clean2 = train_clean %>% select(-pf_ss_disappearances_organized,
                                      -ef_legal_enforcement,
                                      -ef_legal_police,
                                      -ef_legal_crime,
                                      -ef_legal_gender)
test_clean2 = test_clean %>% select(-pf_ss_disappearances_organized,
                                      -ef_legal_enforcement,
                                      -ef_legal_police,
                                      -ef_legal_crime,
                                      -ef_legal_gender)

train_clean2 = as.data.frame(train_clean2)
test_clean2 = as.data.frame(test_clean2)
glimpse(train_clean2)
hetcor(train_clean2)

plot_scatterplot(train_clean2, by="hf_score")

lm.fit2 = lm(hf_score ~ ., data=train_clean2)
summary(lm.fit2)

crPlots(lm.fit2)
qqPlot(lm.fit2,main="QQ Plot")
vif(lm.fit2)
outlierTest(lm.fit2)
durbinWatsonTest(lm.fit2)
ncvTest(lm.fit2)
spreadLevelPlot(lm.fit2)

train_clean3 = train_clean2 %>% select(-396,-194,-302,-572,-95,-101,-819,-296,-557)
test_clean3 = test_clean2 %>% select(-396,-194,-302,-572,-95,-101,-819,-296,-557)

lm.fit3 = lm(hf_score ~., data=train_clean3)
summary(lm.fit3)

lm.pred = predict(lm.fit3,newdata=test_clean3,type="response")

sum_pred = sum(lm.pred)
sum_y = sum(test_clean3$hf_score)

accuracy = sum_pred/sum_y
accuracy

lm.pred
test_clean3$hf_score
