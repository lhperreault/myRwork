
# What factors influence the return on investment (ROI) for real estate investments, and can a regression model be built to predict the expected ROI of a given investment property? 


# This is the question I asked myself and is what this code ia based off of. 
# There's two parts... one is plotting the correlation and time series graph (except i dont use (ts))
# The second... is using the XGBoost Model to make prediction on the housing variable.
# This could help economists for businesses that rely heavily on economic factors to understand the impact several economic factors have on each other. 

## upload libraries and datasets
library(readr)
library(readxl)
library(readr)
library(tidyverse)

Primary_Energy_Supply <- read_csv("DP_LIVE_15042023232223446.csv")
GDP  <- read_csv("DP_LIVE_15042023231602899.csv")
Crude_Oil_Prices <- read_csv("DP_LIVE_15042023232640540.csv")
Foreign_Direct_Investment <- read_csv("DP_LIVE_16042023221449851.csv")
House_Hold_Savings <- read_csv("DP_LIVE_16042023221003487.csv")
Interest_Rate <- read_excel("Interest_Rate.xlsx")

# filter/clean/make the values unique so we can keep track of them later.
Primary_Energy_Supply  <- Primary_Energy_Supply %>% 
  filter(LOCATION == "USA")
Primary_Energy_Supply$V_PES <- Primary_Energy_Supply$Value
Primary_Energy_Supply <- Primary_Energy_Supply %>% select(LOCATION, INDICATOR, TIME, V_PES)

GDP <- GDP %>% 
  filter(LOCATION == "USA")
GDP$V_GDP <- GDP$Value
GDP <- GDP %>% filter(V_GDP > 100000)
GDP <- GDP %>% select(LOCATION, INDICATOR, TIME, V_GDP)

Crude_Oil_Prices <- Crude_Oil_Prices %>% 
  filter(LOCATION == "USA")
Crude_Oil_Prices$V_COP <- Crude_Oil_Prices$Value
Crude_Oil_Prices <- Crude_Oil_Prices %>% select(LOCATION, INDICATOR, TIME, V_COP)
Foreign_Direct_Investment <- Foreign_Direct_Investment %>% 
  filter(LOCATION == "USA")
Foreign_Direct_Investment$V_FDI <- Foreign_Direct_Investment$Value
Foreign_Direct_Investment <- Foreign_Direct_Investment %>% select(LOCATION, INDICATOR, TIME, V_FDI)

House_Hold_Savings <- House_Hold_Savings %>% 
  filter(LOCATION == "USA")
House_Hold_Savings$V_HHS <- House_Hold_Savings$Value
House_Hold_Savings <- House_Hold_Savings %>% select(LOCATION, INDICATOR, TIME, V_HHS)

Interest_Rate$TIME <- Interest_Rate$Year
Interest_Rate$TIME <- substr(Interest_Rate$TIME, 1, 4)
Interest_Rate <- na.omit(Interest_Rate)
Interest_Rate$Value <- Interest_Rate$`Average U.S.`
Interest_Rate$V_IR <- Interest_Rate$Value
Interest_Rate$TIME <- as.numeric(Interest_Rate$TIME)
Interest_Rate <- Interest_Rate %>% select( TIME, 'Annual Percentage' , V_IR)

#######################DONT RUN IF YOU WANT TO XGBOOST##
### This is Scaling for creating a correlation grid and graph.
GDP$V_GDP <- GDP$Value   
Primary_Energy_Supply$V_PES <- Primary_Energy_Supply$Value
Crude_Oil_Prices$V_COP <- Crude_Oil_Prices$Value 
scale <- left_join(Crude_Oil_Prices,GDP, by = 'TIME')
scale <- left_join(scale, Primary_Energy_Suppply, by = 'TIME')
scale <- left_join(scale, House_Hold_Savings, by = 'TIME')
scale <- left_join(scale, Foreign_Direct_Investment, by = 'TIME')
scale <- left_join(scale, Interest_Rate , by = 'TIME')
scale <- scale %>% select(V_HHS, V_FDI, V_IR, V_GDP, V_COP, V_PES)
scale$V_IR <- as.numeric(scale$V_IR)
scaled <- scale(scale)
scaled <- data.frame(scaled)
scaled <- scaled %>%
  mutate(TIME = seq(from = 2000, length.out = 23))

##### DONNT RUNNNNN for xgboost #####
## Joining Back into the proper datasets for the correlation grid and graph.
#
GDP <- right_join(scaled, GDP, by = 'TIME')
GDP <- GDP %>% select(V_GDP.x, TIME, LOCATION, INDICATOR)
GDP$Value <- GDP$V_GDP.x
GDP <- GDP %>% select(Value, TIME, LOCATION, INDICATOR)
#
Primary_Energy_Supply <- right_join(scaled, Primary_Energy_Supply, by = 'TIME')
Primary_Energy_Supply <- Primary_Energy_Supply %>% select(V_PES.x, TIME, LOCATION, INDICATOR)
Primary_Energy_Supply$Value <- Primary_Energy_Supply$V_PES.x
Primary_Energy_Supply <- Primary_Energy_Supply %>% select(Value, TIME, LOCATION, INDICATOR)
#
Crude_Oil_Prices <- right_join(scaled, Crude_Oil_Prices, by = 'TIME')
Crude_Oil_Prices <- Crude_Oil_Prices %>% select(V_COP.x, TIME, LOCATION, INDICATOR)
Crude_Oil_Prices$Value <- Crude_Oil_Prices$V_COP.x
Crude_Oil_Prices <- Crude_Oil_Prices %>% select(Value, TIME, LOCATION, INDICATOR)
#
Foreign_Direct_Investment <- right_join(scaled, Foreign_Direct_Investment, by = 'TIME')
Foreign_Direct_Investment <- Foreign_Direct_Investment %>% select(V_FDI.x, TIME, LOCATION, INDICATOR)
Foreign_Direct_Investment$Value <- Foreign_Direct_Investment$V_FDI.x
Foreign_Direct_Investment <- Foreign_Direct_Investment %>% select(Value, TIME, LOCATION, INDICATOR)

House_Hold_Savings <- right_join(scaled, House_Hold_Savings, by = 'TIME')
House_Hold_Savings <- House_Hold_Savings %>% select(V_HHS.x, TIME, LOCATION, INDICATOR)
House_Hold_Savings$Value <- House_Hold_Savings$V_HHS.x
House_Hold_Savings <- House_Hold_Savings %>% select(Value, TIME, LOCATION, INDICATOR)
House_Hold_Savings <- na.omit(House_Hold_Savings)

Interest_Rate <- right_join(scaled, Interest_Rate, by = 'TIME')
Interest_Rate$INDICATOR <- c('Int_Rate')
Interest_Rate$LOCATION <- c('USA')
Interest_Rate <- Interest_Rate %>% select(V_IR.x, TIME, LOCATION, INDICATOR)
Interest_Rate$Value <- Interest_Rate$V_IR.x
Interest_Rate <- Interest_Rate %>% select(Value, TIME, LOCATION, INDICATOR)

###########DONT RUNNNNNNNN FOR XGBOOOSTTTTT #######################################
####Union into one variable to see graph and correlation grid.
dataset <- union(Crude_Oil_Prices, GDP)
dataset <- union(dataset, Primary_Energy_Supply)
dataset <- union(dataset, Foreign_Direct_Investment)
dataset <- union(dataset, House_Hold_Savings)
dataset <- union(dataset, Interest_Rate)

################# RUN FOR XGBOOST MODEL ##  ##############

#left join to join them together
dataset1 <- left_join(Crude_Oil_Prices, Foreign_Direct_Investment, by = 'TIME')
dataset1 <- left_join(dataset1, House_Hold_Savings, by = 'TIME')
dataset1 <- left_join(dataset1, GDP, by = 'TIME')
dataset1 <- left_join(dataset1, Primary_Energy_Supply, by = 'TIME')
dataset1 <- left_join(dataset1, Interest_Rate, by = 'TIME')

##### IGNORE THIS SECTION FOR XGBOOST LATERRRRR #######################CORRELATION
library(RColorBrewer)
dataset1 <- na.omit(dataset1)
cor_matrix <- cor(dataset1)
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black", 
         cl.cex = 0.8, addCoef.col = "black")

###### CAN ONLY BE DONE WITH THE UNIONS ##### SO IGNORE FOR XGBOOST ##########
dataset1 <- scale(dataset1)
dataset1 <- data.frame(dataset1)
ggplot(dataset1 , aes(x= TIME))+
  geom_line(aes(y=V_FDI), show.legend = FALSE)+
  geom_line(aes(y=V_HHS), show.legend = FALSE)+
  geom_line(aes(y=V_GDP), show.legend = FALSE)+
  geom_line(aes(y=V_PES), show.legend = FALSE)+
  geom_line(aes(y=V_COP), show.legend = FALSE)+
  geom_line(aes(y=V_IR), show.legend = FALSE)+
  geom_line(aes(y=V_SP5), show.legend = FALSE)+
  geom_line(aes(y=V_INF), show.legend = FALSE)+
  geom_line(aes(y=V_HP), show.legend = FALSE)

#PLOT##### IGNORE FOR XGBOOST
library(ggplot2)
dataset$Metric <- dataset$INDICATOR
ggplot()+
  geom_smooth(aes(x = dataset$TIME, y = dataset$Value, color = dataset$Metric))+
  geom_jitter(aes(x = dataset$TIME, y = dataset$Value, color = dataset$Metric))+
  geom_line(aes(x = dataset$TIME, y = dataset$Value, color = dataset$Metric))
# still working on this facet wrap below
ggplot(dataset, aes(x = TIME, y = Value))+
  geom_line()+
  geom_point()+
  facet_wrap( ~Metric )

#########READ##########
#I wanna get something that I can choose what metrics to plug in and it will predict which variable i ask it and give the confidence level of it. 
# remember TO NOT scale anything and no unions

############### optional to add these three indicators below. These most likely will be used as a dependent variable. 
# SP500
library(readxl)
SP500 <- read_excel("SP500.xlsx")
SP500$V_SP5 <- SP500$VALUE
SP500$V_SP5 <- substr(SP500$V_SP5, 1, 4)
SP500 <- SP500 %>%
  mutate(TIME = seq(from = 2000, length.out = 23))
dataset1 <- left_join(dataset1, SP500, by = 'TIME')
dataset1 <- dataset1 %>% 
  select( TIME, V_FDI, V_HHS, V_GDP, V_PES, V_COP, V_IR, V_SP5)
dataset1$V_SP5 <- as.numeric(dataset1$V_SP5)

# Inflation
Inflation <- read_excel("Inflation.xlsx")
dataset1 <- left_join(dataset1, Inflation, by = 'TIME')
dataset1 <- dataset1 %>% 
  select( TIME, V_FDI, V_HHS, V_GDP, V_PES, V_COP, V_IR, V_SP5, V_INF)

# Housing Prices 
Housing_Prices <- read_excel("Housing_Prices.xlsx")
Housing_Prices$TIME <- as.numeric(Housing_Prices$Year)
dataset1 <- left_join(dataset1, Housing_Prices, by = 'TIME')
dataset1$V_HP <- as.numeric(dataset1$HPI)
dataset1 <- dataset1 %>% 
  select( TIME, V_FDI, V_HHS, V_GDP, V_PES, V_COP, V_IR, V_SP5, V_INF, V_HP)

####### manipulate this to be our final dataset before doing the XGBOOST. so dataset1 should always stay the same. 
dataset_final <- dataset1 %>% 
  select(TIME, V_GDP, V_COP, V_HHS, V_INF, V_HP) ### worked well

dataset_final <- dataset1 %>% 
  select(TIME, V_GDP, V_COP, V_INF, V_HP, V_SP5)

### ORRRRRRR

dataset_final <- dataset1 %>% 
  select(TIME, V_FDI, V_HHS, V_GDP, V_PES, V_COP, V_IR, V_SP5, V_INF, V_HP)

#####
##### Split into training and test sets 
dataset <- dataset_final
dataset <- na.omit(dataset)
library(caTools)
set.seed(123)
split = sample.split(dataset$V_SP5, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
## Now I transition into proving the correlation and seeing which variables are worth keeping in my model. 
regressor = lm(formula = V_SP5 ~ .,
               data = dataset)
summary(regressor)

#Fitting XGBoost to the Training set make sure to change [-6] if your independent variable changes position.
#install.packages('xgboost')
library(xgboost)

classifier = xgboost(data = as.matrix(training_set[-6]),
                    label = training_set$V_SP5, nrounds = 250)

# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$V_SP5, k = 10)
cv = lapply(folds, function(x) {
  training_fold = dataset[-x, ]
  test_fold = dataset[x, ]
  classifier = xgboost(data = as.matrix(training_set[-6]), label = training_set$V_SP5, nrounds = 100)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-6]))
  y_pred <- data.frame(y_pred)
  accuracy = (mean((test_set$V_SP5 - abs(y_pred$y_pred - test_set$V_SP5))/test_set$V_SP5))
  
  return(accuracy)
})
accuracy = mean(as.numeric(cv))

########################### old way of validating
###########################
##### making our test_set predictions to compare with
y_pred = predict(classifier, newdata = as.matrix(test_set[-6]))
y_pred

accuracy = (mean((test_set$V_SP5 - abs(y_pred$y_pred - test_set$V_SP5))/test_set$V_SP5))
accuracy
# MAY 2023 For the Housing Prices wit all variables
# 527.2562 501.1008 547.2791 740.8076
# 480.4935 578.6827 476.9033 529.9952 665.8897
# Very Close

# AUG 2023 even closer now
# REAL 485.81 582.31 490.45 529.02 646.33
# MACHINE 480.4935 578.6827 476.9033 529.9952 665.8897

# prediction error gives us a number to understand how close our prediction is to the real value. It's best to have a lower value.
y_pred <- data.frame(y_pred)
mse <- mean((y_pred$y_pred - test_set$V_HP)^2)
print(mse)

# With the Year, GDP, Crude Oil Prices, House Hold Savings, Inflation = Housing Prices
# other then this there arent too many that predict each other. Or i need to wor on my model.

### So, I have done this twice now. I will see if I chance the year(-1) of house prices and see if the other four data points can predict the next year's house prices
### Next addition to this will do the code above and make the model better. Maybe I will find more correlations or insights.

# thanks