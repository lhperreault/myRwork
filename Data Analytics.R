rm(list=ls())
graphics.off()

library(ggplot2)
library(dplyr)
library(tidyverse)

library(readxl)
Pressure_Washing_2020_Data_2_ <- read_excel("Pressure Washing 2020 Data (2).xlsx", 
                                            col_types = c("skip", "text", "date", 
                                                          "numeric", "text", "date", "numeric", 
                                                          "numeric", "text", "text", "date", 
                                                          "numeric", "text", "numeric"))
View(Pressure_Washing_2020_Data_2_)

PW <- Pressure_Washing_2020_Data_2_

PW$Date_Subscribed

  
 ## for fun
Assetf <- PW %>% 
  filter( Asset == "Gas" , Asset_Cost > 30, Asset_Cost < 90)

ggplot ( data = Assetf) + 
  geom_boxplot(aes(x = Asset , y = Asset_Cost))
mean(Assetf$Asset_Cost)                     
 ### average cost to fill up my car everytime is $64
 
 ### adding expenses ###

PW <- PW %>% 
  mutate(Expenses = ifelse(Asset == "Gas" | Asset == "bleach", Asset, NA ))
PW <- PW %>% 
 mutate(Expenses_Cost = ifelse(Asset == "Gas" | Asset == "bleach", Asset_Cost, NA))

 ### changing assets ###

PW <- PW %>% 
  mutate(Asset = ifelse(Asset != "Gas" & Asset != "bleach", Asset, NA  ))
PW <- PW %>% 
  mutate(Asset_Cost = ifelse(Asset != "Gas" & Asset != "bleach", Asset_Cost, NA  ))
 ### totals ### 
Totals <- function(x) {
  sumy <- sum(x, na.rm = TRUE)
  sumy
}

# Total for marketing cost is 1254.36 
Marketing_Expense <- (Totals(PW$Marketing_Cost))

# Total for Asset Cost is 1307.7
Filter <- PW %>% filter(Asset != 'Pressure Washer 3400', Asset != "car", Asset != "Ladder", Asset != "second pressure washer"	
)
Net_Asset_Expenses <- Totals(Filter$Asset_Cost)

# Total in Expenses is 1595.79
Net_Expenses <- Totals(PW$Expenses_Cost)

# Total of just gas is 933.24
Filter_2 <- PW %>% filter(Expenses == "Gas")
Net_Gas_Expense <- Totals(Filter_2$Expenses_Cost)

# Total of bleach purchased is 662.55
Filter_3 <- PW %>% filter(Expenses == "bleach")
Net_Bleach_Expense <- Totals(Filter_3$Expenses_Cost)

#### Operational Expenses (Asset Cost + Marketing Cost + Operational Expenses) is 4157.85
Operational_Expenses <- sum(Totals(Filter$Asset_Cost) + (Totals(PW$Marketing_Cost) + Totals(PW$Expenses_Cost)))

# Total Revenue is 19,795
Net_Revenue <- Totals(PW$Price)

# Gross Profit ( revenue - expenses) is 18,199.21
Gross_Profit <- sum(Totals(PW$Price) - Totals(PW$Expenses_Cost))

# Operational Profit (Revenue - operational expenses) is 15,637.15
Operational_Income <- sum(Totals(PW$Price) - Operational_Expenses)
Net_Income <- Operational_Income

# Equity that I had in physical assets prior to 2022 spring is 1495
Filter_4 <- PW %>% filter(Asset == 'Pressure Washer 3400'| Asset == "Ladder"| Asset == "second pressure washer"	)
Totals(Filter_4$Asset_Cost)

#Equity total at the end of the season 
 
### Income Statement ###
Description <- c("Net_Revenue", "Gross_Profit", "Operational_Income", "Net_Income")
Value <- c(Net_Revenue, Gross_Profit, Operational_Income, Net_Income )
Income_Statement <- data.frame(Description, Value)

## Net_Revenue 19795.00 Gross_Profit 18199.21 Operational_Income 15637.15 Net_Income 15637.15

# Exporting Some new or edited data sets for the business excel sheets #
install.packages("writexl")
library("writexl")
write_xlsx(Income_Statement,"Income_Statement.xlsx")
write_xlsx(PW,"PressureWashing2022.xlsx")

