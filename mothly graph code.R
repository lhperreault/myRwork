
run <- function() {
library(fpp2)
library(fma)
library(forecast)
library(expsmooth)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
library(lubridate)
  
setwd("~/Udemy/Segnasa")
  
db <- read_csv("export (1).csv")

db$`Fecha contratada`
plot(x = db$`Fecha contratada`, y = db$`Costo anual 2`)

# Convert the date column to a Date object

db$date_column <- as.Date(db$`Fecha contratada`)

# Create new columns for year, month, and day
db$year <- year(db$date_column)
db$month <- month(db$date_column)
db$day <- day(db$date_column)

# Grouping by year and month

dbgrouped <- db %>% group_by(year,month) %>%
  summarise(Count = n(), Total = sum(`Costo anual 2`))
head(dbgrouped)

dbgrouped2 <- db %>% group_by(month) %>%
  summarise(Count = n(), Total = sum(`Costo anual 2`))

# Assuming dbgrouped2 is your data frame
## new
# Calculate the percentage change
dbgrouped2$PercentageChange <- c(NA, diff(dbgrouped2$Total) / lag(dbgrouped2$Total)[-1]) * 100

# Create a vector to store the colors
colors <- ifelse(dbgrouped2$PercentageChange >= 0, "#00FF00", "#FF0000")  # Green and red color codes
colors[1] <- "#00FF00"  # First column is green

# Plot the graph with colored bars
p <- ggplot(dbgrouped2) +
  geom_col(aes(x = month, y = Total, fill = colors)) +
  geom_text(aes(x = month, y = Total,
                label = paste0(round(PercentageChange, 1), "%")),
            position = position_nudge(y = -220), size = 2.5, color = "white") +
  scale_y_continuous(breaks = seq(0, ceiling(max(dbgrouped2$Total, na.rm = TRUE)), by = 1000)) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Percentage Change", title = "Monthly Percentage Change")

# Display the plot
print(p)

#new too
p <- ggplot(dbgrouped2) +
  geom_col(aes(x = month, y = Total), fill = "black") +
  geom_text(data = subset(dbgrouped2, Total/sum(Total)*100 > 1),
            aes(x = month, y = Total, label = paste0(round(Total/sum(Total)*100, 1), "%")),
            position = position_stack(vjust = 0.8), size = 2.5, color = "white") +
  scale_y_continuous(breaks = seq(0, max(dbgrouped2$Total), by = 2000)) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Mes", y = "Total", title = "Monthly Totals")


# Convert the ggplot object to a plotly object
p <- ggplotly(p, tooltip = c("Total"))

# Show the interactive plot
p

library(plotly)
###new
# Calculate the percentage change
dbgrouped$PercentageChange <- c(NA, diff(dbgrouped$Total) / lag(dbgrouped$Total)[-1]) * 100

p <- ggplot(dbgrouped) +
  geom_col(aes(x = month, y = Total, fill = as.factor(year)), position = "dodge") +
  geom_text(aes(x = month, y = Total,
                label = paste0(round(PercentageChange, 1), "%")),
            position = position_nudge(y = -220), size = 2, color = "black") +  # Adjust the size and position of the labels
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(breaks = seq(0, max(dbgrouped$Total), by = 1000)) +
  labs(x = "Month", y = "Total", title = "Monthly Totals") +
  scale_fill_brewer(palette = "Set1", name = "Year") +
  theme_minimal()

# Convert the ggplot object to a plotly object
dbgrouped <- db %>% group_by(year,month) %>%
  summarise(Count = n(), Total = sum(`Costo anual 2`))

dbgrouped$PercentageChange <- c(NA, diff(dbgrouped$Total) / lag(dbgrouped$Total)[-1]) * 100

dbgrouped <- dbgrouped %>%
  arrange(year, month)

# Plot the data
p <- ggplot(dbgrouped) +
  geom_col(aes(x = interaction(year, month, lex.order = TRUE), y = Total, fill = as.factor(year)), position = "dodge") +
  geom_text(aes(x = interaction(year, month, lex.order = TRUE), y = Total,
                label = paste0(round(PercentageChange, 1), "%")),
            position = position_nudge(y = 220), size = 2, color = "black") +
  scale_y_continuous(labels = function(y) sprintf("%s %s", month.abb[as.numeric(y) %% 1 * 1000], floor(as.numeric(y))), expand = c(0, 0)) +
  scale_x_discrete(breaks = unique(interaction(dbgrouped$year, dbgrouped$month, lex.order = TRUE))) +
  labs(x = "Month", y = "Total", title = "Monthly Totals") +
  scale_fill_brewer(palette = "Set1", name = "Year") +
  coord_flip() +
  theme_minimal()

print(p)
# Show the interactive plot
p

}

run()

# Specify the file path
file_path <- "export (1).csv"

# Check if the file exists before attempting to remove it
if (file.exists(file_path)) {
  # Remove the file
  file.remove(file_path)
  cat("File", file_path, "has been deleted.\n")
} else {
  cat("File", file_path, "does not exist.\n")
}




ggplot(dbgrouped) +
  geom_col(aes(x = month, y = Total), fill = "black") +
  scale_y_continuous(breaks = seq(0, max(dbgrouped$Total), by = 2000))+
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Mes", y = "Total", title = "Monthly Totals")

ggplot(dbgrouped) +
  geom_col(aes(x = month, y = Total, fill = as.factor(year)), position = "dodge") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(breaks = seq(0, max(dbgrouped$Total), by = 1000)) +
  labs(x = "Month", y = "Total", title = "Monthly Totals") +
  scale_fill_brewer(palette = "Set1", name = "Year") +
  theme_minimal()

library(plotly)
p <- ggplot(dbgrouped) +
  geom_col(aes(x = month, y = Total, fill = as.factor(year)), position = "dodge") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(breaks = seq(0, max(dbgrouped$Total), by = 1000)) +
  labs(x = "Month", y = "Total", title = "Monthly Totals") +
  scale_fill_brewer(palette = "Set1", name = "Year") +
  theme_minimal()

# Convert the ggplot object to a plotly object
p <- ggplotly(p, tooltip = c("Total"))

# Show the interactive plot
p
  `install.packages('rsconnect')
install.packages("rsconnect")
library(rsconnect)

rsconnect::setAccountInfo(name='e8zkdg-luke-perreault',
                          token='D4441F336384FA15419E6B3C21B95DAF',
                          secret='<SECRET>')
rsconnect::setAccountInfo(name='e8zkdg-luke-perreault',
                          token='D4441F336384FA15419E6B3C21B95DAF',
                          secret='3rg1YNpqgedEFMwAoSz7/gqKA8wiLJksrzteLb/h')
rsconnect::deployApp('~/Udemy/Segnasa/plot2colors.html')

rsconnect::deployApp('~/Udemy/Segnasa/plot2colors.html')