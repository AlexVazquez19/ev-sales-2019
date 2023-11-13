## Set Up ----

# load libraries
library(ggplot2)
library(GGally)
library(tidyr)
library(dplyr)
library(fixest)
library(fastDummies)

# clear environment
rm(list = ls())

# set working directory
setwd("/Users/alejandrovazquez/Desktop/econ121/ev-sales-2019")

# load data
evsales <- read.csv("ev_sales_data_2019.csv")

# verify data types are what they should be
sapply(evsales, class)

# generate dummy variable for power_type (EV/PHEV)
evsales <- 
  evsales %>% 
  mutate(power_type_d = case_when(
    (power_type=="EV") ~ 1,
    (power_type=="PHEV") ~ 0))

# generate dummy variables for vehicle_type
evsales <- dummy_cols(evsales, select_columns = "vehicle_type")

## Exploratory Analysis ----

# view the vehicle manufacturers are represented in this dataset
unique_makes <- unique(evsales$make)
print(unique_makes)

# view the top 5 vehicles by 2019 sales
sorted_data <- evsales[order(-evsales$total_sales_2019), ]
top_5 <- head(sorted_data, 5)
top_5
  # We can already see that the Tesla Model 3 may be an outlier by sales

# view the bottom 5 vehicles by 2019 sales
bottom_5 <- tail(sorted_data, 5)
bottom_5

# create a bar chart to show the vehicle types in our dataset
vehicle_counts <- evsales %>% 
  group_by(vehicle_type) %>% 
  summarise(count = n())
ggplot(vehicle_counts, aes(x=vehicle_type, y=count)) +
  geom_bar(stat="identity", fill="darkblue") +
  labs(title="Bar Plot of Vehicle Types", x="Vehicle Type", y="Count") +
  theme_minimal()

# We must now remove outliers so we can focus on the main trends in the majority
# of the data in our scatterplot matrix.

# create box plot for total sales 2019
ggplot(evsales, aes(x="", y=total_sales_2019)) +
  geom_boxplot(fill="lightblue") +
  labs(title="Boxplot of Total Sales 2019", y="Total Sales", x="") +
  theme_minimal()
  # It seems that there is an outlier with extremely high sales (Tesla Model 3)

# lets remove the sales outlier and view the boxplot again
evsales_no_outliers <- evsales %>% filter(total_sales_2019 < 150000)
ggplot(evsales_no_outliers, aes(x="", y=total_sales_2019)) +
  geom_boxplot(fill="lightblue") +
  labs(title="Boxplot of Total Sales 2019", y="Total Sales", x="") +
  theme_minimal()

# create box plots for horsepower and torque
long_data_hp <- gather(evsales, key="variable", value="value", horsepower, torque)
ggplot(long_data_hp, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title="Comparison of Sales, Range, and MSRP", y="Value", x="Variable")

# create box plot for base MSRP
ggplot(evsales, aes(x="", y=base_msrp)) +
  geom_boxplot(fill="lightgreen") +
  labs(title="Boxplot of Base MSRP", y="Base MSRP", x="") +
  theme_minimal()
  # It seems there is an outlier for price

# lets remove the MSRP outlier and view the boxplot again
evsales_no_outliers <- evsales_no_outliers %>% filter(base_msrp < 140000)
ggplot(evsales_no_outliers, aes(x="", y=base_msrp)) +
  geom_boxplot(fill="lightgreen") +
  labs(title="Boxplot of Base MSRP", y="Base MSRP", x="") +
  theme_minimal()

# create box plot for charge time at 220v
ggplot(evsales, aes(x="", y=charge_time_220v)) +
  geom_boxplot(fill="darkgreen") +
  labs(title="Boxplot of Charge Time at 220V", y="Charge Time (Hrs)", x="") +
  theme_minimal()

# create box plot for combined MPGe
ggplot(evsales, aes(x="", y=mpge_combined)) +
  geom_boxplot(fill="cadetblue") +
  labs(title="Boxplot of Combined MPGe", y="Combined MPGe", x="") +
  theme_minimal()

# split data based on EV or PHEV
ev <- subset(evsales_no_outliers, power_type == "EV")
phev <- subset(evsales_no_outliers, power_type == "PHEV")

# Create scatterplot matrix to identify relationships between variables
evsales_subset <- ev[, c("total_sales_2019", "max_range", 
                                          "battery_cap_kwh", "charge_time_110v", 
                                          "charge_time_220v", "mpge_combined")]
ggpairs(evsales_subset)



model <- feols(total_sales_2019 ~ power_type_d + vehicle_type + base_msrp + max_range, 
                        data = evsales_no_outliers, 
                        vcov = 'hetero')
print(model)

