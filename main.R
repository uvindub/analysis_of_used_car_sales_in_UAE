###1) Data Acquisition

##1.1 Install & Load Required Packages
install.packages("tidyverse")
library(tidyverse)
library(scales)
library(ggplot2)
library(reshape2)

##1.2 Import the Dataset into R
car_sales <- read_csv("C:/Users/kgubi/OneDrive/Desktop/AS474_Project/AS474_Final_Project_Group1/data/Dubizzle Used Car Sales.csv")
attach(car_sales)

###############################################################################################

###2)	Exploratory Data Analysis (EDA)

##2.1 Explore the Dataset
str(car_sales)
names(car_sales)
head(car_sales)


##2.2 heck unique values of each variable and convert characters to factors
unique_values <- unique(car_sales$body_condition)
print(unique_values)

car_sales$body_condition <- as.factor(car_sales$body_condition)

unique_values <- unique(car_sales$mechanical_condition)
print(unique_values)

car_sales$mechanical_condition <- as.factor(car_sales$mechanical_condition)

unique_values <- unique(car_sales$seller_type)
print(unique_values)

car_sales$seller_type <- as.factor(car_sales$seller_type)

unique_values <- unique(car_sales$body_type)
print(unique_values)

car_sales$body_type <- as.factor(car_sales$body_type)

unique_values <- unique(car_sales$no_of_cylinders)
print(unique_values)

car_sales$no_of_cylinders <- as.factor(car_sales$no_of_cylinders)

unique_values <- unique(car_sales$transmission_type)
print(unique_values)

car_sales$transmission_type <- as.factor(car_sales$transmission_type)

unique_values <- unique(car_sales$regional_specs)
print(unique_values)

car_sales$regional_specs <- as.factor(car_sales$regional_specs)

unique_values <- unique(car_sales$horsepower)
print(unique_values)

car_sales$horsepower <- as.factor(car_sales$horsepower)

unique_values <- unique(car_sales$fuel_type)
print(unique_values)

car_sales$fuel_type <- as.factor(car_sales$fuel_type)

unique_values <- unique(car_sales$steering_side)
print(unique_values)

car_sales$steering_side <- as.factor(car_sales$steering_side)

unique_values <- unique(car_sales$year)
print(unique_values)

car_sales$year <- as.factor(car_sales$year)

unique_values <- unique(car_sales$color)
print(unique_values)

car_sales$color <- as.factor(car_sales$color)

unique_values <- unique(car_sales$emirate)
print(unique_values)

car_sales$emirate <- as.factor(car_sales$emirate)

unique_values <- unique(car_sales$motors_trim)
print(unique_values)

car_sales$motors_trim <- as.factor(car_sales$motors_trim)

unique_values <- unique(car_sales$company)
print(unique_values)

car_sales$company <- as.factor(car_sales$company)

unique_values <- unique(car_sales$model)
print(unique_values)

car_sales$model <- as.factor(car_sales$model)


str(car_sales)


##2.3 Rename values in transmission_type & steering_side columns
car_sales <- car_sales %>%
  mutate(transmission_type = recode(transmission_type, 
                                    "Manual Transmission" = "Manual",
                                    "Automatic Transmission" = "Auto"))

car_sales <- car_sales %>%
  mutate(steering_side = recode(steering_side, 
                                "Left Hand Side" = "Left",
                                "Right Hand Side" = "Right"))

##2.4 Capitalize first letter in company column
car_sales$company <- str_to_title(car_sales$company)

##2.5 Check and Remove Missing values & Duplicate Values

#Count the number of missing values per variable
colSums(is.na(car_sales))

#Count the incomplete data rows
sum(!complete.cases(car_sales))

#Remove rows with missing values
car_sales <- na.omit(car_sales)

#Identify duplicated rows
duplicated_rows <- duplicated(car_sales)

#Count the number of duplicated rows
sum(duplicated_rows)

#Remove duplicated rows
car_sales <- car_sales[!duplicated_rows, ]

#Calculate the number of rows contaning "Unknown" values in any variable
sum(apply(car_sales == "Unknown", 1, any))


#Calculate the number of rows contaning "" values in any variable
sum(apply(car_sales == "None", 1, any))

# Remove rows with "Unknown" & "None" values in any variable
car_sales <- car_sales %>%  
  filter(across(everything(), ~.x != "Unknown" & .x != "None"))

##2.6 Summary Statistics of Numeric Variables
summary(car_sales[, c("price_in_aed", "kilometers")])




##2.7 Data Visualization


#2.7.1.Bar Chart - Distribution of Sales by Body Condition

ggplot(data = car_sales, aes(x = body_condition, fill = ..count..)) +
  geom_bar(fill = "#9769AE") +
  xlab("Body Condition") +
  ylab("No of Sales") +
  ggtitle("Distribution of Sales by Body Condition") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank()
  )

#2.7.2.Bar Chart - Distribution of Sales by Mechanical Condition

ggplot(data = car_sales, aes(x = mechanical_condition, fill = ..count..)) +
  geom_bar(fill = "#9769AE") +
  xlab("Mechanical Condition") +
  ylab("No of Sales") +
  ggtitle("Distribution of Sales by Mechanical Condition") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank()
  )
 

#2.7.3.Bar Chart - Distribution of Sales by Seller Types

ggplot(data = car_sales, aes(x = seller_type, fill = ..count..)) +
  geom_bar(fill = "#9769AE") +
  xlab("Seller Type") +
  ylab("No of Sales") +
  ggtitle("Distribution of Sales by Seller Types") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank()
  )

#2.7.4.Bar Chart - Distribution of Sales by Body Types

ggplot(data = car_sales, aes(x = body_type, fill = ..count..)) +
  geom_bar(fill = "#9769AE") +
  xlab("Body Type") +
  ylab("No of Sales") +
  ggtitle("Distribution of Sales by Body Types") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank())

#2.7.5.Bar Chart - Distribution of Sales by Cylinder Count

cylinder_levels <- c("3", "4", "5","6","8","10","12")  
car_sales$no_of_cylinders <- factor(car_sales$no_of_cylinders, levels = cylinder_levels)


ggplot(data = car_sales, aes(x = no_of_cylinders, fill = ..count..)) +
  geom_bar(fill = "#9769AE") +
  xlab("No of Cylinders") +
  ylab("No of Sales") +
  ggtitle("Distribution of Sales by Cylinder Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank()
  )

#2.7.6.Bar Chart - Distribution of Sales by Transmission Types

ggplot(data = car_sales, aes(x = transmission_type, fill = ..count..)) +
  geom_bar(fill = "#9769AE") +
  xlab("Transmission Type") +
  ylab("No of Sales") +
  ggtitle("Distribution of Sales by Transmission Types") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank())

#2.7.7.Bar Chart - Distribution of Sales by Horsepower

ggplot(data = car_sales, aes(x = horsepower, fill = ..count..)) +
  geom_bar(fill = "#9769AE") +
  xlab("Horsepower") +
  ylab("No of Sales") +
  ggtitle("Distribution of Sales by Horsepower") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank())

#2.7.8.Bar Chart - Distribution of Sales by Fuel Type

ggplot(data = car_sales, aes(x = fuel_type, fill = ..count..)) +
  geom_bar(fill = "#9769AE") +
  xlab("Fuel Tyoe") +
  ylab("No of Sales") +
  ggtitle("Distribution of Sales by Fuel Type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank())

#2.7.9.Bar Chart - Distribution of Sales by Body Color

ggplot(data = car_sales, aes(x = color, fill = ..count..)) +
  geom_bar(fill = "#9769AE") +
  xlab("Body Color") +
  ylab("No of Sales") +
  ggtitle("Distribution of Sales by Body Color") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank())

#2.7.10.Bar Chart - Distribution of Sales by Steering Side

ggplot(data = car_sales, aes(x = steering_side, fill = ..count..)) +
  geom_bar(fill = "#9769AE") +
  xlab("Steering Side") +
  ylab("No of Sales") +
  ggtitle("Distribution of Sales by Steering Side") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank())

#2.7.11.Bar Chart - Distribution of Sales by Regional Specification

regional_counts <- count(car_sales, regional_specs)
regional_counts$percentage <- regional_counts$n / sum(regional_counts$n) * 100

ggplot(data = regional_counts, aes(x = n, y = regional_specs, fill = percentage)) +
  geom_col(fill = "#9769AE") +
  ylab("Regional Specification") +
  xlab("No of Sales") +
  ggtitle("Distribution of Sales by Regional Specification") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5))


#2.7.12.Bar Chart - Distribution of Sales by Emirates

emirate_counts <- count(car_sales, emirate)
emirate_counts$percentage <- emirate_counts$n / sum(emirate_counts$n) * 100

ggplot(data = emirate_counts, aes(x = n, y = emirate, fill = percentage)) +
  geom_col(fill = "#9769AE") +
  xlab("Emirate") +
  ylab("No of Sales") +
  ggtitle("Distribution of Sales by Emirates") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5))


#2.7.13.Bar Chart - Distribution of Sales by Top 10 Motor Trim Types

top_motor_trims <- car_sales %>%
  count(motors_trim) %>%
  top_n(10)

top_motor_trims$percentage <- top_motor_trims$n / sum(top_motor_trims$n) * 100

ggplot(data = top_motor_trims, aes(x = n, y = reorder(motors_trim, n), fill = percentage)) +
  geom_col(fill = "#9769AE") +
  ylab("Motor Trim Types") +
  xlab("No of Sales") +
  ggtitle("Distribution of Sales by Top 10 Motor Trim Types") +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5))

#2.7.14.Bar Chart - Distribution of Sales by Top 10 Manufacturers

top_manufacturers <- car_sales %>%
  count(company) %>%
  top_n(10) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(data = top_manufacturers, aes(x = n, y = reorder(company, n), fill = percentage)) +
  geom_col(fill = "#9769AE") +
  ylab("Car Manufacturer") +
  xlab("No of Sales") +
  ggtitle("Distribution of Sales by Top 10 Manufacturers") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5))


#2.7.15.Histogram - Price Distribution

ggplot(data = car_sales, aes(x = price_in_aed)) +
  geom_histogram(binwidth = 5000, fill = "#000000", color = "#ff726f", alpha = 0.8) +
  xlab("Price (AED)") +
  ylab("Count") +
  ggtitle("Distribution of Car Prices") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_line(color = "#d3d3d3")) +
  scale_x_continuous(labels = scales::comma)

#Removing outliers

q1 <- quantile(car_sales$price_in_aed, 0.25)
q3 <- quantile(car_sales$price_in_aed, 0.75)
iqr <- q3 - q1

lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

car_sales_1 <- car_sales %>%
  filter(price_in_aed >= lower_bound & price_in_aed <= upper_bound)

# Calculate mean and median
mean_price <- mean(car_sales_1$price_in_aed)
median_price <- median(car_sales_1$price_in_aed)

#Histogram without outliers
ggplot(data = car_sales_1, aes(x = price_in_aed)) +
  geom_histogram(binwidth = 5000, fill = "#EAE1EE", color = "#9769AE", alpha = 0.8) +
  xlab("Price (AED)") +
  ylab("Frequency") +
  ggtitle("Distribution of Car Prices(Outliers Removed)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#d3d3d3")
  ) +
  scale_x_continuous(labels = comma) +
  geom_vline(xintercept = mean_price, color = "#FF5733", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_price, color = "#2980B9", linetype = "dotted", size = 1) +
  annotate("text", x = mean_price, y = 10, label = "Mean", color = "#FF5733", size = 4) +
  annotate("text", x = median_price, y = 10, label = "Median", color = "#2980B9", size = 4)

#2.7.16. Scatter Plot - Price vs. Kilometer

ggplot(data = car_sales_1, aes(x = kilometers, y = price_in_aed)) +
  geom_point(color = "#ff726f", alpha = 0.8, size = 3) +
  xlab("Kilometer") +
  ylab("Price (AED)") +
  ggtitle("Price vs. Kilometer") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#d3d3d3"),
    panel.background = element_rect(fill = "#EAE1EE")
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

# Calculate the lower and upper bounds for outliers using the IQR method
kilometer_lower <- quantile(car_sales_1$kilometers, 0.25) - 1.5 * IQR(car_sales_1$kilometers)
kilometer_upper <- quantile(car_sales_1$kilometers, 0.75) + 1.5 * IQR(car_sales_1$kilometers)


# Scatter Plot without outliers
ggplot(data = car_sales_2, aes(x = kilometers, y = price_in_aed)) +
  geom_point(color = "#9769AE", alpha = 0.8, size = 3) +
  geom_smooth(method = "lm", color = "#FF5733", se = FALSE) +  # Add the trend line here
  xlab("Kilometer") +
  ylab("Price (AED)") +
  ggtitle("Price vs. Kilometer (Outliers Removed)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#d3d3d3"),
    panel.background = element_rect(fill = "#EAE1EE")
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)


#2.7.17.Box Plot - Price by Body Type

ggplot(data = car_sales_2, aes(x = body_type, y = price_in_aed)) +
  geom_boxplot(fill = "#9769AE", color = "#000000", alpha = 0.8, outlier.shape = NA) +
  xlab("Body Type") +
  ylab("Price (AED)") +
  ggtitle("Price by Body Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#d3d3d3"),
    panel.background = element_rect(fill = "#EAE1EE")
  )+ scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


#2.7.18.Box Plot - Price by Fuel Type:

ggplot(data = car_sales_2, aes(x = fuel_type, y = price_in_aed)) +
  geom_boxplot(fill = "#9769AE", color = "#000000", alpha = 0.8, outlier.shape = NA) +
  xlab("Fuel Type") +
  ylab("Price (AED)") +
  ggtitle("Price by Fuel Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#d3d3d3"),
    panel.background = element_rect(fill = "#EAE1EE")
  )+ scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

#2.7.19. Box Plot - Price by Cylinder Count

ggplot(data = car_sales_2, aes(x = no_of_cylinders, y = price_in_aed)) +
  geom_boxplot(fill = "#9769AE", color = "#000000", alpha = 0.8, outlier.shape = NA) +
  xlab("Number of Cylinders") +
  ylab("Price (AED)") +
  ggtitle("Price by Cylinder Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#d3d3d3"),
    panel.background = element_rect(fill = "#EAE1EE")
  )+ scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

#2.7.20 Box Plot - Price by Horsepower

ggplot(data = car_sales_2, aes(x = horsepower, y = price_in_aed)) +
  geom_boxplot(fill = "#9769AE", color = "#000000", alpha = 0.8, outlier.shape = NA) +
  xlab("Horsepower") +
  ylab("Price (AED)") +
  ggtitle("Price by Horsepower") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#d3d3d3"),
    panel.background = element_rect(fill = "#EAE1EE")
  )+ scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

#2.7.21. Stacked Bar Chart- Distribution of Sales by Body Type & Number of Cylinders

ggplot(data = car_sales_2, aes(x = body_type, fill = as.factor(no_of_cylinders))) +
  geom_bar(position = "stack") +
  xlab("Body Type") +
  ylab("No of Sales") +
  ggtitle("Distribution of Sales by Body Type & Number of Cylinders") +
  scale_fill_viridis_d(name = "Cylinder Count" , option = "turbo") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "right"
  )

#2.7.22. Line Chart- Distribution of Average Price by Manufaturing Year(2000 to 2022) & Body Type

average_price_by_year_body <- car_sales_2 %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= 2000 & year <= 2022) %>%
  group_by(year, body_type) %>%
  summarise(avg_price = mean(price_in_aed), .groups = "drop")

ggplot(data = average_price_by_year_body, aes(x = year, y = avg_price, color = body_type)) +
  geom_point() +
  geom_line() +
  xlab("Manufacturing Year") +
  ylab("Average Price (AED)") +
  ggtitle("Distribution of Average Price by Manufaturing Year(2000 to 2022) & Body Type") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                "#aec7e8", "#ffbb78", "#98df8a")) +
  scale_x_continuous(
    breaks = seq(min(average_price_by_year_body$year), max(average_price_by_year_body$year), by = 1)
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank()
  )


#2.7.23. Line Chart- Distribution of Average Price by Regional Specifications & Horsepower

average_price_by_regional_spec_horsepower <- car_sales_2 %>%
  group_by(regional_specs, horsepower) %>%
  summarise(avg_price = mean(price_in_aed), .groups = "drop")

ggplot(data = average_price_by_regional_spec_horsepower, aes(x = regional_specs, y = avg_price, color = horsepower, group = horsepower)) +
  geom_point() +
  geom_line() +
  xlab("Regional Specification") +
  ylab("Average Price (AED)") +
  ggtitle("Distribution of Average Price by Regional Specifications & Horsepower") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                "#aec7e8", "#ffbb78", "#98df8a")) +
  scale_x_discrete() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank()
  )

#2.7.24. Distribution of Average Price by Top 10 Manufactures & Color

top_10_manufacturers <- car_sales_2 %>%
  group_by(company) %>%
  summarise(count = n()) %>%
  top_n(10, count)

average_price_by_manufactures_color <- car_sales_2 %>%
  filter(company %in% top_10_manufacturers$company) %>%
  group_by(company, color) %>%
  summarise(avg_price = mean(price_in_aed), .groups = "drop")

ggplot(data = average_price_by_manufactures_color, aes(x = company, y = avg_price, color = color, group = color)) +
  geom_point() +
  geom_line() +
  xlab("Manufacturers") +
  ylab("Average Price (AED)") +
  ggtitle("Distribution of Average Price by Top 10 Manufactures & Color") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
                                "#c49c94", "#dbdb8d")) +
  scale_x_discrete() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank()
  )

#2.7.25. Grouped Bar chart- Distribution of Sales by Top 10 Manufactures & Seller Type
top_10_manufacturers <- car_sales_2 %>%
  group_by(company) %>%
  summarise(count = n()) %>%
  top_n(10, count)

count_by_manufacturers_seller <- car_sales_2 %>%
  filter(company %in% top_10_manufacturers$company) %>%
  group_by(company, seller_type) %>%
  summarise(count = n(), .groups = "drop")

ggplot(data = count_by_manufacturers_seller, aes(x = company, y = count, fill = seller_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Manufacturers") +
  ylab("No of Sales") +
  ggtitle("") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_blank()
  )

##2.8. Correlation Analysis


#2.8.1 Calculate correlations with numeric variables

car_sales_2$horsepower_numeric <- as.numeric(as.factor(car_sales_2$horsepower))
car_sales_2$no_of_cylinders_numeric <- as.numeric(as.factor(car_sales_2$no_of_cylinders))

correlation_matrix <- cor(car_sales_2[, c("price_in_aed", "kilometers", "horsepower_numeric","no_of_cylinders_numeric")])
print(correlation_matrix)

# Convert correlation matrix to long format
melted_matrix <- melt(correlation_matrix)
melted_matrix
# Create heatmap
ggplot(data = melted_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#f6edf6", mid = "#dda0dd", high = "#300030",
    midpoint = 0, na.value = "gray",
    guide = guide_colorbar(
      barwidth = 1.8, barheight = 8, title.position = "top",
      title.hjust = 0.2, title.vjust = 2
    )
  ) +
  xlab("") +
  ylab("") +
  ggtitle("Correlation Matrix Heatmap") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

#2.8.2 Calculate correlations with categorical variable

#Check significant association between Seller type & Body tpye
contingency_table <- table(car_sales_2$seller_type, car_sales_2$body_type)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

#Check significant association between Regional Specs & No of cylinders
contingency_table <- table(car_sales_2$regional_specs, car_sales_2$no_of_cylinders)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

#Check significant association between Emirate & Manufacturer
contingency_table <- table(car_sales_2$emirate, car_sales_2$company)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

#Check significant association between Color & Fuel Type
contingency_table <- table(car_sales_2$color, car_sales_2$fuel_type)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

#Check significant association between Body Condition vs Mechanical Condition
contingency_table <- table(car_sales_2$body_condition, car_sales_2$mechanical_condition)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)



##########################################################################################

###3.Statistical Modeling

##3.1 Build the Regression Model

model_1 <- lm(price_in_aed ~ kilometers + body_condition + mechanical_condition + seller_type + body_type +
               no_of_cylinders + transmission_type + regional_specs + horsepower + fuel_type +
               steering_side + year + color + emirate + motors_trim + company + model + date_posted, data = car_sales_2)

##3.2 Assess Model Performance
anova(model_1)
model_1_anova <- anova(model_1)
write.csv(model_1_anova, file = "model_1_anova.csv", row.names = TRUE)
summary(model_1)

model_2 <- lm(price_in_aed ~ kilometers + body_condition + seller_type + body_type +
                no_of_cylinders + transmission_type + regional_specs + horsepower + fuel_type +
                steering_side + year + color + emirate + motors_trim + company + model + date_posted, data = car_sales_2)

anova(model_2)
model_2_anova <- anova(model_2)
write.csv(model_2_anova, file = "model_2_anova.csv", row.names = TRUE)
summary(model_2)

##3.3 Residual Analysis

res <- model_2$residuals

#1. Shapiro-Wilk normality test
shapiro.test(res) 

length(res) #cannot do the shapiro test since the sample size is greater than 5000

#2. Create a QQ plot of residuals to check whether "Residuals are normally distributed". 
par(mar = c(5, 5, 4, 2) + 0.1)
plot(qqnorm(res), main = "Normal Q-Q Plot for Residuals", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", pch = 16, col = "steelblue")
qqline(res, col = "red", lwd = 2)
abline(h = quantile(res, c(0.25, 0.75)), lty = 2, col = "gray")
legend("topleft", legend = "Reference Line", col = "red", lwd = 2, bty = "n")

#3. Find mean of residuals to check whether "Residuals have a zero mean".
mean(res)

#4. Create Residuals vs Fitted values plot to check whether "Residuals have a constant variance".
par(mar = c(5, 5, 4, 2) + 0.1)
plot(pred, res, main = "Residuals vs. Fitted Values", xlab = "Fitted Values", ylab = "Residuals", pch = 16, col = "steelblue")
abline(h = 0, col = "red", lwd = 2)
abline(v = 0, col = "blue", lwd = 2)
legend("topright", legend = c("Residuals", "y = 0", "x = 0"), col = c("steelblue", "red", "blue"), lwd = 2, bty = "n")

#5. Create Residuals vs Order plot to check whether "Residuals are independently distributed".
par(mar = c(5, 5, 4, 2) + 0.1)
plot( res, main = "Residuals vs. Order", xlab = "Order", ylab = "Residuals", pch = 16, col = "steelblue")
abline(h = 0, col = "red", lwd = 2)
legend("topright", legend = c("Residuals", "y = 0"), col = c("steelblue", "red"), lwd = 2, bty = "n")

#6. To check prediction accuracy
#Calculate Mean Absolute Error (MAE)
mean(abs(pred - car_sales_2$price_in_aed))
MAE <- mean(abs(pred - car_sales_2$price_in_aed))

#Calculate Root Mean Squared Error (RMSE)
sqrt(mean((pred - car_sales_2$price_in_aed)^2))
RMSE <- sqrt(mean((pred - car_sales_2$price_in_aed)^2))

# Print the MAE and RMSE
cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")


