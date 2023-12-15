
library ("ggplot2")
library(dplyr)

options (scipen = 999, digits = 3)

data <-  read.csv('data/2015.csv')

data

state_code_01 <- data %>% filter(STATE_CODE == 01)
state_code_04 <- data %>% filter(STATE_CODE == 04)
state_code_01
state_code_04
compound_1 <- state_code_04 %>% filter(COMPOUND == "Atrazine")
compound_1

# Plotting the bar graph with a legend
ggplot(compound_1, aes(x = as.factor(COUNTY_CODE), y = HIGH_ESTIMATE, fill = COMPOUND)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "HIGH ESTIMATE BASED ON COMPOUND",
       x = "COUNTY CODE",
       y = "HIGH ESTIMATE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  guides(fill = guide_legend(title = "Compound"))  # Adding legend with title

compound_2 <- state_code_04 %>% filter(COMPOUND %in% c("Atrazine", "Azoxystrobin"))
compound_2
# Plotting the bar graph with a legend
ggplot(compound_2, aes(x = as.factor(COUNTY_CODE), y = HIGH_ESTIMATE, fill = COMPOUND)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(title = "HIGH ESTIMATE BASED ON COMPOUND (Stacked)",
       x = "COUNTY CODE",
       y = "HIGH ESTIMATE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  guides(fill = guide_legend(title = "Compound")) 




# Line graph
ggplot(compound_2, aes(x = as.factor(COUNTY_CODE), y = HIGH_ESTIMATE, color = COMPOUND, group = COMPOUND)) +
  geom_line() +
  labs(
    title = "HIGH ESTIMATE BASED ON COMPOUND (Line Graph)",
    x = "COUNTY CODE",
    y = "HIGH ESTIMATE"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  guides(color = guide_legend(title = "Compound"))



#heatmap

heatmap_data <- compound_2 %>%
  group_by(COMPOUND, COUNTY_CODE) %>%
  summarise(mean_high_estimate = mean(HIGH_ESTIMATE, na.rm = TRUE))

# Create a heatmap
ggplot(heatmap_data, aes(x = as.factor(COUNTY_CODE), y = COMPOUND, fill = mean_high_estimate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +  # Adjust the color scale
  labs(
    title = "High Estimate Heatmap",
    x = "County Code",
    y = "Compound"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))



 

  

# Grouping data by compound and county and calculating mean high estimate
compound_data <- compound_2 %>%
  group_by(COMPOUND, COUNTY_CODE) %>%
  summarise(mean_high_estimate = mean(HIGH_ESTIMATE, na.rm = TRUE)) %>%
  arrange(COUNTY_CODE)

# Creating a scatter plot for mean high estimates of different compounds across counties
ggplot(compound_data, aes(x = as.factor(COUNTY_CODE), y = mean_high_estimate, color = COMPOUND)) +
  geom_point(position = position_jitter(width = 0.3), size = 2) +
  labs(
    title = "Mean High Estimates of Compounds across Counties",
    x = "County Code",
    y = "Mean High Estimate"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


#violin garph

# Filtering the data to exclude NA values in HIGH_ESTIMATE
filtered_data <- compound_2[!is.na(compound_2$HIGH_ESTIMATE), ]

# Creating a violin plot for high estimates of different compounds within each county
ggplot(filtered_data, aes(x = as.factor(COMPOUND), y = HIGH_ESTIMATE, fill = COMPOUND)) +
  geom_violin() +
  labs(
    title = "Distribution of High Estimates for Compounds within Counties",
    x = "Compound",
    y = "High Estimate"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


#bar plot
# Grouping data by compound and county and calculating mean high estimate
compound_data <- compound_2 %>%
  group_by(COMPOUND, COUNTY_CODE) %>%
  summarise(mean_high_estimate = mean(HIGH_ESTIMATE, na.rm = TRUE)) %>%
  arrange(COUNTY_CODE)

# Creating a grouped bar plot for mean high estimates of different compounds across counties
ggplot(compound_data, aes(x = as.factor(COUNTY_CODE), y = mean_high_estimate, fill = COMPOUND)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Mean High Estimates of Compounds across Counties",
    x = "County Code",
    y = "Mean High Estimate"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))






# Grouping data by compound and county and calculating mean high estimate
compound_data <- compound_2 %>%
  group_by(COMPOUND, COUNTY_CODE) %>%
  summarise(mean_high_estimate = mean(HIGH_ESTIMATE, na.rm = TRUE)) %>%
  arrange(COUNTY_CODE)

# Creating a plot for mean high estimates of different compounds across counties
ggplot(compound_data, aes(x = as.factor(COUNTY_CODE), y = mean_high_estimate, color = COMPOUND)) +
  geom_line(aes(group = COMPOUND)) +
  geom_point() +
  labs(
    title = "Mean High Estimates of Compounds across Counties",
    x = "County Code",
    y = "Mean High Estimate"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


