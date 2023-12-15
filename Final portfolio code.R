library ("ggplot2")
library(dplyr)

options (scipen = 999, digits = 3)

data1 <-  read.csv('data/2015.csv')
data2 <-  read.csv('data/2014.csv')



# Merge datasets based on specified attributes
merged_data <- inner_join(data1, data2, 
                          by = c("COMPOUND", "YEAR", "STATE_CODE", "COUNTY_CODE", "LOW_ESTIMATE", "HIGH_ESTIMATE"))


# Merging data1 and data2 vertically
merged_data <- rbind(data1, data2)


# Remove rows with missing or non-numeric values in LOW_ESTIMATE and HIGH_ESTIMATE columns
cleaned_merged_data <- merged_data[!is.na(merged_data$LOW_ESTIMATE) & is.finite(merged_data$LOW_ESTIMATE) &
                                     !is.na(merged_data$HIGH_ESTIMATE) & is.finite(merged_data$HIGH_ESTIMATE), ]

# Extracting data from cleaned merged_data
x <- cleaned_merged_data$STATE_CODE  # State_Code

yl <- cleaned_merged_data$LOW_ESTIMATE  # LOW_ESTIMATE
yl <- yl[!is.na(yl) & is.finite(yl)]

# Set ylim based on valid LOW_ESTIMATE values
ylimits <- range(yl)

#Graph 1

# Create a blank plot with valid LOW_ESTIMATE values
plot(x, yl, xlab = "State_Code", ylab = "Usage pesticide-use", 
     main = "Pesticide-use in all states in year 2014 - LOW_ESTIMATE", 
     col = "blue", pch = 16, xlim = c(0, max(x)), ylim = ylimits, yaxt = "n")
axis(2, at = seq(0, ceiling(ylimits[2]), by = 200000), las = 1)

yh <- cleaned_merged_data$HIGH_ESTIMATE  # HIGH_ESTIMATE
yh <- yh[!is.na(yh) & is.finite(yh)]  # Remove NAs and non-finite values

# Set ylim based on valid HIGH_ESTIMATE values
ylimits <- range(yh)

# Add points for HIGH_ESTIMATE values
points(x, yh, col = "red", pch = 4)

# Add legend
legend("topright", legend = c("LOW_ESTIMATE", "HIGH_ESTIMATE"), col = c("blue", "red"), pch = c(16, 4))
show (plot)


#graph 2

# Input state_code
state_code <- 16

# Filter data for the specified state_code and years from merged_data
state_2014 <- subset(merged_data, STATE_CODE == state_code & YEAR == 2014)
state_2015 <- subset(merged_data, STATE_CODE == state_code & YEAR == 2015)

# Extract unique COUNTY_CODE values for the state in both years
county_code_2014 <- unique(state_2014$COUNTY_CODE)
county_code_2015 <- unique(state_2015$COUNTY_CODE)

# Extracting data for LOW_ESTIMATE and HIGH_ESTIMATE for both years
x_2014 <- state_2014$COUNTY_CODE  # County_Code for 2014
x_2015 <- state_2015$COUNTY_CODE  # County_Code for 2015
yl_2014 <- state_2014$LOW_ESTIMATE  # LOW_ESTIMATE for 2014
yl_2015 <- state_2015$LOW_ESTIMATE  # LOW_ESTIMATE for 2015
yh_2014 <- state_2014$HIGH_ESTIMATE  # HIGH_ESTIMATE for 2014
yh_2015 <- state_2015$HIGH_ESTIMATE  # HIGH_ESTIMATE for 2015

# Set the plot size
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))  # Set up a side-by-side plot layout with smaller margins

# Plot LOW_ESTIMATE for both years
plot(x_2014, yl_2014, type = "l", col = "blue", xlab = "County_Code", ylab = "Usage pesticide-use",
     main = paste("LOW_ESTIMATE in state", state_code))
lines(x_2015, yl_2015, col = "green")
legend("topright", legend = c("LOW_ESTIMATE 2014", "LOW_ESTIMATE 2015"), col = c("blue", "green"), lty = 1)

# Plot HIGH_ESTIMATE for both years
plot(x_2014, yh_2014, type = "l", col = "red", xlab = "County_Code", ylab = "Usage pesticide-use",
     main = paste("HIGH_ESTIMATE in state", state_code))
lines(x_2015, yh_2015, col = "purple")
legend("topright", legend = c("HIGH_ESTIMATE 2014", "HIGH_ESTIMATE 2015"), col = c("red", "purple"), lty = 1)


#graph 3


# Unique state codes in merged_data
states_code <- unique(merged_data$STATE_CODE)

# Iterate over the first 5 state codes
for (sc in states_code[1:5]) {
  # Filter data for the state code
  st <- merged_data %>% filter(STATE_CODE == sc)
  
  # Group compounds and count occurrences
  st_cp <- st %>%
    count(COMPOUND) %>%
    arrange(desc(n)) %>%
    top_n(5)  # Select top 5 frequently used compounds
  
  # Create a bar plot for the top compounds
  p <- ggplot(st_cp, aes(x = reorder(COMPOUND, -n), y = n)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(x = "Compounds", y = "Frequently Used", 
         title = paste("The compounds used in STATE", sc)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()
  
  # Print the plot to display it
  print(p)
}




#graph 4

library(gridExtra)

# Function to calculate state-wise mean
get_data_state_mean_est <- function(data) {
  data %>%
    group_by(STATE_CODE) %>%
    summarize(Mean_Value = mean(LOW_ESTIMATE + HIGH_ESTIMATE, na.rm = TRUE))
}

# Calculate mean value for merged_data in 2014
data2014_state_mean <- get_data_state_mean_est(filter(merged_data, YEAR == 2014))

# Create bar plot for 2014
plot_2014 <- ggplot(data2014_state_mean, aes(x = factor(STATE_CODE), y = Mean_Value, fill = factor(STATE_CODE))) +
  geom_bar(stat = "identity") +
  labs(x = "State Code", y = "Mean Pesticide Use", title = "Mean value of pesticide-use of all states in year 2014") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  guides(fill = FALSE)  # Hide legend

# Calculate mean value for merged_data in 2015
data2015_state_mean <- get_data_state_mean_est(filter(merged_data, YEAR == 2015))

# Create bar plot for 2015
plot_2015 <- ggplot(data2015_state_mean, aes(x = factor(STATE_CODE), y = Mean_Value, fill = factor(STATE_CODE))) +
  geom_bar(stat = "identity") +
  labs(x = "State Code", y = "Mean Pesticide Use", title = "Mean value of pesticide-use of all states in year 2015") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  guides(fill = FALSE)  # Hide legend

# Arrange plots side by side
grid.arrange(plot_2014, plot_2015, ncol = 2)



#graph 5 

# Replace NA values with 0 in LOW_ESTIMATE and HIGH_ESTIMATE columns
merged_data$LOW_ESTIMATE[is.na(merged_data$LOW_ESTIMATE)] <- 0
merged_data$HIGH_ESTIMATE[is.na(merged_data$HIGH_ESTIMATE)] <- 0

# Calculate differences between 2014 and 2015 for each state
data_diff_state_mean <- merged_data %>%
  group_by(STATE_CODE) %>%
  summarize(LOW_DIFF = sum(LOW_ESTIMATE[YEAR == 2015]) - sum(LOW_ESTIMATE[YEAR == 2014]),
            HIGH_DIFF = sum(HIGH_ESTIMATE[YEAR == 2015]) - sum(HIGH_ESTIMATE[YEAR == 2014]))

# Create a Total_Difference column as the sum of LOW_DIFF and HIGH_DIFF
data_diff_state_mean <- mutate(data_diff_state_mean, Total_Difference = LOW_DIFF + HIGH_DIFF)

# Bar plot for differences
ggplot(data_diff_state_mean, aes(x = factor(STATE_CODE), y = Total_Difference, fill = factor(STATE_CODE))) +
  geom_bar(stat = "identity") +
  labs(x = "State Code", y = "Differences", title = "Differences (changes) in all States between year 2014 and year 2015") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  guides(fill = FALSE)  # Hide legend

# Pie chart for differences
ggplot(data_diff_state_mean, aes(x = "", y = Total_Difference, fill = factor(STATE_CODE))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, fill = "State Code", 
       title = "Differences (changes) in all States between year 2014 and year 2015") +
  theme_void() +
  guides(fill = guide_legend(title = "State Code"))  # Legend title

