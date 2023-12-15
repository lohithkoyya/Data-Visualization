##############################
##  Assignment : Problem set# 7
##Author: Lkoyya

##############################

### BEGIN ###

##############################
##  Load in packages and data
##############################

# Load the required libraries
library(tidyverse)
library(socviz)
library(maps)
install.packages("sf")
library(sf)

county_data #This is another socviz package dataset containing county-level demographic,
#election, and geographic data, put together from Census Bureau and 
#CDC sources.

#1#create a county-level choropleth visualizing
#the geographic distribution of the variable(s).
library(socviz)
library(ggplot2)

county_map <- socviz::county_map
county_data <- socviz::county_data

county_full <- left_join(county_map, county_data, by = "id")

# Check the data type of pop_dens and convert it to numeric if needed
county_full$pop_dens <- as.numeric(county_full$pop_dens)

# Create the choropleth map with a continuous color scale
p <- ggplot(data = county_full,
            aes(x = long, y = lat, fill = pop_dens, group = group)) +
  geom_polygon(color = "gray90", size = 0.05) +
  coord_equal() +
  scale_fill_gradient(low = "blue", high = "red") +  # Customize colors as needed
  labs(fill = "Population per square mile") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")

p


#visualize the variable or variables using some non-
#map display of your choice that we have covered so far this semester


# Create a scatterplot of female vs. white population, with color mapped to state
ggplot(county_data, aes(x = white, y = female, color = state)) +
  geom_point() +
  labs(title = "Female Population vs. White Population by State",
       x = "White Population (%)",
       y = "Female Population (%)") +
  theme_minimal()



# Create a bar plot of average household income by state
bar_plot <- ggplot(county_data, aes(x = state, y = hh_income)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue") +
  labs(title = "Average Household Income by State",
       x = "State",
       y = "Average Household Income") +
  theme_minimal()

print(bar_plot)



# Create a box plot of population density by census region
box_plot <- ggplot(county_data, aes(x = census_region, y = pop_dens)) +
  geom_boxplot(fill = "orange", color = "darkred") +
  labs(title = "Population Density by Census Region",
       x = "Census Region",
       y = "Population Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(box_plot)


# End########












