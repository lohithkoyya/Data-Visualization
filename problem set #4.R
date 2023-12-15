# 1. Revisit the gapminder plots at the beginning of the chapter and experiment
#with different ways to facet the data. 
#Try plotting population and per capita GDP while faceting on year, 
#or even on country. In the latter case you will get a lot of panels, 
#and plotting them straight to the screen may take a long time. 
#Instead, assign the plot to an object and save it as a PDF file to your figures/ 
#folder. Experiment with the height and width of the figure.# Load the required libraries
library(tidyverse)
library(gapminder)
gapminder
head(gapminder)
library(ggplot2)


gss_sm  #This is a sampling of variables from the 2016 General Social Survey (GSS).
#The GSS is a nationally-representative survey of U.S. adults on a wide 
#variety of demographic, political, economic, cultural, occupational, 
#attitudinal, etc., characteristics. It is collected roughly every two years 
#(though in past years it was collected almost every year). The main GSS 
#follows a repeated cross-section design, meaning that data collectors 
#take a nationally-representative sample of U.S. adults living in households 
#(where only one person +18 per household is interviewed, though a "household" 
#may also include just that single person) that allows researchers to assess 
#trends over time---for example, how U.S. adults' attitudes toward science 
#have changed over time.
# Create a plot faceted by year
plot_by_year <- ggplot(data = gapminder,
                       aes(x = year, y = pop)) +
  geom_line(aes(group = country)) +
  labs(x = "Year", y = "Population") +
  facet_wrap(~ year, ncol = 3) +
  theme_minimal()

# Create a plot faceted by country (Note: This may generate many panels)
plot_by_country <- ggplot(data = gapminder,
                          aes(x = year, y = gdpPercap)) +
  geom_line(aes(group = country)) +
  labs(x = "Year", y = "GDP per Capita") +
  facet_wrap(~ country, ncol = 4) +
  theme_minimal()
print(plot_by_country)
print(plot_by_year)

# Save the plots as PDF files with different dimensions
save_plot_as_pdf(plot_by_year, "figures/plot_by_year.pdf", width = 10, height = 6)
save_plot_as_pdf(plot_by_country, "figures/plot_by_country.pdf", width = 16, height = 10)


#	2. Investigate the difference between a formula written as facet_grid(sex ~ race) 
#versus one written as facet_grid(~ sex + race).

# Create a ggplot object with facet_grid(sex ~ race)
plot_sex_race <- ggplot(data = gss_sm, aes(x = age, y = childs)) +
  geom_point(alpha = 0.2) +
  facet_grid(sex ~ race) +
  labs(title = "Faceted Plot: sex ~ race", x = "Age", y = "Number of Children")

# Create a ggplot object with facet_grid(~ sex + race)
plot_combinations <- ggplot(data = gss_sm, aes(x = age, y = childs)) +
  geom_point(alpha = 0.2) +
  facet_grid(~ sex + race) +
  labs(title = "Faceted Plot: ~ sex + race", x = "Age", y = "Number of Children")
print(plot_sex_race)
print(plot_combinations)








#3.Experiment to see what happens when you use facet_wrap() 
#with more complex forumulas like facet_wrap(~ sex + race) 
#instead of facet_grid. Like facet_grid(), the facet_wrap() 
#function can facet on two or more variables at once. 
#But it will do it by laying the results out in a 
#wrapped one-dimensional table instead of a fully cross-classified grid.

# Create a ggplot object with facet_wrap(~ sex + race)
plot_wrap <- ggplot(gss_sm, aes(x = age)) +  # Replace 'value' with the correct variable name ('age' in this example)
  geom_histogram() +
  facet_wrap(~ sex + race) +
  labs(title = "Faceted Plot: ~ sex + race", x = "Age", y = "Frequency")

# Print the plot
print(plot_wrap)

# Print the plots
print(plot_sex_race)
print(plot_combinations)










#Frequency polygons are closely related to histograms. 
#Instead of displaying the count of observations using bars, 
#they display it with a series of connected lines instead. 
#You can try the various geom_histogram() calls in this chapter using geom_freqpoly() instead.


# Sample data (you should replace this with your actual dataset)
data <- data.frame(
  value = c(10, 15, 12, 18, 8, 14)
)

# Create a ggplot object with geom_freqpoly()
plot_freqpoly <- ggplot(data, aes(x = value)) +
  geom_freqpoly(binwidth = 2) +  # Adjust binwidth as needed
  labs(title = "Frequency Polygon", x = "Value", y = "Frequency")

# Print the frequency polygon
print(plot_freqpoly)

# Load the required libraries
library(ggplot2)
library(gapminder)

# Create a ggplot object with geom_freqpoly() using Gapminder data
plot_freqpoly <- ggplot(data = gapminder, aes(x = lifeExp)) +
  geom_freqpoly(binwidth = 2) +  # Adjust binwidth as needed
  labs(title = "Frequency Polygon of Life Expectancy", x = "Life Expectancy", y = "Frequency")

# Print the frequency polygon
print(plot_freqpoly)







#A histogram bins observations for one variable and 
#shows a bars with the count in each bin. 
#We can do this for two variables at once, too. 
#The geom_bin2d() function takes two mappings, x and y. 
#It divides your plot into a grid and colors 
#the bins by the count of observations in them. 
#Try using it on the gapminder data to plot life expectancy versus per capita GDP. 
#Like a histogram, you can vary the number or width of the bins for both x or y. 
#Instead of saying bins = 30 or binwidth = 1, provide a number for both x and y with, 
#for example, bins = c(20, 50). If you specify bindwith instead, 
#you will need to pick values that are on the same scale as the variable you are mapping.

# Create a ggplot object with geom_bin2d()
plot_2d <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_bin2d(bins = c(20, 50)) +  # Adjust the number of bins as needed
  labs(title = "2D Binning Plot: Life Expectancy vs. GDP per Capita", 
       x = "GDP per Capita", 
       y = "Life Expectancy")

# Print the 2D binning plot
print(plot_2d)





#Density estimates can also be drawn in two dimensions. 
#The geom_density_2d() function draws contour lines estimating 
#the joint distribution of two variables. Try it with the midwest data, 
#for example, plotting percent below the poverty line (percbelowpoverty) 
#against percent college-educated (percollege). Try it with and without a geom_point() layer.
# Sample data (you should replace this with your actual dataset)
# Assuming you have 'midwest' dataset with 'percbelowpoverty' and 'percollege'
midwest <- data.frame(
  percbelowpoverty = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55),
  percollege = c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75)
)

# Create a ggplot object with geom_density_2d() without points
plot_density_2d <- ggplot(midwest, aes(x = percbelowpoverty, y = percollege)) +
  geom_density_2d() +
  labs(title = "2D Density Plot: percbelowpoverty vs. percollege", 
       x = "Percent Below Poverty Line", 
       y = "Percent College-Educated")

# Create a ggplot object with geom_density_2d() and points
plot_density_2d_with_points <- ggplot(midwest, aes(x = percbelowpoverty, y = percollege)) +
  geom_density_2d() +
  geom_point() +  # Add points
  labs(title = "2D Density Plot with Points: percbelowpoverty vs. percollege", 
       x = "Percent Below Poverty Line", 
       y = "Percent College-Educated")

# Print the plots
print(plot_density_2d)
print(plot_density_2d_with_points)





