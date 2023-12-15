##############################
##  Problem set # 5
##  Author - lohith siva venkata Ramakrishna
##  
##############################

### BEGIN ###
library(tidyverse)
library(socviz)
library(ggrepel)
library(gapminder)
library(dplyr)
library(ggplot2)
library(ggplot2)
library(ggrepel)
library(ggplot2)
library(ggrepel)


##############################
##  Take a look at the data
##############################

gapminder

gss_sm  

organdata 

elections_historic 

# Load your dataset here or create one with a "win_party" variable

# Filter the data for elections since 1992
elections_historic <- subset(elections_historic, year >= 1992)

#1# The subset() function is very useful 
#when used in conjunction with a series of layered geoms. 
#Go back to your code for the Presidential Elections plot (Figure 5.18) 
#and redo it so that it shows all the data points but only labels elections since 1992.
#You might need to look again at the elections_historic data to see 
#what variables are available to you. You can also experiment with 
#subsetting by political party, or changing the colors of the 
#points to reflect the winning party.

# Create the scatterplot with labels for elections since 1992
p <- ggplot(data = filtered_elections, aes(x = popular_pct, y = ec_pct, label = year, color = win_party)) +
  geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +
  geom_point(aes(fill = win_party)) +
  geom_text_repel(segment.size = 0, box.padding = 0.5, min.segment.length = 0.1) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = party_colors) +
  scale_fill_manual(values = party_colors) +
  labs(x = "Winner's share of Popular Vote",
       y = "Winner's share of Electoral College Votes",
       title = "Presidential Elections: Popular & Electoral College Margins",
       subtitle = "Elections since 1992",
       caption = "Data for 2016 are provisional.") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        text = element_text(family = "Tahoma"))

# Save the plot as an image
ggsave("figures/election_plot.png", plot = p, units = "in", width = 10, height = 6)

# Show the plot
print(p)

#2#Use geom_point() and reorder() to make a Cleveland dot plot of all Presidential elections, ordered by share of the popular vote.
# Create the Cleveland dot plot
p <- ggplot(elections_historic, aes(x = reorder(year, -popular_pct), y = popular_pct)) +
  geom_point() +
  labs(x = "Year", y = "Share of Popular Vote",
       title = "Presidential Elections: Share of Popular Vote",
       subtitle = "Ordered by Share of Popular Vote") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
ggsave("figures/popular_vote_dot_plot.png", plot = p, units = "in", width = 10, height = 6)

# Show the plot
print(p)

#4#The main action verbs in the dplyr library are group_by(), 
#filter(), select(), summarize(), and mutate(). 
#Practice with them by revisiting the gapminder data to see if you 
#can reproduce a pair of graphs from Chapter One, shown here again in Figure 5.28. 
#You will need to filter some rows, group the data by continent, 
#and calculate the mean life expectancy by continent before 
#beginning the plotting process.

# Calculate the mean life expectancy by year and continent
life_expectancy_by_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(mean_life_expectancy = mean(lifeExp))

# Create the plot
ggplot(life_expectancy_by_continent, aes(x = year, y = mean_life_expectancy, color = continent)) +
  geom_line() +
  labs(x = "Year", y = "Mean Life Expectancy",
       title = "Life Expectancy Over Time by Continent") +
  scale_color_manual(values = c("Asia" = "red", "Europe" = "blue", "Africa" = "green",
                                "Americas" = "purple", "Oceania" = "orange")) +
  theme_minimal()
 

# Filter the data for the year 2007
gapminder_2007 <- gapminder %>%
  filter(year == 2007)

# Create the plot
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  labs(x = "GDP per Capita (2007)", y = "Life Expectancy (2007)",
       title = "Life Expectancy vs. GDP per Capita (2007)") +
  scale_color_manual(values = c("Asia" = "red", "Europe" = "blue", "Africa" = "green",
                                "Americas" = "purple", "Oceania" = "orange")) +
  scale_x_log10() +  # Use a logarithmic scale for GDP per capita
  theme_minimal()


library(dplyr)
#5#Get comfortable with grouping, mutating, and summarizing data in pipelines. 
#This will become a routine task as you work with your data. 
#There are many ways that tables can be aggregated and transformed. 
#Remember group_by() groups your data from left to right, 
#with the rightmost or innermost group being the level calculations will be done at;
#mutate() adds a column at the current level of grouping; and summarize() a
#ggregates to the next level up. Try creating some grouped objects from the GSS data,
#calculating frequencies as you learned in this Chapter, 
#and then check to see if the totals are what you expect. 
#For example, start by grouping degree by race, like this:
gss_sm %>%
  group_by(degree) %>%
  summarize(N = n()) %>%
  mutate(pct = round(N / sum(N) * 100, 1)) %>%
  arrange(desc(pct))



gss_sm %>%
  group_by(race) %>%
  summarize(N = n()) %>%
  mutate(pct = round(N / sum(N) * 100, 0)) %>%
  group_by(race) %>%
  summarize(total_pct = sum(pct))



gss_sm %>%
  group_by(region) %>%
  summarize(N = n()) %>%
  mutate(pct = round(N / sum(N) * 100, 0)) %>%
  group_by(region) %>%
  summarize(total_pct = sum(pct))


#6#This code is similar to what you saw earlier, 
#but a little more compact. (We calculate the pct values directly.) 
#Check the results are as you expect by grouping by race and summing the percentages.
#Try doing the same exercise grouping by sex or region.

# Calculate 'total_pct' directly by race
result_race <- gss_sm %>%
  group_by(race) %>%
  summarize(total_pct = sum(1 / n()) * 100)  # Calculate percentage directly

# View the results
print(result_race)




# Calculate 'total_pct' directly by region
result_region <- gss_sm %>%
  group_by(region) %>%
  summarize(total_pct = sum(1 / n()) * 100)  # Calculate percentage directly

# View the results
print(result_region)



# Calculate 'total_pct' directly by sex
result_sex <- gss_sm %>%
  group_by(sex) %>%
  summarize(total_pct = sum(1 / n()) * 100)  # Calculate percentage directly

# View the results
print(result_sex)

#9#Experiment with the gapminder data to practice some of the new geoms 
#we have learned. Try examining population or life expectancy over time using a 
#series of boxplots. (Hint: you may need to use the group aesthetic in the aes() 
#call.) Can you facet this boxplot by continent? Is anything different if you 
#create a tibble from gapminder that explicitly groups the data by year and 
#continent first, and then create your plots with that?

# Boxplot of population over time, faceted by continent
ggplot(gapminder, aes(x = year, y = pop, group = year)) +
  geom_boxplot() +
  facet_wrap(~continent, scales = "free_y") +
  labs(x = "Year", y = "Population") +
  theme_minimal()

# Boxplot of life expectancy over time, faceted by continent
ggplot(gapminder, aes(x = year, y = lifeExp, group = year)) +
  geom_boxplot() +
  facet_wrap(~continent, scales = "free_y") +
  labs(x = "Year", y = "Life Expectancy") +
  theme_minimal()



#10#Read the help page for geom_boxplot() and 
#take a look at the notch and varwidth options. 
#Try them out to see how they change the look of the plot.

# Create a tibble with data grouped by year and continent
gapminder_grouped <- gapminder %>%
  group_by(year, continent) %>%
  summarise(pop = mean(pop), lifeExp = mean(lifeExp))

# Boxplot of population over time, faceted by continent
ggplot(gapminder_grouped, aes(x = year, y = pop, group = year)) +
  geom_boxplot() +
  facet_wrap(~continent, scales = "free_y") +
  labs(x = "Year", y = "Population") +
  theme_minimal()

# Boxplot of life expectancy over time, faceted by continent
ggplot(gapminder_grouped, aes(x = year, y = lifeExp, group = year)) +
  geom_boxplot() +
  facet_wrap(~continent, scales = "free_y") +
  labs(x = "Year", y = "Life Expectancy") +
  theme_minimal()




install.packages("cowplot")
library(cowplot)
install.packages("gridExtra")
library(gridExtra)
grid.arrange()

# (basic_boxplot, notched_boxplot, variable_width_boxplot)



# Load the gapminder dataset
data(gapminder)

# Create a dataset using gapminder
data <- gapminder

# View the first few rows of the dataset
head(data)
# Basic boxplot
# Load the gapminder dataset
data(gapminder)

# Create a boxplot of gdpPercap by continent
ggplot(gapminder, aes(x = continent, y = gdpPercap)) +
  geom_boxplot()


# Boxplot with notches
notched_boxplot <- ggplot(data, aes(x = continent, y = gdpPercap)) +
  geom_boxplot(notch = TRUE) +
  labs(title = "Boxplot with Notches")
print(notched_boxplot)

# Boxplot with variable widths
variable_width_boxplot <- ggplot(data, aes(x = continent, y = gdpPercap)) +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Boxplot with Variable Widths")
print(variable_width_boxplot)






#11#As an alternative to geom_box plot() 
#try geom_violin() for a similar plot, 
#but with a mirrored density distribution instead of a box and whiskers.

# 11 basic_violin_plot <- ggplot(gapminder, aes(x = continent, y = lifeExp)) +
  geom_violin()

# Display the basic violin plot
basic_violin_plot


