##############################
##  ggplot_intro.R: Problem set#3
##  Note: ggplot 
##  Author: Ram koyya
##############################

#Problem Set #3

library(tidyverse)
library(gapminder)


##############################
##  Take a look at the data
##############################

gapminder

##############################
##  Build a plot, layer by layer
##############################

ram<- ggplot(data = gapminder)

ram<- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp)) 

ram #An empty plot

ram + geom_point() #A simple scatterplot

ram <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y=lifeExp))
ram + geom_smooth() #Plot smoothed conditional means with a 95% CI

ram <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y=lifeExp))
ram + geom_point() + geom_smooth() +  #Now things are getting interesting! We have
  #"layered" two different geoms on top of the 
  #data: a scatterplot (geom_point()) and a 
  #smoothed conditional means plot (geom_smooth())
  
  ram <- ggplot(data = gapminder,
              mapping = aes(x = gdpPercap,
                            y=lifeExp))
ram  + geom_point() + geom_smooth(method = "lm") #What if we want just the linear
#trend line? That is, the slope we
#would get from a simple OLS regression?

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y=lifeExp))
p + geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() #What if we want to "transform" an axis scale to make the plot
#more legible?

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y=lifeExp))
p + geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::dollar) #The x-axis is in dollars, so let's 
#adjust those labels

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = "purple"))
p + geom_point() +
  geom_smooth(method = "loess") +
  scale_x_log10() #Oops! What happened?! Well, remember that aethetic mappings
#associate the specified plotting characteristic to an object
#in your data---that is, a variable. So ggplot2 is trying to 
#treat the word "purple" as a variable---not as a specified color.
#It does not see a variable called "purple" in the gapminder
#dataset, so it simply repeats the word "purple" for each row
#in the dataset. So it's like ggplot2 created a new column
#in gapminder, where each row's entry in that new column is a
#single category called "purple." Essentially, you have a new
#variable that does not vary.You get red as the color 
#because the first color in ggplot2's default color scheme

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))
p + geom_point(color = "purple") +
  geom_smooth(method = "loess") +
  scale_x_log10() #That's better! To color all the points purple, we had to
#explicitly tell the geom_point() layer that we want the color
#to be purple. Notice that we did not attempt to map a plot
#characteristic to a variable called "purple."

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp)) 
p + geom_point(alpha = 0.3) + #alpha of 0 = the points are completely transparent;
  #alpha of 1 = the points are not at all transparent
  geom_smooth(color = "orange", se = FALSE, size = 2, method = "lm") +
  scale_x_log10() #Playing around with some different arguments (alpha, size
#color, and some T/F logicals)

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y=lifeExp))
p + geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.") #A more polished plot

##############################
##  Setting mappings other 
##    than X and Y
##############################

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = continent))
p + geom_point() +
  geom_smooth(method = "loess") +
  scale_x_log10() #We told ggplot2 that we want to map the color aesthetic onto
#a variable called "continent." In effect, we have separate
#smoothed conditional mean trends and scatterplots for each of the
#the continent categories listed in the "continent" variable.
#We get separate trend lines and scatterplots because we made the
#color mapping *before* we added the geoms. 

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = continent,
                          fill = continent))
p + geom_point() +
  geom_smooth(method = "loess") +
  scale_x_log10() #"Fill in" the color of different plot items--in this case,
#the confidence intervals. If we had plotted, say, a bar graph,
#then, the "fill" mapping would have colored in the bars

#Watch what happens
#if we instead set the aesthetic mapping within a geom layer as
#as opposed to the initial data layer:

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))
p + geom_point(mapping = aes(color = continent)) +
  geom_smooth(method = "loess") +
  scale_x_log10() #One trend line, but points colored by continent

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))
p + geom_point() +
  geom_smooth(mapping = aes(color = continent), method = "loess") +
  scale_x_log10() #One color for the scatterplot, five trend lines distinguished
#by color

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))
p + geom_point(mapping = aes(color = log(pop))) +
  scale_x_log10() #Mapping the color aesthetic to a continuous variable

##############################
##  Save your work
##############################

#Let's save that more-or-less polished scatterplot from earlier in the script.
#First, we must recreate it and actually "apply" the layers to p:

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y=lifeExp))
p <- p + geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.") 

p #Notice that p now includes all of the layers. This is because we set p
#above to be p PLUS all the extra geom and scale layers

ggsave("figures/second_plot.png", plot = p) #Note the addition of "figures/" in the
#plot name. This is specifying that we
#want to put the PNG file in our 
#figures sub-folder. This also assumes
#that your working directory is your
#SOC_546 folder.

### END ###


#1.	What happens when you put the geom_smooth() function before geom_point() 
           #instead of after it? What does this tell you about how the plot is drawn? 
           #Think about how this might be useful when drawing plots.











#2.	Change the mappings in the aes() function so that 
    #you plot Life Expectancy against population (pop) 
    #rather than per capita GDP. What does that look like? 
    #What does it tell you about the unit of observation in the dataset?








  
  
#3.	What happens if you map color to year instead of continent?
   #Is the result what you expected? 
   #Think about what class of object year is. 
   #Remember you can get a quick look at the top of the data, 
   #which includes some shorthand information on the class of each variable, 
   #by typing gapminder.











#4.	Instead of mapping color = year, what happens if you try color = factor(year)?









  
#5.	As you look at these different scatterplots, think about Figure 3.13 
                #a little more critically. We worked it up to the point 
                #where it was reasonably polished, 
                #but is it really the best way to display this country-year data? 
                #What are we gaining and losing by ignoring the temporal 
                #and country-level structure of the data? 
                #How could we do better? 
                #Sketch out what an alternative visualization might look like.


