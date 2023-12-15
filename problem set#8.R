
# Load the necessary libraries
library(tidyverse)
library(ggplot2)
library(socviz)

# Load the opiates dataset
opiates

# Create a scatter plot
scatter_plot <- ggplot(data = opiates, aes(x = deaths, y = population)) +
  geom_point() +
  labs(x = "Deaths", y = "Population", title = "Scatter Plot of Deaths vs. Population")

# Display the scatter plot
print(scatter_plot)



# Create a simple scatter plot
scatter_plot <- ggplot(data = opiates, aes(x = deaths, y = population)) +
  geom_point() +
  labs(x = "Deaths", y = "Population", title = "Scatter Plot of Deaths vs. Population") +
  theme_minimal()  # Use a minimal theme to remove unnecessary elements

# Display the simple scatter plot
print(scatter_plot)

# Load the necessary libraries
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(ggpubr)

# Ensure the "opiates" dataset is available in your R environment
# If you've loaded it using a package, it should be available

# Create a quick scatter plot
plot1 <- ggplot(data = subset(opiates, year == 2014),
                aes(x = deaths, y = population, label = state))

plot1 <- plot1 +
  geom_smooth(method = "lm", se = FALSE, color = "gray80") +
  geom_point(mapping = aes(color = population)) +
  geom_text_repel(data = subset(opiates, year == 2014 & deaths > 7000), size = 2) +
  labs(x = "Deaths",
       y = "Population",
       color = "Population",
       title = "Opiates Data",
       subtitle = "Year 2014",
       caption = expression(paste(italic("Source"), ": Opiates annual report."))) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(legend.position = c(.8, .82),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold", size = 6.5),
        legend.background = element_rect(color = "gray50"),
        legend.text = element_text(size = 6),
        plot.title = element_text(face = "bold"),
        text = element_text(family = "Tahoma")) +
  scale_color_continuous(low = "#660066", high = "#f37735",
                         breaks = c(min(opiates[which(opiates$year == 2014),]$population),
                                    max(opiates[which(opiates$year == 2014),]$population)),
                         labels = scales::comma)

# Save the plot to a file
ggsave("figures/opiates_plot.png", units = "in", width = 8, height = 6, plot = plot1)
