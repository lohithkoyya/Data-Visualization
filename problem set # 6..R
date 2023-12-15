# Load necessary libraries
library(broom)
library(ggplot2)
library(margins)
library(survival)  # Load the survival package

# Load the lung dataset
data(lung)

# Fit multiple nested OLS regression models on the lung dataset
models <- list()

# Model 1: OLS model with age as the dependent variable
model1 <- lm(age ~ sex + ph.ecog + ph.karno + meal.cal + wt.loss, data = lung)
models[["Model 1 (Age)"]] <- model1

# Model 2: OLS model with ph.karno as the dependent variable
model2 <- lm(ph.karno ~ sex + ph.ecog + age + meal.cal + wt.loss, data = lung)
models[["Model 2 (ph.karno)"]] <- model2

# Model 3: OLS model with meal.cal as the dependent variable
model3 <- lm(meal.cal ~ sex + ph.ecog + ph.karno + age + wt.loss, data = lung)
models[["Model 3 (Meal Cal)"]] <- model3

# Create a list to store coefficients and marginal effects for each model
results_list <- list()

# Loop through each model, extract coefficients and marginal effects, and create plots
for (model_name in names(models)) {
  model <- models[[model_name]]
  
  # Extract coefficient information
  coefficients <- tidy(model)
  
  # Store coefficients in the results list
  results_list[[model_name]] <- list(Coefficients = coefficients)
  
  # Create a coefficient plot for this model
  p_coeff <- ggplot(data = coefficients, aes(x = reorder(term, estimate), y = estimate,
                                             ymin = estimate - 1.96 * std.error,
                                             ymax = estimate + 1.96 * std.error,
                                             color = estimate))
  p_coeff <- p_coeff + geom_hline(yintercept = 0, color = "gray80") +
    geom_pointrange() + coord_flip() +
    scale_color_gradient2(low = "#fb5901", mid = "#493267",
                          high = "#fb5901", midpoint = 0) +
    guides(color = FALSE) +
    labs(x = NULL, y = "Coefficient Estimate") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          text = element_text(family = "Tahoma"),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 14)) +
    ggtitle(paste("Coefficient Plot for", model_name, "on Lung Dataset"))
  
  # Create a marginal effects plot for this model
  margins_model <- margins(model)
  p_marginal <- plot(margins_model, plot = FALSE)
  
  # Print or save the coefficient and marginal effects plots
  print(p_coeff)
  print(p_marginal)
  
  # Store marginal effects in the results list
  results_list[[model_name]]$MarginalEffects <- margins_model
}

# You can access coefficients and marginal effects for each model using results_list
# For example, coefficients for Model 1 can be accessed with results_list[["Model 1 (Age)"]]$Coefficients
# Marginal effects for Model 1 can be accessed with results_list[["Model 1 (Age)"]]$MarginalEffects
