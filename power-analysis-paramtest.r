library(paramtest)
library(tidyverse)
set.seed(1234)

# Create a function with a linear model with:
# - 2 independent variables
# - 1 interaction
lm_interaction <- function(n, b1, b2, b3, b0=0, x1.m=0, x1.sd=1, x2.m=0, x2.sd=1, alpha=.05) {
  # Populate data.
  x1 <- rnorm(n, x1.m, x1.sd)
  x2 <- rnorm(n, x2.m, x2.sd)
  
  # Residual variance.
  yvar <- sqrt(1 - b1^2 - b2^2 - b3^2)
  y <- rnorm(n, b0 + b1*x1 + b2*x2 + b3*x1*x2, yvar)
  
  # Linear model.
  model <- lm(y ~ x1 + x2 + x1*x2)
  
  # Get main effect.
  x1.b <- coef(summary(model))['x1', 'Estimate']
  x1.p <- coef(summary(model))['x1', 'Pr(>|t|)']

  # Get main effect.
  x2.b <- coef(summary(model))['x2', 'Estimate']
  x2.p <- coef(summary(model))['x2', 'Pr(>|t|)']

  # Get interaction.
  int.b <- coef(summary(model))['x1:x2', 'Estimate']
  int.p <- coef(summary(model))['x1:x2', 'Pr(>|t|)']
  
  # Return as list.
  return(c(x1.b    = x1.b,
           x1.p    = x1.p,
           x1.sig  = x1.p < alpha,
           x2.b    = x2.b,
           x2.p    = x2.p,
           x2.sig  = x2.p < alpha, 
           int.b   = int.b, 
           int.p   = int.p, 
           int.sig = int.p < alpha))
}

# Grid search simulation using 8 CPU cores, increasing sample size with coefficients:
# - main effect: .25 
# - main effect: .15
# - interaction: .10
power_lm_int <- grid_search(lm_interaction, params=list(n=seq(50, 900, by=50)),
                            b1=.25, b2=.15, b3=.10, 
                            n.iter=5000, output='data.frame', parallel='snow', ncpus=8)

# Summarize power results.
power <- results(power_lm_int) %>%
  group_by(n.test) %>%
  summarise("Main effect 1" = mean(x1.sig),
            "Main effect 2" = mean(x2.sig),
            "Interaction" = mean(int.sig))

# Print results.
power

# Plot power graph.  
power %>%
  gather("term", "power", "Main effect 1", "Main effect 2", "Interaction") %>%
    ggplot(aes(x=n.test, y=power, group=factor(term), color=factor(term), linetype=factor(term), shape=factor(term))) +
      geom_point(size=3) +
      geom_line() +
      scale_x_continuous(breaks=c(seq(0, 10000, by=200)), limits=c(50, 900)) +
      geom_hline(yintercept=0.95, alpha=0.3, linetype=2) +
      scale_y_continuous(breaks=c(seq(0, 1, by=0.1)), limits=c(0, 1)) +
      labs(title = paste0("Monte Carlo simulation (n=", NROW(power_lm_int$tests$iter)," tests) of power for linear model"),
           subtitle = "Dashed horizontal line indicate 95% power", 
           x = "Sample Size",
           y = "Power",
           color = "Predictors",
           linetype = "Predictors",
           shape = "Predictors") +
      theme_minimal()
