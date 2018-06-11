library(paramtest)
library(tidyverse)
set.seed(1234)

# Create a function with a linear model with:
# - 3 independent variables
# - 4 interactions ("full factorial")
lm_interaction <- function(n, b0=0, b1, b2, b3, b4, b5, b6, b7,
                           x1.m=0, x1.sd=1.5,
                           x2.m=0, x2.sd=1.5,
                           x3.m=0, x3.sd=1.5, alpha=0.05) {
  # Populate data.
  x1 <- rnorm(n, x1.m, x1.sd)
  x2 <- rnorm(n, x2.m, x2.sd)
  x3 <- rnorm(n, x3.m, x3.sd)
  
  # Residual variance.
  yvar <- sqrt(1 - b1^2 - b2^2 - b3^2 - b4^2)
  y <- rnorm(n, b0 + b1*x1 + b2*x2 + b3*x3 + b4*x1*x2 + b5*x2*x3 + b6*x1*x3 + b7*x1*x2*x3, yvar)
  
  # Linear model.
  model <- lm(y ~ x1 + x2 + x3 + x1*x2 + x2*x3 + x1*x3 + x1*x2*x3)
  
  # Get main effects.
  x1.b <- coef(summary(model))['x1', 'Estimate']
  x1.p <- coef(summary(model))['x1', 'Pr(>|t|)']
  x2.b <- coef(summary(model))['x2', 'Estimate']
  x2.p <- coef(summary(model))['x2', 'Pr(>|t|)']
  x3.b <- coef(summary(model))['x3', 'Estimate']
  x3.p <- coef(summary(model))['x3', 'Pr(>|t|)']
  
  # Get interactions.
  x1x2.b <- coef(summary(model))['x1:x2', 'Estimate']
  x1x2.p <- coef(summary(model))['x1:x2', 'Pr(>|t|)']
  x2x3.b <- coef(summary(model))['x2:x3', 'Estimate']
  x2x3.p <- coef(summary(model))['x2:x3', 'Pr(>|t|)']
  x1x3.b <- coef(summary(model))['x1:x3', 'Estimate']
  x1x3.p <- coef(summary(model))['x1:x3', 'Pr(>|t|)']
  x1x2x3.b <- coef(summary(model))['x1:x2:x3', 'Estimate']
  x1x2x3.p <- coef(summary(model))['x1:x2:x3', 'Pr(>|t|)']
  
  return(c(x1.b    = x1.b,
           x1.p    = x1.p,
           x1.sig  = x1.p < alpha,
           x2.b    = x2.b,
           x2.p    = x2.p,
           x2.sig  = x2.p < alpha, 
           x3.b    = x3.b,
           x3.p    = x3.p,
           x3.sig  = x3.p < alpha, 
           x1x2.b   = x1x2.b, 
           x1x2.p   = x1x2.p, 
           x1x2.sig = x1x2.p < alpha,
           x2x3.b   = x2x3.b, 
           x2x3.p   = x2x3.p, 
           x2x3.sig = x2x3.p < alpha,
           x1x3.b   = x1x3.b, 
           x1x3.p   = x1x3.p, 
           x1x3.sig = x1x3.p < alpha,
           x1x2x3.b   = x1x2x3.b, 
           x1x2x3.p   = x1x2x3.p, 
           x1x2x3.sig = x1x2x3.p < alpha))
}

# Grid search simulation using 8 CPU cores, with increasing sample size.
power_lm_int <- grid_search(lm_interaction, params=list(n=seq(100, 1200, by=50)),
                            b1=.10, b2=.15, b3=.10,          # Main effects.
                            b4=.07, b5=.07, b6=.07, b7=.05,  # Interactions.
                            n.iter=10000, output='data.frame', parallel='snow', ncpus=8)

# Summarize power results.
power <- results(power_lm_int) %>%
  group_by(n.test) %>%
  summarise("Main effect 1" = mean(x1.sig),            # b1
            "Main effect 2" = mean(x2.sig),            # b2
            "Main effect 3" = mean(x3.sig),            # b3    
            "Interaction 1 x 2" = mean(x1x2.sig),      # b4
            "Interaction 2 x 3" = mean(x2x3.sig),      # b5
            "Interaction 1 x 3" = mean(x1x3.sig),      # b6
            "Interaction 1 x 2 x 3" = mean(x1x2x3.sig) # b7
            )

# Print results.
power

# Plot power graph.  
power %>%
  gather("term", "power", "Main effect 1", "Main effect 2", "Main effect 3", "Interaction 1 x 2",
         "Interaction 2 x 3", "Interaction 1 x 3", "Interaction 1 x 2 x 3") %>%
    ggplot(aes(x=n.test, y=power, group=factor(term), color=factor(term), linetype=factor(term), shape=factor(term))) +
      geom_point(size=3) +
      geom_line() +
      scale_x_continuous(breaks=c(seq(0, 10000, by=100))) +
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
