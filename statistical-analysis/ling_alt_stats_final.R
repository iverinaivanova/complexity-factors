setwd('../stats')

getwd()
# Install required packages (run once)
install.packages(c("brms", "tidyverse"))

# Load libraries
library(brms)
library(tidyverse)
library(ggplot2)

# Load your hybrid sample
data <- read.csv("full_dt.csv")
summary(data)
head(data, n=7)
tail(data)

data$clause <- factor(data$clause, levels = c("that-cl", "to-inf"))
data$clause_numeric <- ifelse(data$clause == "that-cl", 0, 1)

# Define 13 predictors (update to your actual column names)
predictors <- c("negation", "passivization", "modalV", 
                "noun.mod", "quantifier", "AdvCl", "CompCl",
                "AdnomCl", "Adv", "More.than.1.Adv", "More.than.5.Less.than.10.Const",
                "More.than.10.Const", "supplement")

# No correlations between features larger than 0.7
cor(data[, predictors])

#Calculate Baseline Log-Odds; calculating the proportion of the 2 groups to determine the prior intercept
# The proportion of "to-inf" is 0.8178437. The log-odds of "to-inf" is 1.501. 
# This means that when all parameters are absent, the log-odds of choosing the 
# to-inf is 1.501, which corresponds to 81.78%.
prop.table(table(data$clause_numeric))


print("Data columns:")
print(colnames(data))
print("Missing predictors:")
print(setdiff(predictors, colnames(data)))  # Should be empty

# Main effects formula
formula_main <- reformulate(predictors, response = "clause_numeric")
print("Main formula:")
print(formula_main)

# Main effects model
bayesian_mdl_1 <- brm(
  formula = formula_main,
  data = data,
  family = bernoulli(link = "logit"),
  prior = c(
    # prior(normal(0, 1.5), class = "b"),  # Tighter prior for binary predictor effects
    # prior(normal(0, 2), class = "Intercept")  # Adjust based on baseline log-odds
    prior(normal(1.501, 1), class = "Intercept"), # Adjusted based on baseline log-odds
    prior(normal(0, 1), class = "b")
  ),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  control = list(adapt_delta = 0.95),  # Improve sampling stability
  seed = 42
)

summary(bayesian_mdl_1)

summary_mdl1 <- summary(bayesian_mdl_1)

fixed_effects <- summary_mdl1$fixed

# Convert to data frame
effects_df <- as.data.frame(fixed_effects)
effects_df$Predictor <- rownames(fixed_effects)
rownames(effects_df) <- NULL

# Exclude Intercept for clarity (optional)
effects_df <- effects_df %>% filter(Predictor != "Intercept")

# Plot coefficients with CIs
ggplot(effects_df, aes(x = Predictor, y = Estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = `l-95% CI`, ymax = `u-95% CI`), width = 0.2) +
  coord_flip() +  # Flip for readability
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Main Effects on Log-odds of 'to-inf' (1)",
       x = "Predictor", y = "Estimate (Negative = Favors 'that-cl')") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))



# Posterior predictive checks
pp_check(bayesian_mdl_1, type = "dens_overlay")

# Model evaluation
library(loo)
loo_result <- loo(bayesian_mdl_1)
loo_result
pareto_k <- loo_result$diagnostics$pareto_k
# Summary statistics
summary(pareto_k)
# Count by category
table(cut(pareto_k, breaks = c(-Inf, 0.5, 0.7, 1, Inf), labels = c("Good", "Ok", "Bad", "Very Bad")))

library(knitr)

# Step 1: Extract coefficients
coef <- as.data.frame(fixef(bayesian_mdl_1))
coef$Parameter <- rownames(coef)

# Step 2: Filter negative coefficients (exclude intercept)
neg_coef <- coef[coef$Parameter != "Intercept" & coef$Estimate < 0, ]

# Step 3: Compute odds ratios and percentage decrease
neg_coef$OR_that_cl <- 1 / exp(neg_coef$Estimate)  # OR for "that-cl"
neg_coef$OR_to_inf <- exp(neg_coef$Estimate)       # OR for "to-inf"
neg_coef$Percent_Decrease_to_inf <- (1 - neg_coef$OR_to_inf) * 100  # % decrease for "to-inf"
neg_coef$OR_lower <- 1 / exp(neg_coef$Q97.5)       # Lower 95% CrI for OR_that_cl
neg_coef$OR_upper <- 1 / exp(neg_coef$Q2.5)        # Upper 95% CrI for OR_that_cl

# Step 4: Classify strength based on OR_that_cl
neg_coef$Strength <- cut(neg_coef$OR_that_cl,
                         breaks = c(0, 2, 5, Inf),
                         labels = c("Weak", "Moderate", "Strong"),
                         right = FALSE)

# Step 5: Create table
results <- neg_coef[, c("Parameter", "Estimate", "OR_that_cl", "OR_lower", "OR_upper", "Percent_Decrease_to_inf", "Strength")]
colnames(results) <- c("Predictor", "Coefficient", "OR (that-cl)", "95% CrI Lower", "95% CrI Upper", "% Decrease in Odds (to-inf)", "Strength")

formatted_table <- format(results, digits = 2, nsmall = 2)
print(formatted_table, row.names = FALSE)


fixed_effects <- summary(bayesian_mdl_1)$fixed
effects_df <- as.data.frame(fixed_effects)
effects_df$Predictor <- rownames(fixed_effects)
predictors_df <- effects_df[effects_df$Predictor != "Intercept", ]
top_9_that_cl <- head(predictors_df[predictors_df$Estimate < 0 & 
                                      predictors_df$`l-95% CI` < 0 & 
                                      predictors_df$`u-95% CI` < 0, ], 9)
top_predictors <- top_9_that_cl$Predictor
print(top_predictors)

# Ensure top_predictors is a character vector
print("Top 9 predictors:")
print(top_predictors)

pairwise_interactions <- c( "negation:passivization", 
                             "negation:modalV", 
                             "negation:noun.mod", 
                             "negation:Adv",
                             "negation:More.than.1.Adv", 
                             "negation:More.than.5.Less.than.10.Const", 
                             "negation:More.than.10.Const", 
                             "passivization:modalV", 
                             "passivization:noun.mod", 
                             "passivization:Adv", 
                             "passivization:More.than.1.Adv",
                             "passivization:More.than.5.Less.than.10.Const", 
                             "passivization:More.than.10.Const", 
                             "modalV:noun.mod", 
                             "modalV:Adv",
                             "modalV:More.than.1.Adv", 
                             "modalV:More.than.5.Less.than.10.Const", 
                             "modalV:More.than.10.Const",
                             "noun.mod:Adv",  
                             "noun.mod:More.than.1.Adv",
                             "noun.mod:More.than.5.Less.than.10.Const", 
                             "noun.mod:More.than.10.Const",
                             "Adv:More.than.5.Less.than.10.Const", 
                             "Adv:More.than.10.Const", 
                             "More.than.1.Adv:More.than.5.Less.than.10.Const",                           "More.than.1.Adv:More.than.10.Const" 
                          )
# Construct formula with main effects and interactions
formula_interaction <- as.formula(
  paste("clause_numeric ~", paste(top_9_predictors, collapse = " + "), "+",
        paste(pairwise_interactions, collapse = " + "))
)
print("Interaction formula:")
print(formula_interaction)


mf <- model.frame(formula_interaction, data = data)
dim(mf)  # Should be 3549 rows, 37 columns (clause_numeric + 36 terms)
head(mf) # Verify predictors are there

interaction_model <- brm(
  formula = formula_interaction,
  data = data,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(1.501, 1), class = "Intercept"), # Adjusted based on baseline log-odds
    prior(normal(0, 1), class = "b")
  ),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 42,
  control = list(adapt_delta = 0.95)    # Handle complexity
)

# Check results
summary(interaction_model)

loo_result_interact <- loo(interaction_model)
loo_result_interact
pareto_k_interact <- loo_result_interact$diagnostics$pareto_k
# Summary statistics
summary(pareto_k_interact)
# Count by category
table(cut(pareto_k_interact, breaks = c(-Inf, 0.5, 0.7, 1, Inf), labels = c("Good", "Ok", "Bad", "Very Bad")))


# Step 1: Extract coefficients
coef <- as.data.frame(fixef(interaction_model))
coef$Parameter <- rownames(coef)

# Step 2: Filter negative coefficients (exclude intercept)
neg_coef <- coef[coef$Parameter != "Intercept" & coef$Estimate < 0, ]

# Step 3: Compute odds ratios and percentage decrease
neg_coef$OR_that_cl <- 1 / exp(neg_coef$Estimate)  # OR for "that-cl"
neg_coef$OR_to_inf <- exp(neg_coef$Estimate)       # OR for "to-inf"
neg_coef$Percent_Decrease_to_inf <- (1 - neg_coef$OR_to_inf) * 100  # % decrease for "to-inf"
neg_coef$OR_lower <- 1 / exp(neg_coef$Q97.5)       # Lower 95% CrI for OR_that_cl
neg_coef$OR_upper <- 1 / exp(neg_coef$Q2.5)        # Upper 95% CrI for OR_that_cl

# Step 4: Classify strength based on OR_that_cl
neg_coef$Strength <- cut(neg_coef$OR_that_cl,
                         breaks = c(0, 2, 5, Inf),
                         labels = c("Weak", "Moderate", "Strong"),
                         right = FALSE)

# Step 5: Create table
results <- neg_coef[, c("Parameter", "Estimate", "OR_that_cl", "OR_lower", "OR_upper", "Percent_Decrease_to_inf", "Strength")]
colnames(results) <- c("Predictor", "Coefficient", "OR (that-cl)", "95% CrI Lower", "95% CrI Upper", "% Decrease in Odds (to-inf)", "Strength")

formatted_table <- format(results, digits = 2, nsmall = 2)
print(formatted_table, row.names = FALSE)




