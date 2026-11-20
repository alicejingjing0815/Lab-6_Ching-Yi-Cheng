## Lab 6:Ordinal Logistic Regression with the Big Five

# Introduction

# Workflow

# Research Question and Prediction

# Dataset


# Method
# Part 1.Load packages and prepare the setup
#| label: setup
#| include: false

library(tidyverse)
library(readxl)
library(psych)
library(ordinal)
library(ggeffects)
library(broom)
library(gt)

fmt <- function(x) sprintf("%.2f", x)
pct <- function(x) scales::percent(x, accuracy = 0.1)
p_label <- function(p) {
  if (p < .001) "< .001" else paste0("= ", sub("^0", "", sprintf("%.3f", p)))
}

response_levels <- c(
  "Very inaccurate", "Moderately inaccurate", "Slightly inaccurate",
  "Slightly accurate", "Moderately accurate", "Very accurate"
)

keys <- list(
  Agreeableness     = c("-A1", "A2", "A3", "A4", "A5"),
  Conscientiousness = c("C1", "C2", "C3", "-C4", "-C5"),
  Extraversion      = c("-E1", "-E2", "E3", "E4", "E5"),
  Neuroticism       = c("N1", "N2", "N3", "N4", "N5"),
  Openness          = c("O1", "-O2", "O3", "O4", "-O5")
)

item_names <- sub("^-", "", unlist(keys, use.names = FALSE))
raw_bfi <- read_excel("bfi.xlsx")

items <- raw_bfi |>
  select(all_of(item_names)) |>
  mutate(across(everything(), as.numeric))

stopifnot(all(unlist(items) %in% c(1:6, NA)))

scores <- psych::scoreItems(
  keys, items, totals = FALSE, min = 1, max = 6,
  missing = TRUE, impute = "none"
)$scores

lab_data <- tibble(
  A3 = factor(items$A3, levels = 1:6,
              labels = response_levels, ordered = TRUE),
  extra = if_else(
    complete.cases(items[paste0("E", 1:5)]),
    scores[, "Extraversion"],
    NA_real_
  ),
  dominant = factor(
    colnames(scores)[max.col(scores, ties.method = "first")],
    levels = names(keys)
  )
) |>
  drop_na(A3, extra, dominant) |>
  mutate(extra_c = extra - mean(extra))

extra_mean <- mean(lab_data$extra)


# Part 2. Examine the Outcomes
## 2.1 A3 Response Frequency Table
a3_counts <- lab_data |>
  count(A3, .drop = FALSE) |>
  mutate(p = n / sum(n))

a3_counts |>
  gt() |>
  cols_label(A3 = "Response to A3", n = "Count", p = "Proportion") |>
  fmt_percent(columns = p, decimals = 1)


## 2.2 Bar Chart of A3 Responses
ggplot(a3_counts, aes(A3, n)) +
  geom_col(fill = "#376D80", width = 0.7) +
  scale_x_discrete(labels = scales::label_wrap(13), drop = FALSE) +
  labs(x = NULL, y = "Participants") +
  theme_minimal()


## 2.3 Jittered Dot Plot of A3 Responses by Extraversion
ggplot(lab_data, aes(extra, as.integer(A3))) +
  geom_point(
    position = position_jitter(width = 0.06, height = 0.14, seed = 9041),
    alpha = 0.22, colour = "#376D80"
  ) +
  scale_y_continuous(breaks = 1:6, labels = response_levels) +
  labs(x = "Extraversion (subscale mean)", y = "Response to A3") +
  theme_minimal()


## 2.4 Bar Chart of Dominant Traits
lab_data |>
  count(dominant, .drop = FALSE) |>
  ggplot(aes(n, fct_reorder(dominant, n))) +
  geom_col(fill = "#77618A", width = 0.7) +
  labs(x = "Participants", y = "Highest-scoring trait") +
  theme_minimal()

obs_tab <- a3_counts |>
  mutate(
    cumulative_p = cumsum(n) / sum(n),
    cumulative_odds = if_else(
      row_number() < n(), cumulative_p / (1 - cumulative_p), NA_real_
    ),
    cumulative_logit = if_else(
      row_number() < n(), qlogis(cumulative_p), NA_real_
    )
  )


# Part 3. Cumulative Probabilities
## 3.1 Observed Cumulative Probabilities, Odds, and Log-Odds
obs_tab |>
  gt() |>
  cols_label(
    A3 = "Response to A3", n = "Count", p = "P(Y = j)",
    cumulative_p = "P(Y ≤ j)", cumulative_odds = "Odds of Y ≤ j",
    cumulative_logit = "Log-odds of Y ≤ j"
  ) |>
  fmt_number(columns = c(p, cumulative_p), decimals = 3) |>
  fmt_number(columns = c(cumulative_odds, cumulative_logit), decimals = 2) |>
  sub_missing(missing_text = "—")


#Part 4. Fit and Interpret Cumulative Logit Models
## 4.1 Fit the Threshold-Only and Extraversion Models
stopifnot(all(a3_counts$n > 0))
m0 <- clm(A3 ~ 1, data = lab_data, link = "logit")
m1 <- clm(A3 ~ extra_c, data = lab_data, link = "logit")

summary(m1)


## 4.2 Interpret the Thresholds at Average Extraversion
th <- m1$alpha
b <- coef(m1)[["extra_c"]]
cum_avg <- plogis(th)
cat_avg <- diff(c(0, cum_avg, 1))
endorse_avg <- 1 - cum_avg[3]

tibble(
  Response = response_levels,
  Threshold = c(unname(th), NA_real_),
  `P(Y ≤ j)` = c(unname(cum_avg), 1),
  `P(Y = j)` = cat_avg
) |>
  gt() |>
  fmt_number(columns = Threshold, decimals = 2) |>
  fmt_percent(columns = c(`P(Y ≤ j)`, `P(Y = j)`), decimals = 1) |>
  sub_missing(missing_text = "—")


## 4.3 Compare M0 Thresholds with Observed Cumulative Log-Odds
tibble(
  Cutpoint = names(m0$alpha),
  Observed_logit = head(obs_tab$cumulative_logit, -1),
  M0_threshold = unname(m0$alpha)
) |>
  gt() |>
  fmt_number(columns = c(Observed_logit, M0_threshold), decimals = 3)


#Part 5. Compare the Models and Interpret the Slope
## 5.1 Likelihood-Ratio Test
lrt <- anova(m0, m1)
lrt
lr_stat <- lrt$LR.stat[2]
lr_df <- lrt$df[2]
lr_p <- lrt$`Pr(>Chisq)`[2]


## 5.2 AIC Comparison and Model Selection
lr_stat <- lrt$LR.stat[2]
lr_df <- lrt$df[2]
lr_p <- lrt$`Pr(>Chisq)`[2]
aic_difference <- abs(AIC(m0) - AIC(m1))

tibble(
  Model = c("M0: Thresholds only", "M1: Extraversion"),
  Parameters = c(length(coef(m0)), length(coef(m1))),
  `Log-likelihood` = c(as.numeric(logLik(m0)), as.numeric(logLik(m1))),
  AIC = c(AIC(m0), AIC(m1))
) |>
  mutate(`ΔAIC` = AIC - min(AIC)) |>
  gt() |>
  fmt_number(columns = c(`Log-likelihood`, AIC, `ΔAIC`), decimals = 2)


## 5.3 Interpret the Extraversion Slope on the Latent Scale
middle_width <- mean(diff(th)[2:3])
fraction_of_middle_category <- abs(b) / middle_width


# Part 6. Check the Proportional-Odds Assumption
## 6.1 Likelihood-Ratio Test for Proportional Odds
po_test <- nominal_test(m1)
po_test

po_lrt <- po_test["extra_c", "LRT"]
po_df  <- po_test["extra_c", "Df"]
po_p   <- po_test["extra_c", "Pr(>Chi)"]


## 6.2 Estimate Threshold-Specific Odds Ratios for the Plot
m_nom <- clm(A3 ~ 1, nominal = ~ extra_c, data = lab_data, link = "logit")

if (po_p < .05) {
  print(anova(m1, m_nom))
}

nom_coefs <- coef(m_nom)
nom_terms <- paste0(names(th), ".extra_c")
stopifnot(all(nom_terms %in% names(nom_coefs)))

nom_est <- nom_coefs[nom_terms]
nom_se <- sqrt(diag(vcov(m_nom)))[nom_terms]
z_95 <- qnorm(.975)

threshold_or <- tibble(
  Threshold = names(th),
  OR = exp(-nom_est),
  low = exp(-nom_est - z_95 * nom_se),
  high = exp(-nom_est + z_95 * nom_se),
  Model = "Threshold-specific"
)

if (po_p < .05) {
  threshold_or |>
    select(-Model) |>
    gt() |>
    fmt_number(columns = c(OR, low, high), decimals = 2) |>
    print()
}


# Part 7. Interpret Odds Ratios and Predicted Probabilities
## 7.1 Shared Cumulative Odds Ratio
or_tab <- tidy(m1, conf.int = TRUE, exponentiate = TRUE) |>
  filter(term == "extra_c", coef.type == "location")

odds_change <- abs(or_tab$estimate - 1)
odds_direction <- if (or_tab$estimate >= 1) "higher" else "lower"


## 7.2 Predicted Probability of Endorsing the Item
# Use equal one-point steps within the observed range.
raw_range <- range(lab_data$extra)
score_grid <- seq(ceiling(raw_range[1]), floor(raw_range[2]), by = 1)

prob_tab <- tibble(
  Extraversion = score_grid,
  Probability = plogis(b * (Extraversion - extra_mean) - th[3]),
  Odds = Probability / (1 - Probability),
  `Odds ratio vs previous score` = Odds / lag(Odds),
  `Change vs previous score (pp)` = 100 * (Probability - lag(Probability))
)

prob_tab |>
  gt() |>
  fmt_percent(columns = Probability, decimals = 1) |>
  fmt_number(
    columns = c(Odds, `Odds ratio vs previous score`,
                `Change vs previous score (pp)`),
    decimals = 2
  ) |>
  sub_missing(missing_text = "—")



## 7.3 Predicted Probabilities for All Response Categories
#| label: fig-category-probabilities
#| fig-cap: "Predicted response probabilities for the comfort item from the proportional-odds model."

pred <- ggpredict(m1, terms = "extra_c [all]") |>
  as.data.frame()

category <- as.character(pred$response.level)
category_index <- match(category, as.character(seq_along(response_levels)))

pred <- pred |>
  mutate(
    Extraversion = x + extra_mean,
    Response = factor(
      coalesce(response_levels[category_index], category),
      levels = response_levels
    )
  )

stopifnot(!anyNA(pred$Response), !anyNA(pred$predicted))

ggplot(pred, aes(Extraversion, predicted, fill = Response, group = Response)) +
  geom_area(position = position_stack(reverse = TRUE)) +
  scale_fill_viridis_d(drop = FALSE) +
  scale_y_continuous(labels = scales::label_percent()) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    x = "Extraversion (subscale mean)",
    y = "Predicted probability",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


## 7.4 Shared and Threshold-Specific Odds Ratios
#| label: fig-odds-ratios
#| fig-cap: "Shared and threshold-specific cumulative odds ratios for Extraversion."

bind_rows(
  tibble(
    Threshold = "Shared (proportional odds)",
    OR = or_tab$estimate,
    low = or_tab$conf.low,
    high = or_tab$conf.high,
    Model = "Shared"
  ),
  threshold_or
) |>
  mutate(Threshold = fct_rev(fct_inorder(Threshold))) |>
  ggplot(aes(OR, Threshold, colour = Model)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = low, xmax = high)) +
  scale_x_log10() +
  labs(x = "Odds ratio for responding above the threshold",
       y = NULL, colour = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Part 8. Findings and Limitations
