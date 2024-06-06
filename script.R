variable_types <- data.frame(
  variable = c("A", "B", "C", "D", "E"),
  type = c("numeric", "binary", "numeric", "numeric", "binary")
)


generate_random_data <- function(n, limits, steps) {
  data <- data.frame(matrix(ncol = length(limits), nrow = n))
  colnames(data) <- names(limits)
  
  for (var in names(limits)) {
    if (steps[[var]] > 0) {
      seq_values <- seq(limits[[var]][1], limits[[var]][2], by = steps[[var]])
      seq_values <- sample(seq_values, n, replace = TRUE)
      data[[var]] <- seq_values
    } else {
      data[[var]] <- runif(n, limits[[var]][1], limits[[var]][2])
    }
  }
  return(data)
}


limits <- list(A = c(18, 90), B = c(0, 1), C = c(50, 100), D = c(150, 200), E = c(0, 1))
steps <- list(A = 1, B = 1, C = 0, D = 0, E = 1)


set.seed(123)

random_data <- generate_random_data(100, limits, steps)
print(head(random_data))


# Function to brainstorm models
brainstorm_models <- function(variable_types, data) {
  models <- list()
  
  # Linear Model for Numeric Variables
  numeric_vars <- variable_types$variable[variable_types$type == "numeric"]
  if (length(numeric_vars) > 1) {
    formula <- as.formula(paste(numeric_vars[1], "~", paste(numeric_vars[-1], collapse = " + ")))
    models$linear_model <- lm(formula, data = data)
    cat("Linear model created with formula:", deparse(formula), "\n")
  }
  
  # Logistic Regression for Binary Outcomes
  binary_vars <- variable_types$variable[variable_types$type == "binary"]
  if (length(binary_vars) > 0) {
    for (binary_var in binary_vars) {
      predictors <- setdiff(variable_types$variable, binary_var)
      formula <- as.formula(paste(binary_var, "~", paste(predictors, collapse = " + ")))
      models[[paste("logistic_model", binary_var, sep = "_")]] <- glm(formula, data = data, family = binomial)
      cat("Logistic model created for", binary_var, "with formula:", deparse(formula), "\n")
    }
  }
  
  return(models)
}


models <- brainstorm_models(variable_types, random_data)
sink("model_summaries.txt")
print(summary(models$linear_model))
print(summary(models$logistic_model_E))
sink()
save(random_data, models, file = "models_and_data.RData")


writeLines(c(
  "# Model Documentation",
  "## Linear Model Summary",
  capture.output(summary(models$linear_model)),
  "## Logistic Model Summary (E)",
  capture.output(summary(models$logistic_model_E))
), "README.md")


# Create an RMarkdown file (modeling.Rmd)
writeLines(c(
  "---",
  "title: 'Model Documentation'",
  "output: html_document",
  "---",
  "",
  "```{r setup, include=FALSE}",
  "knitr::opts_chunk$set(echo = TRUE)",
  "library(tidyverse)",
  "library(dplyr)",
  "library(ggplot2)",
  "library(MASS)",
  "library(caret)",
  "library(random)",
  "```",
  "",
  "# Variable Types",
  "```{r}",
  "variable_types <- data.frame(",
  "  variable = c('A', 'B', 'C', 'D', 'E'),",
  "  type = c('numeric', 'binary', 'numeric', 'numeric', 'binary')",
  ")",
  "print(variable_types)",
  "```",
  "",
  "# Generate Random Data",
  "```{r}",
  "generate_random_data <- function(n, limits, steps) {",
  "  data <- data.frame(matrix(ncol = length(limits), nrow = n))",
  "  colnames(data) <- names(limits)",
  "  for (var in names(limits)) {",
  "    if (steps[[var]] > 0) {",
  "      seq_values <- seq(limits[[var]][1], limits[[var]][2], by = steps[[var]])",
  "      seq_values <- sample(seq_values, n, replace = TRUE)",
  "      data[[var]] <- seq_values",
  "    } else {",
  "      data[[var]] <- runif(n, limits[[var]][1], limits[[var]][2])",
  "    }",
  "  }",
  "  return(data)",
  "}",
  "limits <- list(A = c(18, 90), B = c(0, 1), C = c(50, 100), D = c(150, 200), E = c(0, 1))",
  "steps <- list(A = 1, B = 1, C = 0, D = 0, E = 1)",
  "set.seed(123)",
  "random_data <- generate_random_data(100, limits, steps)",
  "print(head(random_data))",
  "```",
  "",
  "# Brainstorm Models",
  "```{r}",
  "brainstorm_models <- function(variable_types, data) {",
  "  models <- list()",
  "  numeric_vars <- variable_types$variable[variable_types$type == 'numeric']",
  "  if (length(numeric_vars) > 1) {",
  "    formula <- as.formula(paste(numeric_vars[1], '~', paste(numeric_vars[-1], collapse = ' + ')))",
  "    models$linear_model <- lm(formula, data = data)",
  "    cat('Linear model created with formula:', deparse(formula), '\\n')",
  "  }",
  "  binary_vars <- variable_types$variable[variable_types$type == 'binary']",
  "  if (length(binary_vars) > 0) {",
  "    for (binary_var in binary_vars) {",
  "      predictors <- setdiff(variable_types$variable, binary_var)",
  "      formula <- as.formula(paste(binary_var, '~', paste(predictors, collapse = ' + ')))",
  "      models[[paste('logistic_model', binary_var, sep = '_')]] <- glm(formula, data = data, family = binomial)",
  "      cat('Logistic model created for', binary_var, 'with formula:', deparse(formula), '\\n')",
  "    }",
  "  }",
  "  return(models)",
  "}",
  "models <- brainstorm_models(variable_types, random_data)",
  "print(summary(models$linear_model))",
  "print(summary(models$logistic_model_E))",
  "```"
), "modeling.Rmd")

# Render RMarkdown to HTML
# Create an RMarkdown file (modeling.Rmd)
writeLines(c(
  "---",
  "title: 'Model Documentation'",
  "output: html_document",
  "---",
  "",
  "```{r setup, include=FALSE}",
  "knitr::opts_chunk$set(echo = TRUE)",
  "library(tidyverse)",
  "library(dplyr)",
  "library(ggplot2)",
  "library(MASS)",
  "library(caret)",
  "library(random)",
  "```",
  "",
  "# Variable Types",
  "```{r}",
  "variable_types <- data.frame(",
  "  variable = c('A', 'B', 'C', 'D', 'E'),",
  "  type = c('numeric', 'binary', 'numeric', 'numeric', 'binary')",
  ")",
  "print(variable_types)",
  "```",
  "",
  "# Generate Random Data",
  "```{r}",
  "generate_random_data <- function(n, limits, steps) {",
  "  data <- data.frame(matrix(ncol = length(limits), nrow = n))",
  "  colnames(data) <- names(limits)",
  "  for (var in names(limits)) {",
  "    if (steps[[var]] > 0) {",
  "      seq_values <- seq(limits[[var]][1], limits[[var]][2], by = steps[[var]])",
  "      seq_values <- sample(seq_values, n, replace = TRUE)",
  "      data[[var]] <- seq_values",
  "    } else {",
  "      data[[var]] <- runif(n, limits[[var]][1], limits[[var]][2])",
  "    }",
  "  }",
  "  return(data)",
  "}",
  "limits <- list(A = c(18, 90), B = c(0, 1), C = c(50, 100), D = c(150, 200), E = c(0, 1))",
  "steps <- list(A = 1, B = 1, C = 0, D = 0, E = 1)",
  "set.seed(123)",
  "random_data <- generate_random_data(100, limits, steps)",
  "print(head(random_data))",
  "```",
  "",
  "# Brainstorm Models",
  "```{r}",
  "brainstorm_models <- function(variable_types, data) {",
  "  models <- list()",
  "  numeric_vars <- variable_types$variable[variable_types$type == 'numeric']",
  "  if (length(numeric_vars) > 1) {",
  "    formula <- as.formula(paste(numeric_vars[1], '~', paste(numeric_vars[-1], collapse = ' + ')))",
  "    models$linear_model <- lm(formula, data = data)",
  "    cat('Linear model created with formula:', deparse(formula), '\\n')",
  "  }",
  "  binary_vars <- variable_types$variable[variable_types$type == 'binary']",
  "  if (length(binary_vars) > 0) {",
  "    for (binary_var in binary_vars) {",
  "      predictors <- setdiff(variable_types$variable, binary_var)",
  "      formula <- as.formula(paste(binary_var, '~', paste(predictors, collapse = ' + ')))",
  "      models[[paste('logistic_model', binary_var, sep = '_')]] <- glm(formula, data = data, family = binomial)",
  "      cat('Logistic model created for', binary_var, 'with formula:', deparse(formula), '\\n')",
  "    }",
  "  }",
  "  return(models)",
  "}",
  "models <- brainstorm_models(variable_types, random_data)",
  "print(summary(models$linear_model))",
  "print(summary(models$logistic_model_E))",
  "```"
), "modeling.Rmd")

# Render RMarkdown to HTML
rmarkdown::render("modeling.Rmd")
