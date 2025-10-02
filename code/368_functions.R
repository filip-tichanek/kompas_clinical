#' @title run: Run or Load & Save Models
#'
#' @description This function either loads an existing model from a specified 
#' file path if it has been saved previously, or runs the provided model 
#' expression and saves it. The function checks if the `reuse` parameter is set
#' to TRUE, in which case it attempts to load the model from the specified path. 
#' If the model does not exist or cannot be read, or if `reuse` is FALSE, 
#' the function will evaluate the expression and save the result if `reuse` 
#' is still TRUE. The function can be used for any computationally demnding
#' task.
#'
#' @param expr An expression defining the model to run.
#' @param path The file path where the model should be saved, used for both 
#'        saving and loading the model.
#' @param reuse A boolean indicating whether to attempt to reuse saved models. 
#'        If TRUE, it will try to load the model from the given path, 
#'        and if the model cannot be loaded or does not exist,
#'        it will run the expression and save the result.
#' @return The model object, either loaded or evaluated from the expression.
#' @examples
#' # Example usage:
#' # Assuming you have a model expression my_model_expr and a path my_model_path
#' result <- run(my_model_expr, my_model_path, reuse = TRUE)

run <- function(expr, path, reuse = TRUE) {
  # Initialize 'fit' to NULL to ensure it's always defined
  fit <- NULL
  
  # Construct the file path only if 'reuse' is TRUE
  if (reuse) {
    path <- paste0(path, ".Rds")
    fit <- suppressWarnings(try(readRDS(path), silent = TRUE))
    
    # Check if 'fit' is an error (file not found or couldn't be read)
    if (inherits(fit, "try-error")) {
      fit <- NULL
    }
  }
  
  if (is.null(fit)) {
    fit <- eval(substitute(expr))
    
    # Save 'fit' only if 'reuse' is TRUE and a valid 'path' is provided
    if (reuse && !is.null(path) && nzchar(path)) {
      saveRDS(fit, file = path)
    }
  }
  return(fit)
}





#' @title pltmd: Generate Diagnostic Plots for a Fitted Model Object
#'
#' @description This function generates diagnostic plots for a fitted model object. 
#' When quantile regression model is visualized, use `quant` argument for specification
#' of quantile. For example, if 'tau = c(0.1, 0.5, 0.9)', and the median is of interest,
#' `quant` should be set to 2 to visualize the median.
#'
#' @param model The model object from which predictions and residuals will be extracted.
#' @param quant An optional integer indicating which quantile's results to use for the plots.
#' @return None, but prints combined diagnostic plots directly.
#' @import ggplot2
#' @import gridExtra
#' @examples
#' # Assuming 'model' is a fitted 'lqmm' model object:
#' pltmd(model, 2)
pltmd <- function(model, quant = NULL) {
  # Create a data frame from the model based on the specified quantile.
  if (is.null(quant)) {
    model_df <- data.frame(
      fitted = predict(model),
      stdresid = residuals(model, type = "pearson")
    )
  } else {
    model_df <- data.frame(
      fitted = predict(model, level=1)[, quant],
      stdresid = residuals(model, type = "pearson", level=1)[, quant]
    )
  }
  
  # Generate diagnostic plots
  plot1 <- ggplot(model_df, aes(fitted, stdresid)) +
    geom_point(color = 'skyblue4', alpha=0.6) +
    geom_smooth(method = "loess") +
    labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
    theme(plot.title = element_text(size = 9),
          text = element_text(size=9))
  
  plot2 <- ggplot(model_df, aes(sample = stdresid)) +
    stat_qq(color = 'skyblue4', alpha=0.99, pch=1) +
    stat_qq_line() +
    labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
    theme(plot.title = element_text(size = 9),
          text = element_text(size=9))
  
  plot3 <- ggplot(model_df, aes(fitted, sqrt(abs(stdresid)))) +
    geom_point(color = 'skyblue4', alpha=0.6) +
    geom_smooth(method = "loess") +
    labs(title = "Scale-Location", x = "Fitted values", y = "Sqrt(|Standardized residuals|)") +
    theme(plot.title = element_text(size = 9),
          text = element_text(size=9))
  
  plot4 <- ggplot(model_df, aes(stdresid)) +
    geom_histogram(fill = 'skyblue2', col='grey60') +
    labs(title = "Standardized residuals distribution") +
    theme(plot.title = element_text(size = 9),
          text = element_text(size=9))
  
  # Combine and display the plots
  plots <- list(plot1, plot2, plot3, plot4)
  combined_plot <- plot_grid(plotlist = plots, ncol = 2)
  print(combined_plot)
}

  



#' @title p_val: Compute Bootstrap-Based P-Value
#'
#' @description This function calculates a p-value using bootstrapped coefficients
#' to test the hypothesis that these coefficients differ from a reference value,
#' typically representing a null effect. It supports both two-sided and one-sided
#' tests. Two-sided tests check if the coefficients are significantly different
#' in either direction from the reference value, while one-sided tests check for
#' differences in a specified direction only.
#'
#' @param data A vector of bootstrapped coefficients.
#' @param dir The direction for testing: "max" for two-sided tests, "<" for testing
#' if less than the reference, ">" for testing if greater than the reference.
#' @param tres The reference value against which the coefficients are tested.
#' @param mc A small constant added to both numerator and denominator to stabilize
#' the calculation, default is 0.1.
#' @return The computed p-value.
#' @examples
#' # Assuming `boot_coefs` is a numeric vector of bootstrapped coefficients and 
#' the null hypothesis value is 0:
#' p_val(boot_coefs, "max", 0)  # Two-sided test
#' p_val(boot_coefs, "<", 0)    # One-sided test, checking if less than 0
#' p_val(boot_coefs, ">", 0)    # One-sided test, checking if greater than 0
p_val <- function(data, dir, tres, mc = 0.1) {
  if (dir == 'max') {
    return((mc + min(length(data[data > tres]), length(data[data < tres]))) /
             (mc + (length(data) * 0.5)))
  } else if (dir == '<') {
    return((mc + length(data[data < tres])) / (mc + length(data)))
  } else if (dir == '>') {
    return((mc + length(data[data > tres])) / (mc + length(data)))
  } else {
    print('ERROR: Invalid direction specified. Use "max", "<", or ">".')
  }
}





#' @title AICc
#' @description Calculate sample-size-corrected AIC using information in 'model'.
#' The `model` is a fitted `rq` object from `quantreg` R package.
#' @param model A fitted `rq` object.
#' @return AICc value.
#' @export
AICc <- function(model) {
  
  n <- nrow(dat_mod)
  
  if(class(model)[1] == 'rqfit'){
    k <- dim(model$x)[2]
  } else if(class(model)[1] == 'lmerMod') {
    
    k <- ncol(ranef(model)[[1]]) + length(model@beta)
    
    } else {
      k <- length(coef(model))
}
  aicc <- AIC(model) + ((2 * k * (k + 1)) / (n - k - 1))
  return(aicc)
}





#' @title exte
#'
#' @description The function takes a data frame object of bootstrapped regression coefficients 
#' (a `model` argument) and summarizes regression coefficients, their 95% confidence intervals, 
#' and p-value. An `nquantile` argument determines the order of the quantile of interest in the 
#' `tau` argument. The function is dependent on the `p_val` function defined above.
#'
#' @param model A data frame object containing bootstrapped regression coefficients.
#' @param nquantile An integer specifying the order of the quantile of interest in the `tau` argument.
#' @param nround An integer specifying the number of digits to round the results to (default is 2).
#'
#' @return A data frame containing the estimated coefficients, their 95% confidence intervals, 
#' and p-values.
#'
#' @details This function summarizes regression coefficients, their confidence intervals, and 
#' p-values based on bootstrapped regression coefficients. It calculates the median estimate, 
#' lower and upper bounds of the 95% confidence interval, and the p-value for each coefficient.
#'
#' @seealso \code{\link{p_val}}
#'
#' @examples
#' # Create a sample data frame of bootstrapped regression coefficients
#' model <- data.frame(
#'   Intercept = rnorm(100),
#'   GroupA = rnorm(100),
#'   GroupB = rnorm(100)
#' )
#' # Summarize the coefficients
#' exte(model, nquantile = 2)
#'
#' @export
exte <- function(model, nquantile, nround = 2) {
  
  dff <- data.frame(model[[nquantile]]) %>%
    mutate(
      OM = `X.Intercept.`,
      VG = `X.Intercept.` + GRPVG,
      VN = `X.Intercept.` + GRPVN
    ) %>%
    mutate(
      VN_OM = VN - OM,
      VG_OM = VG - OM,
      VN_VG = VN - VG
    ) %>%
    select(
      VN_OM, VG_OM, VN_VG, everything()
    ) %>%
    select(
      -`X.Intercept.`, -GRPVG, -GRPVN, -OM,
      -VG, -VN
    )
  
  results <- data.frame(
    t(
      sapply(
        dff,
        function(p) quantile(p, probs = c(0.5, 0.025, 0.975))
      )
    ),
    p_val = sapply(
      dff, function(x) p_val(x, dir = 'max', tres = 0)
    )
  )
  
  colnames(results) <- c("Estimate", "CI-L", "CI-U", "P")
  
  results[, c(2:3)] <- round(results[, c(2:3)], nround)
  
  return(results)
}





#' @title rqfit
#'
#' @description The function pre-specifies default quantile regression models, with possible 
#' addition (`include` argument) or exclusion (`exclude` argument) of other predictors. 
#' `quantiles` enables specifying quantiles of interest to be modeled. `type` specifies which 
#' predefined models will be run (3 options: `adult`, `child`, `child_old`).
#'
#' @param exclude A character vector specifying variables to exclude from the model formula.
#' @param include A character vector specifying variables to include in the model formula.
#' @param quantiles A numeric vector specifying the quantiles of interest to be modeled.
#' @param type A character scalar specifying the type of predefined models to run.
#'
#' @return A fitted quantile regression model.
#'
#' @details This function pre-specifies default quantile regression models, `quantreg` package, 
#' based on the specified type. It allows for the inclusion or exclusion of 
#' predictors and specifies the quantiles of interest to be modeled.
#'
#' @seealso \code{\link[rq]{rq}}
#'
#' @examples
#' # Fit a quantile regression model for adults
#' rqfit(type = 'adult')
#'
#' @export
rqfit <- function(exclude = NULL, include = NULL, 
                  quantiles = c(0.2, 0.5, 0.8),
                  type = 'child') {
  exclude <- as.character(exclude)
  include <- as.character(include)
  
  # Define the base part of the model formula
  if(type == 'adult') {
    base_formula <- "outcome ~ 
  SEX + 
  GRP + 
  aAGE"
  } else if(type == 'child_old') {
    base_formula <- "outcome ~ 
  SEX + 
  aBreastFeed_full_duration + 
  GRP + 
  log2_age"
  } else {
    base_formula <- "outcome ~ 
  SEX + 
  aBreastFeed_full_stopped + 
  aBreastFeed_full_duration + 
  aBreastFeed_total_stopped + 
  GRP + 
  log2_age"
  }
  
  # Dynamically modify the formula to exclude specified variables
  if (!is.null(exclude) && length(exclude) > 0) {
    for (var in exclude) {
      base_formula <- gsub(paste0("\\+?\\s*", var, "\\s*\\+?"), " + ", base_formula)
    }
  }
  
  # Remove trailing or leading pluses
  base_formula <- gsub("^\\s*\\+\\s*|\\s*\\+\\s*$", "", base_formula)
  # Replace multiple pluses with a single plus
  base_formula <- gsub("\\s*\\+\\s*\\+\\s*", " + ", base_formula)
  
  # Dynamically add included variables, if any
  if (!is.null(include) && length(include) > 0) {
    included_vars <- paste(include, collapse = " + ")
    base_formula <- paste(base_formula, included_vars, sep = " + ")
  }
  
  # Define the full model formula
  model_formula <- as.formula(base_formula)
  
  # Fit the model
  model <- rq(
    model_formula, 
    tau = quantiles,
    data = dat_mod,
    ci = FALSE
  )
  
  return(model)
}





#' @title clust_boot
#'
#' @description The function takes a quantile regression model fitted with the `quantreg` package 
#' (`model`) and provides R bootstrap resamplings of the families (`FAM` variable). The `jitter` 
#' argument is added to avoid a singular matrix.
#'
#' @param model A quantile regression model fitted with the `quantreg` package.
#' @param seed An integer specifying the random seed for reproducibility.
#' @param R An integer specifying the number of bootstrap resamplings.
#' @param jitter A logical indicating whether to add jitter to the data to avoid a singular matrix.
#'
#' @return A list of bootstrap estimates for each quantile.
#'
#' @details This function performs R bootstrap resamplings of the families in the dataset and fits 
#' quantile regression models to each bootstrap sample. Jitter is optionally added to the data to 
#' avoid a singular matrix.
#'
#' @seealso \code{\link[rq]{rq}}
#'
#' @examples
#' # Perform cluster bootstrap
#' clust_boot(model = my_model, seed = 123, R = 100, jitter = TRUE)
#'
#' @export
clust_boot <- function(model, seed, R, jitter = TRUE) {
  
  set.seed(seed) 
  quantils <- model$tau
  
  model_data <- data.frame(FAM = dat_mod$FAM, 
                           model$model)
  
  matres <- matrix(NA,
                   nrow = R,
                   ncol = dim(model$x)[2])
  colnames(matres) <- colnames(model$x)
  
  boot_estimates <- replicate(length(quantils), matres, simplify = FALSE)
  
  # Perform bootstrap with clustering
  for (i in 1:R) {
    # Sample clusters
    sampled_FAM <- sample(unique(model_data$FAM), replace = TRUE)
    FAM_tibble <- tibble(FAM = sampled_FAM)
    
    boot_data <- FAM_tibble %>%
      left_join(model_data, by = "FAM",
                relationship = "many-to-many") 
    
    if (jitter) {
      boot_data_jittered <- boot_data %>%
        mutate(across(where(is.numeric), ~ jitter(.x)))
    }
    
    # Fit the model to the bootstrap sample
    if (jitter) {
      boot_model <- rq(model$formula, 
                       data = boot_data_jittered, tau = quantils)
    } else {
      boot_model <- rq(model$formula, 
                       data = boot_data, tau = quantils)
    }
    
    for (ii in 1:length(quantils)) {
      boot_estimates[[ii]][i, ] <- coef(boot_model)[, ii]
    }
  }
  
  return(boot_estimates)
}





#' @title add_AIC
#'
#' @description The function only automates the addition of a new row to the AIC change tables for children groups.
#'
#' @param table The AIC change table to which the new row will be added.
#'
#' @return The updated AIC change table with the new row added.
#'
#' @examples
#' # Add a new row to the AIC change table
#' add_AIC(my_AIC_table)
#'
#' @export
add_AIC <- function(table, mixef = FALSE) {
  bAIC <- AICc(mod_main)
  
  if(mixef == FALSE){
    estimands <- c("q20", "median", "q80")
    for (i in seq_along(estimands)) {
      newrow <- data.frame(
      outcome = column_name, 
      estimand = estimands[i],
      log2_age = safe_AIC_diff(bAIC, mod_log2_age, i),
      aSEX = safe_AIC_diff(bAIC, mod_nsex, i),
      Breast_Feed = safe_AIC_diff(bAIC, mod_nbf, i),
      other_cov = safe_AIC_diff(bAIC, mod_other_cov, i),
      diet = safe_AIC_diff(bAIC, mod_ndiet, i)
    )
    table <- rbind(table, newrow)
    }
  } else {
    estimands <- c("mean")
    for (i in seq_along(estimands)) {
      newrow <- data.frame(
        outcome = column_name, 
        estimand = estimands[i],
        log2_age = safe_AIC_diff(bAIC, mod_log2_age, i),
        aSEX = safe_AIC_diff(bAIC, mod_nsex, i),
        Breast_Feed = safe_AIC_diff(bAIC, mod_nbf, i),
        other_cov = safe_AIC_diff(bAIC, mod_other_cov, i),
        diet = safe_AIC_diff(bAIC, mod_ndiet, i),
        family = safe_AIC_diff(bAIC, mod_nonran, i)
      )
      table <- rbind(table, newrow)
    }
    
  }
  
  return(table)
}





#' @title add_AIC_adult
#'
#' @description The function only automates the addition of a new row to the AIC change tables for adults.
#'
#' @param table The AIC change table to which the new row will be added.
#'
#' @return The updated AIC change table with the new row added.
#'
#' @examples
#' # Add a new row to the AIC change table for adults
#' add_AIC_adult(my_AIC_table)
#'
#' @export
add_AIC_adult <- function(table, mixef = FALSE) {
  bAIC <- AICc(mod_main)
  
  if(mixef == FALSE){
    estimands <- c("q20", "median", "q80")
    for (i in seq_along(estimands)) {
      newrow <- data.frame(
        outcome = column_name, 
        estimand = estimands[i],
        aAGE = safe_AIC_diff(bAIC, mod_age, i),
        aSEX = safe_AIC_diff(bAIC, mod_nsex, i),
        other_cov = safe_AIC_diff(bAIC, mod_other_cov, i),
        diet = safe_AIC_diff(bAIC, mod_ndiet, i)
      )
      table <- rbind(table, newrow)
    }
  } else {
    estimands <- c("mean")
    for (i in seq_along(estimands)) {
      newrow <- data.frame(
        outcome = column_name, 
        estimand = estimands[i],
        aAGE = safe_AIC_diff(bAIC, mod_age, i),
        aSEX = safe_AIC_diff(bAIC, mod_nsex, i),
        other_cov = safe_AIC_diff(bAIC, mod_other_cov, i),
        diet = safe_AIC_diff(bAIC, mod_ndiet, i),
        family = safe_AIC_diff(bAIC, mod_nonran, i)
      )
      table <- rbind(table, newrow)
    }
    
  }
  
  return(table)
}





#' @title safe_AIC_diff
#'
#' @description Calculate the difference between the AIC of two models, returning NA when one of the models is missing.
#'
#' @param baseline The AIC of the baseline model.
#' @param compared_model The AIC of the model being compared.
#' @param ind The index of the model being compared.
#'
#' @return The difference in AIC between the baseline model and the model being compared, or NA if the model being compared is missing.
#'
#' @examples
#' # Calculate the AIC difference between two models
#' safe_AIC_diff(baseline_AIC, compared_AIC, 1)
#'
#' @export
safe_AIC_diff <- function(baseline, compared_model, ind = NULL) {
  tryCatch({
    if(is.null(ind)){
      baseline - AICc(compared_model)
    } else{
    baseline[ind] - AICc(compared_model)[ind]}
  }, error = function(e) {
    # Return NA in case of an error (e.g., model not present)
    NA
  })
}





#' @title add_eff
#'
#' @description The function only automates the addition of a new row to the diet difference table.
#'
#' @param table The diet difference table to which the new row will be added.
#' @param res The result matrix containing diet differences and p-values.
#'
#' @return The updated diet difference table with the new row added.
#'
#' @examples
#' # Add a new row to the diet difference table
#' add_eff(my_diet_table, my_results)
#'
#' @export
add_eff <- function(table, res, mixef = FALSE) {
  
  if(mixef == FALSE){
    estimands <- c("q20", "median", "q80")
    } else {
      estimands <- c("mean")
      }
    for (i in seq_along(estimands)) {
      pos2 <- 1 + ((i-1)*4)
      newrow <- data.frame(
        outcome = column_name, 
        estimand = estimands[i],
        VN_OM_diff  = res[1, pos2],
        VN_OM_P = res[1, 4*i],
        VG_OM_diff  = res[2, pos2],
        VG_OM_P = res[2, 4*i],
        VN_VG_diff  = res[3, pos2],
        VN_VG_P = res[3, 4*i]
        )
      table <- rbind(table, newrow)
      } 
  return(table)
}

#' @title heat_map
#'
#' @description Calculating z-score of model coefficients and visualizing as volcano plots. Visualizing AIC changes as heatmaps.
#'
#' @param df The data frame containing model coefficients.
#'
#' @return A plot displaying the z-scores of model coefficients and AIC changes as heatmaps.
#'
#' @examples
#' # Visualize model coefficients and AIC changes as heatmaps
#' heat_map(my_dataframe)
#'
#' @export
heat_map <- function(df, mixef = FALSE, main = 'Mean', listt = FALSE){
  
  df <- df %>%
    mutate(across(where(is.numeric), ~ifelse(.x > 0, 0, ifelse(.x < -30, -30, .x))))
  
  if(mixef == TRUE){
    rownames(df) <- df$outcome
    p <- pheatmap(df[,c(-1,-2)], 
                  show_rownames = TRUE,
                  cluster_rows = FALSE, 
                  cluster_cols = FALSE,
                  na_col = "black",
                  fontsize = 7,
                  main = main, 
                  silent = TRUE)
    if(listt == FALSE){
      plot_list <-  list()
      plot_list[['p1']] <- p[[4]]
    }else{
      return(p)
    }

    grid.arrange(grobs = plot_list, ncol = 1, main = "AIC change")
    } else {
      q20_df <- df[df$estimand=="q20",]
      rownames(q20_df) <- q20_df$outcome
      p1 <- pheatmap(q20_df[,c(-1,-2)], 
                 show_rownames = TRUE,
                 cluster_rows = FALSE, 
                 cluster_cols = FALSE,
                 na_col = "black",
                 fontsize = 7,
                 main = "20% quantile", 
                 silent = TRUE)
      
      q50_df <- df[df$estimand=="median",]
      rownames(q50_df) <- q50_df$outcome
      p2 <- pheatmap(q50_df[,c(-1,-2)],show_rownames = TRUE,
                 cluster_rows = FALSE, 
                 cluster_cols = FALSE,
                 na_col = "black",
                 fontsize = 7,
                 main = "Median", 
                 silent = TRUE)
      
      q80_df <- df[df$estimand=="q80",]
      rownames(q80_df) <- q80_df$outcome
      p3 <- pheatmap(q80_df[,c(-1,-2)], show_rownames = TRUE,
                 cluster_rows = FALSE, 
                 cluster_cols = FALSE,
                 na_col = "black",
                 fontsize = 7,
                 main = "80% quantile", 
                 silent = TRUE)
  
      plot_list <-  list()
      plot_list[['p1']] <- p1[[4]]
      plot_list[['p2']] <- p2[[4]]
      plot_list[['p3']] <- p3[[4]]
      
      grid.arrange(grobs = plot_list, ncol = 3, main = "AIC change")}
  }



#' @title rlme
#'
#' @description This function fits predefined linear mixed-effects models with options to 
#' dynamically include or exclude predictors. It also provides an option to omit random effects and
#' fit the model using either linear, or robust, (mixed-effects) regression.
#'
#' @param exclude A character vector specifying variables to be excluded from the model 
#' formula.
#' @param include A character vector specifying variables to be included in the model 
#' formula.
#' @param type A character scalar specifying the type of predefined models to run. 
#' Valid options are 'adult', 'child', and 'child_old'.
#' @param main A boolean indicating whether the model should be fitted using robust 
#' methods (`TRUE`) or simpler methods (`FALSE`). Default is FALSE.
#' @param remove_random A boolean indicating whether to remove random effects from 
#' the model. If `TRUE`, the model is fitted using `lm()` or `rlm()` depending on the 
#' `main` parameter. Default is FALSE.
#'
#' @return An object of class `merMod` for mixed-effects models or `lm`/`rlm` for linear/robust
#' regression models without random effects.
#'
#' @details The function dynamically constructs a model formula based on input parameters
#' and the specified model type. It is designed for flexibility in various datasets and 
#' analysis requirements.
#'
#' @examples
#' # Fit a robust mixed-effects model for children with additional predictors
#' rlme(include = c("newPredictor1", "newPredictor2"), type = 'child', main = TRUE)
#'
#' # Fit a linear model for adults without random effects
#' rlme(type = 'adult', remove_random = TRUE, main = FALSE)
#'
#' @importFrom lme4 lmer
#' @importFrom robustlmm rlmer
#' @importFrom MASS rlm
#' @export

rlme <- function(exclude = NULL, include = NULL,
                 type = "child", main = FALSE, remove_random = FALSE) {
  exclude <- as.character(exclude)
  include <- as.character(include)

  # Define the base part of the model formula
  base_formula <- switch(type,
    "adult" = "outcome ~ GRP + SEX + aAGE",
    "child_old" = "outcome ~ GRP + SEX + aBreastFeed_full_duration + log2_age",
    "outcome ~ GRP + SEX + aBreastFeed_full_stopped + aBreastFeed_full_duration + aBreastFeed_total_stopped + log2_age"
  )

  # Add random effects if not removed
  if (!remove_random) {
    base_formula <- paste(base_formula, "+ (1|FAM)")
  }

  # Exclude variables if specified
  if (!is.null(exclude) && length(exclude) > 0) {
    for (var in exclude) {
      pattern <- paste0("\\+?\\s*", var, "\\s*\\+?")
      base_formula <- gsub(pattern, " + ", base_formula, perl = TRUE)
    }
  }

  # Clean up formula syntax
  base_formula <- gsub("^\\s*\\+\\s*|\\s*\\+\\s*$", "", base_formula)
  base_formula <- gsub("\\s*\\+\\s*\\+\\s*", " + ", base_formula)

  # Include additional variables if specified
  if (!is.null(include) && length(include) > 0) {
    included_vars <- paste(include, collapse = " + ")
    base_formula <- paste(base_formula, included_vars, sep = " + ")
  }

  # Define the full model formula
  model_formula <- as.formula(base_formula)

  # Fit the model based on 'main' and 'remove_random'
  if (remove_random) {
    if (main) {
      model <- rlm(model_formula, data = dat_mod)
    } else {
      model <- lm(model_formula, data = dat_mod)
    }
  } else {
    if (main) {
      model <- rlmer(model_formula, data = dat_mod)
    } else {
      model <- lmer(model_formula, data = dat_mod, REML = FALSE)
    }
  }

  return(model)
}



#' @title emm
#'
#' @description This function calculates estimated marginal means (EMMs) for all 
#' pairwise comparisons of a given grouping variable in the specified model, 
#' without any p-value adjustments.
#' The results include estimates, confidence intervals, and p-values, formatted in a 
#' data frame.
#'
#'
#'@param model An object representing a fitted linear (mixed-effect) model, 
#'either from `lme4::lmer`, `robustlmm::rlmer`, `MASS:rlm` or simple `lm`
#'
#' @return A data frame with columns for the estimate, lower and upper confidence intervals,
#' and p-values for each pairwise comparison.
#'
#' @importFrom tidyverse %>%
#' @importFrom emmeans emmeans pairs
#' @import lme4
#' @import robustlmm
#' @import MASS
#' @examples
#' # For lmer model:
#' # model <- lme4::lmer(response ~ treatment + (1|group), data = dataset)
#' # For rlmer model:
#' # model <- robustlmm::rlmer(response ~ treatment + (1|group), data = dataset)
#' # res(model)

emm <- function(model){
  
  contrasts <- list(
    "VN vs OM" = c(OM = -1, VG =  0, VN = 1),
    "VG vs OM" = c(OM = -1, VG =  1, VN = 0),
    "VN vs VG" = c(OM =  0, VG = -1, VN = 1)
  )
  
  emm <- emmeans(mod_main, ~ GRP)
  tr <- contrast(emm, 
                 contrasts, 
                 adjust = "none", 
                 infer = c(TRUE, TRUE)) %>% 
    data.frame()
  
  tr <- tr[, c(1:2, 5:6, 8)]
  rownames(tr) <- tr$contrast
  tr <- tr %>% select(-contrast)
  colnames(tr) <- c('Estimate', 'CI-L', 'CI-U', 'P')
  return(tr)
}





#' Clustered Data Sampler
#'
#' This function generates resampled datasets from clustered data using various sampling methods.
#'
#' @param data A data frame containing the dataset to be sampled.
#' @param clust_id A string specifying the column name in `data` that identifies the clusters.
#' @param outcome A string specifying the column name in `data` that represents the outcome variable.
#' @param seed An optional integer for setting the random seed to ensure reproducibility.
#' @param sample_method A string specifying the sampling method. Options are 'boot' for bootstrap,
#' 'atypboot' for atypical bootstrap, and any other string for k-fold cross-validation. Default is 'boot'.
#' @param N An integer specifying the number of bootstrap samples or atypical bootstrap samples to
#' generate. Default is 10.
#' @param k An integer specifying the number of folds for cross-validation. Default is 10.
#'
#' @return Depending on `sample_method`, the function returns:
#' \itemize{
#'   \item If `sample_method` is 'boot', a list of `N` bootstrap samples.
#'   \item If `sample_method` is 'atypboot', a list containing two lists: the training datasets and
#'   the validation datasets.
#'   \item Otherwise, a list containing two lists: the training datasets and the validation datasets
#'   for k-fold cross-validation.
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage with bootstrap method
#' sampled_data <- clustdat_sampler(data, clust_id = "cluster", outcome = "outcome", seed = 123,
#' sample_method = 'boot', N = 10)
#'
#' # Example usage with atypical bootstrap method
#' sampled_data <- clustdat_sampler(data, clust_id = "cluster", outcome = "outcome", seed = 123,
#' sample_method = 'atypboot', N = 10)
#'
#' # Example usage with k-fold cross-validation
#' sampled_data <- clustdat_sampler(data, clust_id = "cluster", outcome = "outcome", seed = 123,
#' sample_method = 'kfold', k = 5)
#' }
#'
#' @export




clustdat_sampler <- function(data,
                             clust_id, 
                             outcome,
                             seed = NULL, 
                             sample_method = 'boot',
                             N = 10, 
                             k = 10){
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  data <- data %>% 
    mutate(obs_id = as.character(1:nrow(data)))
  
  if(colnames(data[clust_id]) != 'id'){
    data <- data %>% 
      mutate(id = data[[clust_id]]) %>% 
      select(-dplyr::all_of(clust_id))
  }
  
  if(colnames(data[outcome]) != 'outcome'){
    data <- data %>% 
      mutate(outcome = data[[outcome]]) %>% 
      select(-dplyr::all_of(outcome))
  }
  
  data <- data %>% 
    mutate(obs_id = as.character(1:nrow(data))) %>%
    select(obs_id, id, dplyr::everything())
  
  
  if(sample_method == 'boot'){
    
    reset <- list()
    
    for (i in 1:N) {
      tmp <- data.frame(id = sample(unique(data$id), 
                                    length(unique(data$id)), 
                                    replace = TRUE))
      
      tmp <- tmp %>% 
        mutate(id_sec = factor(1:nrow(tmp))) %>% 
        left_join(data,
                  by = 'id',
                  relationship = "many-to-many")
      
      reset[[i]] <- tmp
    }
    
    return(reset)
    
  } else if (sample_method == 'atypboot') {
    
    train_data <- list()
    valid_data <- list()
    
    for (i in 1:N) {
      repeat {
        train <- data.frame(id = sample(unique(data$id), 
                                        length(unique(data$id)), 
                                        replace = TRUE))
        
        temp_train <- train %>% 
          left_join(data, by = 'id', relationship = "many-to-many")
        
        temp_valid <- data.frame(
          obs_id = data[!data$obs_id %in% temp_train$obs_id, ]$obs_id) %>%
          left_join(data, by = 'obs_id')
        
        if (!mean(temp_train$outcome) %in% c(0, 1) & !mean(temp_valid$outcome) %in% c(0, 1)) {
          train_data[[i]] <- temp_train
          valid_data[[i]] <- temp_valid
          {break} 
        }
      }
    }
    
    return(list(train_data, valid_data))
    
    
  } else {
    
    unique_ids <- unique(data$id)
    
    id_groups <- sample(rep(1:k, length.out = length(unique_ids)))
    
    train_data <- list()
    valid_data <- list()
    
    for (i in 1:k) {
      ids_include <- unique_ids[id_groups != i]
      ids_not_include <- unique_ids[id_groups == i]
      
      train_data[[i]] <- data %>%
        filter(id %in% ids_include)
      
      valid_data[[i]] <- data %>%
        filter(id %in% ids_not_include)
      
    }
    
    return(list(train_data, valid_data))
    
  }
  
}


  


#' clustered_glmnet
#'
#' This function fits a GLMNET model to clustered data with different sampling methods.
#'
#' @param data A data frame containing the dataset to be sampled.
#' @param outcome A string specifying the column name in `data` representing the outcome variable.
#' @param clust_id A string specifying the column name in `data` identifying the clusters.
#' @param sample_method A string specifying the sampling method. Options are 'boot' for bootstrap,
#' 'atypboot' for the bootstrap specifically designed for predictive model validation 
#' (standard cluster bootstrap to get training data and rest data for validation),
#'  and 'cv' for cross-validation. Default is 'boot'.
#' @param N An integer specifying the number of bootstrap or atypical bootstrap samples to
#' generate. Default is 10.
#' @param k An integer specifying the number of folds for cross-validation. Default is 10.
#' @param alphas A numeric vector of alpha values to be tested in GLMNET. Default is seq(0, 1, by = 0.2).
#' @param family A string specifying the family for the GLMNET model. 
#' Default is 'binomial' (currently the only option).
#' @param seed An optional integer for setting the random seed to ensure reproducibility. Default is 123.
#'
#' @return A list containing the following elements:
#' \itemize{
#'   \item \code{model_summary} A data frame with model summary statistics.
#'   \item \code{valid_performances} A data frame with validation performances.
#'   \item \code{predictions} A data frame with predictions.
#'   \item \code{betas} A matrix with model coefficients.
#'   \item \code{plot} A ggplot object with the calibration plot.
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage with default parameters
#' result <- clustered_glmnet(data, outcome = "outcome", clust_id = "cluster")
#' }
#'
#' @export

clustered_glmnet <- function(data,
                             outcome,
                             clust_id,
                             sample_method = 'boot',
                             N = 10,
                             k = 10, 
                             alphas = seq(0, 1, by = 0.2),
                             family = 'binomial',
                             seed = 123) {
  
  
  set.seed(seed)
  
  ## where to save relevant information
  auc_resamp_test <-  vector('double', N)
  accuracy_resamp_test <-  vector('double', N)
  auc_resamp_validation <- vector('double', N)
  accuracy_resamp_validation <- vector('double', N)
  predictions <- vector("list", N)
  
  
  ## original data in a matrix form
  original_outcome <- base::as.matrix(data[[outcome]])
  original_predictors <- data %>% 
    dplyr::select(-dplyr::all_of(c(outcome, clust_id))) %>% 
    as.matrix()
  
  
  
  # prediction on original sample
  
  ## optimize lambda and alpha
  lamb_1se <- vector('double', length(alphas))
  alpha <- vector('double', length(alphas))
  deviance <- vector('double', length(alphas))
  
  for(a in seq_along(alphas)){
    tr <- cv.glmnet(x = original_predictors, 
                    y = original_outcome, 
                    alpha = alphas[a], 
                    family = family,
                    type.measure = 'deviance')
    lamb_1se[a] <- tr[["lambda.1se"]]
    
    alpha[a] = alphas[a]
    deviance[a] = tr$cvm[which(tr$lambda == tr[["lambda.1se"]])]
  }
  
  optim_par <- data.frame(lamb_1se, alpha, deviance) %>% 
    arrange(deviance)
  
  
  ## fit with optimized hyperparameters
  fit <- glmnet(x = original_predictors, 
                y = original_outcome, 
                alpha = optim_par$alpha[1],
                lambda = optim_par$lamb_1se[1],
                family = family)
  
  
  ## get predictions and performance
  prediction <- data.frame(
    predicted_orig = as.numeric(predict(fit, newx = original_predictors)),
    outcome = original_outcome)
  
  fitted <- data.frame(
    alpha = optim_par$alpha[1],
    lambda = optim_par$lamb_1se[1],
    auc = roc(outcome ~ predicted_orig, 
              data = prediction,
              direction = '<',
              levels = c(0, 1))$auc,
    accuracy = mean(ifelse(
      prediction$predicted_orig > 0, 1, 0) == prediction$outcome))
  
  
  
  # glmnet on simulated data 
  
  
  ## simulated data
  
  sampled_data <- clustdat_sampler(data, 
                                   clust_id = clust_id, 
                                   outcome = outcome,
                                   sample_method = sample_method,
                                   N = N, 
                                   k = k, 
                                   seed = seed)
  
  for (i in 1:N){
    
    
    ## sampled data in a matrix form
    if(sample_method == 'boot'){ 
      sampled_outcome <- as.matrix(sampled_data[[i]]$outcome)
      sampled_predictors <- sampled_data[[i]] %>% 
        select(-outcome, -id, -id_sec, -obs_id) %>% 
        as.matrix()
    } else if(sample_method == 'cv'){
      sampled_outcome <- as.matrix(sampled_data[[1]][[i]]$outcome)
      sampled_predictors <- sampled_data[[1]][[i]] %>% 
        select(-outcome, -id, -obs_id) %>% 
        as.matrix()
    } else {
      sampled_outcome <- as.matrix(sampled_data[[1]][[i]]$outcome)
      sampled_predictors <- sampled_data[[1]][[i]] %>% 
        select(-outcome, -id, -obs_id) %>% as.matrix()
    }
    
    
    ## re-optimize alpha and lambda
    lamb_1se <- vector('double', length(alphas))
    alpha <- vector('double', length(alphas))
    deviance <- vector('double', length(alphas))
    
    for(a in seq_along(alphas)){
      tr <- cv.glmnet(x = sampled_predictors, 
                      y = sampled_outcome, 
                      alpha = alphas[a], 
                      family = family,
                      type.measure = 'deviance')
      lamb_1se[a] <- tr[["lambda.1se"]]
      
      alpha[a] = alphas[a]
      deviance[a] = tr$cvm[which(tr$lambda == tr[["lambda.1se"]])]
    }
    
    optim_par <- data.frame(lamb_1se, alpha, deviance) %>% 
      arrange(deviance)
    
    
    ## fit models with re-optimized hyperparameters
    sampled_fit <- glmnet(sampled_predictors,
                          sampled_outcome, 
                          alpha = optim_par$alpha[1],
                          lambda = optim_par$lamb_1se[1],
                          family = family)
    
    
    ## get predictions
    prediction_onSampled <- data.frame(
      predicted = as.numeric(predict(sampled_fit, newx = sampled_predictors)),
      outcome = sampled_outcome)
    
    if(sample_method == 'cv'){
      valid_outcome <- as.matrix(sampled_data[[2]][[i]]$outcome)
      valid_predictors <- sampled_data[[2]][[i]] %>% 
        select(-outcome, -id, -obs_id) %>% 
        as.matrix()
      
      prediction_onValidation <- data.frame(
        predicted = as.numeric(predict(sampled_fit, newx = valid_predictors)),
        outcome = valid_outcome,
        iteration = i)
      
    } else if(sample_method == 'atypboot'){
      valid_outcome <- as.matrix(sampled_data[[2]][[i]]$outcome)
      valid_predictors <- sampled_data[[2]][[i]] %>% 
        select(-outcome, -id, -obs_id) %>% 
        as.matrix()
      
      prediction_onValidation <- data.frame(
        predicted = as.numeric(predict(sampled_fit, newx = valid_predictors)),
        outcome = valid_outcome,
        iteration = i)
      
    } else {
      prediction_onValidation <- data.frame(
        predicted = as.numeric(predict(sampled_fit, newx = original_predictors)),
        outcome = original_outcome,
        iteration = i
      )
    } 
    
    
    ## record performances
    auc_resamp_test[i] <-  roc(outcome ~ predicted, 
                               data = prediction_onSampled,
                               direction = '<',
                               levels = c(0, 1))$auc
    
    accuracy_resamp_test[i] <-  mean(ifelse(
      prediction_onSampled$predicted > 0, 1, 0) == prediction_onSampled$outcome)
    
    auc_resamp_validation[i] <- roc(outcome ~ predicted, 
                                    data = prediction_onValidation,
                                    direction = '<',
                                    levels = c(0, 1))$auc
    
    accuracy_resamp_validation[i] <-  mean(ifelse(
      prediction_onValidation$predicted > 0, 1, 0) == prediction_onValidation$outcome)
    
    
    if(sample_method == 'boot'){
      predictions[[i]] <- data.frame(prediction_onValidation, id_obs = data[[clust_id]])
    } else{
      predictions[[i]] <- data.frame(prediction_onValidation, id_obs = sampled_data[[2]][[i]]$obs_id)
    }
    
    
  }
  
  ## connect predictions
  predictions <- bind_rows(predictions)
  
  
  predictions2 <- predictions %>% 
    group_by(id_obs) %>%
    summarise(
      predicted = mean(predicted),
      outcome = mean(outcome)
    ) %>% 
    ungroup()
  
  ## aggregate obtained information
  valid_performances <- data.frame(
    auc_resamp_test,
    auc_resamp_validation,
    auc_optimism = auc_resamp_test - auc_resamp_validation,
    accuracy_resamp_test,
    accuracy_resamp_validation,
    accuracy_optimism = accuracy_resamp_test - accuracy_resamp_validation)
  
  betas <- fit$beta
  
  if(sample_method == 'boot'){
    model_summary <- fitted %>% 
      mutate(
        auc_optimism_corrected = auc - mean(valid_performances$auc_optimism),
        auc_optimism_corrected_CIL = auc - quantile(valid_performances$auc_optimism, probs = 0.975),
        auc_optimism_corrected_CIU = auc - quantile(valid_performances$auc_optimism, probs = 0.025),
        
        auc_CI_L2 = quantile(valid_performances$auc_resamp_validation, 
                             probs = 0.025) - (
                               mean(valid_performances$auc_resamp_test) - auc),
        
        auc_CI_U2 = quantile(valid_performances$auc_resamp_validation, 
                             probs = 0.975) - (
                               mean(valid_performances$auc_resamp_test) - auc),
        
        accuracy_optimism_corrected = accuracy - mean(valid_performances$accuracy_optimism),
        
        accuracy_optimism_corrected_CIL = accuracy - quantile(
          valid_performances$accuracy_optimism, probs = 0.975),
        
        accuracy_optimism_corrected_CIU = accuracy - quantile(
          valid_performances$accuracy_optimism, probs = 0.025),
        
        
        accuracy_CI_L2 = quantile(valid_performances$accuracy_resamp_validation, 
                                  probs = 0.025) - (
                                    mean(valid_performances$accuracy_resamp_test) - accuracy),
        
        accuracy_CI_U2 = quantile(valid_performances$accuracy_resamp_validation, 
                                  probs = 0.975) - (
                                    mean(valid_performances$accuracy_resamp_test) - accuracy),
      ) %>% select(alpha, lambda, 
                   auc, auc_optimism_corrected:auc_CI_U2, 
                   accuracy, accuracy_optimism_corrected:accuracy_CI_U2)
  } else {
    
    model_summary <- fitted %>% 
      mutate(
        auc_optimism_corrected = mean(valid_performances$auc_resamp_validation),
        auc_optimism_corrected_CIL = quantile(
          valid_performances$auc_resamp_validation, probs = 0.025),
        
        auc_optimism_corrected_CIU = quantile(
          valid_performances$auc_resamp_validation, probs = 0.975),
        
        accuracy_optimism_corrected = mean(valid_performances$accuracy_resamp_validation),
        
        accuracy_optimism_corrected_CIL =  quantile(
          valid_performances$accuracy_resamp_validation, probs = 0.025),
        
        accuracy_optimism_corrected_CIU =  quantile(
          valid_performances$accuracy_resamp_validation, probs = 0.975)) %>% 
      
      select(alpha, lambda, auc, 
             auc_optimism_corrected:auc_optimism_corrected_CIU,
             accuracy, accuracy_optimism_corrected:accuracy_optimism_corrected_CIU)
    
  }
  
  calibration_plot <- suppressWarnings(
    predictions2 %>% 
      mutate(iteration = factor('A'),
             predicted = inv_logit(predicted)) %>% 
      
      ggplot(aes(x = predicted, y = outcome, group = iteration)) +
      
      
      geom_smooth(data = predictions,
                  aes(x = inv_logit(predicted), 
                      y = outcome),
                  se = FALSE,
                  color = 'grey35',
                  linewidth = 0.1,
                  method = 'loess',
                  span = 2/log10(nrow(prediction))) +
      
      geom_smooth(method = 'loess',
                  se = TRUE,
                  color = 'red',
                  fill = 'red', 
                  alpha = 0.25, 
                  span = 2/log10(nrow(prediction))) + 
      
      coord_cartesian(x = c(min(inv_logit(predictions$predicted)),
                            max(inv_logit(predictions$predicted))), 
                      y = c(0,1)) + 
      
      geom_abline(slope = 1, intercept = 0, linewidth = 1, linetype = 'dashed') +
      labs(x = "Prediction", y = "Outcome")
    
  )
  
  
  ## define outputs
  return(list(model_summary = model_summary, 
              valid_performances = valid_performances, 
              predictions = prediction, 
              betas = betas, 
              plot = calibration_plot) 
  )
  
}


#' compare_predmod
#'
#' This function compares two predictive models based on a specified measure.
#'
#' @param model_simpler A list containing the simpler model's performance data.
#' @param model_complex A list containing the complex model's performance data.
#' @param measure A string specifying the performance measure to compare. Default is
#' 'auc_resamp_validation' (currently the only option)
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{auc_dif} The mean difference in the specified measure.
#'   \item \code{CI_L} The lower bound of the confidence interval for the difference.
#'   \item \code{CI_U} The upper bound of the confidence interval for the difference.
#'   \item \code{P} The proportion of times the complex model outperforms the simpler model.
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage
#' comparison <- compare_predmod(model_simpler, model_complex, measure = 'auc_resamp_validation')
#' }
#'
#' @export


compare_predmod <- function(model_simpler, 
                            model_complex, 
                            measure = 'auc_resamp_validation'){
  
  model_simpler <- model_simpler$valid_performances
  model_complex <- model_complex$valid_performances
  
  min_nrow <- min(c(nrow(model_simpler), nrow(model_complex)))
  
  model_simpler <- model_simpler[1:min_nrow,]
  model_complex <- model_complex[1:min_nrow,]
  
  tr <- c(model_complex[[measure]] - model_simpler[[measure]])
  
  result <- data.frame(
    auc_dif = mean(tr),
    CI_L = quantile(tr, probs = 0.025),
    CI_U = quantile(tr, probs = 0.975),
    P = (min(c(length(tr[tr<0]), length(tr[tr>0]))) + 0.5)/(0.5 + length(tr)*0.5)
  )
  
  return(t(result))
  
}


## Simple functions

logit <- function(x){log(x/(1-x))}
inv_logit <- function(x){exp(x)/(1+exp(x))}

clean_outcome_names <- function(names) {
  str_replace_all(names, c("^a" = "", "log2_a" = "log2_"))
}
