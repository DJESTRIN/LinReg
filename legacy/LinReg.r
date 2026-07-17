library(R6)
library(optparse)
library(MuMIn)
library(lme4)
library(emmeans)

# LinReg class
LinReg <- R6Class(
  "LinReg",
  public = list(
    distribution = NULL,
    regression_type = NULL,
    backbone = NULL,

    #Build object
    initialize = function(regression_type, distribution) {
      self$regression_type <- regression_type
      self$distribution <- distribution
      self$get_backbone_package()},

    # Get suggested underlying package
    get_backbone_package = function() {
        if (self$regression_type == "glmm"){
            self$backbone <- "glmer"
        } else if (self$regression_type == "lmm"){
            self$backbone <- "lmer"
        } else if (self$regression_type == "glm"){
            self$backbone <- "glmer"
        } else if (self$regression_type == "lm"){
            self$backbone <- "lmer"
        }
    },

    # Build model based on user defined formula
    build_model = function(formula){
      self$current_model <- lmer(formula,data=self$dataframe)},

    # Print all attributes of object
    print = function() {
      cat("LinReg: \n")
      cat("Regression Type: ", self$regression_type, "\n")
      cat("Data Distribution: ", self$distribution, "\n")
      cat("Backbone: ", self$backbone, "\n")},

    # Calculate Estimated Marginal Means
    emmeans = function(variable) {
      self$current_emmeans<-emmeans(self$current_model, ~variable)
      self$emmeans_summary<-summary(self$current_emmeans)},

    # Test normailty via shapiro test
    check_normality = function(...) {
      additional_args <- list(...) 
      if (length(additional_args) == 0) {
        shapiro.test(additional_args[0])
        } else {shapiro.test(self$y)}},

    # Calculate R^2 for current model
    get_R2 = function() {
      self$current_r2 <- r.squaredGLMM(self$current_model)},

    # Calculate F-tests for current model's fixed effects and interactions
    omnibus_tests = function() {
      self$omnibus_results <- anova(self$current_model) },

    # Post hoc t-tests for comparing estimated marginal means
    post_hoc_ttests = function(correction="bonferroni") {
      pairs(self$current_emmeans, adjust = correction)},

    # Generate plots of model's performance
    model_performance_plots = function() {
      cat("Not set up yet")},

    # Calculate Akaike information criterion 
    model_AIC_test = function () {
      self$current_model_aic <- AIC(self$current_model)},

    #Compare the fit of two or more models via anova
    model_comparisons = function (...) {
      models_oh <- list(...)
      self$model_comparisons_oh <- aov(models_oh)},

    # Handle missing data ... may be moved to different class
    missing_data = function () {
      cat("Not set up yet")},

    #Plot regression results. Or plot model results generally. 
    plot_regression = function() {
      cat("Not set up yet")},

    # Calculate Cohen's D
    effect_sizes = function(effect_name) {
      fixed_effects <- fixef(model)
      standard_errors <- sqrt(diag(vcov(model)))
      self$current_cohen_d <- fixed_effects[effect_name] / standard_errors[effect_name]},

    # general pipeline for running code
    pipeline = function(formula) {

    }
  )
)


# Create an instance of the Person class
p <- LinReg$new("lmm", "normal")
p$print()
