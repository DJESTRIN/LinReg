library(R6)

# Define an R6 class
LinReg <- R6Class(
  "LinReg",
  public = list(
    distribution = NULL,
    regression_type = NULL,
    backbone = NULL,
    initialize = function(regression_type, distribution) {
      self$regression_type <- regression_type
      self$distribution <- distribution
      self$get_backbone_package()
    },
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
    print = function() {
      cat("LinReg: \n")
      cat("Regression Type: ", self$regression_type, "\n")
      cat("Data Distribution: ", self$distribution, "\n")
      cat("Backbone: ", self$backbone, "\n")
    },
    emmeans = function() {},
    check_normality = function() {},
    get_R2 = function() {},
    omnibus_tests = function() {},
    post_hoc_ttests = function() {},
    model_performance_plots = function() {},
    model_AIC_test = function () {},
    model_comparisons = function () {},
    missing_data = function () {},
    pipeline = function() {},
    plot_regression() {}
  )
)

# Create an instance of the Person class
p <- LinReg$new("lmm", "normal")
p$print()