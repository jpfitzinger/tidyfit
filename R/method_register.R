# Methods Register

METHOD <- R6::R6Class(
  "tidyFit.method",
  public = list(
    method = NULL,
    estimator = NULL,
    vector_args = NULL,
    list_args = NULL,
    no_grid_args = NULL,
    cv = NULL,
    regress = NULL,
    classify = NULL,
    has_predict_method = NULL,
    uses_index = NULL,
    multinomial = NULL,
    nonstandard_formula = NULL,
    has_importance_method = NULL,
    has_coef_method = NULL,

    initialize = function(method, estimator, regress, classify, multinomial,
                          cv, uses_index, nonstandard_formula,
                          vector_args, list_args, no_grid_args,
                          has_predict_method, has_coef_method, has_importance_method) {
      self$method <- method
      self$estimator <- estimator
      self$regress <- regress
      self$classify <- classify
      self$vector_args <- vector_args
      self$list_args <- list_args
      self$no_grid_args <- no_grid_args
      self$multinomial <- multinomial
      self$cv <- cv
      self$uses_index <- uses_index
      self$nonstandard_formula <- nonstandard_formula
      self$has_predict_method <- has_predict_method
      self$has_coef_method <- has_coef_method
      self$has_importance_method <- has_importance_method
    },

    check = function(type) {
      return(as.list(self)[[type]])
    }
  )
)

METHOD_REGISTER <- list(
  adalasso = METHOD$new(
    method = "adalasso", estimator = "glmnet::glmnet", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "offset", "lambda", "exclude", "lower.limits", "upper.limits"),
    list_args = c(),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  anova = METHOD$new(
    method = "anova", estimator = "stats::anova", regress = TRUE, classify = TRUE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "subset", "offset"),
    list_args = c("control", "contrasts"),
    no_grid_args = c("weights"),
    has_predict_method = FALSE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  bayes = METHOD$new(
    method = "bayes", estimator = "arm::bayesglm", regress = TRUE, classify = TRUE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("subset", "weights", "offset", "start", "etastart", "mustart", "prior.mean", "prior.scale", "prior.df"),
    list_args = c("contrasts", "control"),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  blasso = METHOD$new(
    method = "blasso", estimator = "monomvn::blasso", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("beta", "rd", "ab"),
    list_args = c(),
    no_grid_args = c(),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  bma = METHOD$new(
    method = "bma", estimator = "BMS::bms", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("start.value"),
    list_args = c(),
    no_grid_args = c(),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  bridge = METHOD$new(
    method = "bridge", estimator = "monomvn::bridge", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("beta", "rd", "ab"),
    list_args = c(),
    no_grid_args = c(),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  boost = METHOD$new(
    method = "boost", estimator = "mboost::glmboost", regress = TRUE, classify = TRUE,
    multinomial = FALSE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "offset", "oobweights"),
    list_args = c("control", "contrasts.arg"),
    no_grid_args = c("control"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  chisq = METHOD$new(
    method = "chisq", estimator = "stats::chisq.test", regress = FALSE, classify = TRUE,
    multinomial = TRUE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("p"), list_args = c(), no_grid_args = c(),
    has_predict_method = FALSE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  cor = METHOD$new(
    method = "cor", estimator = "stats::cor.test", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("subset"), list_args = c(), no_grid_args = c(),
    has_predict_method = FALSE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  enet = METHOD$new(
    method = "enet", estimator = "glmnet::glmnet", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "offset", "lambda", "exclude", "lower.limits", "upper.limits"),
    list_args = c(),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  genetic = METHOD$new(
    method = "genetic", estimator = "gaselect::genAlg", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c(),
    list_args = c(),
    no_grid_args = c("control"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  gets = METHOD$new(
    method = "gets", estimator = "gets::gets", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("subset", "weights", "offset"),
    list_args = c("contrasts"),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  glm = METHOD$new(
    method = "glm", estimator = "stats::glm", regress = TRUE, classify = TRUE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("subset", "weights", "offset", "start", "etastart", "mustart"),
    list_args = c("contrasts", "control"),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  glmm = METHOD$new(
    method = "glmm", estimator = "lme4::glmer", regress = TRUE, classify = TRUE,
    multinomial = FALSE, cv = FALSE, uses_index = TRUE, nonstandard_formula = TRUE,
    vector_args = c("subset", "weights", "offset", "start", "etastart", "mustart"),
    list_args = c("contrasts"),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  group_lasso = METHOD$new(
    method = "group_lasso", estimator = "gglasso::gglasso", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("group", "lambda"),
    list_args = c(),
    no_grid_args = c(),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  hfr = METHOD$new(
    method = "hfr", estimator = "hfr::cv.hfr", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "kappa"),
    list_args = c(),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  lasso = METHOD$new(
    method = "lasso", estimator = "glmnet::glmnet", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "offset", "lambda", "exclude", "lower.limits", "upper.limits"),
    list_args = c(),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  lm = METHOD$new(
    method = "lm", estimator = "stats::lm", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("subset", "weights", "offset"),
    list_args = c("contrasts"),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  mrmr = METHOD$new(
    method = "mrmr", estimator = "mRMRe::mRMR.ensemble", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c(),
    list_args = c(),
    no_grid_args = c(),
    has_predict_method = FALSE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  mslm = METHOD$new(
    method = "mslm", estimator = "MSwM::msmFit", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "sw"),
    list_args = c("control"),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  nnet = METHOD$new(
    method = "nnet", estimator = "nnet::nnet", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("subset", "weights", "offset", "Wts", "mask"),
    list_args = c("contrasts"),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = FALSE, has_importance_method = TRUE
  ),
  pcr = METHOD$new(
    method = "pcr", estimator = "pls::pcr", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("subset", "scale", "lower", "upper", "ncomp", "ncomp_pct"),
    list_args = c(),
    no_grid_args = c("scale"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  plsr = METHOD$new(
    method = "plsr", estimator = "pls::plsr", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("subset", "scale", "lower", "upper", "ncomp", "ncomp_pct"),
    list_args = c(),
    no_grid_args = c("scale"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  quantile = METHOD$new(
    method = "quantile", estimator = "quantreg::rq", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("subset", "weights", "offset", "tau"),
    list_args = c("contrasts"),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  quantile_rf = METHOD$new(
    method = "quantile_rf", estimator = "quantregForest::quantregForest", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "classwt", "cutoff", "strata", "sampsize", "tau"),
    list_args = c(),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = FALSE, has_importance_method = TRUE
  ),
  relief = METHOD$new(
    method = "relief", estimator = "CORElearn::attrEval", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c(), list_args = c(), no_grid_args = c(),
    has_predict_method = FALSE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  rf = METHOD$new(
    method = "rf", estimator = "randomForest::randomForest", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "classwt", "cutoff", "strata", "sampsize"),
    list_args = c(),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = FALSE, has_importance_method = TRUE
  ),
  ridge = METHOD$new(
    method = "ridge", estimator = "glmnet::glmnet", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "offset", "lambda", "exclude", "lower.limits", "upper.limits"),
    list_args = c(),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  robust = METHOD$new(
    method = "robust", estimator = "MASS::rlm", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("subset", "weights", "offset", "w", "init", "u"),
    list_args = c("contrasts", "lqs.control"),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  spikeslab = METHOD$new(
    method = "spikeslab", estimator = "BoomSpikeSlab::lm.spike", regress = TRUE, classify = TRUE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("optional.coefficient.estimate", "prior.inclusion.probabilities", "weight", "subset", "initial.value", "sampler.weights"),
    list_args = c("contrasts"),
    no_grid_args = c("prior"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  subset = METHOD$new(
    method = "subset", estimator = "bestglm::bestglm", regress = TRUE, classify = TRUE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("subset", "weights", "offset"),
    list_args = c("contrasts"),
    no_grid_args = c("weights"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  svm = METHOD$new(
    method = "svm", estimator = "e1071::svm", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("scale", "class.weights", "subset"),
    list_args = c(),
    no_grid_args = c(),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  tvp = METHOD$new(
    method = "tvp", estimator = "shrinkTVP::shrinkTVP", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c(),
    list_args = c("hyperprior_param", "sv_param", "MH_tuning", "starting_vals"),
    no_grid_args = c(),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  )
)
