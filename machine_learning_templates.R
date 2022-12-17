library(ggplot2)
library(dplyr)
library(odbc)
library(pool)
install.packages("superml")
library(superml)
library(caret)
install.packages('lightgbm')
library(lightgbm)
install.packages('tidymodels')
library(tidymodels)  
library(tune)     
library(workflows)  
library(tictoc) 
install.packages('ranger')
library(ranger)
install.packages('AmesHousing')
library(AmesHousing)
install.packages('janitor')
library(janitor)
library(rsample)
library(recipes)
install.packages('parsnip')
library(parsnip)
library(dials)
library(yardstick)
library(treesnip)
install.packages('kernlab')
library(kernlab)
install.packages('remotes')
library(remotes)
remotes::install_github("curso-r/treesnip")
install.packages("lightgbm", repos = "https://cran.r-project.org")

data <- read.csv("C:\\Users\\allen\\Desktop\\Others\\PitchData.csv")

#summary of data
str(data)

#Check severity of class imbalance.
round(prop.table(table(data$PitchResult)), 2)

#encode the variables
label <- LabelEncoder$new()
data$BallStrikeCount <- label$fit_transform(data$BallStrikeCount)

#convert the data types
categorical_variables <- c('Season', 'GameId', 'ResultId', 'PitchId', 'FieldTeamId', 'BatTeamId', 'PitcherId', 'CatcherId',
                           'BatterId', 'BatSide', 'UmpireId', 'BallsBefore', 'StrikesBefore', 'BallStrikeCount', 'PitchResult')

data[categorical_variables] <- lapply(data[categorical_variables], factor)



#------------------------------------SVM wt Bayesian Optimization-----------------------------------------------------------------------------
#select the features interested
data1 <- data[c('BatSide', 'UmpireId', 'BallsBefore', 'StrikesBefore', 'BallStrikeCount', 'GridX', 'GridY', 'CatcherLocationX', 
                'CatcherLocationY', 'HorizMiss', 'PitchResult')]

#train/test split
tr_te_split <- initial_split(data1, prop = 4/5)
data1_train <- training(tr_te_split)
data1_test  <- testing(tr_te_split)

#5-fold cross validation
folds <- vfold_cv(data1_train, v = 5)

#use a recipe to preprocess the dataset
data1_pre_proc <-
  recipe(PitchResult ~ ., data = data1_train)

#use a support vector machine to model the data. Let's use a radial basis function (RBF) kernel and tune its main parameter (??). 
#Additionally, the main SVM parameter, the cost value, also needs optimization.
svm_mod <-
  svm_rbf(mode = "classification", cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab")

#These two objects (the recipe and model) will be combined into a single object via the workflow() function from the workflows package
#this object will be used in the optimization process.
svm_wflow <-
  workflow() %>%
  add_model(svm_mod) %>%
  add_recipe(data1_pre_proc)

#From this object, we can derive information about what parameters are slated to be tuned.
svm_set <- parameters(svm_wflow)
svm_set

#
search_res <-
  svm_wflow %>% 
  tune_bayes(
    resamples = folds,
    # To use non-default parameter ranges
    param_info = svm_set,
    # Generate five at semi-random to start
    initial = 5,
    iter = 10,
    # How to measure performance?
    metrics = metric_set(accuracy),
    control = control_bayes(no_improve = 30, verbose = TRUE)
  )




#------------------------------------Random Forest wt Bayesian Optimization-----------------------------------------------------------------------------
#select the interested features
data <- data[c('BatSide', 'UmpireId', 'BallsBefore', 'StrikesBefore', 'BallStrikeCount', 'GridX', 'GridY', 'CatcherLocationX', 
               'CatcherLocationY', 'HorizMiss', 'PitchResult')]

# Split data into train and test data and create resamples for tuning
set.seed(42)
train_test_split_data <- initial_split(data)
data_train <- training(train_test_split_data)
data_test <-  testing(train_test_split_data)

# create resamples
folds <- vfold_cv(data_train, v = 5)

# Pre-Processing the data with{recipes}
set.seed(42)
rec <- recipe(PitchResult ~., 
              data = data_train)

trained_rec<-  prep(rec, training = data_train, retain = TRUE)
# create the train and test set 
train_data <- as.data.frame(juice(trained_rec))
test_data  <- as.data.frame(bake(trained_rec, new_data = data_test))

# Build the model (generate the specifications of the model) 
model_spec_default <- rand_forest(mode = "classification")%>%set_engine("ranger", verbose = TRUE)

# fit the model
model_fit_default <- model_spec_default%>%fit(PitchResult ~ . , data_train)
# Show the configuration of the fitted model
model_fit_default

# Performance and statistics: 
test_results_default <- 
  test_data %>%
  select(PitchResult) %>%
  as_tibble() %>%
  mutate(
    model_class_default = predict(model_fit_default, new_data = test_data) %>% 
      pull(.pred_class),
    model_prob_default  = predict(model_fit_default, new_data = test_data, type = "prob") %>% 
      pull(.pred_2))

# Compute the AUC value
auc_default <- test_results_default %>% roc_auc(truth = PitchResult, model_prob_default) 
cat("The default model scores", auc_default$.estimate, " AUC on the testing data")

# Here we can also compute the confusion matrix 
conf_matrix <- test_results_default%>%conf_mat(truth = PitchResult, model_class_default)
conf_matrix

# Build the model to tune and leave the tuning parameters empty (Placeholder with the tune() function)
model_def_to_tune <- rand_forest(mode = "classification", 
                                 mtry = tune(),         # mtry is the number of predictors that will be randomly 
                                 #sampled at each split when creating the tree models.
                                 trees = tune(),        # trees is the number of trees contained in the ensemble.
                                 min_n =  tune())%>% # min_n is the minimum number of data points in a node 
  #that are required for the node to be split further. 
  set_engine("ranger") #  computational engine

# Build the workflow object
model_wflow <-
  workflow() %>%
  add_model(model_def_to_tune) %>%
  add_recipe(rec)

tune_args(model_wflow)

HP_set <- parameters(model_wflow)
HP_set

without_output <- select(data_train, -PitchResult)
HP_set <- finalize(HP_set, without_output)
HP_set

my_finalize_func <- function(result_tuning, my_recipe, my_model) {
  # Accessing the tuning results
  bestParameters <- select_best(result_tuning, metric = "roc_auc", maximize = TRUE)
  # Finalize recipe
  final_rec <- 
    rec %>%
    finalize_recipe(bestParameters) %>%
    prep()
  # Attach the best HP combination to the model and fit the model to the complete training data(data_in_scope_train) 
  final_model <-
    my_model %>%
    finalize_model(bestParameters) %>%
    fit(PitchResult ~ ., data = juice(final_rec))
  # Prepare the finale trained data to use for performing model validation. 
  df_train_after_tuning <- as.data.frame(juice(final_rec)) 
  df_test_after_tuning <- as.data.frame(bake(final_rec, new_data = data_test))
  # Predict on the testing data 
  results_ <- 
    df_test_after_tuning%>%
    select(PitchResult) %>%
    as_tibble()%>%
    mutate(
      model_class = predict(final_model, new_data = df_test_after_tuning) %>% 
        pull(.pred_class),
      model_prob  = predict(final_model, new_data = df_test_after_tuning, type = "prob") %>% 
        pull(.pred_2))
  # Compute the AUC  
  auc <-  results_%>% roc_auc(truth = PitchResult, model_prob)
  # Compute the confusion matrix
  confusion_matrix <- conf_mat(results_, truth= PitchResult, model_class)
  # Plot the ROC curve
  rocCurve <- roc_curve(results_, truth = PitchResult, model_prob)%>%
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path(colour = "darkgreen", size = 1.5) +
    geom_abline(lty = 3, size= 1, colour = "darkred") +
    coord_equal()+
    theme_light()
  new_list <- list(auc, confusion_matrix, rocCurve)  
  return(new_list)
}

# Start the Baysian HP search process
search_results_bayesian <- tune_bayes(
  model_wflow,                              # workflows object defined above             
  resamples = folds,                        # rset() object defined above
  param_info = HP_set,                      # HP set defined above (updated HP set)
  initial = 5 ,                             # here you could also use the results of the Grid Search
  iter = 5,                                 # max number of search iterations
  metrics = metric_set(roc_auc),            # to optimize for the roc_auc metric 
  control = control_bayes(no_improve = 3,   # cutoff for the number of iterations without better results.
                          save_pred = TRUE, # output of sample predictions should be saved.
                          verbose = TRUE))

# Get the best HP combination
best_HP_Bayesian <- select_best(search_results_bayesian, metric = "roc_auc", maximize = TRUE)
best_HP_Bayesian

# Build the final model (apply my_finalize_func)
Finalize_Bayesian <- my_finalize_func(search_results_bayesian, rec, model_def_to_tune)
# Get the AUC value
cat(" Tuned model via Bayesian method scores", Finalize_Bayesian[[1]]$.estimate, "AUC on the testing data", "\n")
##  Tuned model via Bayesian method scores 0.8295968 AUC on the testing data
cat("The Confusion Matrix", "\n")
## The Confusion Matrix
print(Finalize_Bayesian[[2]])
cat("And the ROC curve:", "\n")
## And the ROC curve:
print(Finalize_Bayesian[[3]])

# Build a new table with the achieved AUC's
xyz <- tibble(Method = c("Default", "Bayesian Optimization"), 
              AUC_value = c(auc_default$.estimate, Finalize_Bayesian[[1]]$.estimate))
default_value <- c(mtry = model_fit_default$fit$mtry, trees=  model_fit_default$fit$num.trees,min_n = model_fit_default$fit$min.node.size)
vy <- bind_rows(default_value, best_HP_Bayesian )
all_HP <- bind_cols(xyz, vy)
all_HP%>%knitr::kable(
  caption = "AUC Values and the best hyperparameter combination: we can see that the Bayesian hyperparameter using the {tune} package improved the performance (AUC) of our model, but what about using the caret package ?")


#----------------------------------------LightGBM--------------------------------------------------------------------------------------------
#select the interested features
data <- data[c('BatSide', 'BallsBefore', 'StrikesBefore', 'BallStrikeCount', 'GridX', 'GridY', 'CatcherLocationX', 
               'CatcherLocationY', 'HorizMiss', 'PitchResult')]

data_split <- rsample::initial_split(
  data,
  prop = 0.8
)

preprocessing_recipe <-
  recipes::recipe(PitchResult ~ ., data = training(data_split)) %>%
  # combine low frequency factor levels
  recipes::step_other(all_nominal(), threshold = 0.01) %>%
  # remove no variance predictors which provide no predictive information 
  recipes::step_nzv(all_nominal()) %>%
  # prep the recipe so it can be used on other data
  prep()

data_cv_folds <-
  recipes::bake(
    preprocessing_recipe,
    new_data = training(data_split)
  ) %>%
  rsample::vfold_cv(v = 5)

# Build the model (generate the specifications of the model) 
lgbm_model<-
  parsnip::boost_tree(
    mode = "classification",
    trees = 60,
    min_n = tune(),
    tree_depth = tune(),
  ) %>%
  set_engine("lightgbm", objective = 'binary',verbose=TRUE)

lgbm_params <-
  dials::parameters(
    # The parameters have sane defaults, but if you have some knowledge 
    # of the process you can set upper and lower limits to these parameters.
    min_n(), # 2nd important
    tree_depth() # 3rd most important
  )

lgbm_grid <-
  dials::grid_max_entropy(
    lgbm_params,
    size = 60 # set this to a higher number to get better results
  )

lgbm_wf <- 
  workflows::workflow() %>%
  add_model(lgbm_model
  ) %>%
  add_formula(PitchResult ~ .)

lgbm_tuned <- tune::tune_grid(
  object = lgbm_wf,
  resamples = data_cv_folds,  
  grid = lgbm_grid,
  metrics = yardstick::metric_set(average_precision),
  control = tune::control_grid(verbose = TRUE)
)

lgbm_tuned %>%
  tune::show_best(metric = "average_precision",n = 5)

lgbm_best_params <-
  lgbm_tuned %>%
  tune::select_best("average_precision")

lgbm_model_final <-
  lgbm_model%>%
  finalize_model(lgbm_best_params)

test_processed  <- bake(preprocessing_recipe, new_data = testing(data_split))
test_prediction <- 
  trained_model_all_data %>% 
  # use the training model fit to predict the test data
  predict(new_data = test_processed) %>%
  bind_cols(testing(data_split))

#-------------------------------------------------------------------------------------------------
library(xgboost)
data(agaricus.train, package = "xgboost")
dtrain <- xgb.DMatrix(agaricus.train$data,
                      label = agaricus.train$label)
library(xgboost)
library(Matrix)

data <- data[c('BatSide', 'BallsBefore', 'StrikesBefore', 'BallStrikeCount', 'GridX', 'GridY', 'CatcherLocationX', 
               'CatcherLocationY', 'HorizMiss', 'PitchResult')]

train_test_split_data <- initial_split(data)
data_train <- training(train_test_split_data)
data_test <-  testing(train_test_split_data)

data(data_train, package = "xgboost")
dtrain <- xgb.DMatrix(data_train,
                      label = data_train$PitchResult)
cv_folds <- KFold(agaricus.train$label, nfolds = 5,
                  stratified = TRUE, seed = 0)
xgb_cv_bayes <- function(max_depth, min_child_weight, subsample) {
  cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
                             max_depth = max_depth,
                             min_child_weight = min_child_weight,
                             subsample = subsample, colsample_bytree = 0.3,
                             lambda = 1, alpha = 0,
                             objective = "binary:logistic",
                             eval_metric = "auc"),
               data = dtrain, nround = 100,
               folds = cv_folds, prediction = TRUE, showsd = TRUE,
               early_stopping_rounds = 5, maximize = TRUE, verbose = 0)
  list(Score = cv$evaluation_log$test_auc_mean[cv$best_iteration],
       Pred = cv$pred)
}
OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                bounds = list(max.depth = c(2L, 6L),
                                              min_child_weight = c(1L, 10L),
                                              subsample = c(0.5, 0.8)),
                                init_grid_dt = NULL, init_points = 10, n_iter = 20,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)

#--------------------------------------------------------------------------------------
xgb_opt <- function(train_data,
                    train_label,
                    test_data,
                    test_label,
                    objectfun,
                    evalmetric,
                    eta_range = c(0.1, 1L),
                    max_depth_range = c(4L, 6L),
                    nrounds_range = c(70, 160L),
                    subsample_range = c(0.1, 1L),
                    bytree_range = c(0.4, 1L),
                    init_points = 4,
                    n_iter = 10,
                    acq = "ei",
                    kappa = 2.576,
                    eps = 0.0,
                    optkernel = list(type = "exponential", power = 2),
                    classes = NULL
)
{
  
  quo_train_label <- enquo(train_label)
  data_train_label <- (train_data %>% select(!! quo_train_label))[[1]]
  
  quo_test_label <- enquo(test_label)
  data_test_label <- (test_data %>% select(!! quo_test_label))[[1]]
  
  train_mx <- sparse.model.matrix(data_train_label ~ ., train_data)
  test_mx <- sparse.model.matrix(data_test_label ~ ., test_data)
  
  if (class(data_train_label) == "factor"){
    dtrain <- xgb.DMatrix(train_mx, label = as.integer(data_train_label) - 1)
  } else{
    dtrain <- xgb.DMatrix(train_mx, label = data_train_label)}
  
  
  if (class(data_test_label) == "factor"){
    dtest <- xgb.DMatrix(test_mx, label = as.integer(data_test_label) - 1)
  } else{
    dtest <- xgb.DMatrix(test_mx, label = data_test_label)}
  
  
  #about classes
  if (grepl("logi", objectfun) == TRUE){
    xgb_holdout <- function(object_fun,
                            eval_met,
                            num_classes,
                            eta_opt,
                            max_depth_opt,
                            nrounds_opt,
                            subsample_opt,
                            bytree_opt) {
      
      object_fun <- objectfun
      eval_met <- evalmetric
      
      model <- xgb.train(params = list(objective = object_fun,
                                       eval_metric = eval_met,
                                       nthread = 1,
                                       eta = eta_opt,
                                       max_depth = max_depth_opt,
                                       subsample = subsample_opt,
                                       colsample_bytree = bytree_opt),
                         data = dtrain,
                         nrounds = nrounds_opt)
      t_pred <- predict(model, newdata = dtest)
      Pred <- sum(diag(table(data_test_label, t_pred)))/nrow(test_data)
      list(Score = Pred, Pred = Pred)
    }
  } else{
    xgb_holdout <- function(object_fun,
                            eval_met,
                            num_classes,
                            eta_opt,
                            max_depth_opt,
                            nrounds_opt,
                            subsample_opt,
                            bytree_opt) {
      
      object_fun <- objectfun
      eval_met <- evalmetric
      
      num_classes <- classes
      
      model <- xgb.train(params = list(objective = object_fun,
                                       num_class = num_classes,
                                       nthread = 1,
                                       eval_metric = eval_met,
                                       eta = eta_opt,
                                       max_depth = max_depth_opt,
                                       subsample = subsample_opt,
                                       colsample_bytree = bytree_opt),
                         data = dtrain,
                         nrounds = nrounds_opt)
      t_pred <- predict(model, newdata = dtest)
      Pred <- sum(diag(table(data_test_label, t_pred)))/nrow(test_data)
      list(Score = Pred, Pred = Pred)
    }
  }
  
  opt_res <- BayesianOptimization(xgb_holdout,
                                  bounds = list(eta_opt = eta_range,
                                                max_depth_opt = max_depth_range,
                                                nrounds_opt = nrounds_range,
                                                subsample_opt = subsample_range,
                                                bytree_opt = bytree_range),
                                  init_points,
                                  init_grid_dt = NULL,
                                  n_iter,
                                  acq,
                                  kappa,
                                  eps,
                                  optkernel,
                                  verbose = TRUE)
  
  return(opt_res)
  
}