library(rpart)
library(rpart.utils)
library(rpart.plot)
library(dplyr)
library('ROCR') # for reference model gain curve

topN <- function(v, N=10, min_cases=500, other_label="Other"){
  library(dplyr)
  v <- as.character(v)
  v[is.na(v)] <- "missing"
  v[v==""] <- "missing"  # do we need different kinds of missingness?
  top_cats <- table(v) %>% sort(decreasing=TRUE) %>% head(n=N)
  top_cats <- top_cats[top_cats > min_cases]
  new_levels <- unique(c(names(top_cats), "missing", other_label))
  w <- factor(v, levels=new_levels)
  w[is.na(w)] <- other_label # spelled and capitalized as in original dataset
  unused <- names(table(w))[table(w) == 0]
  new_levels <- new_levels[!(new_levels %in% unused)]
  factor(w, levels=new_levels)
}


multi_roc <- function(model_list, test_set, title="", outcome="ordered"){
  # model_list is a named list of trained model objects
  predictions <- do.call(cbind, lapply(model_list, function(fit) rxPredict(fit, test_set)))
  names(predictions) <- names(model_list)
  
  predictions$outcome <- test_set[[outcome]]
  
  rxRocCurve("outcome", names(model_list), predictions, numBreaks=1000, main=title)
}

get_rules <- function(myTree){
  library(rpart.utils)
  
  nodeIDs <-  as.integer(row.names(myTree$frame))
  names(nodeIDs) <- nodeIDs
  
  conditions_list <- rpart.lists(myTree)
  node_path_list <- rpart.rules(myTree)
  
  res <- lapply(nodeIDs, function(node_id){
    node_path <- lapply(
      strsplit(node_path_list[[node_id]], ",")[[1]],
      function(edge_str){
        # side_num <- strsplit(edge_str, '')[[1]]
        side_num <- c(substring(edge_str, 1, 1), substring(edge_str, 2, nchar(edge_str)))
        conditions_list[[side_num[1]]][as.integer(side_num[2])][[1]]
      }
    )
  })
  
  condition2r <- function(condition){
    # condition <- res[[11]][[1]]
    # variable <- names(condition)
    type <- class(condition)
    target <- as.vector(condition)
    comparison <- attr(condition,"compare")
    variable <- names(comparison)[[1]]
    # c(variable=variable, type=type, target=target, comparison=comparison)
    
    rule_parts <- switch(type,
                         character = sprintf("%s %%in%% c('%s')", 
                                             variable, paste(target, collapse="','")),
                         numeric = { 
                           if (comparison == "=") comparison <- "==" 
                           sprintf("%s %s %s",
                                   variable, comparison, target)}
                         ,
                         "" # "OOPS"
    )
    
    return(rule_parts)
  }
  
  multi_conditions <- function(node_specs){
    paste(sapply(node_specs, condition2r), collapse=" & ")
  }
  
  lapply(res, multi_conditions)
}


evaluate_rule <- function(myRule, myData, outcome){
  myRule <- if ("" == myRule) myRule else sprintf("which(%s)", myRule)
  cmd <- sprintf("with(myData, myData[%s, ])", myRule)
  rule_cases <- eval(parse(text=cmd))
  
  yval=sum(rule_cases[[outcome]]==TRUE)/nrow(rule_cases)
  yval[0 == nrow(rule_cases)] <- 0
  
  data.frame(n=nrow(rule_cases), yval=yval)
}


train_validated_tree <- function(with_formula, training_set, test_set, ...){
  outcome <- as.character(with_formula[[2]])
  if (class(training_set[[outcome]]) != "logical") stop("outcome must be logical, not a factor")
  if(require("RevoScaleR")){
    validated_tree <- as.rpart(rxDTree(with_formula, training_set, ...)) # no dots in formula!
    reference_model <- rxDForest(with_formula, training_set)

    reference_pred_test <- rxPredict(reference_model, data=test_set, extraVarsToWrite=outcome)
    pred_obj_test <- ROCR::prediction(reference_pred_test[[paste0(outcome, "_Pred")]], reference_pred_test[[outcome]])
    gain_test <- ROCR::performance(pred_obj_test, "tpr", "rpp")
    
    reference_pred_train <- rxPredict(reference_model, data=training_set, extraVarsToWrite=outcome)
    pred_obj_train <- ROCR::prediction(reference_pred_train[[paste0(outcome, "_Pred")]], reference_pred_train[[outcome]])
    gain_train <- ROCR::performance(pred_obj_train, "tpr", "rpp")
    
  } else {
    library("rpart")
    library("randomForest")
    validated_tree <- rpart(with_formula, training_set, ...)
    
    # modify formula for randomForest: convert outcome to factor in formula
    form_parts <- as.character(with_formula)
    rf_formula <- formula(sprintf("factor(%s) ~ %s", outcome, form_parts[3]))
    reference_model <- randomForest(rf_formula, training_set)
    
    predict_rf <- function(model, test_data){
      # Put predictions in the same format as rxPredict
      # If you ever wanted an argument for %>%, here you go.
      dep_var <- gsub(")", "", gsub("factor(", "", as.character(reference_model$terms)[2], fixed=TRUE), fixed=TRUE)
      pred <- list()
      # predictions are in a matrix, second column is for TRUE
      pred[[paste0(dep_var, "_Pred")]] <- predict(model, test_data, type="prob")[,2]
      pred[[dep_var]] <- test_data[[dep_var]]
      pred <- as.data.frame(pred)
      pred
    }
    
    reference_pred_test <- predict_rf(reference_model, test_set)
    pred_obj_test <- ROCR::prediction(reference_pred_test[[paste0(outcome, "_Pred")]], reference_pred_test[[outcome]])
    gain_test <- ROCR::performance(pred_obj_test, "tpr", "rpp")
    
    reference_pred_train <- predict_rf(reference_model, training_set)
    pred_obj_train <- ROCR::prediction(reference_pred_train[[paste0(outcome, "_Pred")]], reference_pred_train[[outcome]])
    gain_train <- ROCR::performance(pred_obj_train, "tpr", "rpp")
    
  }
  
  quantilize_gain <- function(x, y, N=100){
    x_quantile <- quantile(x, probs=(0:N)/N)
    x_cut <- cut(x, breaks=x_quantile, include.lowest=TRUE)
    y_step <- aggregate(y, by=list(x_cut), max)$x
    
    data.frame(x=x_quantile, y=c(0, y_step))
  }
  
  if (length(gain_test@x.values[[1]]) < 300){
    test_df <- data.frame(x=gain_test@x.values[[1]], y=gain_test@y.values[[1]])
    train_df <- data.frame(x=gain_train@x.values[[1]], y=gain_train@y.values[[1]])
  } else {
    test_df <- quantilize_gain(x=gain_test@x.values[[1]], y=gain_test@y.values[[1]])
    train_df <- quantilize_gain(x=gain_train@x.values[[1]], y=gain_train@y.values[[1]])
  }
  
  validated_tree$reference_model_gain <- list(test=test_df, train=train_df)
  
  rule_strings <- get_rules(validated_tree)
  test_set_tabulation <- do.call("rbind", 
                                 lapply(rule_strings,
                                        evaluate_rule, test_set, outcome))
  validated_tree$frame$n_test <- test_set_tabulation$n
  validated_tree$frame$yval_test <- test_set_tabulation$yval
  
  validated_tree$frame$rule <- rule_strings[as.character(row.names(validated_tree$frame))]
  
  class(validated_tree)  <- c(class(validated_tree), "validated_tree")
  return(validated_tree)
}

split_by_var <- function(dataset, split_var){
  unique_ids <- unique(dataset[[split_var]])
  training_idx <- sample(unique_ids, size=(2/3)*length(unique_ids))
  in_training <- dataset[[split_var]] %in% training_idx
  list(training = dataset[in_training,], test=dataset[!in_training,])
}

generate_titanic_validated_tree <- function(){
  library(rpart.plot)
  data(ptitanic)
  # head(ptitanic)
  sapply(ptitanic, function(v) sum(is.na(v)))  # 263 cases missing age
  
  age_model <- rpart(age ~ ., ptitanic)
  qtitanic <- ptitanic
  qtitanic$age[is.na(ptitanic$age)] <- predict(age_model, ptitanic[is.na(ptitanic$age),])
  
  # outcome must be logical, not a factor
  qtitanic$survived <- qtitanic$survived == "survived"
  
  in_training <- sample(c(TRUE, FALSE), nrow(qtitanic), prob=c(2/3, 1/3), replace=TRUE)
  
  titanic_validated_tree <- train_validated_tree(survived ~ pclass + sex + age + sibsp + parch, 
                                                 qtitanic[in_training,], 
                                                 qtitanic[!in_training,])
  # titanic_validated_tree$frame
  
  saveRDS(titanic_validated_tree, "test_data/titanic_validated_tree.Rds")
}
