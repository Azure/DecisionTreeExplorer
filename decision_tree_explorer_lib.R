library(rpart)
library(rpart.utils)
library(rpart.plot)
library(dplyr)

topN <- function(v, N=10, min_cases=500, other_label="Other"){
  library(dplyr)
  v <- as.character(v)
  v[is.na(v)] <- "missing"
  v[v==""] <- "missing"  # are there really different kinds of missingness?
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
  validated_tree <- if(require("RevoScaleR")){
    as.rpart(rxDTree(with_formula, training_set, ...)) # no dots in formula!
  } else {
    library("rpart")
    rpart(with_formula, training_set, ...)
  }
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
