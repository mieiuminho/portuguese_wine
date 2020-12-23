#!/usr/bin/env Rscript

library(ggplot2)
library(lattice)
library(caret)  # confusion matrix
library(plyr)  # progress bar

shuffle_dataframe <- function(df) {
  rows <- sample(nrow(df))
  return (df[rows, ])
}

split_idxs <- function(nrows, k) {
  ls <- list();
  group_size <- round(nrows / k)

  for(idx in 1:(k - 1)) {
    ls <- c(ls, list( ((idx - 1)*group_size + 1):(group_size*idx) ))
  }

  if(k > 0) {
    ls <- c(ls, list(((k-1)*group_size + 1):nrows) )
  }

  return (ls)
}

form_random_groups <- function(df, k) {
  sf_df <- shuffle_dataframe(df)
  idx_ls <- split_idxs(nrow(df), k)
  group_ls <- list()
  i <- 1

  for(gr in idx_ls) {
    group_ls[[i]] <- sf_df[gr,]
    i <- i + 1
  }

  return (group_ls)
}

cross_validation_w_qda <- function(df, k, modelused) {
  errors <- array(0, dim = k)
  folds <- form_random_groups(df, k)

  for(idx in 1:k) {
    test_data <- folds[[idx]]

    # pick first fold
    for(ff in 1:k) {
      if(ff != idx) {
        train_data <- folds[[ff]]
        idx_main_fold <- ff
        break
      }
    }

    # append other folds
    for(ff in 1:k) {
      if(ff != idx && ff != idx_main_fold) {
        train_data <- rbind(train_data, folds[[ff]])
      }
    }

    model <- qda(modelused, data =train_data)
    pred <- predict(model, test_data);
    errors[idx] <- 1 - mean(pred$class == test_data$quality)
  }

  return (errors)
}

cross_validation_w_lda <- function(df, k, modelused) {
  errors <- array(0, dim = k)
  folds <- form_random_groups(df, k)

  for(idx in 1:k) {
    test_data <- folds[[idx]]

    # pick first fold
    for(ff in 1:k) {
      if(ff != idx) {
        train_data <- folds[[ff]]
        idx_main_fold <- ff
        break
      }
    }

    # append other folds
    for(ff in 1:k) {
      if(ff != idx && ff != idx_main_fold) {
        train_data <- rbind(train_data, folds[[ff]])
      }
    }

    model <- lda(modelused, data =train_data)
    pred <- predict(model, test_data);
    errors[idx] <- 1 - mean(pred$class == test_data$quality)
  }

  return (errors)
}

cross_validation_w_knn <- function(df, k, kParam) { # quality ~ .
  errors <- array(0, dim = k)
  folds <- form_random_groups(df, k)

  for(idx in 1:k) {
    test_data <- folds[[idx]]

    # pick first fold
    for(ff in 1:k) {
      if(ff != idx) {
        train_data <- folds[[ff]]
        idx_main_fold <- ff
        break
      }
    }

    # append other folds
    for(ff in 1:k) {
      if(ff != idx && ff != idx_main_fold) {
        train_data <- rbind(train_data, folds[[ff]])
      }
    }

    pred <- knn(train_data, test_data, train_data[,12], k=kParam)
    errors[idx] <- 1 - mean(pred == test_data$quality)
  }

  return (errors)
}

#' @param k Number of iterations.
cross_validation_k_fold_glm <- function(df, k = 10) {
    # Models
    models <- NULL

    # Accuracy
    accuracy <- NULL

    # False positive rate
    fpr <- NULL

    # False negative rate
    fnr <- NULL

    # Initialize progress bar
    pbar <- create_progress_bar("text")
    pbar$init(k)

    set.seed(2020)
    folds <- form_random_groups(df, k)

    for(idx in 1:k) {
        test_data <- folds[[idx]]

        # pick first fold
        for(ff in 1:k) {
            if(ff != idx) {
                train_data <- folds[[ff]]
                idx_main_fold <- ff
                break
            }
        }

        # append other folds
        for(ff in 1:k) {
            if(ff != idx && ff != idx_main_fold) {
                train_data <- rbind(train_data, folds[[ff]])
            }
        }

        # Fitting
        model <- glm(is_good ~ ., data = train_data)

        # Actual answers
        answers <- test_data$is_good
        test_data$is_good <- NULL

        # Predict results
        results_prob <- predict(model, test_data, type = "response")
        # If prob > 0.5 then 1, else 0
        results <- ifelse(results_prob > 0.5, 1, 0)

        # Collecting results
        errors <- mean(answers != results)
        accuracy[idx] <- 1 - errors

        # Confusion matrix
        cm <- confusionMatrix(data = factor(results), reference = factor(answers))
        fpr[idx] <- cm$table[2]/(nrow(train_data))
        fnr[idx] <- cm$table[3]/(nrow(train_data))

        pbar$step()
    }

    cat("\n")

    return(list(accuracy = accuracy, false_positive_rate = fpr, false_negative_rate = fnr))
}


#' @param k Number of iterations.
#' @param split Percentage to split dataset.
cross_validation_monte_carlo_logit <- function(data, split = 0.7, k = 500) {

    # False positive rate
    fpr <- NULL

    # False negative rate
    fnr <- NULL

    # Accuracy
    acc <- NULL

    # Models
    models <- NULL

    # Initialize progress bar
    pbar <- create_progress_bar("text")
    pbar$init(k)

    set.seed(123)

    for (i in 1:k) {
        smp_size <- floor(split * nrow(data))
        index <- sample(seq_len(nrow(data)), size = smp_size)
        train <- data[index, ]
        test <- data[-index, ]

        # Fitting
        model <- glm(is_good ~ ., family = binomial, data = train)

        # Actual answers
        answers <- test$is_good

        test$is_good <- NULL

        # Predict results
        results_prob <- predict(model, test, type = "response")

        # If prob > 0.5 then 1, else 0
        results <- ifelse(results_prob > 0.5, 1, 0)

        # Accuracy calculation
        misClasificError <- mean(answers != results)

        # Collecting results
        acc[i] <- 1 - misClasificError

        # Confusion matrix
        cm <- confusionMatrix(data = factor(results), reference = factor(answers))
        fpr[i] <- cm$table[2]/(nrow(data) - smp_size)
        fnr[i] <- cm$table[3]/(nrow(data) - smp_size)

        pbar$step()
    }

    cat("\n")

    return(list(accuracy = acc, false_positive_rate = fpr, false_negative_rate = fnr))
}

