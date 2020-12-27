#!/usr/bin/env Rscript

DATA_PATH <- "../data"

# load data
red_wine <- read.csv(paste(DATA_PATH, "winequality-red.csv", sep = "/"), sep = ";")
white_wine <- read.csv(paste(DATA_PATH, "winequality-white.csv", sep = "/"), sep = ";")
red_wine <- red_wine[red_wine$quality > 3,]
white_wine <- white_wine[white_wine$quality < 9,]
red_wine[,12] <- as.factor(red_wine[,12])
white_wine[,12] <- as.factor(white_wine[,12])

print("Data loaded into `red_wine` and `white_wine` dataframe objects")

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

cross_validation <- function(df, k) {
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

    model <- qda(quality ~ ., data =train_data)
    pred <- predict(model, test_data);
    errors[idx] <- 1 - mean(pred$class == test_data$quality)
  }

  return (errors)
}
