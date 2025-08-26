# atwl_construct_a_int.R

# Project Overview:
# This project aims to construct an interactive machine learning model integrator, 
# allowing users to visualize and compare different machine learning models.

# Load necessary libraries
library(caret)
library(dplyr)
library(ggplot2)
library(plumber)
library(widgetframe)

# Define a function to load datasets
load_dataset <- function(dataset_name) {
  # Load dataset from a predefined list of datasets
  datasets <- list(
    "iris" = iris,
    "mtcars" = mtcars,
    " diamonds" = diamonds
  )
  return(datasets[[dataset_name]])
}

# Define a function to train a machine learning model
train_model <- function(dataset, model_type) {
  # Train a machine learning model based on the selected model type
  if(model_type == "random_forest") {
    model <- randomForest::randomForest(dataset[, -ncol(dataset)], dataset[, ncol(dataset)])
  } else if(model_type == "decision_tree") {
    model <- rpart::rpart(dataset[, -ncol(dataset)], dataset[, ncol(dataset)])
  } else if(model_type == "svm") {
    model <- e1071::svm(dataset[, -ncol(dataset)], dataset[, ncol(dataset)])
  }
  return(model)
}

# Define a function to evaluate a machine learning model
evaluate_model <- function(model, dataset) {
  # Evaluate the machine learning model using metrics such as accuracy and F1 score
  predictions <- predict(model, dataset[, -ncol(dataset)])
  confusion_matrix <- caret::confusionMatrix(predictions, dataset[, ncol(dataset)])
  return(confusion_matrix)
}

# Create a plumber API to interact with the model integrator
api <- plumb("api")
api$GET("/datasets", function() {
  # Return a list of available datasets
  datasets <- names(load_dataset)
  return(list(datasets = datasets))
})

api$POST("/train", function(dataset_name, model_type) {
  # Train a machine learning model based on the selected dataset and model type
  dataset <- load_dataset(dataset_name)
  model <- train_model(dataset, model_type)
  return(list(model = model))
})

api$POST("/evaluate", function(model, dataset_name) {
  # Evaluate the machine learning model using the selected dataset
  dataset <- load_dataset(dataset_name)
  evaluation <- evaluate_model(model, dataset)
  return(list(evaluation = evaluation))
})

# Run the plumber API
api$run()