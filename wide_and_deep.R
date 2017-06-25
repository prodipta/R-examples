require(keras)

# see https://rstudio.github.io/keras/index.html for installation
# and installation of tensorflow
# see https://archive.ics.uci.edu/ml/datasets/adult for dataset description
# see https://www.tensorflow.org/tutorials/wide_and_deep for wide and deep
# tutorial and the python code on github. This version is different in 
# terms of features and modelling parameters for simplicity, but describe the 
# basics of wide and deep model
# here numeric columns are treated as wide and categorical as deep

# helper functions
download_data <- function(train_file, test_file){
  train_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
  test_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test"
  
  download.file(train_url,train_file)
  download.file(test_url,test_file)
}
read_data_file <- function(train_file, test_file){
  cols <- c("age","workclass","fnlwgt","education","education_num",
                "marital_status","occupation","relationship","race",
                "sex","capital_gain","capital_loss","hours_per_week",
                "native_country","target")
  train_data <- read.csv(train_file, header = FALSE)
  colnames(train_data) <- cols
  train_data <- na.omit(train_data)
  test_data <- read.csv(test_file, header = FALSE)
  colnames(test_data) <- cols
  test_data <- na.omit(test_data)
  
  # remove final weight field, we do not need it
  train_data$fnlwgt <- test_data$fnlwgt <- NULL
  
  # convert the target to 1 (>=50K) or 0
  train_data$target <- ifelse(as.character(train_data$target)==" <=50K", 0,1)
  test_data$target <- ifelse(as.character(test_data$target)==" <=50K.", 0,1)
  
  return(list(train=train_data, test=test_data))
}
one_hot_encoding <- function(data, cols){
  ohc <- model.matrix(~ . + 0, data=data[,cols], contrasts.arg = 
                        lapply(data[,cols],contrasts,contrasts=FALSE))
  return(ohc)
}
preprocess_data <- function(data, wide_cols, deep_cols){
  train <- data$train
  train_wide <- train[,wide_cols]
  train_deep <- one_hot_encoding(train[,deep_cols])
  train_y <- train$target
  
  test <- data$test
  test_wide <- test[,wide_cols]
  test_deep <- one_hot_encoding(test[,deep_cols])
  test_y <- test$target
  
  # remove mis-matched columns for missing categories between test and train
  common_cols <- colnames(train_deep)[colnames(train_deep)%in%colnames(test_deep)]
  train_deep <- train_deep[,common_cols]
  test_deep <- test_deep[,common_cols]
  
  train <- list(wide=train_wide, deep=train_deep, y=train_y)
  test <- list(wide=test_wide, deep=test_deep, y=test_y)
  
  return(list(train=train,test=test))
}

# start with downloading data, skip if data already available
download_data("train.csv","test.csv")

# read the data files from disk, clean and convert target to binary field
data <- read_data_file("train.csv","test.csv")

# pre-process: split wide and deep variables, one-hot-encode deep variables
wide_cols <- c("age","education_num","capital_gain","capital_loss",
               "hours_per_week")
deep_cols <- c("workclass","education","marital_status","occupation",
               "relationship","race","sex","native_country")
data <- preprocess_data(data, wide_cols, deep_cols)

# set up the network and training parameters
size_deep <- NCOL(data$train$deep)
size_wide <- NCOL(data$train$wide)
drop_out <- 0.3
epoch <- 500
batchsize <- 50
activation_fn <- "tanh"
early_stopping <- callback_early_stopping(monitor = 'val_loss', patience = 10)

# design the network
wide <- layer_input(size_wide)
encoded_wide <- wide %>% 
  layer_dense(units = 1, activation = activation_fn)
deep <- layer_input(size_deep)
encoded_deep <- deep %>% 
  layer_dense(units = round(size_deep/2)) %>%
  layer_dropout(rate=drop_out)%>%
  layer_activation(activation = activation_fn) %>%
  layer_dense(units = round(size_deep/4)) %>%
  layer_dropout(rate=drop_out)%>%
  layer_activation(activation = activation_fn) %>%
  layer_dense(units=1)

merged <- list(encoded_wide, encoded_deep) %>%
  layer_concatenate(axis=1) %>%
  layer_dropout(rate=drop_out)

preds <- merged %>%
  layer_dense(units=1, activation = activation_fn)

wide_n_deep <- keras_model(inputs = list(wide,deep), outputs = preds)

wide_n_deep %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'rmsprop'
)

# train the network
wide_n_deep %>% fit(
  x = list(as.matrix(data$train$wide), as.matrix(data$train$deep)),
  y = data$train$y,
  batch_size = batchsize,
  epochs = epoch,
  validation_split=0.2,
  callbacks = c(early_stopping)
)

# evaluate the test set
yhat <- wide_n_deep %>% predict(list(as.matrix(data$test$wide),as.matrix(data$test$deep)))
yhat <- ifelse(yhat > 0.5, 1,0)
table(data$test$y, yhat)

