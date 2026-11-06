install.packages("xgboost")


library("xgboost")
library(caret)

Sys.which("g++")

raw.dat <- s.rums.cc[  s.rums.cc$Species == species.sheep,c( 
  'feed_intake_g_d' 
  , 'bw_kg' 
  , 'bw_kg.e75'
  , 'adg_g_day'
  , 'NDF_nutrition'
  
  ,'CP_nutrition'
  
  
  , "B.Code"
  
)]

# Assuming 'train_data' and 'test_data' are data frames and 'target_variable' is the target column name
# Convert data frames to matrix
train_matrix <- as.matrix(raw.dat[, -which(names(raw.dat) ==   'feed_intake_g_d' )])
train_labels <- raw.dat$feed_intake_g_d 
test_matrix <- as.matrix(raw.dat[, -which(names(raw.dat) ==   'feed_intake_g_d' )])
test_labels <- raw.dat$feed_intake_g_d 

# Convert to xgb.DMatrix
dtrain <- xgb.DMatrix(data = raw.dat, label = train_labels)
dtest <- xgb.DMatrix(data = raw.dat, label = test_labels)



params <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  gamma = 0,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100, # Number of boosting rounds
  watchlist = list(train = dtrain, test = dtest), # Watchlist for evaluation during training
  verbose = 1
)


predictions <- predict(xgb_model, dtest)

RMSE <- sqrt(mean((test_labels - predictions)^2))