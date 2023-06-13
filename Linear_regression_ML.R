# ---------------- Libraries -------------------------

if("mlbench" %in% rownames(installed.packages()) == F) {install.packages("mlbench")}
library(mlbench)
if("caret" %in% rownames(installed.packages()) == F) {install.packages("caret")}
library(caret)

# --------------- DATA -----------------
data(BostonHousing)
head(BostonHousing)
sum(is.na(BostonHousing)) # Check for NA values
set.seed(100) # Set seed to reproducee model

TrainingIndex <- createDataPartition(BostonHousing$medv, p=0.8, list = FALSE) # Randomly split data by 80%-20%
TrainingSet <- BostonHousing[TrainingIndex,] # Training Set
TestingSet <- BostonHousing[-TrainingIndex,] # Test Set

# ------------------- TRAINING MODEL -------------------
Model <- train(medv ~ ., data = TrainingSet,
               method = "lm",
               preProcess=c("scale","center"), # Standardise each variable
               trControl= trainControl(method="none") # Used for resampling data
)

# ------------- MODEL PREDICTIONS ---------------------
Model.training <- predict(Model, TrainingSet) # Predict training set
Model.testing <- predict(Model, TestingSet) # Predict testing set

# ----------------- MODEL PERFORMANCE -----------------------
plot(TrainingSet$medv,Model.training, col = "blue" )
plot(TestingSet$medv,Model.testing, col = "blue" )
summary(Model)

# ------------- SUMMARY STATISTICS -------------------

training_cor = cor(TrainingSet$medv,Model.training)
testing_cor = cor(TestingSet$medv,Model.testing)

R2_training = training_cor^2
R2_testing = testing_cor^2
