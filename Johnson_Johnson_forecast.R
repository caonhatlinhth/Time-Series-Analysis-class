#Read the library
library(fpp3)
library(tsibble)
library(forecast)
library(MASS)

#Read the data
data('JohnsonJohnson')
JohnsonJohnson

#convert to tsibble
tsibble_johnson <- as_tsibble(JohnsonJohnson)

########################################################################################
#QUESTION ABOUT DATA
########################################################################################

# Q6. find the frequency of the dataset
frequency(tsibble_johnson)

# Q7. Decompose the time series
decompose_johnson <- tsibble_johnson %>%
  model(stl = STL(value)) %>%
  components() 

# Plot the decomposed time series
autoplot(decompose_johnson)

# Q8.Compare two model: additive and multiplicative
additive_aic <- auto.arima(log(JohnsonJohnson), seasonal = TRUE)$aic

multi_aic <- auto.arima(JohnsonJohnson, seasonal = TRUE)$aic

additive_aic
multi_aic

#Q9. Find the Box-cox value of lambda that is best for this data
best_lambda <- round(BoxCox.lambda(JohnsonJohnson),4)
best_lambda

#Q10. The FT measure of the strength of trend for this data?
Ft_measure <- round(max(0,1 - var(decompose_johnson$remainder)/(var(decompose_johnson$trend + decompose_johnson$remainder))),3)
Ft_measure

#Q11. FS measure of the strength of seasonality for this data
Fs_measure <- round(max(0,1 - var(decompose_johnson$remainder)/(var(decompose_johnson$season_year + decompose_johnson$remainder))),3)
Fs_measure

########################################################################################
#PREPROCESSING AND MODEL FITTING
########################################################################################

#Split the data into train and test set
n <- nrow(tsibble_johnson)
train_data <- tsibble_johnson[1:(n-10), ]
test_data <- tsibble_johnson[(n-9):n, ]
train_data
test_data

#Model fitting
#MEAN Model
mean_model <- train_data %>%
  model(
    'Mean' = MEAN(box_cox(value, lambda = 0.1540791)))
mean_fc <- mean_model %>%
  forecast(h = 10)

#Naive model
naive_model <- train_data %>%
  model(
    'Naive' = NAIVE(box_cox(value, lambda = 0.1540791)))
naive_fc <- naive_model %>%
  forecast(h = 10)

#seasonal Naive Method
snaive_model <- train_data %>%
  model(
    'SNaive' = SNAIVE(box_cox(value, lambda = 0.1540791)))
snaive_fc <- snaive_model %>%
  forecast(h = 10)

#Drift Method
drift_model <- train_data %>%
  model(
    'Drift' = RW(box_cox(value, lambda = 0.1540791) ~ drift()))
drift_fc <- drift_model %>%
  forecast(h = 10)

#STL decomposition method
stl_model <- train_data %>%
  model('stl' = decomposition_model(STL(value ~ trend(window = 7), 
                                        robust = TRUE),NAIVE(season_adjust)))
#TSLM method
tslm_model <- train_data %>%
  model(
    'tslm' = TSLM(box_cox(value, lambda = 0.1540791) ~ trend() + season()))
tslm_fc <- tslm_model %>%
  forecast(h = 10)

########################################################################################
#PREDICTION QUESTIONS
########################################################################################

#Q12. Make the prediction for quarter 3 of 1979 using Mean model
mean_fc_q3 <- round(mean_fc$.mean[5], 2)
mean_fc_q3

#Q13. Make the prediction for quarter 3 of 1979 using Naive model
naive_fc_q3 <- round(naive_fc$.mean[5], 1)
naive_fc_q3

#Q14. Make the prediction for Quarter 3 of 1979 using the Seasonal Naive model
snaive_fc_q3 <- round(snaive_fc$.mean[5], 2)
snaive_fc_q3

#Q15. Make the prediction for quarter 3 of 1979 using the TSLM model
tslm_fc_q3 <- round(tslm_fc$.mean[5], 1)
tslm_fc_q3

########################################################################################
#MODEL EVALUATION
########################################################################################

#evaluate table
mean_accuracy <- fabletools::accuracy(mean_fc, test_data)
mean_accuracy

naive_accuracy <- fabletools::accuracy(naive_fc, test_data)
naive_accuracy

snaive_accuracy <- fabletools::accuracy(snaive_fc, test_data)
snaive_accuracy

drift_accuracy <- fabletools::accuracy(drift_fc, test_data)
drift_accuracy

stl_accuracy <- forecast::accuracy(stl_fc, test_data)
stl_accuracy

tslm_accuracy <- fabletools::accuracy(tslm_fc, test_data)
tslm_accuracy

# Determine which method performs best
model_evaluation <- rbind(mean_accuracy, naive_accuracy, snaive_accuracy, drift_accuracy, tslm_accuracy)
best_model <- model_evaluation [which.min(model_evaluation$RMSE), ".model"] 
best_model

#18. Find the forecast error for Quarter 3 of 1979 for the Random walk with drift model
drift_fc_q3_1979 <- drift_fc$.mean[5]
error_drift <- round((test_data$value[5] - drift_fc_q3_1979), 3)
error_drift

#19. Find the biggest auto correlation for the TSLM model
resid_acf <- augment(tslm_model) %>%
  ACF(.resid)

max_acf <- round(max(abs(resid_acf$acf[-1])),3)
max_acf

#Q20. Find the test statistic for the Ljung-Box portmanteau test for autocorrelation for the residuals
ljung_box_test <- Box.test(augment(tslm_model)$.resid, lag = 10, type = "Ljung-Box")
ljung_box_stat <- round(ljung_box_test$statistic,1)
ljung_box_stat

#Q21. Test if the residuals for the TSLM model resemble white noise
if (ljung_box$lb_pvalue < 0.05) {
  print("The residuals do not resemble white noise.")
} else {
  print("The residuals resemble white noise.")
}
