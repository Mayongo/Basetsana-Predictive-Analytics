library(forecast)

data<- Exports_R
head(data)


#formatting data as time series data

ts_data <- ts(data$Export, start = c(1988, 1), 
              end = c(2024,3 ), frequency = 4)
ts_data

n <- length(ts_data)

train_size <- round(0.8 * n)

# Split data
train_data <- window(ts_data, end = c(1988 + (train_size - 1) %/% 4, (train_size - 1) %% 4 + 1))
test_data  <- window(ts_data, start = c(1988 + (train_size) %/% 4, (train_size) %% 4 + 1))

# View lengths
length(train_data)
length(test_data)


# Training the model
model <- auto.arima(train_data, order = c(1, 2, 1))
accuracy(fitted(model), train_data)


# Forecast next periods (length of test data)
forecast_horizon <- length(test_data)
forecast_result <- forecast(model, h = forecast_horizon)
accuracy(forecast_result, test_data)

# Plot Forecast vs Actual Test Data
plot(forecast_result)
lines(test_data, col = 'red')

#fitting the best model 
best_model <- Arima(ts_data, order = c(1,2,1))  
summary(best_model)

#forecasting exports for the next four quarters
future_forecast <- forecast(best_model, h = 4)
forecast_table <- as.data.frame(future_forecast)
print(forecast_table)




