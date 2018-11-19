#Loading the required packages
lirary('ggplot2')
library('forecast')
library('tseries')
#Importing the data
daily_data =read.csv(file.choose(),header=TRUE)
daily_data$Date = as.Date(daily_data$dteday)
#plotting the actual data
ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") +
  xlab("")
count_ts = ts(daily_data[, c('cnt')])
#Removing the outliers and missing values
daily_data$clean_cnt = tsclean(count_ts)

#Plotting the data without outliers
ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')
#Smoothing the series
daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)

#Plotting the smoothed series
ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')
count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)

#decomposing the data
decomp = stl(count_ma, s.window="periodic")

deseasonal_cnt <- seasadj(decomp)
plot(decomp)
#Checking whether the data is stationary
adf.test(count_ma, alternative = "stationary")
#Autocorrelation plots
Acf(count_ma, main='')
Pacf(count_ma, main='')
#Calculating the differences between the consecutive values
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")
#Plotting ACF and PACF 
Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

