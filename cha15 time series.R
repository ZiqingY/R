### Chapter 15: Time Series ###

# 1. creating a time-series object in R
library(xts)
sales <- c(18, 33, 41, 7, 34, 35, 24, 25, 24, 21, 25, 20, 
           22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35) 
date <- seq(from = as.Date("2018/1/1"), 
            to = as.Date("2019/12/1"), 
            by = "month")
sales.xts <- xts(sales, date)

sales.xts["2018"]
sales.xts["2018-3/2019-5"]

quarterlies <- apply.quarterly(sales.xts, sum)
quarterlies

# poltting time-series
library(ggplot2)
library(forecast)
autoplot(sales.xts) + 
  geom_line(color="blue") +
  scale_x_date(date_breaks="1 months", 
               date_labels="%b %y") + 
  labs(x="", y="Sales", title="Customized Time Series Plot") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x=element_blank())

# 2. smoothing with simple moving averages(0, 3, 7, 15)
theme_set(theme_bw()) 
ylim <- c(min(Nile), max(Nile))

autoplot(Nile) + 
  ggtitle("Raw time series") + 
  scale_y_continuous(limits=ylim) 

autoplot(ma(Nile, 3)) + 
  ggtitle("Simple Moving Averages (k=3)") + 
  scale_y_continuous(limits=ylim) 

autoplot(ma(Nile, 7)) + 
  ggtitle("Simple Moving Averages (k=7)") + 
  scale_y_continuous(limits=ylim) 

autoplot(ma(Nile, 15)) + 
  ggtitle("Simple Moving Averages (k=15)") + 
  scale_y_continuous(limits=ylim)

# 2.1 seasonal decomposition
autoplot(AirPassengers)

lAirPassengers <- log(AirPassengers) 
autoplot(lAirPassengers, ylab="log(AirPassengers)")
fit_sede <- stl(lAirPassengers, s.window="period")
autoplot(fit_sede)
   
library(directlabels)
# month plot: all January values from 1949 to 1960 connected, all February values connected, and so on
ggmonthplot(AirPassengers) +
  labs(title="Month plot: AirPassengers", 
       x="", 
       y="Passengers (thousands)")
# seasonal plot by year
p <- ggseasonplot(AirPassengers) + geom_point() +
  labs(title="Seasonal plot: AirPassengers", 
       x="", 
       y="Passengers (thousands)") 
direct.label(p)


# 3. Exponential forecasting models: Y(t) = level + irregular(t) , Y(t+1) = c(0)Y(t) + c(1)Y(t-1)...
library(forecast)
fit_ef <- ets(nhtemp, model="ANN")
fit_ef
forecast(fit_ef, 1)
autoplot(forecast(fit_ef, 1)) + 
  labs(x = "Year", 
       y = expression(paste("Temperature (", degree*F,")",)), 
       title = "New Haven Annual Mean Temperature")
accuracy(fit_ef)

# Holt and Holt-Winters exponential smoothing: Y(t) = level + slope*t + irregular(t)
fit_hhes <- ets(log(AirPassengers), model="AAA")
fit_hhes
accuracy(fit_hhes)
pred <- forecast(fit_hhes, 5)
pred
autoplot(pred) + 
  labs(title = "Forecast for Air Travel", 
       y = "Log(AirPassengers)", 
       x ="Time")

# automatic exponential forecasting: select a best-fitting model
fit_ae <- ets(JohnsonJohnson)
fit_ae
autoplot(forecast(fit_ae)) + 
  labs(x = "Time", 
       y = "Quarterly Earnings (Dollars)", 
       title="Johnson and Johnson Forecasts")


# 4. ARIMA forecasting models: ARIMA(p, d, q)
autoplot(Nile)
ndiffs(Nile)  # best difference (d) for stationary data
dNile <- diff(Nile)
autoplot(dNile)

autoplot(Acf(dNile)) # determining p q
autoplot(Pacf(dNile))

fit_arima <- arima(Nile, order=c(0,1,1)) 
fit_arima
accuracy(fit_arima)

# evaluating assumptions
df <- data.frame(resid = as.numeric(fit_arima$residuals))
ggplot(df, aes(sample = resid)) + 
  stat_qq() + stat_qq_line() + 
  labs(title="Normal Q-Q Plot")
Box.test(fit$residuals, type="Ljung-Box")

# making forecast
forecast(fit_arima, 3)
autoplot(forecast(fit_arima, 3)) + labs(x="Year", y="Annual Flow")

# automated ARIMA forecasting, finds best d, p, q itself
fit_autoarima <- auto.arima(sunspots)