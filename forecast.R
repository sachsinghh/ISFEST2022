# Metode Peramalan
#install packages
install.packages("TSA")
install.packages("forecast")
install.packages('tseries')
install.packages("normtest")

#load packages
library(TSA)
library(forecast)
library(tseries)
library(normtest)
library(mvnormtest)

tahun <- c(2010,2011,2012,2013,2014,2015)
jumlah_lulus <- c(21,20,18,40,22,19)

df <- data.frame(tahun,jumlah_lulus)
View(df)
lulus = df$jumlah_lulus

# PART 1
# jadiin data kita jadi data time series
lulusts = ts(lulus, frequency = 1, start = c(2010,1))  # freq = 1 karena data tahunan

head(lulusts)
plot(lulusts) 

difflulus = diff(lulusts, differences = 1)
plot(difflulus)
adf.test(diffsuhu) 
difflulus2 = diff(lulusts, differences = 2)
plot(difflulus2)



tsdisplay(difflulus2) #liat Plot ACF, PACF


#PART 3
#dari hasil plot acf, pacf, dan eacf, dicoba bbrp model yg kira2 cocok
#kandidat model (cari yg terbaik)
#caranya = cari nilai log likelihood yg terbesar, ato nilai AIC dan BIC yang terkecil
model1 = Arima(lulusts, order = c(0,2,0))
model2 = Arima(lulusts, order = c(1,2,0))
model3 = Arima(lulusts, order = c(0,2,1))
#panggil semua model, bandingin MLE dan AIC
model1
model2
model3
cbind(model1,model2,model3)
# yang terbaik adalah model3 = arima(0,2,1) krn log likelihood paling gede

#PART 4
#bikin model asli
fit = Arima(lulusts, order = c(0,2,1))
fit  #nilai ar sama ma itu taksiran parameter
#Metode momen = MME, default = ML (maximum likelihood)

#PART 5
#diagnostic checking / cek asumsi
# 1. cek independensi / uncorrelation dari residual
# Hipotesis = H0 : residual TIDAK ada korelasi, H1 : residual ada korelasi
checkresiduals(fit)  #karena p-value > alpha, H0 diterima
#dari korelogramnya kalo gaada lag yang ngelewatin garis = H0 diterima
# 2. cek normality residual
# Hipotesis = H0: residual data berdist. normal, H1: residual data TIDAK berdist. normal
qqnorm(lulusts, nrepl = 2000) #p-value>alpha = H0 diterima
# 3. overfitting data
# naikin orde ar dulu, baru ma
# untuk liat fit ke overfit nilai loglik sama koefisiennya jomplang, apa fit udah cukup
overfit1 = Arima(lulusts, order = c(2,1,1)) #bandingin koef ar dari overfit1 sama fit
overfit2 = Arima(lulusts, order = c(1,1,2)) #bandingin koef ma
overfit1 
overfit2
cbind(fit,overfit1,overfit2)

#PART 6
#Cross-Validation
#data dibagi 2, training dan testing data
#kita pake data 1880-1980, 1981-1985 buat trial/testing
actual = window(lulusts, start=c(2015))  # data u/ testing, data mulai dr 1981
lulusts2 = window(lulusts, end = c(2014)) #data u/ training, berakhir di 1980
fit2 = Arima(lulusts2, order=c(0,2,1)) #TRAINING
#lakukan forecasting u/ data testing
forecast2 = forecast(fit2, h=1)
plot(forecast2)
cbind(actual,forecast2)
# selanjutnya, lakukan forecasting untuk data keseluruhan
forecast = forecast(fit, h=1)
plot(forecast)
forecast
