
#Load packages
library(ggplot2); library(seacarb); # this has the seafet functions (sf_...)
library(reshape2); library(plyr); library(dplyr);
library(xts) #interactive plots
library(RVAideMemoire)
library(Rmisc)
library(gridExtra)
library(plyr)
library(grid)
library(PMCMRplus)
library(readr)

#set working directory
setwd("~/Dropbox/Duke/ORCC 2024/Enviromental Data")

# Data from NOAA Tides and Currents stations (https://tidesandcurrents.noaa.gov)
# Virginia Key, FL station (https://tidesandcurrents.noaa.gov/stationhome.html?id=8723214)
# Beaufort, Duke Marine Lab, NC (https://tidesandcurrents.noaa.gov/stationhome.html?id=8656483)

#5 year-----------------------
#Load data 
yr2020 <- read_csv("NOAA_temp_data_2020.csv")
# Remove rows where there is missing temperature data
yr2020 <- yr2020[!(yr2020$WaterTemp_F == "-"), ]
#formatting date/time accordingly for analyses 
yr2020$Date_Time <- as.POSIXct(as.POSIXct(as.character(yr2020$Date_Time), tz="GMT", format = "%m/%d/%y %H:%M"))
#format location as a factor
yr2020$Location <- as.factor(yr2020$Location)
#format water temperature as numeric
yr2020$WaterTemp_F <- as.numeric(yr2020$WaterTemp_F)
#add column of water temp in Celsius
yr2020$WaterTemp_C <- (yr2020$WaterTemp_F - 32) / 1.8

yr2021 <- read_csv("NOAA_temp_data_2021.csv")
# Remove rows where there is missing temperature data
yr2021 <- yr2021[!(yr2021$WaterTemp_F == "-"), ]
#formatting date/time accordingly for analyses 
yr2021$Date_Time <- as.POSIXct(as.POSIXct(as.character(yr2021$Date_Time), tz="GMT", format = "%m/%d/%y %H:%M"))
#format location as a factor
yr2021$Location <- as.factor(yr2021$Location)
#format water temperature as numeric
yr2021$WaterTemp_F <- as.numeric(yr2021$WaterTemp_F)
#add column of water temp in Celsius
yr2021$WaterTemp_C <- (yr2021$WaterTemp_F - 32) / 1.8

yr2022 <- read_csv("NOAA_temp_data_2022.csv")
# Remove rows where there is missing temperature data
yr2022 <- yr2022[!(yr2022$WaterTemp_F == "-"), ]
#formatting date/time accordingly for analyses 
yr2022$Date_Time <- as.POSIXct(as.POSIXct(as.character(yr2022$Date_Time), tz="GMT", format = "%m/%d/%y %H:%M"))
#format location as a factor
yr2022$Location <- as.factor(yr2022$Location)
#format water temperature as numeric
yr2022$WaterTemp_F <- as.numeric(yr2022$WaterTemp_F)
#add column of water temp in Celsius
yr2022$WaterTemp_C <- (yr2022$WaterTemp_F - 32) / 1.8

yr2023 <- read_csv("NOAA_temp_data_2023.csv")
# Remove rows where there is missing temperature data
yr2023 <- yr2023[!(yr2023$WaterTemp_F == "-"), ]
#formatting date/time accordingly for analyses 
yr2023$Date_Time <- as.POSIXct(as.POSIXct(as.character(yr2023$Date_Time), tz="GMT", format = "%m/%d/%y %H:%M"))
#format location as a factor
yr2023$Location <- as.factor(yr2023$Location)
#format water temperature as numeric
yr2023$WaterTemp_F <- as.numeric(yr2023$WaterTemp_F)
#add column of water temp in Celsius
yr2023$WaterTemp_C <- (yr2023$WaterTemp_F - 32) / 1.8

yr2024 <- read_csv("NOAA_temp_data_2024.csv")
# Remove rows where there is missing temperature data
yr2024 <- yr2024[!(yr2024$WaterTemp_F == "-"), ]
#formatting date/time accordingly for analyses 
yr2024$Date_Time <- as.POSIXct(as.POSIXct(as.character(yr2024$Date_Time), tz="GMT", format = "%m/%d/%y %H:%M"))
#format location as a factor
yr2024$Location <- as.factor(yr2024$Location)
#format water temperature as numeric
yr2024$WaterTemp_F <- as.numeric(yr2024$WaterTemp_F)
#add column of water temp in Celsius
yr2024$WaterTemp_C <- (yr2024$WaterTemp_F - 32) / 1.8

fiveyear <- rbind(yr2020, yr2021, yr2022, yr2023, yr2024)

#Subset data by location
BNC <- subset(fiveyear, Location == "BNC")
VKFL <- subset(fiveyear, Location == "VKFL")

#summary statistics at each site
mean(BNC$WaterTemp_C) #20.30525
range(BNC$WaterTemp_C) #5.722222 - 31.000000
sd(BNC$WaterTemp_C) #6.488392

mean(VKFL$WaterTemp_C) #27.61669
range(VKFL$WaterTemp_C) #18.77778 - 34.00000
sd(VKFL$WaterTemp_C) #3.172304

#summary statistics at each site each year
#subset data by location for each year
BNC2020 <- subset(yr2020, Location == "BNC")
VKFL2020 <- subset(yr2020, Location == "VKFL")
mean(BNC2020$WaterTemp_C) # 20.31726
range(BNC2020$WaterTemp_C) # 6.388889 - 31.000000
mean(VKFL2020$WaterTemp_C) # 27.8623
range(VKFL2020$WaterTemp_C) # 20.11111 - 33.88889

BNC2021 <- subset(yr2021, Location == "BNC")
VKFL2021 <- subset(yr2021, Location == "VKFL")
mean(BNC2021$WaterTemp_C) # 20.13783
range(BNC2021$WaterTemp_C) # 7.111111 - 31.000000
mean(VKFL2021$WaterTemp_C) # 28.22616
range(VKFL2021$WaterTemp_C) # 19.27778 - 34.00000

BNC2022 <- subset(yr2022, Location == "BNC")
VKFL2022 <- subset(yr2022, Location == "VKFL")
mean(BNC2022$WaterTemp_C) # 20.19669
range(BNC2022$WaterTemp_C) # 5.722222 - 30.888889
mean(VKFL2022$WaterTemp_C) # 27.3351
range(VKFL2022$WaterTemp_C) # 18.77778 - 32.88889

BNC2023 <- subset(yr2023, Location == "BNC")
VKFL2023 <- subset(yr2023, Location == "VKFL")
mean(BNC2023$WaterTemp_C) # 20.16241
range(BNC2023$WaterTemp_C) # 8.111111 - 31.000000
mean(VKFL2023$WaterTemp_C) # 27.4105
range(VKFL2023$WaterTemp_C) # 19.27778 - 34.00000

BNC2024 <- subset(yr2024, Location == "BNC")
VKFL2024 <- subset(yr2024, Location == "VKFL")
mean(BNC2024$WaterTemp_C) # 20.74166
range(BNC2024$WaterTemp_C) # 5.888889 30.277778
mean(VKFL2024$WaterTemp_C) # 27.21791
range(VKFL2024$WaterTemp_C) # 19.5 - 33.5

#Kolmogorov-Smirnov test to examine differences in the distribution of observed temperature values at each site
#Asymptotic two-sample Kolmogorov-Smirnov test
ks.test(BNC$WaterTemp_C,VKFL$WaterTemp_C) 
#D = 0.53828, p-value < 2.2e-16

#Permanova on temperature distributions
perm.anova(WaterTemp_C ~ Location, data = fiveyear, nperm = 999)
#           Sum Sq    Df Mean Sq F value Pr(>F)    
#Location  1146482     1 1146482   43999  0.001 ***
#  Residuals 2235292 85785      26                   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Hartley's maximum F-ratio test to examine differences in the variance of temperature at each site
hartleyTest(WaterTemp_C ~ Location, data = fiveyear)
# F Max = 4.1834, df = 42958, k = 2, p-value < 2.2e-16

#Plotting time-series of temperature at each location
BNC_plot <- ggplot(data = BNC, aes(Date_Time, WaterTemp_C)) +
  geom_line(colour = 'blue') +
  ylim(0, 35) +
  geom_hline(yintercept = 20.30525, colour = "blue", linetype="dashed", linewidth = 1) + #add mean as a line
  xlab("Date") +
  theme_bw(base_size = 20) +
  scale_x_datetime(date_breaks = "1 year")
BNC_plot
ggsave("BNC_plot_5year.pdf", plot = BNC_plot, device = "pdf", width = 7, height = 7)

VKFL_plot <- ggplot(data = VKFL, aes(Date_Time, WaterTemp_C)) +
  geom_line(colour = 'red') +
  ylim(0, 35) +
  geom_hline(yintercept = 27.61669, colour = "red", linetype="dashed", linewidth = 1) + #add mean as a line
  xlab("Date") +
  theme_bw(base_size = 20) +
  scale_x_datetime(date_breaks = "1 year")
VKFL_plot
ggsave("VKFL_plot_5year.pdf", plot = VKFL_plot, device = "pdf", width = 7, height = 7)

Both_plot_5year <-ggplot(data = fiveyear, aes(x = Date_Time, y = WaterTemp_C, color = Location)) +
  geom_line() +
  scale_color_manual(values = c("BNC" = "blue", "VKFL" = "red")) +
  ylim(0, 35) +
  geom_hline(yintercept = 20.30525, colour = "blue", linetype="dashed", linewidth = 1) + #add mean as a line
    geom_hline(yintercept = 27.61669, colour = "red", linetype="dashed", linewidth = 1) + #add mean as a line
    xlab("Year") +
  theme_bw(base_size = 20) +
  scale_x_datetime(date_breaks = "1 year")
Both_plot_5year
ggsave("Both_plot_5year.pdf", plot = Both_plot_5year, device = "pdf", width = 7, height = 5)

#Plot overlaid densities of observed temperatures at each site
density_plot <- ggplot(fiveyear, aes(x = WaterTemp_C, fill = Location)) +
  geom_density(alpha = 0.3) + 
  scale_fill_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  geom_vline(xintercept = mean(BNC$WaterTemp_C), colour = "blue", linetype = "dashed", size = 1.3) +
  geom_vline(xintercept = mean(VKFL$WaterTemp_C), colour = "red", linetype = "dashed",  size = 1.3) +
  xlim(0, 35) +
  theme_bw(base_size = 20)
density_plot
ggsave("density_plot_5year.pdf", plot = density_plot, device = "pdf", width = 7, height = 7)


#Compute autocorrelation using hourly lags
BNC.auto <- acf(BNC$WaterTemp_C, plot = FALSE)
VKFL.auto <- acf(VKFL$WaterTemp_C, plot = FALSE)

BNC.auto <- as.data.frame(BNC.auto$acf[0:47])
BNC.auto$Lag <- seq(from = 1, to = 47, by = 1)
colnames(BNC.auto) <- c("Autocorr", "Lag")

VKFL.auto <- as.data.frame(VKFL.auto$acf[0:47])
VKFL.auto$Lag <- seq(from = 1, to = 47, by = 1)
colnames(VKFL.auto) <- c("Autocorr", "Lag")

BNC.auto$Location <- "BNC"
VKFL.auto$Location <- "VKFL"

all.auto <- rbind(BNC.auto, VKFL.auto)

#plotting autocorrelation with hourly lag
auto.hourly <- ggplot(all.auto, aes(x = Lag, y = Autocorr, shape = Location, colour = Location)) +
  geom_point(size = 2)+
  scale_shape_manual("Location", values = c("BNC" = 16, "VKFL" = 18)) +
  scale_colour_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  ylab("Temperature Autocorrelation") +
  xlab("Lag (hours)") +
  theme_bw(base_size = 20)
auto.hourly
ggsave("auto_hourly_5year.pdf", plot = auto.hourly, device = "pdf", width = 7, height = 5)

#Compute autocorrelation with daily lags
#take average of every day's temperature
BNC_daily <- as.data.frame(sapply(split(BNC$WaterTemp_C, rep(1:(nrow(BNC)/24), each=24)), mean))
colnames(BNC_daily) <- c("WaterTemp_C")
VKFL_daily <- as.data.frame(sapply(split(VKFL$WaterTemp_C, rep(1:(nrow(VKFL)/24), each=24)), mean))
colnames(VKFL_daily) <- c("WaterTemp_C")

#re-run autocorrelation on average daily values
BNC.auto <- acf(BNC_daily$WaterTemp_C, plot = FALSE)
VKFL.auto <- acf(VKFL_daily$WaterTemp_C, plot = FALSE)

BNC.auto <- as.data.frame(BNC.auto$acf[0:33])
BNC.auto$Lag <- seq(from = 1, to = 33, by = 1)
colnames(BNC.auto) <- c("Autocorr", "Lag")
VKFL.auto <- as.data.frame(VKFL.auto$acf[0:33])
VKFL.auto$Lag <- seq(from = 1, to = 33, by = 1)
colnames(VKFL.auto) <- c("Autocorr", "Lag")

BNC.auto$Location <- "BNC"
VKFL.auto$Location <- "VKFL"

all.auto <- rbind(BNC.auto, VKFL.auto)

#plotting autocorrelation with daily lag
auto.daily <- ggplot(all.auto, aes(x = Lag, y = Autocorr, shape = Location, colour = Location)) +
  geom_point(size = 2)+
  scale_shape_manual("Location", values = c("BNC" = 16, "VKFL" = 18)) +
  scale_colour_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  ylab("Temperature Autocorrelation") +
  xlab("Lag (days)") +
  theme_bw(base_size = 20)
auto.daily
ggsave("auto_daily_5year.pdf", plot = auto.daily, device = "pdf", width = 7, height = 5)


# Compute the spectrum for each location
spec_BNC <- spectrum(BNC$WaterTemp_C)
spec_VKFL <- spectrum(VKFL$WaterTemp_C)

# Plot the spectrum
plot(spec_BNC)
plot(spec_VKFL)

# Inspect results
print(spec_BNC$spec)  # Spectral density
print(spec_BNC$freq)  # Frequencies corresponding to the spectral density
print(spec_VKFL$spec)  # Spectral density
print(spec_VKFL$freq)  # Frequencies corresponding to the spectral density

# Find the dominant frequency (highest spectral density, first peak)
dominant_index_BNC <- which.max(spec_BNC$spec)
dominant_freq_BNC <- spec_BNC$freq[dominant_index_BNC]
dominant_power_BNC <- spec_BNC$spec[dominant_index_BNC]
dominant_index_VKFL <- which.max(spec_VKFL$spec)
dominant_freq_VKFL <- spec_VKFL$freq[dominant_index_VKFL]
dominant_power_VKFL <- spec_VKFL$spec[dominant_index_VKFL]

# Convert the dominant frequency to period (1 / frequency)
dominant_period_BNC <- 1 / dominant_freq_BNC
dominant_period_VKFL <- 1 / dominant_freq_VKFL

# Print the results
cat("Dominant Frequency:", dominant_freq_BNC, "\n") #0.0001157407 
cat("Dominant Period:", dominant_period_BNC, "units of time\n") #8640 units of time = 360 days
cat("Dominant Frequency:", dominant_freq_VKFL, "\n") #0.0001157407 
cat("Dominant Period:", dominant_period_VKFL, "units of time\n") #8640 units of time = 360 days

#Identify other dominant frequencies
#sort the spectral density values in decreasing order, skipping the already identified first peak
sorted_indices_BNC <- order(spec_BNC$spec, decreasing = TRUE)
sorted_indices_BNC <- sorted_indices_BNC[sorted_indices_BNC != dominant_index_BNC]  # Remove the first peak
sorted_indices_VKFL <- order(spec_VKFL$spec, decreasing = TRUE)
sorted_indices_VKFL <- sorted_indices_VKFL[sorted_indices_VKFL != dominant_index_VKFL]  # Remove the first peak

# Get the next few dominant frequencies and their corresponding powers
num_next_peaks_BNC <- 3  # For example, extract the next 3 peaks
next_dominant_freqs_BNC <- spec_BNC$freq[sorted_indices_BNC[1:num_next_peaks_BNC]]
next_dominant_powers_BNC <- spec_BNC$spec[sorted_indices_BNC[1:num_next_peaks_BNC]]
num_next_peaks_VKFL <- 3  # For example, extract the next 3 peaks
next_dominant_freqs_VKFL <- spec_VKFL$freq[sorted_indices_VKFL[1:num_next_peaks_VKFL]]
next_dominant_powers_VKFL <- spec_VKFL$spec[sorted_indices_VKFL[1:num_next_peaks_VKFL]]

# Convert to periods
next_dominant_periods_BNC <- 1 / next_dominant_freqs_BNC
next_dominant_periods_VKFL <- 1 / next_dominant_freqs_VKFL

# Print the results
for (i in 1:num_next_peaks_BNC) {
  cat(paste("Next Dominant Frequency", i, ":", next_dominant_freqs_BNC[i], "\n"))
  cat(paste("Next Dominant Period", i, ":", next_dominant_periods_BNC[i], "units of time\n"))
}
#Next Dominant Frequency 1 : 4.62962962962963e-05 
#Next Dominant Period 1 : 21600 units of time = 900 days = ~30 months or 2.5 years
#Next Dominant Frequency 2 : 2.31481481481481e-05 
#Next Dominant Period 2 : 43200 units of time = 1800 days = 5 years
#Next Dominant Frequency 3 : 6.94444444444444e-05 
#Next Dominant Period 3 : 14400 units of time = 600 days = 1.64 years

for (i in 1:num_next_peaks_VKFL) {
  cat(paste("Next Dominant Frequency", i, ":", next_dominant_freqs_VKFL[i], "\n"))
  cat(paste("Next Dominant Period", i, ":", next_dominant_periods_VKFL[i], "units of time\n"))
}
#Next Dominant Frequency 1 : 6.94444444444444e-05 
#Next Dominant Period 1 : 14400 units of time = 600 days = 1.64 years
#Next Dominant Frequency 2 : 2.31481481481481e-05 
#Next Dominant Period 2 : 43200 units of time = 1800 days = 5 years
#Next Dominant Frequency 3 : 0.000185185185185185 
#Next Dominant Period 3 : 5400 units of time = 225 days = ~7.4 months

#Visualize the spectrum
# Plot the spectrum
plot(spec_BNC, main = "Power Spectrum BNC")

# Highlight the dominant frequency
points(dominant_freq_BNC, dominant_power_BNC, col = "blue", pch = 19)

# Highlight the next dominant frequencies
for (i in 1:num_next_peaks_BNC) {
  points(next_dominant_freqs_BNC[i], next_dominant_powers_BNC[i], col = "lightblue", pch = 19)
}

#Visualize the spectrum
# Plot the spectrum
plot(spec_VKFL, main = "Power Spectrum VKFL")

# Highlight the dominant frequency
points(dominant_freq_VKFL, dominant_power_VKFL, col = "red", pch = 19)

# Highlight the next dominant frequencies
for (i in 1:num_next_peaks_VKFL) {
  points(next_dominant_freqs_VKFL[i], next_dominant_powers_VKFL[i], col = "pink", pch = 19)
}


# Decompose the time series
# Compute the Fourier transform of the time series
fft_result_BNC <- fft(spec_BNC$freq)

# Plot the Fourier coefficients
plot(Mod(fft_result_BNC), type = "l", main = "Fourier Transform Magnitude")



#2022--------------------
#Load data
yr2022 <- read_csv("NOAA_temp_data_2022.csv")
# Remove rows where there is missing temperature data
yr2022 <- yr2022[!(yr2022$WaterTemp_F == "-"), ]
#formatting date/time accordingly for analyses 
yr2022$Date_Time <- as.POSIXct(as.POSIXct(as.character(yr2022$Date_Time), tz="GMT", format = "%m/%d/%y %H:%M"))
#format location as a factor
yr2022$Location <- as.factor(yr2022$Location)
#format water temperature as numeric
yr2022$WaterTemp_F <- as.numeric(yr2022$WaterTemp_F)
#add column of water temp in Celsius
yr2022$WaterTemp_C <- (yr2022$WaterTemp_F - 32) / 1.8

#Subset data by location
BNC <- subset(yr2022, Location == "BNC")
VKFL <- subset(yr2022, Location == "VKFL")

#summary statistics at each site
mean(BNC$WaterTemp_C) #20.19669
range(BNC$WaterTemp_C) #5.722222-30.888889

mean(VKFL$WaterTemp_C) #27.3351
range(VKFL$WaterTemp_C) #18.77778 32.88889

#Kolmogorov-Smirnov test to examine differences in the distribution of observed temperature values at each site
#Asymptotic two-sample Kolmogorov-Smirnov test
ks.test(BNC$WaterTemp_C,VKFL$WaterTemp_C) 
#D = 0.56524, p-value < 2.2e-16

#Permanova on temperature distributions
perm.anova(WaterTemp_C ~ Location, data = yr2022, nperm = 999)
#Sum Sq    Df Mean Sq F value Pr(>F)    
#Location  222399     1  222399    8257  0.001 ***
#  Residuals 470173 17456      27 

#Hartley's maximum F-ratio test to examine differences in the variance of temperature at each site
hartleyTest(WaterTemp_C ~ Location, data = yr2022)
# F Max = 5.4246, df = 8702, k = 2, p-value < 2.2e-16

#Plotting time-series of temperature at each location
BNC_plot <- ggplot(data = BNC, aes(Date_Time, WaterTemp_C)) +
  geom_line(colour = 'blue') +
  ylim(0, 35) +
  geom_hline(yintercept = 20.19669, colour = "blue", linetype="dashed", linewidth = 1) + #add mean as a line
  xlab("Date") +
  theme_bw(base_size = 20) +
  scale_x_datetime(date_breaks = "3 month")
BNC_plot
ggsave("BNC_plot_2022.pdf", plot = BNC_plot, device = "pdf", width = 7, height = 7)

VKFL_plot <- ggplot(data = VKFL, aes(Date_Time, WaterTemp_C)) +
  geom_line(colour = 'red') +
  ylim(0, 35) +
  geom_hline(yintercept = 27.3351, colour = "red", linetype="dashed", linewidth = 1) + #add mean as a line
  xlab("Date") +
  theme_bw(base_size = 20) +
  scale_x_datetime(date_breaks = "3 month")
VKFL_plot
ggsave("VKFL_plot_2022.pdf", plot = VKFL_plot, device = "pdf", width = 7, height = 7)


#Plot overlaid densities of observed temperatures at each site
density_plot <- ggplot(yr2022, aes(x = WaterTemp_C, fill = Location)) +
  geom_density(alpha = 0.3) + 
  scale_fill_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  geom_vline(xintercept = mean(BNC$WaterTemp_C), colour = "lightblue", size = 1.3) +
  geom_vline(xintercept = mean(VKFL$WaterTemp_C), colour = "pink", linetype = "dashed",  size = 1.3) +
  xlim(0, 35) +
  theme_bw(base_size = 20)
density_plot


#Compute autocorrelation using hourly lags
BNC.auto <- acf(BNC$WaterTemp_C, plot = FALSE)
VKFL.auto <- acf(VKFL$WaterTemp_C, plot = FALSE)

BNC.auto <- as.data.frame(BNC.auto$acf[0:40])
BNC.auto$Lag <- seq(from = 1, to = 40, by = 1)
colnames(BNC.auto) <- c("Autocorr", "Lag")

VKFL.auto <- as.data.frame(VKFL.auto$acf[0:40])
VKFL.auto$Lag <- seq(from = 1, to = 40, by = 1)
colnames(VKFL.auto) <- c("Autocorr", "Lag")

BNC.auto$Location <- "BNC"
VKFL.auto$Location <- "VKFL"

all.auto <- rbind(BNC.auto, VKFL.auto)

#plotting autocorrelation with hourly lag
auto.hourly <- ggplot(all.auto, aes(x = Lag, y = Autocorr, shape = Location, colour = Location)) +
  geom_point(size = 3)+
  scale_shape_manual("Location", values = c("BNC" = 18, "VKFL" = 16)) +
  scale_colour_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  ylab("pH Autocorrelation") +
  xlab("Lag (hours)") +
  theme_bw(base_size = 20)
auto.hourly


#Compute autocorrelation with daily lags
#take average of every day's temperature
BNC_daily <- as.data.frame(sapply(split(BNC$WaterTemp_C, rep(1:(nrow(BNC)/24), each=24)), mean))
colnames(BNC_daily) <- c("WaterTemp_C")
VKFL_daily <- as.data.frame(sapply(split(VKFL$WaterTemp_C, rep(1:(nrow(VKFL)/24), each=24)), mean))
colnames(VKFL_daily) <- c("WaterTemp_C")

#re-run autocorrelation on average daily values
BNC.auto <- acf(BNC_daily$WaterTemp_C, plot = FALSE)
VKFL.auto <- acf(VKFL_daily$WaterTemp_C, plot = FALSE)


BNC.auto <- as.data.frame(BNC.auto$acf[0:26])
BNC.auto$Lag <- seq(from = 1, to = 26, by = 1)
colnames(BNC.auto) <- c("Autocorr", "Lag")
VKFL.auto <- as.data.frame(VKFL.auto$acf[0:26])
VKFL.auto$Lag <- seq(from = 1, to = 26, by = 1)
colnames(VKFL.auto) <- c("Autocorr", "Lag")

BNC.auto$Location <- "BNC"
VKFL.auto$Location <- "VKFL"

all.auto <- rbind(BNC.auto, VKFL.auto)

#plotting autocorrelation with daily lag
auto.daily <- ggplot(all.auto, aes(x = Lag, y = Autocorr, shape = Location, colour = Location)) +
  geom_point(size = 3)+
  scale_shape_manual("Location", values = c("BNC" = 18, "VKFL" = 16)) +
  scale_colour_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  ylab("pH Autocorrelation") +
  xlab("Lag (hours)") +
  theme_bw(base_size = 20)
auto.daily


#2023--------------

#Load data
yr2023 <- read_csv("NOAA_temp_data_2023.csv")
# Remove rows where there is missing temperature data
yr2023 <- yr2023[!(yr2023$WaterTemp_F == "-"), ]
#formatting date/time accordingly for analyses 
yr2023$Date_Time <- as.POSIXct(as.POSIXct(as.character(yr2023$Date_Time), tz="GMT", format = "%m/%d/%y %H:%M"))

#format location as a factor
yr2023$Location <- as.factor(yr2023$Location)
#format water temperature as numeric
yr2023$WaterTemp_F <- as.numeric(yr2023$WaterTemp_F)

#add column of water temp in Celsius
yr2023$WaterTemp_C <- (yr2023$WaterTemp_F - 32) / 1.8

#Subset data by location
BNC <- subset(yr2023, Location == "BNC")
VKFL <- subset(yr2023, Location == "VKFL")

#summary statistics at each site
mean(BNC$WaterTemp_C) #20.16241
range(BNC$WaterTemp_C) #8.111111 - 31.000000

mean(VKFL$WaterTemp_C) #27.4105
range(VKFL$WaterTemp_C) #19.27778 - 34.00000

#Kolmogorov-Smirnov test to examine differences in the distribution of observed temperature values at each site
#Asymptotic two-sample Kolmogorov-Smirnov test
ks.test(BNC$WaterTemp_C,VKFL$WaterTemp_C) 
#D = 0.59817, p-value < 2.2e-16

#Permanova on temperature distributions
perm.anova(WaterTemp_C ~ Location, data = yr2023, nperm = 999)
#Sum Sq    Df Mean Sq F value Pr(>F)    
#Location  227637     1  227637  9772.8  0.001 ***
#  Residuals 403713 17332      23  

#Hartley's maximum F-ratio test to examine differences in the variance of temperature at each site
hartleyTest(WaterTemp_C ~ Location, data = yr2023)
# F Max = 3.8142, df = 8753, k = 2, p-value < 2.2e-16

#Plotting time-series of temperature at each location
BNC_plot <- ggplot(data = BNC, aes(Date_Time, WaterTemp_C)) +
  geom_line(colour = 'blue') +
  ylim(0, 35) +
  geom_hline(yintercept = 20.19669, colour = "blue", linetype="dashed", linewidth = 1) + #add mean as a line
  xlab("Date") +
  theme_bw(base_size = 20) +
  scale_x_datetime(date_breaks = "3 month")
BNC_plot
ggsave("BNC_plot_2022.pdf", plot = BNC_plot, device = "pdf", width = 7, height = 7)

VKFL_plot <- ggplot(data = VKFL, aes(Date_Time, WaterTemp_C)) +
  geom_line(colour = 'red') +
  ylim(0, 35) +
  geom_hline(yintercept = 27.3351, colour = "red", linetype="dashed", linewidth = 1) + #add mean as a line
  xlab("Date") +
  theme_bw(base_size = 20) +
  scale_x_datetime(date_breaks = "3 month")
VKFL_plot
ggsave("VKFL_plot_2022.pdf", plot = VKFL_plot, device = "pdf", width = 7, height = 7)


#Plot overlaid densities of observed temperatures at each site
density_plot <- ggplot(yr2023, aes(x = WaterTemp_C, fill = Location)) +
  geom_density(alpha = 0.3) + 
  scale_fill_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  geom_vline(xintercept = mean(BNC$WaterTemp_C), colour = "lightblue", size = 1.3) +
  geom_vline(xintercept = mean(VKFL$WaterTemp_C), colour = "pink", linetype = "dashed",  size = 1.3) +
  xlim(0, 35) +
  theme_bw(base_size = 20)
density_plot


#Compute autocorrelation using hourly lags
BNC.auto <- acf(BNC$WaterTemp_C, plot = FALSE)
VKFL.auto <- acf(VKFL$WaterTemp_C, plot = FALSE)

BNC.auto <- as.data.frame(BNC.auto$acf[0:40])
BNC.auto$Lag <- seq(from = 1, to = 40, by = 1)
colnames(BNC.auto) <- c("Autocorr", "Lag")

VKFL.auto <- as.data.frame(VKFL.auto$acf[0:40])
VKFL.auto$Lag <- seq(from = 1, to = 40, by = 1)
colnames(VKFL.auto) <- c("Autocorr", "Lag")

BNC.auto$Location <- "BNC"
VKFL.auto$Location <- "VKFL"

all.auto <- rbind(BNC.auto, VKFL.auto)

#plotting autocorrelation with hourly lag
auto.hourly <- ggplot(all.auto, aes(x = Lag, y = Autocorr, shape = Location, colour = Location)) +
  geom_point(size = 3)+
  scale_shape_manual("Location", values = c("BNC" = 18, "VKFL" = 16)) +
  scale_colour_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  ylab("pH Autocorrelation") +
  xlab("Lag (hours)") +
  theme_bw(base_size = 20)
auto.hourly


#Compute autocorrelation with daily lags
#take average of every day's temperature
BNC_daily <- as.data.frame(sapply(split(BNC$WaterTemp_C, rep(1:(nrow(BNC)/24), each=24)), mean))
colnames(BNC_daily) <- c("WaterTemp_C")
VKFL_daily <- as.data.frame(sapply(split(VKFL$WaterTemp_C, rep(1:(nrow(VKFL)/24), each=24)), mean))
colnames(VKFL_daily) <- c("WaterTemp_C")

#re-run autocorrelation on average daily values
BNC.auto <- acf(BNC_daily$WaterTemp_C, plot = FALSE)
VKFL.auto <- acf(VKFL_daily$WaterTemp_C, plot = FALSE)


BNC.auto <- as.data.frame(BNC.auto$acf[0:26])
BNC.auto$Lag <- seq(from = 1, to = 26, by = 1)
colnames(BNC.auto) <- c("Autocorr", "Lag")
VKFL.auto <- as.data.frame(VKFL.auto$acf[0:26])
VKFL.auto$Lag <- seq(from = 1, to = 26, by = 1)
colnames(VKFL.auto) <- c("Autocorr", "Lag")

BNC.auto$Location <- "BNC"
VKFL.auto$Location <- "VKFL"

all.auto <- rbind(BNC.auto, VKFL.auto)

#plotting autocorrelation with daily lag
auto.daily <- ggplot(all.auto, aes(x = Lag, y = Autocorr, shape = Location, colour = Location)) +
  geom_point(size = 3)+
  scale_shape_manual("Location", values = c("BNC" = 18, "VKFL" = 16)) +
  scale_colour_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  ylab("pH Autocorrelation") +
  xlab("Lag (hours)") +
  theme_bw(base_size = 20)
auto.daily



#2023 7 warm months only------------------------
#Load data
yr2023_7mo <- read_csv("NOAA_temp_data_2023_7mo.csv")
# Remove rows where there is missing temperature data
yr2023_7mo <- yr2023_7mo[!(yr2023_7mo$WaterTemp_F == "-"), ]
#formatting date/time accordingly for analyses 
yr2023_7mo$Date_Time <- as.POSIXct(as.POSIXct(as.character(yr2023_7mo$Date_Time), tz="GMT", format = "%m/%d/%y %H:%M"))

#format location as a factor
yr2023_7mo$Location <- as.factor(yr2023_7mo$Location)
#format water temperature as numeric
yr2023_7mo$WaterTemp_F <- as.numeric(yr2023_7mo$WaterTemp_F)

#add column of water temp in Celsius
yr2023_7mo$WaterTemp_C <- (yr2023_7mo$WaterTemp_F - 32) / 1.8

#Subset data by location
BNC <- subset(yr2023_7mo, Location == "BNC")
VKFL <- subset(yr2023_7mo, Location == "VKFL")

#summary statistics at each site
mean(BNC$WaterTemp_C) #24.44656
range(BNC$WaterTemp_C) #13.22222 - 31.00000

mean(VKFL$WaterTemp_C) #29.53574
range(VKFL$WaterTemp_C) #23.88889 - 34.00000

#Kolmogorov-Smirnov test to examine differences in the distribution of observed temperature values at each site
#Asymptotic two-sample Kolmogorov-Smirnov test
ks.test(BNC$WaterTemp_C,VKFL$WaterTemp_C) 
#D = 0.53883, p-value < 2.2e-16

#Permanova on temperature distributions
perm.anova(WaterTemp_C ~ Location, data = yr2023_7mo, nperm = 999)
#          Sum Sq    Df Mean Sq F value Pr(>F)    
#Location   65379     1   65379  6426.7  0.001 ***
#  Residuals 102726 10098      10 

#Hartley's maximum F-ratio test to examine differences in the variance of temperature at each site
hartleyTest(WaterTemp_C ~ Location, data = yr2023_7mo)
# F Max = 4.244, df = 5133, k = 2, p-value < 2.2e-16

#Plotting time-series of temperature at each location
BNC_plot <- ggplot(data = BNC, aes(Date_Time, WaterTemp_C)) +
  geom_line(colour = 'blue') +
  ylim(0, 35) +
  geom_hline(yintercept = 24.44656, colour = "blue", linetype="dashed", linewidth = 1) + #add mean as a line
  xlab("Date") +
  theme_bw(base_size = 20) +
  scale_x_datetime(date_breaks = "1 month")
BNC_plot
ggsave("BNC_plot_2023_7mo.pdf", plot = BNC_plot, device = "pdf", width = 7, height = 7)

VKFL_plot <- ggplot(data = VKFL, aes(Date_Time, WaterTemp_C)) +
  geom_line(colour = 'red') +
  ylim(0, 35) +
  geom_hline(yintercept = 29.53574, colour = "red", linetype="dashed", linewidth = 1) + #add mean as a line
  xlab("Date") +
  theme_bw(base_size = 20) +
  scale_x_datetime(date_breaks = "1 month")
VKFL_plot
ggsave("VKFL_plot_2023_7mo.pdf", plot = VKFL_plot, device = "pdf", width = 7, height = 7)


#Plot overlaid densities of observed temperatures at each site
density_plot <- ggplot(yr2023_7mo, aes(x = WaterTemp_C, fill = Location)) +
  geom_density(alpha = 0.3) + 
  scale_fill_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  geom_vline(xintercept = mean(BNC$WaterTemp_C), colour = "lightblue", size = 1.3) +
  geom_vline(xintercept = mean(VKFL$WaterTemp_C), colour = "pink", linetype = "dashed",  size = 1.3) +
  xlim(0, 35) +
  theme_bw(base_size = 20)
density_plot


#Compute autocorrelation using hourly lags
BNC.auto <- acf(BNC$WaterTemp_C, plot = FALSE)
VKFL.auto <- acf(VKFL$WaterTemp_C, plot = FALSE)

BNC.auto <- as.data.frame(BNC.auto$acf[0:37])
BNC.auto$Lag <- seq(from = 1, to = 37, by = 1)
colnames(BNC.auto) <- c("Autocorr", "Lag")

VKFL.auto <- as.data.frame(VKFL.auto$acf[0:38])
VKFL.auto$Lag <- seq(from = 1, to = 38, by = 1)
colnames(VKFL.auto) <- c("Autocorr", "Lag")

BNC.auto$Location <- "BNC"
VKFL.auto$Location <- "VKFL"

all.auto <- rbind(BNC.auto, VKFL.auto)

#plotting autocorrelation with hourly lag
auto.hourly <- ggplot(all.auto, aes(x = Lag, y = Autocorr, shape = Location, colour = Location)) +
  geom_point(size = 3)+
  scale_shape_manual("Location", values = c("BNC" = 18, "VKFL" = 16)) +
  scale_colour_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  ylab("pH Autocorrelation") +
  xlab("Lag (hours)") +
  theme_bw(base_size = 20)
auto.hourly


#Compute autocorrelation with daily lags
#take average of every day's temperature
BNC_daily <- as.data.frame(sapply(split(BNC$WaterTemp_C, rep(1:(nrow(BNC)/24), each=24)), mean))
colnames(BNC_daily) <- c("WaterTemp_C")
VKFL_daily <- as.data.frame(sapply(split(VKFL$WaterTemp_C, rep(1:(nrow(VKFL)/24), each=24)), mean))
colnames(VKFL_daily) <- c("WaterTemp_C")

#re-run autocorrelation on average daily values
BNC.auto <- acf(BNC_daily$WaterTemp_C, plot = FALSE)
VKFL.auto <- acf(VKFL_daily$WaterTemp_C, plot = FALSE)


BNC.auto <- as.data.frame(BNC.auto$acf[0:24])
BNC.auto$Lag <- seq(from = 1, to = 24, by = 1)
colnames(BNC.auto) <- c("Autocorr", "Lag")
VKFL.auto <- as.data.frame(VKFL.auto$acf[0:24])
VKFL.auto$Lag <- seq(from = 1, to = 24, by = 1)
colnames(VKFL.auto) <- c("Autocorr", "Lag")

BNC.auto$Location <- "BNC"
VKFL.auto$Location <- "VKFL"

all.auto <- rbind(BNC.auto, VKFL.auto)

#plotting autocorrelation with daily lag
auto.daily <- ggplot(all.auto, aes(x = Lag, y = Autocorr, shape = Location, colour = Location)) +
  geom_point(size = 3)+
  scale_shape_manual("Location", values = c("BNC" = 18, "VKFL" = 16)) +
  scale_colour_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  ylab("pH Autocorrelation") +
  xlab("Lag (hours)") +
  theme_bw(base_size = 20)
auto.daily


#2023 4 warm months only------------------------
#Load data
yr2023_4mo <- read_csv("NOAA_temp_data_2023_4mo.csv")
# Remove rows where there is missing temperature data
yr2023_4mo <- yr2023_4mo[!(yr2023_4mo$WaterTemp_F == "-"), ]
#formatting date/time accordingly for analyses 
yr2023_4mo$Date_Time <- as.POSIXct(as.POSIXct(as.character(yr2023_4mo$Date_Time), tz="GMT", format = "%m/%d/%y %H:%M"))

#format location as a factor
yr2023_4mo$Location <- as.factor(yr2023_4mo$Location)
#format water temperature as numeric
yr2023_4mo$WaterTemp_F <- as.numeric(yr2023_4mo$WaterTemp_F)

#add column of water temp in Celsius
yr2023_4mo$WaterTemp_C <- (yr2023_4mo$WaterTemp_F - 32) / 1.8

#Subset data by location
BNC <- subset(yr2023_4mo, Location == "BNC")
VKFL <- subset(yr2023_4mo, Location == "VKFL")

#summary statistics at each site
mean(BNC$WaterTemp_C) #27.5164
range(BNC$WaterTemp_C) #20.72222 - 31.00000

mean(VKFL$WaterTemp_C) #30.86257
range(VKFL$WaterTemp_C) #28.22222 - 34.00000

#Kolmogorov-Smirnov test to examine differences in the distribution of observed temperature values at each site
#Asymptotic two-sample Kolmogorov-Smirnov test
ks.test(BNC$WaterTemp_C,VKFL$WaterTemp_C) 
#D = 0.69539, p-value < 2.2e-16

#Permanova on temperature distributions
perm.anova(WaterTemp_C ~ Location, data = yr2023_4mo, nperm = 999)
#          Sum Sq   Df Mean Sq F value Pr(>F)    
#Location   15897    1 15896.9  4815.5  0.001 ***
#  Residuals  18757 5682     3.3      

#Hartley's maximum F-ratio test to examine differences in the variance of temperature at each site
hartleyTest(WaterTemp_C ~ Location, data = yr2023_4mo)
# F Max = 3.9829, df = 2925, k = 2, p-value < 2.2e-16

#Plotting time-series of temperature at each location
BNC_plot <- ggplot(data = BNC, aes(Date_Time, WaterTemp_C)) +
  geom_line(colour = 'blue') +
  ylim(0, 35) +
  geom_hline(yintercept = 27.5164, colour = "blue", linetype="dashed", linewidth = 1) + #add mean as a line
  xlab("Date") +
  theme_bw(base_size = 20) +
  scale_x_datetime(date_breaks = "1 month")
BNC_plot
ggsave("BNC_plot_2023_4mo.pdf", plot = BNC_plot, device = "pdf", width = 7, height = 7)

VKFL_plot <- ggplot(data = VKFL, aes(Date_Time, WaterTemp_C)) +
  geom_line(colour = 'red') +
  ylim(0, 35) +
  geom_hline(yintercept = 30.86257, colour = "red", linetype="dashed", linewidth = 1) + #add mean as a line
  xlab("Date") +
  theme_bw(base_size = 20) +
  scale_x_datetime(date_breaks = "1 month")
VKFL_plot
ggsave("VKFL_plot_2023_4mo.pdf", plot = VKFL_plot, device = "pdf", width = 7, height = 7)

Both_plot_4mo <-ggplot(data = yr2023_4mo, aes(x = Date_Time, y = WaterTemp_C, color = Location)) +
  geom_line() +
  scale_color_manual(values = c("BNC" = "blue", "VKFL" = "red")) +
  ylim(0, 35) +
  geom_hline(yintercept = 27.5164, colour = "blue", linetype="dashed", linewidth = 1) + #add mean as a line
  geom_hline(yintercept = 30.86257, colour = "red", linetype="dashed", linewidth = 1) + #add mean as a line
  xlab("Year") +
  theme_bw(base_size = 20) +
  scale_x_datetime(date_breaks = "1 month")
Both_plot_4mo
ggsave("Both_plot_4mo.pdf", plot = Both_plot_4mo, device = "pdf", width = 7, height = 5)

#Plot overlaid densities of observed temperatures at each site
density_plot <- ggplot(yr2023_4mo, aes(x = WaterTemp_C, fill = Location)) +
  geom_density(alpha = 0.3) + 
  scale_fill_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  geom_vline(xintercept = mean(BNC$WaterTemp_C), colour = "lightblue", size = 1.3) +
  geom_vline(xintercept = mean(VKFL$WaterTemp_C), colour = "pink", linetype = "dashed",  size = 1.3) +
  xlim(0, 35) +
  theme_bw(base_size = 20)
density_plot


#Compute autocorrelation using hourly lags
BNC.auto <- acf(BNC$WaterTemp_C, plot = FALSE)
VKFL.auto <- acf(VKFL$WaterTemp_C, plot = FALSE)

BNC.auto <- as.data.frame(BNC.auto$acf[0:35])
BNC.auto$Lag <- seq(from = 1, to = 35, by = 1)
colnames(BNC.auto) <- c("Autocorr", "Lag")

VKFL.auto <- as.data.frame(VKFL.auto$acf[0:35])
VKFL.auto$Lag <- seq(from = 1, to = 35, by = 1)
colnames(VKFL.auto) <- c("Autocorr", "Lag")

BNC.auto$Location <- "BNC"
VKFL.auto$Location <- "VKFL"

all.auto <- rbind(BNC.auto, VKFL.auto)

#plotting autocorrelation with hourly lag
auto.hourly <- ggplot(all.auto, aes(x = Lag, y = Autocorr, shape = Location, colour = Location)) +
  geom_point(size = 3)+
  scale_shape_manual("Location", values = c("BNC" = 18, "VKFL" = 16)) +
  scale_colour_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  ylab("pH Autocorrelation") +
  xlab("Lag (hours)") +
  theme_bw(base_size = 20)
auto.hourly


#Compute autocorrelation with daily lags
#take average of every day's temperature
BNC_daily <- as.data.frame(sapply(split(BNC$WaterTemp_C, rep(1:(nrow(BNC)/24), each=24)), mean))
colnames(BNC_daily) <- c("WaterTemp_C")
VKFL_daily <- as.data.frame(sapply(split(VKFL$WaterTemp_C, rep(1:(nrow(VKFL)/24), each=24)), mean))
colnames(VKFL_daily) <- c("WaterTemp_C")

#re-run autocorrelation on average daily values
BNC.auto <- acf(BNC_daily$WaterTemp_C, plot = FALSE)
VKFL.auto <- acf(VKFL_daily$WaterTemp_C, plot = FALSE)


BNC.auto <- as.data.frame(BNC.auto$acf[0:24])
BNC.auto$Lag <- seq(from = 1, to = 24, by = 1)
colnames(BNC.auto) <- c("Autocorr", "Lag")
VKFL.auto <- as.data.frame(VKFL.auto$acf[0:24])
VKFL.auto$Lag <- seq(from = 1, to = 24, by = 1)
colnames(VKFL.auto) <- c("Autocorr", "Lag")

BNC.auto$Location <- "BNC"
VKFL.auto$Location <- "VKFL"

all.auto <- rbind(BNC.auto, VKFL.auto)

#plotting autocorrelation with daily lag
auto.daily <- ggplot(all.auto, aes(x = Lag, y = Autocorr, shape = Location, colour = Location)) +
  geom_point(size = 3)+
  scale_shape_manual("Location", values = c("BNC" = 18, "VKFL" = 16)) +
  scale_colour_manual("Location", values = c("BNC" = "blue", "VKFL" = "red")) +
  ylab("pH Autocorrelation") +
  xlab("Lag (hours)") +
  theme_bw(base_size = 20)
auto.daily
