# Code for Bachelor's Thesis - Chang Liu - 14845636

rm(list = ls())

# Packages
library(tidyverse)
library(lubridate)
library(zoo)
library(readr)
library(lmtest)
library(sandwich)
library(car)
library(strucchange)
library(AER)



# --------------- Import data and convert to monthly frequency ----------------
# ECB
ecb_rate <- read_csv("ECB Main refinancing operations.csv")
names(ecb_rate)[3] <- "InterestRate"
ecb_rate$DATE <- as.Date(ecb_rate$DATE)
ecb_rate$Month <- floor_date(ecb_rate$DATE, "month")
ecb_rate_monthly <- aggregate(InterestRate ~ Month,
       data = ecb_rate, FUN = mean, na.rm = TRUE)
ecb_rate_monthly <- subset(ecb_rate_monthly, Month >= as.Date("2000-01-01")
                           & Month <= as.Date("2026-02-01"))
# Lagged interest rate
ecb_rate_monthly$i_lag <- c(NA, ecb_rate_monthly$InterestRate[1:(nrow(ecb_rate_monthly)-1)])
ecb_rate_monthly$i_lag2 <- c(NA, NA, ecb_rate_monthly$InterestRate[1:(nrow(ecb_rate_monthly)-2)])
ecb_rate_monthly$i_lag3 <- c(NA, NA, NA, ecb_rate_monthly$InterestRate[1:(nrow(ecb_rate_monthly)-3)])
ecb_rate_monthly <- subset(ecb_rate_monthly, Month >= as.Date("2000-04-01")
                           & Month <= as.Date("2026-02-01"))


ecb_inflation <- read_csv("ECB HICP Inflation Rate.csv")
names(ecb_inflation)[3] <- "inflation"
ecb_inflation$DATE <- as.Date(ecb_inflation$DATE)
ecb_inflation$Month <- as.Date(paste0(format(ecb_inflation$DATE, "%Y-%m"), "-01"))
ecb_inflation <- ecb_inflation[, c("Month", "inflation")]
ecb_inflation <- subset(ecb_inflation,Month >= as.Date("2000-01-01") 
                        & Month <= as.Date("2026-02-01"))
ecb_inflation$pi_lag2 <- c(NA, NA, ecb_inflation$inflation[1:(nrow(ecb_inflation)-2)])
ecb_inflation$pi_lag3 <- c(NA, NA, NA, ecb_inflation$inflation[1:(nrow(ecb_inflation)-3)])
ecb_inflation <- subset(ecb_inflation,Month >= as.Date("2000-04-01") 
                        & Month <= as.Date("2026-02-01"))

ecb_unemployment <- read_csv("ECB unemployment.csv")
names(ecb_unemployment)[3] <- "unemployment"
ecb_unemployment$DATE <- as.Date(ecb_unemployment$DATE)
ecb_unemployment$Month <- as.Date(paste0(format(ecb_unemployment$DATE, "%Y-%m"), "-01"))
ecb_unemployment <- ecb_unemployment[, c("Month", "unemployment")]
ecb_unemployment <- subset(ecb_unemployment, Month >= as.Date("2000-01-01") 
                           & Month <= as.Date("2026-02-01"))
ecb_unemployment$u_lag2 <- c(NA, NA, ecb_unemployment$unemployment[1:(nrow(ecb_unemployment)-2)])
ecb_unemployment$u_lag3 <- c(NA, NA, NA, ecb_unemployment$unemployment[1:(nrow(ecb_unemployment)-3)])
ecb_unemployment <- subset(ecb_unemployment,Month >= as.Date("2000-04-01")
                           &Month <= as.Date("2026-02-01"))

ecb_data <- merge(ecb_rate_monthly, ecb_inflation, by = "Month")
ecb_data <- merge(ecb_data, ecb_unemployment, by = "Month")

# Fed
fed_rate <- read_csv("Fed Federal Funds Effective Rate.csv")
names(fed_rate)[1] <- "DATE"
names(fed_rate)[2] <- "InterestRate"
fed_rate$DATE <- as.Date(fed_rate$DATE)
fed_rate$Month <- as.Date(paste0(format(fed_rate$DATE, "%Y-%m"), "-01"))
fed_rate <- fed_rate[, c("Month", "InterestRate")]
fed_rate <- subset(fed_rate, Month >= as.Date("1969-11-01") 
                   & Month <= as.Date("2026-02-01"))
fed_rate$i_lag <- c(NA, fed_rate$InterestRate[1:(nrow(fed_rate)-1)])
fed_rate$i_lag2 <- c(NA, NA, fed_rate$InterestRate[1:(nrow(fed_rate)-2)])
fed_rate$i_lag3 <- c(NA, NA, NA, fed_rate$InterestRate[1:(nrow(fed_rate)-3)])
fed_rate <- subset(fed_rate,Month >= as.Date("1970-02-01")
                   & Month <= as.Date("2026-02-01"))

fed_inflation <- read_csv("Fed CPI.csv")
fed_inflation$observation_date <- as.Date(fed_inflation$observation_date)
fed_inflation$Month <- as.Date(paste0(format(fed_inflation$observation_date, "%Y-%m"), "-01"))
# Linear interpolation for missing value in 2025-Oct
fed_inflation$CPIAUCSL <- na.approx(fed_inflation$CPIAUCSL,na.rm = FALSE)
# Calculate the annual inflation using log difference of CPI
fed_inflation$inflation <- c(rep(NA, 12), 
    100 * (log(fed_inflation$CPIAUCSL[13:nrow(fed_inflation)]) -
    log(fed_inflation$CPIAUCSL[1:(nrow(fed_inflation)-12)])))
fed_inflation <- fed_inflation[, c("Month", "inflation")]
fed_inflation <- subset(fed_inflation, Month >= as.Date("1969-11-01") 
                        & Month <= as.Date("2026-02-01"))
fed_inflation$pi_lag2 <- c(NA, NA, fed_inflation$inflation[1:(nrow(fed_inflation)-2)])
fed_inflation$pi_lag3 <- c(NA, NA, NA, fed_inflation$inflation[1:(nrow(fed_inflation)-3)])
fed_inflation <- subset(fed_inflation, Month >= as.Date("1970-02-01") 
                        & Month <= as.Date("2026-02-01"))

fed_unemployment <- read_csv("Fed unemployment.csv")
fed_unemployment$observation_date <- as.Date(fed_unemployment$observation_date)
fed_unemployment$Month <- as.Date(paste0(format(fed_unemployment$observation_date, "%Y-%m"), "-01"))
names(fed_unemployment)[2] <- "unemployment"
# Linear Interpolation
fed_unemployment$unemployment <- na.approx(fed_unemployment$unemployment,na.rm = FALSE)
fed_unemployment <- fed_unemployment[, c("Month", "unemployment")]
fed_unemployment <- subset(fed_unemployment, Month >= as.Date("1969-11-01") 
                           & Month <= as.Date("2026-02-01"))
fed_unemployment$u_lag2 <- c(NA, NA, fed_unemployment$unemployment[1:(nrow(fed_unemployment)-2)])
fed_unemployment$u_lag3 <- c(NA, NA, NA, fed_unemployment$unemployment[1:(nrow(fed_unemployment)-3)])
fed_unemployment <- subset(fed_unemployment, Month >= as.Date("1970-02-01") 
                           & Month <= as.Date("2026-02-01"))

fed_data <- merge(fed_rate, fed_inflation, by = "Month")
fed_data <- merge(fed_data, fed_unemployment, by = "Month")


#-------------------------------- Leadership Regime Model --------------------------------
# President Dummies
# ECB
ecb_data$Duisenberg <- ifelse(ecb_data$Month < as.Date("2003-11-01"), 1,0)
ecb_data$Trichet <- ifelse(ecb_data$Month >= as.Date("2003-11-01") 
                           & ecb_data$Month < as.Date("2011-11-01"),1,0)
ecb_data$Draghi <- ifelse(ecb_data$Month >= as.Date("2011-11-01") 
                          & ecb_data$Month < as.Date("2019-11-01"),1,0)
ecb_data$Lagarde <- ifelse(ecb_data$Month >= as.Date("2019-11-01"),1,0)

ecb_data$i_Trichet <- ecb_data$i_lag * ecb_data$Trichet
ecb_data$i_Draghi <- ecb_data$i_lag * ecb_data$Draghi
ecb_data$i_Lagarde <- ecb_data$i_lag * ecb_data$Lagarde

# Fed
fed_data$Burns <- ifelse(fed_data$Month < as.Date("1978-03-01"),1,0)
fed_data$Miller <- ifelse(fed_data$Month >= as.Date("1978-03-01") 
                          & fed_data$Month < as.Date("1979-08-01"),1,0)
fed_data$Volcker <- ifelse(fed_data$Month >= as.Date("1979-08-01") 
                           & fed_data$Month < as.Date("1987-08-01"),1,0)
fed_data$Greenspan <- ifelse(fed_data$Month >= as.Date("1987-08-01") 
                             & fed_data$Month < as.Date("2006-02-01"),1,0)
fed_data$Bernanke <- ifelse(fed_data$Month >= as.Date("2006-02-01") 
                            & fed_data$Month < as.Date("2014-02-01"),1,0)
fed_data$Yellen <- ifelse(fed_data$Month >= as.Date("2014-02-01") 
                          & fed_data$Month < as.Date("2018-02-01"),1,0)
fed_data$Powell <- ifelse(fed_data$Month >= as.Date("2018-02-01"),1,0)

fed_data$i_Miller <- fed_data$i_lag * fed_data$Miller
fed_data$i_Volcker <- fed_data$i_lag * fed_data$Volcker
fed_data$i_Greenspan <- fed_data$i_lag * fed_data$Greenspan
fed_data$i_Bernanke <- fed_data$i_lag * fed_data$Bernanke
fed_data$i_Yellen <- fed_data$i_lag * fed_data$Yellen
fed_data$i_Powell <- fed_data$i_lag * fed_data$Powell

# ECB interaction terms
ecb_data$i_Trichet <- ecb_data$i_lag * ecb_data$Trichet
ecb_data$i_Draghi  <- ecb_data$i_lag * ecb_data$Draghi
ecb_data$i_Lagarde <- ecb_data$i_lag * ecb_data$Lagarde

# Instruments interacted with leadership dummies
ecb_data$z_Trichet_i2 <- ecb_data$i_lag2 * ecb_data$Trichet
ecb_data$z_Trichet_i3 <- ecb_data$i_lag3 * ecb_data$Trichet
ecb_data$z_Draghi_i2  <- ecb_data$i_lag2 * ecb_data$Draghi
ecb_data$z_Draghi_i3  <- ecb_data$i_lag3 * ecb_data$Draghi
ecb_data$z_Lagarde_i2 <- ecb_data$i_lag2 * ecb_data$Lagarde
ecb_data$z_Lagarde_i3 <- ecb_data$i_lag3 * ecb_data$Lagarde

ecb_iv_model <- ivreg(
  inflation ~ i_lag + i_Trichet + i_Draghi + i_Lagarde + unemployment |
    pi_lag2 + pi_lag3 + u_lag2 + u_lag3 + i_lag2 + i_lag3 +
    z_Trichet_i2 + z_Trichet_i3 +
    z_Draghi_i2 + z_Draghi_i3 +
    z_Lagarde_i2 + z_Lagarde_i3,
  data = ecb_data
)

summary(ecb_iv_model, diagnostics = TRUE)

ecb_iv_hac <- coeftest(
  ecb_iv_model,
  vcov. = kernHAC(ecb_iv_model, kernel = "Quadratic Spectral")
)

ecb_iv_ftest <- linearHypothesis(
  ecb_iv_model,
  c("i_Trichet = 0", "i_Draghi = 0", "i_Lagarde = 0"),
  vcov. = kernHAC(ecb_iv_model, kernel = "Quadratic Spectral")
)

ecb_iv_hac
ecb_iv_ftest

#robustness check
ecb_restricted_iv <- ivreg(
  inflation ~ i_lag + unemployment |
    pi_lag2 + pi_lag3 + u_lag2 + u_lag3 + i_lag2 + i_lag3,
  data = ecb_data
)

coeftest(
  ecb_restricted_iv,
  vcov. = kernHAC(ecb_restricted_iv, kernel = "Quadratic Spectral")
)



# Fed interaction terms
fed_data$i_Miller    <- fed_data$i_lag * fed_data$Miller
fed_data$i_Volcker   <- fed_data$i_lag * fed_data$Volcker
fed_data$i_Greenspan <- fed_data$i_lag * fed_data$Greenspan
fed_data$i_Bernanke  <- fed_data$i_lag * fed_data$Bernanke
fed_data$i_Yellen    <- fed_data$i_lag * fed_data$Yellen
fed_data$i_Powell    <- fed_data$i_lag * fed_data$Powell

# Instruments interacted with leadership dummies
for (chair in c("Miller", "Volcker", "Greenspan", "Bernanke", "Yellen", "Powell")) {
  fed_data[[paste0("z_", chair, "_i2")]] <- fed_data$i_lag2 * fed_data[[chair]]
  fed_data[[paste0("z_", chair, "_i3")]] <- fed_data$i_lag3 * fed_data[[chair]]
}

fed_iv_model <- ivreg(
  inflation ~ i_lag + i_Miller + i_Volcker + i_Greenspan +
    i_Bernanke + i_Yellen + i_Powell + unemployment |
    pi_lag2 + pi_lag3 + u_lag2 + u_lag3 + i_lag2 + i_lag3 +
    z_Miller_i2 + z_Miller_i3 +
    z_Volcker_i2 + z_Volcker_i3 +
    z_Greenspan_i2 + z_Greenspan_i3 +
    z_Bernanke_i2 + z_Bernanke_i3 +
    z_Yellen_i2 + z_Yellen_i3 +
    z_Powell_i2 + z_Powell_i3,
  data = fed_data
)

summary(fed_iv_model, diagnostics = TRUE)

fed_iv_hac <- coeftest(
  fed_iv_model,
  vcov. = kernHAC(fed_iv_model, kernel = "Quadratic Spectral")
)

fed_iv_ftest <- linearHypothesis(
  fed_iv_model,
  c("i_Miller = 0", "i_Volcker = 0", "i_Greenspan = 0",
    "i_Bernanke = 0", "i_Yellen = 0", "i_Powell = 0"),
  vcov. = kernHAC(fed_iv_model, kernel = "Quadratic Spectral")
)

fed_iv_hac
fed_iv_ftest

# ------------------------- Structural Break Analysis -------------------------
# ======= First Stage Regression =======
# IV Z_t = (1, pi_{t-2}, pi_{t-3}, u_{t-2}, u_{t-3}, i_{t-2}, i_{t-3})
# Bai-Perron to find breaks in first stage
ecb_bp_first_i <- breakpoints(i_lag ~ pi_lag2 + pi_lag3 + 
                                u_lag2 + u_lag3 + i_lag2 + i_lag3,
                              data = ecb_data, h = 0.1)

fed_bp_first_i <- breakpoints(i_lag ~ pi_lag2 + pi_lag3 + 
                                u_lag2 + u_lag3 + i_lag2 + i_lag3,
                              data = fed_data, h = 0.1)

ecb_bp_first_u <- breakpoints(unemployment ~ pi_lag2 + pi_lag3 + 
                                u_lag2 + u_lag3 + i_lag2 + i_lag3,
                              data = ecb_data, h = 0.1)

fed_bp_first_u <- breakpoints(unemployment ~ pi_lag2 + pi_lag3 + 
                                u_lag2 + u_lag3 + i_lag2 + i_lag3,
                              data = fed_data, h = 0.1)

summary(ecb_bp_first_i)
summary(fed_bp_first_i)
summary(ecb_bp_first_u)
summary(fed_bp_first_u)
ecb_break_dates1_i <- ecb_data$Month[c(33,103,269)]
fed_break_dates1_i <- fed_data$Month[c(129,209)]
ecb_break_dates1_i # [1] "2002-12-01" "2008-10-01" "2022-08-01"
fed_break_dates1_i # [1] "1980-10-01" "1987-06-01"
ecb_break_dates1_u <- ecb_data$Month[c(67,101,134,165,240,271)]
fed_break_dates1_u <- fed_data$Month[602]
ecb_break_dates1_u # [1] "2005-10-01" "2008-08-01" "2011-05-01" "2013-12-01" "2020-03-01" "2022-10-01"
fed_break_dates1_u # [1] "2020-03-01"


# Fitted value for each regime (i_{t-1} and u_t)
ecb_data$regime_i <- 1
ecb_data$regime_i[ecb_data$Month > as.Date("2002-12-01")] <- 2
ecb_data$regime_i[ecb_data$Month > as.Date("2008-10-01")] <- 3
ecb_data$regime_i[ecb_data$Month > as.Date("2022-08-01")] <- 4
fed_data$regime_i <- 1
fed_data$regime_i[fed_data$Month > as.Date("1980-10-01")] <- 2
fed_data$regime_i[fed_data$Month > as.Date("1987-06-01")] <- 3

ecb_data$regime_u <- 1
ecb_data$regime_u[ecb_data$Month > as.Date("2005-10-01")] <- 2
ecb_data$regime_u[ecb_data$Month > as.Date("2008-08-01")] <- 3
ecb_data$regime_u[ecb_data$Month > as.Date("2011-05-01")] <- 4
ecb_data$regime_u[ecb_data$Month > as.Date("2013-12-01")] <- 5
ecb_data$regime_u[ecb_data$Month > as.Date("2020-03-01")] <- 6
ecb_data$regime_u[ecb_data$Month > as.Date("2022-10-01")] <- 7
fed_data$regime_u <- 1
fed_data$regime_u[fed_data$Month > as.Date("2020-03-01")] <- 2


ecb_data$i_lag_hat <- NA
for(r in unique(ecb_data$regime_i)) {
  temp_model <- lm(i_lag ~ pi_lag2 + pi_lag3 + u_lag2 + u_lag3 + i_lag2 + i_lag3,
                   data = subset(ecb_data, regime_i == r))
  ecb_data$i_lag_hat[ecb_data$regime_i == r] <- fitted(temp_model)
}

fed_data$i_lag_hat <- NA
for(r in unique(fed_data$regime_i)) {
  temp_model <- lm(i_lag ~ pi_lag2 + pi_lag3 + u_lag2 + u_lag3 + i_lag2 + i_lag3,
                   data = subset(fed_data, regime_i == r))
  fed_data$i_lag_hat[fed_data$regime_i == r] <- fitted(temp_model)
}

ecb_data$unemployment_hat <- NA
for(r in unique(ecb_data$regime_u)) {
  temp_model <- lm(unemployment ~ pi_lag2 + pi_lag3 + u_lag2 + u_lag3 + i_lag2 + i_lag3,
                   data = subset(ecb_data, regime_u == r))
  ecb_data$unemployment_hat[ecb_data$regime_u == r] <- fitted(temp_model)
}

fed_data$unemployment_hat <- NA
for(r in unique(fed_data$regime_u)) {
  temp_model <- lm(unemployment ~ pi_lag2 + pi_lag3 + u_lag2 + u_lag3 + i_lag2 + i_lag3,
                   data = subset(fed_data, regime_u == r))
  fed_data$unemployment_hat[fed_data$regime_u == r] <- fitted(temp_model)
}


# ======= Second Stage Regression =======
# Bai-Perron to find breaks in second stage
ecb_bp_second <- breakpoints(inflation ~ i_lag_hat + unemployment_hat,
                             data = ecb_data, h = 0.1)

fed_bp_second <- breakpoints(inflation ~ i_lag_hat + unemployment_hat,
                             data = fed_data, h = 0.1)

summary(ecb_bp_second)
summary(fed_bp_second)

ecb_break_dates2 <- ecb_data$Month[c(88,119,200,242,273)]
fed_break_dates2 <- fed_data$Month[c(130,205,359,477,605)]
ecb_break_dates2 #[1] "2007-07-01" "2010-02-01" "2016-11-01" "2020-05-01" "2022-12-01"
fed_break_dates2 #[1] "1980-11-01" "1987-02-01" "1999-12-01" "2009-10-01" "2020-06-01"

# ECB second-stage regimes
ecb_data$regime_second <- 1
ecb_data$regime_second[ecb_data$Month > as.Date("2007-07-01")] <- 2
ecb_data$regime_second[ecb_data$Month > as.Date("2010-02-01")] <- 3
ecb_data$regime_second[ecb_data$Month > as.Date("2016-11-01")] <- 4
ecb_data$regime_second[ecb_data$Month > as.Date("2020-05-01")] <- 5
ecb_data$regime_second[ecb_data$Month > as.Date("2022-12-01")] <- 6

# Fed second-stage regimes
fed_data$regime_second <- 1
fed_data$regime_second[fed_data$Month > as.Date("1980-11-01")] <- 2
fed_data$regime_second[fed_data$Month > as.Date("1987-02-01")] <- 3
fed_data$regime_second[fed_data$Month > as.Date("1999-12-01")] <- 4
fed_data$regime_second[fed_data$Month > as.Date("2009-10-01")] <- 5
fed_data$regime_second[fed_data$Month > as.Date("2020-06-01")] <- 6

# Estimate ECB coefficients by regime
ecb_regime_results <- data.frame()

for (r in sort(unique(ecb_data$regime_second))) {
  temp_data <- subset(ecb_data, regime_second == r)
  temp_model <- lm(inflation ~ i_lag_hat + unemployment_hat, data = temp_data)
  
  temp_coef <- coef(summary(temp_model))
  
  ecb_regime_results <- rbind(
    ecb_regime_results,
    data.frame(
      Regime = r,
      Start = min(temp_data$Month),
      End = max(temp_data$Month),
      Interest_Coefficient = temp_coef["i_lag_hat", "Estimate"],
      Interest_SE = temp_coef["i_lag_hat", "Std. Error"],
      Interest_p = temp_coef["i_lag_hat", "Pr(>|t|)"],
      Unemployment_Coefficient = temp_coef["unemployment_hat", "Estimate"],
      Unemployment_SE = temp_coef["unemployment_hat", "Std. Error"],
      Unemployment_p = temp_coef["unemployment_hat", "Pr(>|t|)"]
    )
  )
}

# Estimate Fed coefficients by regime
fed_regime_results <- data.frame()

for (r in sort(unique(fed_data$regime_second))) {
  temp_data <- subset(fed_data, regime_second == r)
  temp_model <- lm(inflation ~ i_lag_hat + unemployment_hat, data = temp_data)
  
  temp_coef <- coef(summary(temp_model))
  
  fed_regime_results <- rbind(
    fed_regime_results,
    data.frame(
      Regime = r,
      Start = min(temp_data$Month),
      End = max(temp_data$Month),
      Interest_Coefficient = temp_coef["i_lag_hat", "Estimate"],
      Interest_SE = temp_coef["i_lag_hat", "Std. Error"],
      Interest_p = temp_coef["i_lag_hat", "Pr(>|t|)"],
      Unemployment_Coefficient = temp_coef["unemployment_hat", "Estimate"],
      Unemployment_SE = temp_coef["unemployment_hat", "Std. Error"],
      Unemployment_p = temp_coef["unemployment_hat", "Pr(>|t|)"]
    )
  )
}

ecb_regime_results
fed_regime_results

#Save data for Sequential test in STATA
write.csv(ecb_data, "ecb_data.csv", row.names = FALSE)
write.csv(fed_data, "fed_data.csv", row.names = FALSE)
