library(fpp3)
library(forecast)
library(ggplot2)
library(tseries)
library(dplyr)
library(tsibble)

#print(head(aus_retail))

set.seed(12345)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

ggTsDisplayPlot <- gg_tsdisplay(myseries, Turnover)
# print(ggTsDisplayPlot)

lambdaTO <- myseries %>%
  features(Turnover, features = guerrero) %>%
  pull(lambda_guerrero)

# print("lambda Turnover ")
# print(lambdaTO)

result <- myseries %>%
  mutate(Turnover = box_cox(Turnover, lambdaTO)) %>%
  features(Turnover, unitroot_ndiffs)

# print(result)

# #9.6----------------------------------------------------------------
#ar1
y <- numeric(100)
e <- rnorm(100)

for (i in 2:100) {
  y[i] <- 0.6 * y[i - 1] + e[i]
}

sim <- tsibble(idx = seq_len(100), y = y, index = idx)
arPlot <- autoplot(sim) + ggtitle("Time plot with ϕ1 = 0.6")
# print(arPlot)

#----------------------------------------------------------------
#m1
y_MA <- numeric(100)
e_MA <- rnorm(100)

for (i in 2:100) {
  y_MA[i] <- e_MA[i] + 0.6 * e_MA[i - 1]
}
sim_MA <- tsibble(idx = seq_len(100), y = y_MA, index = idx)

maPlot <- autoplot(sim_MA) + ggtitle("MA Time plot with θ1 = 0.6")
# print(maPlot)

for (theta in c(-1, -0.5, 0, 0.5, 1)) {
  for (i in 2:100) {
    sim_MA$y[i] <- e_MA[i] + theta * e_MA[i - 1]
  }
  maThetaPlot <- autoplot(sim_MA) + ggtitle(paste0("Time plot with θ1 =", theta))
# print(maThetaPlot)
}

#arma 1,1
y_ARMA <- numeric(100)
e_ARMA <- rnorm(100)

for (i in 2:100) {
  y_ARMA[i] <- 0.6 * y_ARMA[i - 1] + 0.6 * e_ARMA[i - 1] + e_ARMA[i]
}
sim_ARMA <- tsibble(idx = seq_len(100), y = y_ARMA, index = idx)

armaPlot <- autoplot(sim_ARMA) + ggtitle("ARMA(1,1) model time plot with ϕ1=0.6, θ1 = 0.6 and σ2=1")
# print(armaPlot)

#ar2
y_AR2 <- numeric(100)
for (i in 3:100) {
  y_AR2[i] <- -0.8 * y_AR2[i - 1] + 0.3 * y_AR2[i - 2] + e_ARMA[i]
}
sim_AR2 <- tsibble(idx = seq_len(100), y = y_AR2, index = idx)

ar2Plot <- autoplot(sim_AR2) + ggtitle("AR(2) model time plot with ϕ1=-0.8, ϕ2=0.3 and σ2=1")
print(ar2Plot)