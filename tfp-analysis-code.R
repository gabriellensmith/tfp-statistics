#loading libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(janitor)
library(plotly)
library(leaps)
library(MASS)
library(sjPlot)
library(rstatix)
library(ggpubr)
library(gplots)
library(multcomp)
library(sna)
library(kableExtra)
library(lessR)
library(tsibble)
library(feasts)
library(xts)
library(forecast)

#full data
tfp_all <- read_excel("~/EDS222/tfp-statistics/AgTFPInternational2020_long.xlsx", sheet = 2) |>
  clean_names()

#data dictionary 
tfp_dict <- read_excel("~/EDS222/tfp-statistics/AgTFPInternational2020_long.xlsx", sheet = 1, skip = 2)

#world tfp data
tfp_world <- tfp_all |> 
  filter(level == 'World')

#country tfp data
tfp_country <- tfp_all |> 
  filter(level == 'Country') |> 
  select(country_territory, income, year, tfp_index, c(13:16))

#country plot data 
tfp_country_p <- tfp_all |> 
  filter(level == 'Country',
         income %in% c('HI', 'MI-U', 'MI-L', 'LI')) |> 
  select(year, tfp_index, income) |> 
  group_by(income, year) |> 
  summarize_all(mean, na.rm = TRUE)

#country income factor levels
tfp_country_p$income <- factor(tfp_country_p$income, levels = c('HI', 'MI-U', 'MI-L', 'LI'))

#income class TFP plot
tfp_country_plot <- ggplot(data=tfp_country_p) +
  geom_line(aes(x=year, y=tfp_index, group=income, color=income), alpha = 0.5) + 
  theme_minimal() +
  labs(y = 'TFP Index', x = 'Year') +
  scale_x_discrete(breaks = scales::pretty_breaks(n=10)) +
  scale_color_discrete(name = 'Income Class') +
  theme(plot.title = element_text(hjust = 0.5))

#global TFP plot
tfp_world_plot <- ggplot(data = tfp_world) + 
  geom_line(aes(x=year, y=tfp_index, group = 1)) + 
  labs(y = 'TFP Index') +
  theme_minimal()  +
  scale_x_discrete(breaks = scales::pretty_breaks(n=10)) 

#income class + global TFP plot
subplot(tfp_world_plot, tfp_country_plot, nrows = 2, shareX = TRUE, shareY= TRUE) |> 
  layout(title = list(text = 'Fig 1. Total Factor Production Indices (1961-2020)', font = list(size = 12)), xaxis = list(x = 0.5,y = 0,text = "Income Class Grouped",xref = "paper",yref = "paper",xanchor = "center",yanchor = "bottom",showarrow = FALSE),
         annotations = list(list(x = 0.03,  
                                 y = 0.95,  
                                 text = "Global",  
                                 xref = "paper",  
                                 yref = "paper",  
                                 xanchor = "center",  
                                 yanchor = "bottom",  
                                 showarrow = FALSE) , list(x = 0.13,  
                                                           y = 0.4,  
                                                           text = "Income Class Grouped",  
                                                           xref = "paper",  
                                                           yref = "paper",  
                                                           xanchor = "center",  
                                                           yanchor = "bottom",  
                                                           showarrow = FALSE)))

#variables for regression model decision framework
y <- tfp_country$tfp_index
x1 <- tfp_country$land_index
x2 <- tfp_country$labor_index
x3 <- tfp_country$capital_index
x4 <- tfp_country$materials_index

#lower model
mod0 <- lm(y~1)
#full model
mod_upper <- lm(y~x1+x2+x3+x4)
#stepwise regression
step(mod0, scope=list(lower = mod0, upper=mod_upper))

#reduced model checking
mod <- regsubsets(cbind(x1,x2,x3),y)
summary_mod <- summary(mod)
summary_mod$which
summary_mod$rsq
summary_mod$adjr2

pairs(cbind(x1,x2,x3))

#reduced interaction full model
mod_upper1 <- lm(y~x1+x2+x3+x1*x2+x1*x3+x1*x3)
#stepwise regression
step(mod0, scope=list(lower = mod0, upper=mod_upper1))
#reduced interaction model
model1 <- lm(y~x1+x2+x3+x2:x1)
#model checking
plot(fitted(model1),residuals(model1))
abline(h=0)
qqnorm(residuals(model1))
qqline(residuals(model1))
hist(y)

#transforming model for normalcy
model2 <- lm(log(y) ~ x1+x2+x3+x2:x1)
#transformed model checking 
hist(log(y))
plot(fitted(model2),residuals(model2))
abline(h=0)
qqnorm(residuals(model2))
qqline(residuals(model2))

#final model summary 
summary <- summary(model2)

#summary table final model
tab_model(model2, 
          pred.labels = c('Intercept', 'Land',
                          'Labor', 'Capital', 'Land * Capital'),
          dv.labels = 'Log Total Factor Production',
          string.ci = '95% Conf. Interval',
          string.p = 'P-value',
          title = 'Tbl 1. Transformed Linear Model Results for TFP Regression',
          digits = 7)

#create income class factors 
tfp_income <- tfp_country |> 
  select(income, tfp_index) |>
  filter(income %in% c('LI','MI-U','HI','MI-L')) |> 
  mutate(income = as.factor(income))

#distribution of income class visualization
violin_plot <- ggviolin(tfp_income, x='income', y='tfp_index', fill = 'income', 
                        order = c('LI', 'MI-L', 'MI-U', 'HI'), 
                        ylab = 'TFP Index', xlab = 'Income Class',
                        draw_quantiles = 0.5, add = 'boxplot') 
ggpar(violin_plot, legend.title = 'Income Class', xlab = '',
      caption = 'Fig 2. Distribution of TFP Indices for Varying Income Classes',
      ggtheme = theme_minimal())

#kruskal test for difference in means
income_k <- kruskal_test(tfp_index ~ income, data = tfp_income)

#dunn test for difference in means 
income_pairs_d <- dunn_test(tfp_index ~ income, data = tfp_income, p.adjust.method = 'bonferroni')

#vizualize difference in means 
income_pairs_d <- income_pairs_d |> 
  add_xy_position(x = 'income') 
ggboxplot(tfp_income, x = 'income', y = 'tfp_index',
          order = c('LI', 'MI-L', 'MI-U', 'HI')) +
  stat_pvalue_manual(income_pairs_d, hide.ns = TRUE) +
  labs(title = get_test_label(income_k, detailed = TRUE),
       subtitle = get_pwc_label(income_pairs_d),
       caption = 'Fig 3. Mean difference testing \n for TFP in varying income classes',
       y = 'TFP Index', x = 'Income Level') +
  theme()

#add dates to world data
tfp_time <- tfp_world |> 
  mutate(date = paste0(year, '-01-01'),
         date = as.Date(date, format = '%Y-%m-%d')) |> 
  group_by(date) |> 
  summarize(tfp_index_mean = mean(tfp_index))

#convert data to time series 
ts <- xts(tfp_time$tfp_index_mean, tfp_time$date)

#fit ts to automated ARIMA model
fit <- auto.arima(ts)

#visualize global forecasts 
pred.tr <- predict(fit, n.ahead=10)
U.tr <- pred.tr$pred + 2*pred.tr$se
L.tr <- pred.tr$pred - 2*pred.tr$se
ts.plot(ts, xlim=c(0,length(ts)+12), ylim=c(min(ts),max(ts)+20))
title(main = 'Forecasted TFP Indices for Global Data', sub = 'Fig 4. Forecasts for ARIMA(2,2,1) Model with 95% Confidence Intervals', cex.sub = 0.65)
legend('topleft', inset = 0.02,
       legend = c('Forecast', '95% CI'),
       col = c('red', 'blue'),
       lty = c(1,2))
lines(U.tr, col='blue', lty='dashed')
lines(L.tr, col='blue', lty='dashed')
lines((length(ts)+1):(length(ts)+10), pred.tr$pred, col='red')

#income class ARIMA fitting
tfp_time_li <- tfp_country |> 
  filter(income == 'LI') |> 
  mutate(date = paste0(year, '-01-01'),
         date = as.Date(date, format = '%Y-%m-%d')) |> 
  group_by(date) |> 
  summarize(tfp_index_mean = mean(tfp_index))

ts_li <- xts(tfp_time_li$tfp_index_mean, tfp_time_li$date)
plot(ts_li)

fit_li <- auto.arima(ts_li)
checkresiduals(fit_li)
fit_forecast_li <- forecast(fit_li)
autoplot(fit_forecast_li)

tfp_time_mil <- tfp_country |> 
  filter(income == 'MI-L') |> 
  mutate(date = paste0(year, '-01-01'),
         date = as.Date(date, format = '%Y-%m-%d')) |> 
  group_by(date) |> 
  summarize(tfp_index_mean = mean(tfp_index))

ts_mil <- xts(tfp_time_mil$tfp_index_mean, tfp_time_mil$date)
plot(ts_mil)

fit_mil <- auto.arima(ts_mil)
checkresiduals(fit_mil)
fit_forecast_mil <- forecast(fit_mil)
autoplot(fit_forecast_mil)

tfp_time_mi <- tfp_country |> 
  filter(income == 'MI-U') |> 
  mutate(date = paste0(year, '-01-01'),
         date = as.Date(date, format = '%Y-%m-%d')) |> 
  group_by(date) |> 
  summarize(tfp_index_mean = mean(tfp_index))

ts_mi <- xts(tfp_time_mi$tfp_index_mean, tfp_time_mi$date)
plot(ts_mi)

fit_mi <- auto.arima(ts_mi)
checkresiduals(fit_mi)
fit_forecast_mi <- forecast(fit_mi)
autoplot(fit_forecast_mi)

tfp_time_hi <- tfp_country |> 
  filter(income == 'HI') |> 
  mutate(date = paste0(year, '-01-01'),
         date = as.Date(date, format = '%Y-%m-%d')) |> 
  group_by(date) |> 
  summarize(tfp_index_mean = mean(tfp_index))

ts_hi <- xts(tfp_time_hi$tfp_index_mean, tfp_time_hi$date)
plot(ts_hi)

fit_hi <- auto.arima(ts_hi)
checkresiduals(fit_hi)
fit_forecast_hi <- forecast(fit_hi)
autoplot(fit_forecast_hi)

# par(mfrow = c(2,2))
# plot(fit_forecast_li, main = 'Low Income Class')
# plot(fit_forecast_mil, main = 'Lower-Middle Income Class')
# plot(fit_forecast_mi, main = 'Upper-Middle Income Class')
# plot(fit_forecast_hi, main = 'High Income Class')

#income class forecasts 
pred.hi <- predict(fit_hi, n.ahead=10)
pred.li <- predict(fit_li, n.ahead=10)
pred.mi <- predict(fit_mi, n.ahead=10)
pred.mil <- predict(fit_mil, n.ahead=10)
year <- c(2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030)

#forecast data frame 
predictions <- data.frame(year, pred.li$pred, pred.mil$pred, pred.mi$pred, pred.hi$pred, pred.tr$pred) 
colnames(predictions) <- c('Year', 'Low Income', 'Lower-Middle Income', 'Upper-Middle Income', 'High Income', 'World')
predictions |> 
  kbl(caption = 'Tbl 2. Predicted TFP Indices') |> 
  kable_styling(bootstrap_options = 'striped', full_width = F)