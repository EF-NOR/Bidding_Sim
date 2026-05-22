# ---- Packages ----
# Install once if needed:
# install.packages(c("readxl","dplyr","ggplot2","lubridate","scales","tidyr","broom"))

library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)
library(broom)

# ---- 1) Load data ----
# Replace with your actual file path:
path <- "ProductionNO3_2017.xlsx"

df <- read_excel(path) %>%
  rename(
    date = Date,
    est  = Estimated_MWh,
    act  = Actual_MWh
  ) %>%
  mutate(date = ymd(date))   # converts "2019-01-15" safely to Date type

# ---- 2) Quick check ----
glimpse(df)
summary(df)

# Drop missing values if any
df <- df %>% filter(!is.na(est), !is.na(act))

# ---- 3) Compute error metrics ----
metrics <- df %>%
  summarise(
    n     = n(),
    ME    = mean(act - est),                            # bias
    MAE   = mean(abs(act - est)),
    RMSE  = sqrt(mean((act - est)^2)),
    MAPE  = mean(abs((act - est) / act), na.rm = TRUE) * 100,
    R2    = cor(act, est)^2
  )
print(metrics)

# ---- 4) Plot time series ----
df_long <- df %>%
  pivot_longer(cols = c(est, act), names_to = "series", values_to = "MWh")

ggplot(df_long, aes(x = date, y = MWh, color = series)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(title = "Wind Production: Estimated vs Actual",
       x = NULL, y = "MWh", color = NULL) +
  theme_minimal(base_size = 13)

# ---- 5) Scatter plot with 1:1 line ----
ggplot(df, aes(x = est, y = act)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "gray40") +
  labs(title = "Actual vs Estimated Wind Production",
       x = "Estimated (MWh)", y = "Actual (MWh)") +
  theme_minimal(base_size = 13)

# ---- 6) Residuals ----
df <- df %>% mutate(err = act - est)

ggplot(df, aes(x = date, y = err)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(alpha = 0.8) +
  labs(title = "Residuals (Actual − Estimated) over time",
       x = NULL, y = "Error (MWh)") +
  theme_minimal(base_size = 13)

# ---- 7) Monthly aggregation ----
df_month <- df %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    act = sum(act, na.rm = TRUE),
    est = sum(est, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(df_month, aes(x = month)) +
  geom_line(aes(y = act), linewidth = 0.8) +
  geom_line(aes(y = est), linewidth = 0.8, linetype = 2) +
  labs(title = "Monthly Wind Production: Estimated vs Actual",
       x = NULL, y = "MWh") +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  theme_minimal(base_size = 13)

# ---- 8) Optional: Calibration (to correct bias) ----
fit <- lm(act ~ est, data = df)
summary(fit)

df <- df %>%
  mutate(est_cal = predict(fit, newdata = df))

# Compare metrics before vs after calibration
metrics_compare <- df %>%
  summarise(
    RMSE_raw = sqrt(mean((act - est)^2)),
    RMSE_cal = sqrt(mean((act - est_cal)^2)),
    MAE_raw  = mean(abs(act - est)),
    MAE_cal  = mean(abs(act - est_cal))
  )
print(metrics_compare)

coef <- coef(fit)
lab  <- sprintf("act = %.2f + %.2f × est (R² = %.3f)",
                coef[1], coef[2], summary(fit)$r.squared)

# Place the label toward top-left of your data range
x_lab <- quantile(df$est, 0.05, na.rm = TRUE)
y_lab <- quantile(df$act, 0.95, na.rm = TRUE)

library(ggplot2)

ggplot(df, aes(x = est, y = act)) +
  geom_point(alpha = 0.55) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +           # 1:1 line
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +          # calibration fit
  annotate("label", x = x_lab, y = y_lab, label = lab, hjust = 0) +
  labs(title = "Calibration: Actual vs Estimated",
       x = "Estimated (MWh)", y = "Actual (MWh)") +
  theme_minimal(base_size = 13)

