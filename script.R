### LIBRARIES ###

library(readxl)
library(psych)
library(naniar)
library(survival)
library(packHV)
library(corrplot)
library(dplyr)
library(kableExtra)

# change working directory
setwd("~/Documents/ULiège/Master/Bloc 1/Q1/High Dimensional Statistics/Projects/P1/") # nolint

### SCRIPT ###

# report missing values
report_missing <- function(df) {
  # get number of missing values
  n_missing <- sum(is.na(df))
  # get number of rows
  n_rows <- nrow(df)
  # get number of columns
  n_cols <- ncol(df)
  # get percentage of missing values
  pct_missing <- round(n_missing / (n_rows *  n_cols) * 100, 2)
  # print results
  print(paste0("Missing values: ", n_missing))
  print(paste0("Total rows: ", n_rows))
  print(paste0("Percentage missing: ", pct_missing, "%"))
}

# read in data
df <- read_excel("AirQualityUCI/data_set.xlsx")

# MISSING VALUES
report_missing(df)

png("report/figs/missing_values.png")
print(vis_miss(df[1:13], sort_miss = TRUE))
dev.off()

png("report/figs/missing_values_heatmap.png")
print(gg_miss_upset(df[1:13]))
dev.off()

sink("report/questions/table2.tex")
print(describe(df[, 19:21]) %>% kable(format = "latex", ))
sink()

# graphical summary
png("report/figs/summary_1.png")
par(mfrow = c(2, 3))
hist_boxplot(df$"CO(GT)", main = "CO(GT)")
hist_boxplot(df$"NMHC(GT)", main = "NMHC(GT)")
hist_boxplot(df$"NOx(GT)", main = "NOx(GT)")
hist_boxplot(df$"PT08.S1(CO)", main = "CO Sensor")
hist_boxplot(df$"PT08.S2(NMHC)", main = "NMHC Sensor")
hist_boxplot(df$"PT08.S3(NOx)", main = "NOx Sensor")
dev.off()

png("report/figs/summary_2.png")
par(mfrow = c(2, 3))
hist_boxplot(df$"NO2(GT)", main = "NO2(GT)")
hist_boxplot(df$"C6H6(GT)", main = "C6H6(GT)")
hist_boxplot(df$"AH", main = "Absolute Humidity")
hist_boxplot(df$"PT08.S4(NO2)", main = "NO2 Sensor")
hist_boxplot(df$"PT08.S5(O3)", main = "O3 Sensor")
hist_boxplot(df$"RH", main = "Relative Humidity")
dev.off()

png("report/figs/summary_3.png")
hist_boxplot(df$"T", main = "Temperature")
dev.off()

# complete case strategy
df_clean <- df %>%
  filter(!is.na(`CO(GT)`)) %>%
  filter(!is.na(`NMHC(GT)`)) %>%
  filter(!is.na(`NOx(GT)`)) %>%
  filter(!is.na(`NO2(GT)`)) %>%
  filter(!is.na(`C6H6(GT)`)) %>%
  filter(!is.na(`T`)) %>%
  filter(!is.na(`RH`)) %>%
  filter(!is.na(`AH`))

print(nrow(df_clean))

# CORRELATION
# corr plot
png("report/figs/corr.png")
corrplot(cor(df_clean[, 1:13]), method = "circle")
dev.off()

# scatter matrix plot
png("report/figs/scatter_matrix.png")
plot(df_clean[, 1:13])
dev.off()

write.csv(df, "data_set.txt")

#Principal Component Analysis of dt_clean
pca <- prcomp(df_clean[, 1:13], scale = TRUE)
summary(pca)

# scree plot
png("report/figs/scree_plot.png")

plot(pca, type = "l")
dev.off()

