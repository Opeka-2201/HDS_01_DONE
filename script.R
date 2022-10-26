### LIBRARIES ###

library(readxl)
library(psych)
library(naniar)
library(survival)
library(packHV)
library(corrplot)
library(dplyr)
library(kableExtra)
library(Rtsne)
library(stats)

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
write.csv(df, "data_set.txt")

# MISSING VALUES
report_missing(df)

png("report/figs/missing_values.png")
print(vis_miss(df[1:13], sort_miss = TRUE))
dev.off()

png("report/figs/missing_values_heatmap.png")
print(gg_miss_upset(df[1:13]))
dev.off()

sink("report/questions/temp_table.tex")
print(describe(df) %>% kable(format = "latex", ))
sink()

# graphical summary
png("report/figs/summary_1.png")
par(mfrow = c(2, 3))
hist_boxplot(df$"CO(GT)", main = "CO(GT)", xlab="CO (mg/m3)")
hist_boxplot(df$"NMHC(GT)", main = "NMHC(GT)", xlab="NMHC (ug/m3)")
hist_boxplot(df$"NOx(GT)", main = "NOx(GT)", xlab="NOx response")
hist_boxplot(df$"PT08.S1(CO)", main = "CO Sensor", xlab="CO response")
hist_boxplot(df$"PT08.S2(NMHC)", main = "NMHC Sensor", xlab="NMHC response")
hist_boxplot(df$"PT08.S3(NOx)", main = "NOx Sensor")
dev.off()

png("report/figs/summary_2.png")
par(mfrow = c(2, 3))
hist_boxplot(df$"NO2(GT)", main = "NO2(GT)", xlab="NO2 (ug/m3)")
hist_boxplot(df$"C6H6(GT)", main = "C6H6(GT)", xlab="C6H6 (ug/m3)")
hist_boxplot(df$"AH", main = "Absolute Humidity", xlab="Humidity")
hist_boxplot(df$"PT08.S4(NO2)", main = "NO2 Sensor", xlab="NO2 response")
hist_boxplot(df$"PT08.S5(O3)", main = "O3 Sensor", xlab="O3 response")
hist_boxplot(df$"RH", main = "Relative Humidity", xlab="Humidity (%)")
dev.off()

png("report/figs/summary_3.png")
hist_boxplot(df$"T", main = "Temperature", xlab="Temperature (°C)")
dev.off()

# complete case strategy
df_clean <- df %>%
  filter(!is.na(`CO(GT)`)) %>%
  filter(!is.na(`NMHC(GT)`)) %>%
  filter(!is.na(`NOx(GT)`)) %>%
  filter(!is.na(`NO2(GT)`)) %>%
  filter(!is.na(`C6H6(GT)`)) %>%
  filter(!is.na(`PT08.S1(CO)`)) %>%
  filter(!is.na(`PT08.S2(NMHC)`)) %>%
  filter(!is.na(`PT08.S3(NOx)`)) %>%
  filter(!is.na(`PT08.S4(NO2)`)) %>%
  filter(!is.na(`PT08.S5(O3)`)) %>%
  filter(!is.na(`T`)) %>%
  filter(!is.na(`RH`)) %>%
  filter(!is.na(`AH`)) %>%
  filter(!is.na(`HIGH_CO`)) %>%
  filter(!is.na(`HIGH_NOx`)) %>%
  filter(!is.na(`HIGH_NMHC`)) %>%
  filter(!is.na(`HIGH_NO2`)) %>%
  filter(!is.na(`HIGH_C6H6`)) %>%
  filter(!is.na(`HIGH_T`)) %>%
  filter(!is.na(`HIGH_RH`)) %>%
  filter(!is.na(`HIGH_AH`))

print(paste0("# of remaining rows : ", nrow(df_clean)))

# outlier mahalanobis distance
png("report/figs/outliers.png")
outlier(df_clean[, 1:13], plot = TRUE, bad = 25)
dev.off()

# CORRELATION
# corr plot
png("report/figs/corr.png")
corrplot(cor(df_clean[, 1:13]), method = "circle")
dev.off()

# scatter matrix plot
png("report/figs/scatter_matrix.png")
plot(df_clean[, 1:13])
dev.off()

# scree plot of df_clean
png("report/figs/scree_plot.png")
screeplot(princomp(df_clean[1:13]), type = "lines")
dev.off()

#biplot
png("report/figs/biplot.png")
biplot(princomp(df_clean[1:12]), scale = 0)
dev.off()
