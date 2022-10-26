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

mahalanobis <- function(x, y) {
  # compute the Mahalanobis distance between two vectors
  # x and y are vectors of the same length
  # S is the covariance matrix
  S <- cov(x, y)
  # compute the inverse of the covariance matrix
  S_inv <- solve(S)
  # compute the Mahalanobis distance
  d <- sqrt(t(x - y) %*% S_inv %*% (x - y))
  return(d)
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

print(paste0("# of remaining rows : ", nrow(df_clean)))

df_mahalanobis <- df_clean %>%
  select(`CO(GT)`, `NMHC(GT)`, `NOx(GT)`, `NO2(GT)`, `C6H6(GT)`, `T`, `RH`, `AH`) %>%
  as.matrix()

# compute the Mahalanobis distance between each pair of observations
d <- dist(df_mahalanobis, method = mahalanobis)

# detect outliers
print(outliers <- which(d > 3))


# CORRELATION
# corr plot
png("report/figs/corr.png")
corrplot(cor(df_clean[, 1:13]), method = "circle")
dev.off()

# scatter matrix plot
png("report/figs/scatter_matrix.png")
plot(df_clean[, 1:13])
dev.off()

#Principal Component Analysis of df_clean and plot
pca <- prcomp(df_clean[, 1:10], scale = TRUE)
png("report/figs/pca.png")
plot(pca, type = "lines")
dev.off()

# t-sne analysis with different perplexities
png("report/figs/tsne.png")
par(mfrow = c(2, 2))
tsne_5 <- Rtsne(df_clean[, 1:10], perplexity = 5)
plot(tsne_5$Y, col = df_clean$"T", pch = 20, cex = 0.5, main = "Perplexity = 5")
tsne_10 <- Rtsne(df_clean[, 1:10], perplexity = 10)
plot(tsne_10$Y, col = df_clean$"T", pch = 20, cex = 0.5, main ="Perplexity = 10")
tsne_20 <- Rtsne(df_clean[, 1:10], perplexity = 20)
plot(tsne_20$Y, col = df_clean$"T", pch = 20, cex = 0.5, main = "Perplexity = 20")
tsne_30 <- Rtsne(df_clean[, 1:10], perplexity = 30)
plot(tsne_30$Y, col = df_clean$"T", pch = 20, cex = 0.5, main = "Perplexity = 30")
dev.off()