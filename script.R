### LIBRARIES ###

library(readxl)


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

# report missing values
report_missing(df)

# EXPLORATORY DATA ANALYSIS
# statistical summary
print(summary(df))

# graphical summary
par(mfrow = c(2, 2))
boxplot(df$"CO(GT)", main = "CO")
boxplot(df$"NOx(GT)", main = "NOx")
boxplot(df$"NO2(GT)", main = "NO2")
boxplot(df$"T", main = "Temperature")