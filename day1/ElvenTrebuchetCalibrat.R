library(stringr)

input <- read.csv2("day1/input.txt", header = FALSE, blank.lines.skip = TRUE)
input <- head(input$V1)
digits <- sapply(stringr::str_extract_all(input, "[0-9]"), as.numeric)
recoverCalibrationValue <- function(x) x[1] * 10 + x[length(x)]
calibrationValues <- sapply(digits, recoverCalibrationValue)

sumCalibrationValues <- sum(calibrationValues)
