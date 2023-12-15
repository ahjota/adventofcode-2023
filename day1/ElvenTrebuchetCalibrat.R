library(stringr)

recoverCalibrationValue <- function(x) x[1] * 10 + x[length(x)]

input <- read.csv2("day1/input.txt", header = FALSE, blank.lines.skip = TRUE) %>%
    .$V1 # %>% head
# 
# digits <- sapply(stringr::str_extract_all(input, "[0-9]"), as.numeric)
# calibrationValues <- sapply(digits, recoverCalibrationValue)
# 
# sumCalibrationValues <- sum(calibrationValues)

sumCalibrationValues <- stringr::str_extract_all(input, "[0-9]") %>% 
  sapply(., as.numeric) %>% 
  sapply(., recoverCalibrationValue) %>% 
  sum