library(stringr)

recoverCalibrationValue <- function(input) {
  f <- function(x) x[1] * 10 + x[length(x)]
  
  # assume input is a character vector
  stringr::str_extract_all(input, "[0-9]") %>% 
    sapply(., as.numeric) %>% 
    sapply (., f)
}



input <- read.csv2("day1/input.txt", header = FALSE, blank.lines.skip = TRUE) %>%
    .$V1 # %>% head
# 
# digits <- sapply(stringr::str_extract_all(input, "[0-9]"), as.numeric)
# calibrationValues <- sapply(digits, recoverCalibrationValue)
# 
# sumCalibrationValues <- sum(calibrationValues)

sumCalibrationValues <- sum(recoverCalibrationValue(input))