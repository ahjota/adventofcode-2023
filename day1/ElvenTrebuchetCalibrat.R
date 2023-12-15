library(stringr)

digitSpellings <- c(
  "one" = "1",
  "two" = "2",
  "three" = "3",
  "four" = "4",
  "five" = "5",
  "six" = "6",
  "seven" = "7",
  "eight" = "8",
  "nine" = "9"
)

extractCalibrationValueCandidates <- function(input) {
  candidates <- stringr::str_extract_all(input, "[0-9]|one|two|three|four|five|six|seven|eight|nine")
  sapply(candidates, function(x) stringr::str_replace_all(x, digitSpellings))
}

recoverCalibrationValue <- function(input) {
  f <- function(x) x[1] * 10 + x[length(x)]
  
  # assume input is a character vector
  input %>%
    extractCalibrationValueCandidates %>% 
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

sumCalibrationValues <- input %>%
  recoverCalibrationValue %>% 
  sum