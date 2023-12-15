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
  candidate_matches_raw <- stringr::str_match_all(input,
                                                  # this naive pattern doesn't work for the edge case
                                                  # oneight > "one" "eight"
                                                  # "[0-9]|one|two|three|four|five|six|seven|eight|nine"
                                                  # so let's positive lookahead to make sure that we capture
                                                  # every possible match
                                                  "(?=([0-9]|one|two|three|four|five|six|seven|eight|nine))"
                                                  )
  # this is a matrix where the second column has the capture group,
  # so let's clean this up a bit
  candidate_matches <- sapply(candidate_matches_raw, function(x) x[,2])
  # and then go through and replace text-based digits with numeral-based digits
  sapply(candidate_matches, function(x) stringr::str_replace_all(x, digitSpellings))
}

recoverCalibrationValue <- function(input) {
  # is this faster than paste0 %>% as.numeric?
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