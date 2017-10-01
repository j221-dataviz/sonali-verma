# load required packages
library(dplyr)
library(readr)

# read in clinicaltrials.gov data
chunks <- c(1:6)
cancer_trials <- data_frame()

for (ch in chunks) {
  print(ch)
  tmp <- read_csv(paste0("https://clinicaltrials.gov/ct2/results/download_fields?cond=cancer&down_flds=all&down_count=10000&down_fmt=csv&down_chunk=",ch))
  cancer_trials <- bind_rows(cancer_trials,tmp)
}

# export data
write_csv(cancer_trials, "data/clinicaltrials.csv", na="")

