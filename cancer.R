# load required packages
library(dplyr)
library(readr)
library(stringr)

# read in clinicaltrials.gov data
chunks <- c(1:6)
cancer_trials <- data_frame()

for (ch in chunks) {
  print(ch)
  tmp <- read_csv(paste0("https://clinicaltrials.gov/ct2/results/download_fields?cond=cancer&down_flds=all&down_count=10000&down_fmt=csv&down_chunk=",ch))
  cancer_trials <- bind_rows(cancer_trials,tmp)
}

lung_trials <- read_csv("https://clinicaltrials.gov/ct2/results/download_fields?cond=lung+cancer&down_flds=all&down_fmt=csv&down_count=10000")


# replace "null" in all columns with NA
cancer_trials[cancer_trials == "null"] <- NA

# see what labels we have for conditions, and how many of each
cancer_trials_conditions <- cancer_trials %>%
  group_by(Conditions) %>%
  summarize(number = n()) %>%
  arrange(desc(number))

# some cleaning to group trials for the largest cancer types (not in combination with one of the others)
# let's talk about this - how to write depends on what you want to include/exclude. For example are you interesting in cancer screening trials?
breast_cancer_trials <- cancer_trials %>%
  filter(grepl("Breast", Conditions) & !grepl("Prostate", Conditions) & !grepl("Lung", Conditions) & !grepl("Colorectal", Conditions) & !grepl("Myeloma", Conditions)) %>%
  mutate(type = "Breast")

prostate_cancer_trials <- cancer_trials %>%
  filter(grepl("Prostate", Conditions) & !grepl("Breast", Conditions) & !grepl("Lung", Conditions) & !grepl("Colorectal", Conditions) & !grepl("Myeloma", Conditions)) %>%
  mutate(type = "Prostate")

#KEEP 
lung_cancer_trials <- cancer_trials %>%
  filter(grepl("Lung", Conditions) & !grepl("Breast", Conditions) & !grepl("Prostate", Conditions) & !grepl("Colorectal", Conditions) & !grepl("Myeloma", Conditions)) %>%
  mutate(type = "Lung")

colorectal_cancer_trials <- cancer_trials %>%
  filter(grepl("Colorectal", Conditions) & !grepl("Breast", Conditions) & !grepl("Prostate", Conditions) & !grepl("Lung", Conditions) & !grepl("Myeloma", Conditions)) %>%
  mutate(type = "Colorectal")

myeloma_cancer_trials <- cancer_trials %>%
  filter(grepl("Myeloma", Conditions) & !grepl("Breast", Conditions) & !grepl("Prostate", Conditions) & !grepl("Lung", Conditions) & !grepl("Colorectal", Conditions)) %>%
  mutate(type = "Myeloma")

# combine the filtered and reclassified trials
selected_cancer_trials <- bind_rows(breast_cancer_trials,prostate_cancer_trials,lung_cancer_trials,colorectal_cancer_trials,myeloma_cancer_trials)  

# anti join to filter against other trials
other_cancer_trials <- anti_join(cancer_trials,selected_cancer_trials, by="NCT Number") %>%
  mutate(type = "Other")

# recombine reclassified data
cancer_trials <- bind_rows(selected_cancer_trials,other_cancer_trials)

# create column for start year
cancer_trials <- cancer_trials %>%
  mutate(start_year = substr(`Start Date`, nchar(`Start Date`)-4, nchar(`Start Date`)))

# filter only for trials in US
cancer_trials_us <- cancer_trials %>%
  filter(grepl("United States",Locations))

# this loop creates a new column for each state, and tests whether the location includes that state name
n <- 1
for (s in state.name) {
  print(s)
  cancer_trials_us[[s]] <- grepl(s, cancer_trials_us$Locations)
  n <- n+1
  }

# replace TRUE from this evaluation with 1
cancer_trials_us[cancer_trials_us == TRUE] <- 1
# replace FALSE from this evaluation with 0
cancer_trials_us[cancer_trials_us == FALSE] <- 0

# now add the values across these new columns to find out how many states each trial took place in
cancer_trials_us$number_states = rowSums(cancer_trials_us[,c(33:82)])

# filter for trials in multiple states, and assign value of "multiple" to new column "state"
cancer_trials_us_multiple <- cancer_trials_us %>%
  filter(number_states > 1) %>%
  mutate(state = "multiple")

# filter for trials in one state
cancer_trials_us_single <- cancer_trials_us %>%
  filter(number_states == 1) 

# this loop creates a new colum "state" for trials conducted in just one state
cancer_trials_us_single_edit <- data_frame()

for (s in state.name) {
  print(s)
  tmp <- cancer_trials_us_single %>%
    filter(cancer_trials_us_single[[s]]==1) %>%
    mutate(state = s)
  cancer_trials_us_single_edit <- bind_rows(cancer_trials_us_single_edit,tmp)
}

# recombine data
cancer_trials_us <- bind_rows(cancer_trials_us_multiple,cancer_trials_us_single_edit)

# some cleanup
rm(tmp,n,s,ch,chunks,cancer_trials_us_single,cancer_trials_us_single_edit,cancer_trials_us_multiple,selected_cancer_trials,other_cancer_trials)

# export data
write_csv(cancer_trials_us, "data/cancer_trials_us.csv", na="")



