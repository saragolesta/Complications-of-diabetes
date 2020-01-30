library(tidyverse)
d = read.csv("diabetic_data.csv")
summary(d)

# drop ? race
d = d %>% 
  filter(race != "?") %>% 
  droplevels()

# removing unknown/invalid genders, turn into binary with ref = Female
d = d %>% 
  filter(gender != "Unknown/Invalid") %>% 
  mutate(gender2 = ifelse(gender == "Male", 1, 0)) %>% 
  select(-gender)

# creating new variable of middle of each age bracket
d$age2 = rep(0, length(d$age))
for (i in seq(from = 0, to = 90, by = 10)){
  d$age2[d$age == paste("[", as.character(i), "-", as.character(i+10), ")", sep = "")] = i + 5
}
d = d %>% 
  select(-age)

# weight is missing 96433/99492 = 97% of data, so we drop it
# payer code is missing 39711/99492 = 40% of data, so we drop
# medical specialty is missing 48766/99492 = 50% of data, so we drop
d = d %>% 
  select(-weight, -payer_code, -medical_specialty)

# remove rows where values for diag 1, 2, 3 are all missing
d %>% filter(diag_1 == "?", diag_2 == "?", diag_3 == "?")

summary(d)

# disposition ID, remove dead patients

# diag_1, diag_2, diag_3: remove rows where all three diagnoses are missing values
# but also need to investiage how many missing values are in each diagnoses

# max_glu_serum: drop column as 96420/99492 = 97% of data is missing
# A1Cresult: drop column as 82896/99492 = 83% of data is missing
d = d %>% select(-max_glu_serum, -A1Cresult)

# drop acetohexamide, tolbutamide, troglitazone, examide, citaglipton, 
# glipizde.metformin, glimepiride.pioglitazone as they contain only 1-2 levels 
# and 95-100$ of the values are that they were not prescribed
d = d %>% 
  select(-acetohexamide, -tolbutamide, -troglitazone, -examide, -citaglipton) %>% 
  select(-glipizde.metformin, -glimepiride.pioglitazone)


# for remaining medications, need to figure out a natural order to the 4 levels 

