library(tidyverse)
d = read.csv("preprocessed.csv")
d2 = read.csv("diabetic_data2.csv")

# looking at race in relation to re-admission
d = d %>% filter(race != "") %>% droplevels()
d %>% group_by(race) %>% summarize(prop = sum(readmitted_.30)/n())

# looking at gender vs. re-admission
d %>% group_by(gender) %>% summarize(prop = sum(readmitted_.30)/n())

# distribution of age vs. re_admission
ggplot(data = d[d$readmitted_.30 == "1",], aes(x = age)) + geom_histogram()
ggplot(data = d2, aes(x = age2, fill = readmitted)) + geom_histogram(position = "dodge")
d2 %>% group_by(readmitted) %>% summarise(mean_age = mean(age2))

# distribution of various procedures against readmission  
d = d %>% mutate(tot_num_actions = num_lab_procedures + num_medications + num_procedures)
ggplot(data = d[d$readmitted_.30 == "1",], aes(x = num_lab_procedures)) + geom_histogram()
ggplot(data = d[d$readmitted_.30 == "1",], aes(x = num_procedures)) + geom_histogram()
ggplot(data = d[d$readmitted_.30 == "1",], aes(x = num_medications)) + geom_histogram()
ggplot(data = d[d$readmitted_.30 == "1",], aes(x = tot_num_actions)) + geom_histogram()
ggplot(data = d[d$readmitted_.30 == "1",], aes(x = tot_num_actions)) + geom_density()

# distribution of admission_type
ggplot(data = d[d$readmitted_.30 == "1",], aes(x = admission_type_id)) + 
  geom_histogram()

# time in hospital
# in general (black) and readmitted (red)
ggplot(data = d, aes(x = time_in_hospital)) + 
  geom_density() + 
  geom_density(data = d[d$readmitted_.30 == "1",], color = "red")
# essentially two distributions are the same

# 
ggplot(data = d, aes(x = A1Cresult, y = readmitted_.30)) + 
  geom_bar(stat = "identity") 
d %>% group_by(A1Cresult) %>% summarize(total = sum(readmitted_.30))  
