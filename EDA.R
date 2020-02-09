library(tidyverse)
d = read.csv("combined.csv")
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

# diagnostic 1
diag1 = d %>% select(diag_1, readmitted_.30, readmitted_.30.1, readmitted_NO)
colnames(diag1) = c("diag_1", "RA_under_30", "RA_over_30", "RA_No")
diag1 = diag1 %>% 
  pivot_longer(RA_under_30:RA_No, names_to = "Readmittance_Type", values_to = "Value")
prop_diag1 = diag1 %>% 
  group_by(diag_1, Readmittance_Type) %>% 
  summarize(n = sum(Value)) %>% 
  group_by(diag_1) %>% 
  mutate(prop = n/sum(n))

diag1 %>% group_by(Readmittance_Type) %>% summarize(count = sum(Value))
levels(diag1$diag_1) = c("Others", "428")
ggplot(data = prop_diag1, aes(x = diag_1, y = prop, fill = Readmittance_Type)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(labels = c("Not readmitted", ">30 days", "<30 days"), 
                    values = c("lightpink1", "lightpink3", "lightpink4")) + 
  ggtitle("Readmittance Proportions for Most Frequent IDs in Diag 1") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(fill = "Readmittance Type", x = "Diagnostic 1 IDs", y = "Proportion")

# diagnostic 2
diag1 = d %>% select(diag_1, readmitted_.30, readmitted_.30.1, readmitted_NO)
colnames(diag1) = c("diag_1", "RA_under_30", "RA_over_30", "RA_No")
diag1 = diag1 %>% 
  pivot_longer(RA_under_30:RA_No, names_to = "Readmittance_Type", values_to = "Value")
prop_diag1 = diag1 %>% 
  group_by(diag_1, Readmittance_Type) %>% 
  summarize(n = sum(Value)) %>% 
  group_by(diag_1) %>% 
  mutate(prop = n/sum(n))

diag1 %>% group_by(Readmittance_Type) %>% summarize(count = sum(Value))
levels(diag1$diag_1) = c("Others", "428")
ggplot(data = prop_diag1, aes(x = diag_1, y = prop, fill = Readmittance_Type)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(labels = c("Not readmitted", ">30 days", "<30 days"), 
                    values = c("lightpink1", "lightpink3", "lightpink4")) + 
  ggtitle("Readmittance Proportions for Most Frequent IDs in Diag 1") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(fill = "Readmittance Type", x = "Diagnostic 1 IDs", y = "Proportion")