library(tidyverse)
d = read.csv("combined.csv")
d2 = read.csv("diabetic_data2.csv")

# looking at race in relation to re-admission
d %>% group_by(race) %>% summarize(prop = sum(readmitted_.30)/n())
levels(d$race) = c("Other", "AfricanAmerican", "Asian", "Caucasian",
                   "Hispanic", "Other")
racedf = d %>% group_by(race) %>% summarize(n = n())
racedf %>% mutate(prop = n/sum(n))
ggplot(data = racedf, aes(x = "", y = n, fill = race)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  scale_fill_manual(values = c("lightblue4", "lightpink4", 
                               "lightyellow2", "lavenderblush3", "lightseagreen")) + 
  ggtitle("    Distribution of Races") + theme_void(base_size = 14) + 
  labs(fill = "Race")

racedf = d %>% group_by(race, readmitted_.30) %>% summarize(n = n()) %>% 
  group_by(race) %>% mutate(prop = n/sum(n))
racedf$readmitted_.30 = c(rbind(rep("No", 5), rep("Yes", 5)))
racedf
ggplot(data = racedf, aes(x = race, y = prop, fill = readmitted_.30)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("lightblue2", "lightpink4")) + 
  ggtitle("Proportion of Race x Re-admission") + 
  theme(plot.title = element_text(size = 16), 
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  labs(fill = "Re-admission", x = "Race", y = "Counts")

# looking at gender vs. re-admission
d %>% group_by(gender) %>% summarize(prop = sum(readmitted_.30)/n())
genderdf = d %>% group_by(gender, readmitted_.30) %>% summarize(n = n())
genderdf$readmitted_.30 = c("No", "Yes", "No", "Yes")
genderdf %>% group_by(gender) %>% mutate(prop = n/sum(n))
d %>% group_by(gender) %>% summarize(n = n()) %>% mutate(prop = n/sum(n))

ggplot(data = genderdf, aes(x = gender, y = n, fill = readmitted_.30)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("lightblue2", "lightpink4")) + 
  ggtitle("Distribution of Gender x Re-admission") + 
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14), legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  labs(fill = "Re-admission", x = "Gender", y = "Counts")

# distribution of age vs. re_admission
agedf = d %>% group_by(age, readmitted_.30) %>% summarize(n = n()) %>% 
  group_by(age) %>% mutate(prop = n/sum(n))

agedf$age = as.factor(agedf$age)
agedf$readmitted_.30 = c(rbind(rep("No", 10), rep("Yes", 10)))
ggplot(data = agedf, aes(x = age, y = n)) + geom_bar(stat = "identity") +
  scale_fill_manual(values = c("lightblue4")) + 
  ggtitle("Distribution of Age x Re-admission") + 
  theme(plot.title = element_text(size = 16), 
        axis.text.x = element_text(size = 12)) + 
  labs(fill = "Re-admission", x = "Age", y = "Counts")

ggplot(data = agedf, aes(x = age, y = prop, fill = readmitted_.30)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("lightblue2", "lightpink4")) + 
  ggtitle("Proportion of Age x Re-admission") + 
  theme(plot.title = element_text(size = 16), 
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  labs(fill = "Re-admission", x = "Age", y = "Proportion")

ggplot(data = d2, aes(x = age2, fill = readmitted)) + geom_histogram(position = "dodge")
d2 %>% group_by(readmitted) %>% summarise(mean_age = mean(age2))

# distribution of various procedures against readmission  
d = d %>% mutate(tot_num_actions = num_lab_procedures + num_medications + num_procedures)
proceduresdf = d %>% select(num_lab_procedures, num_procedures, readmitted_.30, 
                            num_medications) %>% 
  mutate(num_tot_procedures = num_lab_procedures + num_procedures)
proceduresdf$readmitted_.30 = factor(ifelse(proceduresdf$readmitted_.30 == 0, "No", "Yes"))

ggplot(data = proceduresdf, aes(x = readmitted_.30, y = num_medications)) + 
  geom_boxplot(fill = c("lightblue1", "lightpink4")) + coord_flip() + 
  ggtitle("Boxplot of Number of Medications") + 
  theme(plot.title = element_text(size = 16), 
        axis.text.x = element_text(size = 12)) + 
  labs(x = "Readmission", y = "Number of Medications")

ggplot(data = proceduresdf, aes(x = readmitted_.30, y = num_tot_procedures)) + 
  geom_boxplot(fill = c("lightblue1", "lightpink4")) + coord_flip() + 
  ggtitle("Boxplot of Number of Procedures") + 
  theme(plot.title = element_text(size = 16), 
        axis.text.x = element_text(size = 12)) + 
  labs(x = "Readmission", y = "Number of Procedures")

ggplot(data = proceduresdf, 
       aes(x = num_tot_procedures, fill = readmitted_.30)) + 
  geom_histogram(alpha = 0.8) + 
  scale_fill_manual(values = c("lightblue3", "lightpink4")) + 
  ggtitle("Distribution of Number of Procedures") + 
  theme(plot.title = element_text(size = 16), 
        axis.text.x = element_text(size = 12), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  labs(fill = "Re-admission", x = "Number of Total Procedures", y = "Count")

ggplot(data = proceduresdf, aes(x = num_medications, fill = readmitted_.30)) + 
  geom_histogram(alpha = 0.8) + 
  scale_fill_manual(values = c("lightblue3", "lightpink4")) + 
  ggtitle("Distribution of Number of Medications") + 
  theme(plot.title = element_text(size = 16), 
        axis.text.x = element_text(size = 12), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  labs(fill = "Re-admission", x = "Number of Medications", y = "Count")

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
colnames(diag1) = c("ID", "RA_under_30", "RA_over_30", "RA_No")
diag1 = diag1 %>% 
  pivot_longer(RA_under_30:RA_No, names_to = "Readmittance_Type", values_to = "Value")
prop_diag1 = diag1 %>% 
  group_by(ID, Readmittance_Type) %>% 
  summarize(n = sum(Value)) %>% 
  group_by(ID) %>% 
  mutate(prop = n/sum(n)) %>% ungroup() %>% 
  mutate(diagnosis = "Diagnosis 1")

diag2 = d %>% select(diag_2, readmitted_.30, readmitted_.30.1, readmitted_NO)
colnames(diag2) = c("ID", "RA_under_30", "RA_over_30", "RA_No")
diag2 = diag2 %>% 
  pivot_longer(RA_under_30:RA_No, names_to = "Readmittance_Type", values_to = "Value")
prop_diag2 = diag2 %>% 
  group_by(ID, Readmittance_Type) %>% 
  summarize(n = sum(Value)) %>% 
  group_by(ID) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup() %>% mutate(diagnosis = "Diagnosis 2")

diag3 = d %>% select(diag_3, readmitted_.30, readmitted_.30.1, readmitted_NO)
colnames(diag3) = c("ID", "RA_under_30", "RA_over_30", "RA_No")
diag3 = diag3 %>% 
  pivot_longer(RA_under_30:RA_No, names_to = "Readmittance_Type", values_to = "Value")
prop_diag3 = diag3 %>% 
  group_by(ID, Readmittance_Type) %>% 
  summarize(n = sum(Value)) %>% 
  group_by(ID) %>% 
  mutate(prop = n/sum(n)) %>% ungroup() %>% 
  mutate(diagnosis = "Diagnosis 3")

prop_diagdf = rbind(prop_diag1, prop_diag2, prop_diag3)

diag1 %>% group_by(Readmittance_Type) %>% summarize(count = sum(Value))

ggplot(data = prop_diag3, aes(x = ID, y = prop, fill = Readmittance_Type)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(labels = c("Not readmitted", ">30 days", "<30 days"), 
                    values = c("lightblue1", "lavenderblush3", "lightpink4")) + 
  ggtitle("Readmittance Prop for Common Diagnoses 1, 2, 3") + 
  theme(plot.title = element_text(size = 14), 
        axis.text.x = element_text(size = 11, angle = 90, hjust = 1, vjust = 0.5), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  labs(fill = "Readmittance Type", x = "Diagnosis 3 IDs", y = "Proportion")

# diagnostic 2

diagnostic_graphs = function(df, diagnosis, title, xlab){
  ggplot(data = df, aes(x = diagnosis, y = prop, fill = Readmittance_Type)) +
    geom_bar(stat = "identity") + 
    scale_fill_manual(labels = c("Not readmitted", ">30 days", "<30 days"), 
                      values = c("lightblue1", "lavenderblush3", "lightpink4")) + 
    ggtitle(title) + 
    theme(plot.title = element_text(size = 14), 
          axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5), 
          legend.text = element_text(size = 14), 
          legend.title = element_text(size = 14)) + 
    labs(fill = "Readmittance Type", x = xlab, y = "Proportion")
}

diag2 = d %>% select(diag_2, readmitted_.30, readmitted_.30.1, readmitted_NO)
colnames(diag2) = c("diag_2", "RA_under_30", "RA_over_30", "RA_No")
diag2 = diag2 %>% 
  pivot_longer(RA_under_30:RA_No, names_to = "Readmittance_Type", values_to = "Value")
prop_diag2 = diag2 %>% 
  group_by(diag_2, Readmittance_Type) %>% 
  summarize(n = sum(Value)) %>% 
  group_by(diag_2) %>% 
  mutate(prop = n/sum(n))

diagnostic_graphs(df = prop_diag2, diagnosis = prop_diag2$diag_2,
                  title = "Readmittance Prop for Most Common Diag 2 IDs", 
                  xlab = "Diagnosis 2 IDs")

# diagnostic 3

diag3 = d %>% select(diag_3, readmitted_.30, readmitted_.30.1, readmitted_NO)
colnames(diag3) = c("diag_3", "RA_under_30", "RA_over_30", "RA_No")
diag3 = diag3 %>% 
  pivot_longer(RA_under_30:RA_No, names_to = "Readmittance_Type", values_to = "Value")
prop_diag3 = diag3 %>% 
  group_by(diag_3, Readmittance_Type) %>% 
  summarize(n = sum(Value)) %>% 
  group_by(diag_3) %>% 
  mutate(prop = n/sum(n))
diagnostic_graphs(df = prop_diag3, diagnosis = prop_diag3$diag_3,
                  title = "Readmittance Prop for Most Common Diag 3 IDs", 
                  xlab = "Diagnosis 3 IDs")
