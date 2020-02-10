library(tidyverse)
d = read.csv("combined.csv")
d2 = read.csv("diabetic_data2.csv")

# looking at race in relation to re-admission
d = d %>% filter(race != "") %>% droplevels()

d %>% group_by(race) %>% summarize(prop = sum(readmitted_.30)/n())
levels(d$race) = c("Other", "AfricanAmerican", "Asian", "Caucasian",
                   "Hispanic", "Other")
racedf = d %>% group_by(race) %>% summarize(n = n())
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

ggplot(data = genderdf, aes(x = gender, y = n, fill = readmitted_.30)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("lightblue2", "lightpink4")) + 
  ggtitle("Distribution of Gender x Re-admission") + 
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14), legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  labs(fill = "Re-admission", x = "Gender", y = "Counts")

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

ggplot(data = prop_diag1, aes(x = diag_1, y = prop, fill = Readmittance_Type)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(labels = c("Not readmitted", ">30 days", "<30 days"), 
                    values = c("lightpink1", "lightpink3", "lightpink4")) + 
  ggtitle("Readmittance Proportions for Most Frequent IDs in Diag 1") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(fill = "Readmittance Type", x = "Diagnostic 1 IDs", y = "Proportion")

# diagnostic 2

diagnostic_graphs = function(df, diagnostic, title, xlab){
  ggplot(data = df, aes(x = diagnostic, y = prop, fill = Readmittance_Type)) +
    geom_bar(stat = "identity") + 
    scale_fill_manual(labels = c("Not readmitted", ">30 days", "<30 days"), 
                      values = c("lightpink1", "lightpink3", "lightpink4")) + 
    ggtitle(title) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
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

diagnostic_graphs(df = prop_diag2, diagnostic = prop_diag2$diag_2,
                  title = "Readmittance Proportion for Top Diagnostic 2 IDs", 
                  xlab = "Diagnostic 2 IDs")

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
prop_diag2
prop_diag3
diagnostic_graphs(df = prop_diag3, diagnostic = prop_diag3$diag_3,
                  title = "Readmittance Proportion for Top Diagnostic 3 IDs", 
                  xlab = "Diagnostic 3 IDs")
