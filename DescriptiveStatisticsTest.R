#------------------------------------SET UP------------------------------------#
#Packages
library(tidyverse)
library(knitr)
library(lubridate)
library(fishualize)
library(plotly)
library(stringr)
library(scales)
library(ggsci)

#Loading Data
data <- read_csv("survey_data_renamed.csv")
codebook <- read_csv("codebook.csv")

#Function to pull questions from codebook (helpful for markdown titles)
lookup <- function(var_name) {
  
  codebook %>% 
    filter(variable == var_name) %>% 
    pull(question)
  
}

#-----------------------------------ANALYSIS-----------------------------------#



#--------------------------Demographics/Background-----------------------------#
#------------------------------------------------------------------------------#

#Age Distribution
AgeDistribution_Plot <- data %>% 
  group_by(age) %>% 
  tally() %>% 
  mutate(Percent = round(100*(n/sum(n)), 2)) %>% 
  ggplot(aes(x = age, y = Percent)) +
  geom_col(fill = "deeppink3") +
  theme_minimal() +
  labs(x = "Age of Respondent", y = "Percent of Sample", title = "Age Distribution") 


#Race table- Overall distribution of our sample  
RaceCount <- data %>% 
  group_by(race) %>% 
  tally() %>% 
  mutate(Percent = round(100*(n/sum(n)), 2)) %>% 
  rename(Count = n,
         `Racial Group` = race) %>% 
  arrange(desc(Percent))

#Race table cleaned (Generalize all smaller samples to "Other")
RaceCountCleaned <- data %>% 
  group_by(race_3) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(Percent = round(100*(n/sum(n)), 2),
      race_3 = fct_relevel(race_3, levels = c("White", "Asian", "Other"))) %>% 
  arrange(race_3) %>% 
  rename(Count = n,
         `Racial Group` = race_3)

#Ethnicity- Looking at distribution of Hispanic/Latinx in our sample
HispanicCount <- data %>% 
  group_by(hispanic) %>% 
  tally() %>% 
  mutate(percent = round(100*(n/sum(n)), 2)) %>% 
  rename(count = n,
         `Hispanic/Latinx` = hispanic)

#Ethnic Composition of Sample
RaceBarChart_HispComposition <- data %>% 
  ggplot(aes(race_3, fill = hispanic)) +
  geom_bar(position = "stack") +
  scale_fill_fish_d(option = "Scarus_quoyi") +
  theme_minimal() + 
  labs(x = NULL, y = "Count", fill = "Hispanic/Latinx",
       title = "Racial and Ethnic Composition of the Sample")

#Gender- Table statstic of Gender distribution of sample
GenderCount <- data %>% 
  group_by(gender) %>% 
  tally() %>% 
  mutate(Percent = round(100*(n/sum(n)), 2)) %>% 
  rename(Count = n,
         `Gender Identity` = gender) %>% 
  arrange(desc(Percent))

#Residency- Table stastic of Residency status count
ResidencyCount <- data %>% 
  group_by(residency) %>% 
  tally() %>% 
  mutate(Percent = round(100*(n/sum(n)), 2)) %>% 
  rename(Count = n,
         `Residency Status` = residency) %>% 
  arrange(desc(Percent))

#Residency- Grouped barchart
RaceDistribution_ByResidency_Barchart <- data %>% 
  ggplot(aes(residency, fill = race_3)) + 
  geom_bar(position = "dodge") +
  scale_fill_fish_d(option = "Scarus_quoyi") +
  theme_minimal() +
  labs(x = NULL, y = "Count", fill = "Racial Identity",
       title = "Residency Status and Race") 

#First Generation
FirstGenCount <- data %>% 
  group_by(first_gen) %>% 
  tally() %>% 
  mutate(Percent = round(100*(n/sum(n)), 2)) %>% 
  rename(Count = n,
         `First Generation` = first_gen)

#First Gen By Racial Identity Plot
FirstGen_Race_Plot <- data %>% 
  ggplot(aes(first_gen, fill = race_3)) + 
  geom_bar(position = "fill") + 
  scale_fill_fish_d(option = "Scarus_quoyi") +
  theme_minimal() + 
  labs(x = "First Generation?", y = "Proportion", fill = "Racial Identity",
       title = "First Generation Status and Racial Identity")

#Political Ideology Distribution
PoliticalIdeology_PercentDistribution <-data %>% 
  ggplot(aes(first_gen, fill = race_3)) + 
  geom_bar(position = "fill") + 
  scale_fill_fish_d(option = "Scarus_quoyi") +
  theme_minimal() + 
  labs(x = "First Generation?", y = "Proportion", fill = "Racial Identity",
       title = "First Generation Status and Racial Identity")

#School Distribution
SchoolCount <- data %>% 
  group_by(school) %>% 
  tally() %>% 
  mutate(Percent = round(100*(n/sum(n)), 2)) %>% 
  rename(Count = n,
         School = school) %>% 
  arrange(desc(Percent))

#Major Distribution
MajorCount <- data %>% 
  group_by(major) %>% 
  tally() %>% 
  mutate(Percent = round(100*(n/sum(n)), 2)) %>% 
  rename(Count = n,
         Major = major) %>% 
  arrange(desc(Percent)) %>% 
  head(10)

#Politcal Views by grade Plot
PoliticalViews_ByGrade_Plot <- data %>%
  group_by(grade, pol_views) %>%
  tally() %>%
  mutate(percent = 100*(n/sum(n))) %>%
  ggplot(aes(x = grade, y = percent, fill = as.factor(pol_views))) +
  geom_col() +
  theme_bw() +
  scale_fill_fish_d(option = "Hypsypops_rubicundus") +
  labs(x = "Grade", y = "Percent",
       title = "Change In Political Ideology Throughout Education",
       fill = "Political Views") + 
  theme(plot.title = element_text(hjust = 0.5))



#----------------------------------Housing-------------------------------------#
#------------------------------------------------------------------------------#

#Secure Housing Before Covid
SecureHousingCountBefore <- data %>% 
  group_by(before_secure) %>% 
  tally() %>% 
  mutate(Percent = round(100*(n/sum(n)), 2)) %>% 
  rename(Count = n,
         `Secure Housing Before` = before_secure) %>% 
  arrange(desc(Percent))

#Housing Types- Percentage changes of before and After
HousingTypeChange <- data %>% 
  group_by(housing_b4) %>% 
  tally() %>% 
  rename(`Housing Type` = housing_b4,
         Before = n) %>% 
  left_join(data %>% 
              group_by(housing_after) %>% 
              tally() %>% 
              rename(`Housing Type` = housing_after,
                     After = n)) %>% 
  mutate(Before = round(100*(Before/nrow(data)), 2),
         After = round(100*(After/nrow(data)), 2),
         `PP Change` = After - Before,
         `A/B Ratio` = round(After/Before, 2))  %>% 
  arrange(desc(`PP Change`))

#Roomates- Percent changes before/after
RoomateChange <- data %>% 
  group_by(mates_b4) %>% 
  tally() %>% 
  rename(`Roommates` = mates_b4,
         Before = n) %>% 
  left_join(data %>% 
              group_by(mates_after) %>% 
              tally() %>% 
              rename(`Roommates` = mates_after,
                     After = n)) %>% 
  mutate(Before = round(100*(Before/nrow(data)), 2),
         After = round(100*(After/nrow(data)), 2),
         `PP Change` = After - Before,
         `A/B Ratio` = round(After/Before, 2))  %>% 
  arrange(desc(`PP Change`))

#Internet Access- Percent changes before/after
InternetAccessChange <- data %>% 
  group_by(internet_b4) %>% 
  tally() %>% 
  rename(`Internet Access` = internet_b4,
         Before = n) %>% 
  left_join(data %>% 
              group_by(internet_after) %>% 
              tally() %>% 
              rename(`Internet Access` = internet_after,
                     After = n)) %>% 
  mutate(Before = round(100*(Before/nrow(data)), 2),
         After = round(100*(After/nrow(data)), 2),
         `PP Change` = After - Before,
         `A/B Ratio` = round(After/Before, 2))  %>% 
  arrange(desc(`PP Change`))

#Computer Access- Percent changes before/after
ComputerAccessChange <- data %>% 
  group_by(computer_b4) %>% 
  tally() %>% 
  rename(`Computer Access` = computer_b4,
         Before = n) %>% 
  left_join(data %>% 
              group_by(computer_after) %>% 
              tally() %>% 
              rename(`Computer Access` = computer_after,
                     After = n)) %>% 
  mutate(Before = round(100*(Before/nrow(data)), 2),
         After = round(100*(After/nrow(data)), 2),
         `PP Change` = After - Before,
         `A/B Ratio` = round(After/Before, 2))  %>% 
  arrange(desc(`PP Change`))
  
#Did your Housing Change(Yes/No)? Distribution
HousingChangeCount <- data %>% 
  filter(!is.na(housing_change)) %>% 
  group_by(housing_change) %>% 
  tally() %>% 
  mutate(Percent = round(100*(n/sum(n)), 2)) %>% 
  rename(Count = n,
         `Housing Change` = housing_change) %>% 
  arrange(desc(Percent))

#Housing Type categories, areas of changes (Yes/No)
HousingTypeRelationToChange <- data %>% 
  group_by(housing_b4, housing_change) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(housing_b4) %>% 
  mutate(type_total = sum(n),
         percent = round(100*(n/type_total), 2)) %>% 
  select(`Housing Type` = housing_b4, percent, housing_change) %>% 
  pivot_wider(names_from = housing_change, values_from = percent)



#--------------------------Online Learning/ Experince--------------------------#
#------------------------------------------------------------------------------#

#Devices used for online learning in our sample (Table)
Devices <- data %>% 
  mutate(dev_desk = ifelse(str_detect(devices, "Desk"), 1, 0)) %>% 
  mutate(dev_lap = ifelse(str_detect(devices, "Lap"), 1, 0)) %>% 
  mutate(dev_tab = ifelse(str_detect(devices, "Tab"), 1, 0)) %>% 
  mutate(dev_phone = ifelse(str_detect(devices, "rt Ph"), 1, 0)) 

Devices <- tibble(device = c("Smart Phone", "Desktop Computer",
                             "Laptop Computer", "Tablet"),
                proportion = c(mean(Devices$dev_phone),
                               mean(Devices$dev_desk),
                               mean(Devices$dev_lap),
                               mean(Devices$dev_tab))) %>%
  mutate(Percent = round(100*proportion, 2)) %>%
  select(`Device Type` = device, Percent) %>%
  arrange(desc(Percent))



#Changes in education behavior Creating group barchart plot)
Education_behavior_plot <- data %>% 
  select('Ability to focus on school work' = edu_focus,
      'Motivation to do school work' = edu_motivation,
      'Time spent on school work' = edu_time, 
      'Attendance at lectures' = edu_attendance,
      'Vocal or written participation during lecutres and section' = edu_participate) %>% 
  pivot_longer(1:5, names_to = "Situations", values_to = "value") %>% 
  mutate(Decreased = ifelse(str_detect(value, "Decreased"),1,0)) %>%
  mutate(About_Same = ifelse(str_detect(value, "About the same"),1,0)) %>% 
  mutate(Increased = ifelse(str_detect(value, "Increased"),1,0)) %>% 
  mutate(Not_Applicable = ifelse(str_detect(value, "Not applicable"),1,0)) %>% 
  group_by(Situations,value) %>% 
  summarise(Decreased = sum(Decreased),
            About_Same = sum(About_Same),
            Increased = sum(Increased),
            Not_Applicable = sum(Not_Applicable)) %>% 
  mutate(Count = Decreased + Increased + About_Same + Not_Applicable) %>% 
  select(Situations, value, Count)


Education_behavior_plot <- Education_behavior_plot %>% 
  ggplot(aes(x = factor(Situations), y = Count, fill = value)) +
  geom_col(position = "dodge") +
  xlab("") + 
  ylab("Number of Participants") +
  scale_x_discrete(labels = wrap_format(10)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette= "Set2")


#Changes in education behavior (Descriptive table)
EducationBehaviorChange_Table <- data %>%
  group_by(edu_attendance) %>%
  tally() %>%
  mutate(n = round(100*(n/sum(n)), 2)) %>% 
  rename(change = edu_attendance, 
         Attendance = n) %>% 
  left_join(data %>%
              group_by(edu_focus) %>%
              tally() %>%
              mutate(n = round(100*(n/sum(n)), 2)) %>% 
              rename(change = edu_focus,
                     Focus = n)) %>% 
  left_join(data %>%
              group_by(edu_motivation) %>%
              tally() %>%
              mutate(n = round(100*(n/sum(n)), 2)) %>% 
              rename(change = edu_motivation,
                     Motivation = n)) %>% 
  left_join(data %>%
              group_by(edu_time) %>%
              tally() %>%
              mutate(n = round(100*(n/sum(n)), 2)) %>% 
              rename(change = edu_time,
                     Time = n)) %>% 
  left_join(data %>%
              group_by(edu_participate) %>%
              tally() %>%
              mutate(n = round(100*(n/sum(n)), 2)) %>% 
              rename(change = edu_participate,
                     Participation = n)) %>% 
  mutate(change = fct_relevel(change, 
                              levels = c("Increased", "About the same",
                                         "Decreased", "Not applicable"))) %>% 
  arrange(change) %>% 
  select(`Since outbreak` = change, Focus, Motivation, 
         Time, Attendance, Participation)

#Percieved effectiveness in Education instruction (Group barchart)
Education_InstructionPlot <- data %>% 
  select('Communication of course material' = edu_comm,
         'Accommodation for special circumstances' = edu_issues,
         'Accommodation for diverse learning needs' = edu_needs) %>% 
  pivot_longer(1:3, names_to = 'EducationInstruction', values_to = 'value') %>% 
  mutate(LessEffective = ifelse(str_detect(value, "Less effective"), 1, 0)) %>%
  mutate(About_same = ifelse(str_detect(value, "About the same"), 1, 0)) %>% 
  mutate(MoreEffective = ifelse(str_detect(value, "More effective"), 1, 0)) %>% 
  mutate(Unsure = ifelse(str_detect(value, "Unsure"), 1, 0)) %>% 
  group_by(EducationInstruction, value) %>% 
  summarise(LessEffective = sum(LessEffective),
            About_same = sum(About_same),
            MoreEffective = sum(MoreEffective),
            Unsure = sum(Unsure)) %>% 
  mutate(Count = LessEffective + About_same + MoreEffective + Unsure) %>% 
  select(EducationInstruction, value, Count)

Education_InstructionPlot <- Education_InstructionPlot %>% 
  ggplot(aes(x = factor(EducationInstruction), y = Count, fill = value)) +
  geom_col(position = "dodge") +
  xlab("") + 
  ylab("Number of Participants") +
  scale_x_discrete(labels = wrap_format(10)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette= "Set2")
  

#Changes in Work Space
WorkSpacePercentageChange <- data %>% 
  group_by(workspace_c) %>% 
  tally() %>% 
  mutate(n = round(100*(n/sum(n)), 2)) %>% 
  rename(Current = n, Response = workspace_c) %>% 
  full_join(data %>% 
              group_by(workspace_b) %>% 
              tally() %>% 
              mutate(n = round(100*(n/sum(n)), 2)) %>% 
              rename(`Pre-outbreak` = n, Response = workspace_b)) %>% 
  mutate(Current = ifelse(is.na(Current), 0, Current),
         `PP Change` = Current - `Pre-outbreak`) %>% 
  select(`Primary workspace` = Response, `Pre-outbreak`, Current, `PP Change`) %>% 
  arrange(desc(Current))
  
#Education resource availability
Education_avalability_plot <- data %>% 
  select('Tutoring' = edu_tutor,
         'Office Hours' = edu_office,
         'Academic Advising' = edu_advising, 
         'Disability Services' = edu_drs,
         'Career Advising' = edu_career) %>% 
  pivot_longer(1:5, names_to = "Situations", values_to = "value") %>% 
  mutate(Less = ifelse(str_detect(value, "Less available"),1,0)) %>%
  mutate(About_Same = ifelse(str_detect(value, "About the same"),1,0)) %>% 
  mutate(More = ifelse(str_detect(value, "More available"),1,0)) %>% 
  mutate(Unsure = ifelse(str_detect(value, "Unsure"),1,0)) %>% 
  group_by(Situations,value) %>% 
  summarise(Less = sum(Less),
            About_Same = sum(About_Same),
            More = sum(More),
            Unsure = sum(Unsure)) %>% 
  mutate(Count = Less + More + About_Same + Unsure) %>% 
  select(Situations, value, Count)


Education_avalability_plot <- Education_avalability_plot %>% 
  ggplot(aes(x = factor(Situations), y = Count, fill = value)) +
  geom_col(position = "dodge") +
  xlab("") + 
  ylab("Number of Participants") +
  scale_x_discrete(labels = wrap_format(10)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette= "Set2")

#---------------------------------Graduation-----------------------------------#
#------------------------------------------------------------------------------#

#Data manipulation for graduation section
data <- data %>% 
  mutate(post_grad_job_intern = ifelse(str_detect(post_grad,"I had/have a new job or internship lined up"), 1, 0)) %>% 
  mutate(post_grad_new_job = ifelse(str_detect(post_grad, "Look for a job"), 1, 0)) %>% 
  mutate(post_grad_current_job = ifelse(str_detect(post_grad, "Keep my current job"), 1, 0)) %>% 
  mutate(post_grad_travel = ifelse(str_detect(post_grad, "Travel for an extended period of time"), 1, 0)) %>% 
  mutate(post_grad_grad_prof = ifelse(str_detect(post_grad, "Graduate school or professional school"), 1, 0)) %>% 
  mutate(post_grad_volunteer_service = ifelse(str_detect(post_grad, "Volunteering or doing service work"), 1, 0)) %>% 
  mutate(post_grad_undecided = ifelse(str_detect(post_grad, "Undecided"), 1, 0))

data <- data %>% 
  mutate(post_grad_job_intern2 = ifelse(post_grad_job_intern == 1,"Yes", "No"))

#Pre-OutBreak Post Grad Plans and graduation status
PreOutBreak_PostGradPlans_Table <- tibble(post_grad =  c("I had/have a new job or internship lined up", "Look for a job", "Keep my current job", "Travel for an extended period of time", "Graduate school or professional school", "Volunteering or doing service work", "Undecided"),
                proportion = c(mean(data$post_grad_job_intern), mean(data$post_grad_new_job), mean(data$post_grad_current_job), mean(data$post_grad_travel), mean(data$post_grad_grad_prof), mean(data$post_grad_volunteer_service),mean(data$post_grad_undecided))) %>% 
  mutate(Percent = round(100*proportion, 2)) %>% 
  select(`Post Graduation Plan` = post_grad, Percent) %>% 
  arrange(desc(Percent))

PostGradPlan_V_GradStatus_Plot <- data %>% 
  select("Had job or int." = post_grad_job_intern,
         "Seek new job" = post_grad_new_job,
         "Keep current job" = post_grad_current_job,
         "Travel" = post_grad_travel,
         "Grad/prof. school" = post_grad_grad_prof,
         "Volunteer service" = post_grad_volunteer_service,
         "Undecided" = post_grad_undecided,
          grad_2020) %>% 
  pivot_longer(1:7, names_to = "Situations", values_to = "count2") %>% 
  group_by(Situations, grad_2020) %>% 
  summarise(count2 = sum(count2))  %>% 
  mutate(Percent = round(100*(count2/sum(count2)), 2))

PostGradPlan_V_GradStatus_Plot <- PostGradPlan_V_GradStatus_Plot %>% 
  ggplot(aes(x = Situations, y = Percent, fill = grad_2020)) +
  geom_col(position = "dodge") +
  xlab("") + 
  ylab("Percent of subset") +
  labs(fill = "Graduating in 2020") +
  scale_x_discrete(labels = wrap_format(10)) +
  theme_minimal() +
  scale_fill_brewer(palette= "Pastel1") +
  coord_flip()


#Post graduation plans and differential changes
PostGradPlan_V_DiffChanges <- data %>% 
  select("Had job or int." = post_grad_job_intern,
         "Keep current job" = post_grad_current_job,
         "Travel" = post_grad_travel,
         "Grad/prof. school" = post_grad_grad_prof,
         "Volunteer service" = post_grad_volunteer_service,
         chng_post_grad) %>% 
  pivot_longer(1:5, names_to = "Situations", values_to = "count2") %>% 
  group_by(Situations, chng_post_grad) %>% 
  summarise(count2 = sum(count2))  %>% 
  mutate(Percent = round(100*(count2/sum(count2)), 2))

PostGradPlan_V_DiffChanges <- PostGradPlan_V_DiffChanges %>% 
  ggplot(aes(x = Situations, y = Percent, fill = chng_post_grad)) +
  geom_col(position = "dodge") +
  xlab("") + 
  ylab("Percent of subset") +
  labs(fill = "Change in Post-Graduation Plan") +
  scale_x_discrete(labels = wrap_format(10)) +
  theme_minimal() +
  scale_fill_brewer(palette= "Pastel1") +
  coord_flip()

#Changes in plans to search for new job
SeekNewJob_V_DiffChanges <- data %>% 
  select("Seek new job" = post_grad_new_job,
         chng_post_grad) %>% 
  pivot_longer(1:1, names_to = "Situations", values_to = "count2") %>% 
  group_by(Situations, chng_post_grad) %>% 
  summarise(count2 = sum(count2))  %>% 
  mutate(Percent = round(100*(count2/sum(count2)), 2))

SeekNewJob_V_DiffChanges <- SeekNewJob_V_DiffChanges %>% 
  ggplot(aes(x = Situations, y = Percent, fill = chng_post_grad)) +
  geom_col(position = "dodge") +
  xlab("") + 
  ylab("Percent of subset") +
  labs(fill = "Change in Post-Graduation Plan") +
  scale_x_discrete(labels = wrap_format(10)) +
  theme_minimal() +
  scale_fill_brewer(palette= "Pastel1") +
  coord_flip()

#Reported Pre-COVID-19 Job or Internship Revoked.
PostGradPlan_V_JobRevoke <- data %>% 
  select("Had job or int." = post_grad_job_intern,
         "Keep current job" = post_grad_current_job,
         job_revoke) %>% 
  pivot_longer(1:2, names_to = "Situations", values_to = "count2") %>% 
  group_by(Situations, job_revoke) %>% 
  summarise(count2 = sum(count2))  %>% 
  mutate(Percent = round(100*(count2/sum(count2)), 2))

PostGradPlan_V_JobRevoke <- PostGradPlan_V_JobRevoke %>% 
  ggplot(aes(x = Situations, y = Percent, fill = job_revoke)) +
  geom_col(position = "dodge") +
  xlab("") + 
  ylab("Percent of subset") +
  labs(fill = "Job Revoked/Not Revoked") +
  scale_x_discrete(labels = wrap_format(10)) +
  theme_minimal() +
  scale_fill_brewer(palette= "Pastel1") +
  coord_flip()


#-------------------------------Mental Health----------------------------------#
#------------------------------------------------------------------------------#

#Reported Changes in Mental Health
mental_health_plot <- data %>%
  select ("Difficulty concentrating on daily tasks" = mh_concentration,
          "Lack of motivation to do daily tasks" = mh_motivation,
          "Anxiety" = mh_anxiety,
          "Feeling sad or depressed" = mh_sad) %>%
  pivot_longer(1:4, names_to = 'Experience', values_to = "value") %>%
  mutate(Less_often = ifelse(str_detect(value, "Less often"),1,0),
         About_same = ifelse(str_detect(value, "About the same"),1,0),
         More_often = ifelse(str_detect(value, "More often"),1,0),
         Not_applicable = ifelse(str_detect(value, "Not applicable"),1,0)) %>%
  group_by(Experience, value) %>%
  summarise(Less_often = sum(Less_often),
            About_same = sum(About_same),
            More_often = sum(More_often),
            Not_applicable = sum(Not_applicable)) %>%
  mutate(Total = Less_often + About_same + More_often + Not_applicable) %>%
  mutate(Percent = round(100*(Total/sum(Total)),2)) %>% 
  select(Experience, value, Total, Percent)

mental_health_plot <- mental_health_plot %>%
  ggplot(aes(x = factor(Experience), y = Percent, fill = value)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Percent of subset", fill = NULL) +
  coord_flip() +
  theme_minimal() + 
  scale_fill_brewer(palette= "Set2") +
  scale_x_discrete(labels = wrap_format(25)) +
  theme(legend.title = element_blank())


#Changes in alcohol and cannabis use by age
data <- data %>% 
  mutate(age21_category = ifelse(age < 21,"Under 21", "21 or older" ))

DrugConsumption_v_age_plot <- data %>%
  select ("Alcohol" = act_alcohol,
          "Marijuana" = act_cannabis,
          "Other Drugs" = act_drugs,
          age21_category) %>%
  pivot_longer(1:3, names_to = 'Experience', values_to = "value") %>%
  mutate(Less = ifelse(str_detect(value, "Less"),1,0),
         About_same = ifelse(str_detect(value, "About the same"),1,0),
         More = ifelse(str_detect(value, "More"),1,0),
         Not_applicable = ifelse(str_detect(value, "Not applicable"),1,0)) %>%
  group_by(Experience, value ,age21_category) %>%
  summarise(Less = sum(Less),
            About_same = sum(About_same),
            More = sum(More),
            Not_applicable = sum(Not_applicable)) %>%
  mutate(Total = Less + About_same + More + Not_applicable) %>%
  mutate(Percent = round(100*(Total/sum(Total)),2))

DrugConsumption_v_age_plot <- DrugConsumption_v_age_plot %>%
  ggplot(aes(x = value, y = Percent, fill = age21_category)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Percent of subset", fill = NULL) +
  coord_flip() +
  theme_minimal() + 
  scale_fill_brewer(palette= "Set2") +
  scale_x_discrete(labels = wrap_format(10)) +
  theme(legend.title = element_blank()) +
  facet_wrap(~Experience)

#---------------------Social Distancing in Community---------------------------#
#------------------------------------------------------------------------------#

# Social Interaction Activities
data <- data %>% 
  mutate(act_chat2 = ifelse(str_detect(act_chat, "Less"), "Less often",
                            ifelse(str_detect(act_chat, "More"), "More often",
                                   act_chat)))
         
Social_Interaction_Plot <- data %>%
  select ("Visit Family and Friends" = act_visit,
          "Messaging, Video Chat, and Phone Calls" = act_chat2) %>%
  pivot_longer(1:2, names_to = 'Experience', values_to = "value") %>%
  mutate(Same = ifelse(str_detect(value, "About the same"),1,0),
         Less = ifelse(str_detect(value, "Less often"),1,0),
         More = ifelse(str_detect(value, "More often"),1,0),
         Not_applicable = ifelse(str_detect(value, "Not applicable"),1,0)) %>%
  group_by(Experience, value ) %>%
  summarise(Same = sum(Same),
            Less = sum(Less),
            More = sum(More),
            Not_applicable = sum(Not_applicable)) %>%
  mutate(Total = Same + Less + More + Not_applicable) %>%
  mutate(Percent = round(100*(Total/sum(Total)),2))

Social_Interaction_Plot <- Social_Interaction_Plot %>%
  ggplot(aes(x = value, y = Total, fill = value)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Number Of Participant", fill = NULL) +
  coord_flip() +
  theme_minimal() + 
  scale_fill_brewer(palette= "Pastel2") +
  scale_x_discrete(labels = wrap_format(10)) +
  theme(legend.title = element_blank()) +
  facet_wrap(~Experience)

#Entertainment
SocialD_Entertainment_Plot <- data %>%
  select ("Watch movies or TV" = act_tv,
          "Reading" = act_read,
          "Playing Games (video, board, card, etc.)" = act_games) %>%
  pivot_longer(1:3, names_to = 'Experience', values_to = "value") %>%
  mutate(Same = ifelse(str_detect(value, "About the same"),1,0),
         Less = ifelse(str_detect(value, "Less"),1,0),
         More = ifelse(str_detect(value, "More"),1,0),
         Not_applicable = ifelse(str_detect(value, "Not applicable"),1,0)) %>%
  group_by(Experience, value ) %>%
  summarise(Same = sum(Same),
            Less = sum(Less),
            More = sum(More),
            Not_applicable = sum(Not_applicable)) %>%
  mutate(Total = Same + Less + More + Not_applicable) %>%
  mutate(Percent = round(100*(Total/sum(Total)),2))

SocialD_Entertainment_Plot <- SocialD_Entertainment_Plot %>%
  ggplot(aes(x = Experience, y = Total, fill = value)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Number Of Participant", fill = NULL) +
  coord_flip() +
  theme_minimal() + 
  scale_fill_brewer(palette= "Pastel2") +
  scale_x_discrete(labels = wrap_format(40)) +
  theme(legend.title = element_blank()) 

#Other changes in life style and behaviors
SocialD_OtherBehaviors_Plot <- data %>%
  select ("Visit the grocery" = act_grocery,
          "Food delivery or take out" = act_takeout,
          "Check the news" = act_news) %>%
  pivot_longer(1:3, names_to = 'Experience', values_to = "value") %>%
  mutate(Same = ifelse(str_detect(value, "About the same"),1,0),
         Less = ifelse(str_detect(value, "Less"),1,0),
         More = ifelse(str_detect(value, "More"),1,0),
         Not_applicable = ifelse(str_detect(value, "Not applicable"),1,0)) %>%
  group_by(Experience, value ) %>%
  summarise(Same = sum(Same),
            Less = sum(Less),
            More = sum(More),
            Not_applicable = sum(Not_applicable)) %>%
  mutate(Total = Same + Less + More + Not_applicable) %>%
  mutate(Percent = round(100*(Total/sum(Total)),2))

SocialD_OtherBehaviors_Plot <- SocialD_OtherBehaviors_Plot %>%
  ggplot(aes(x = Experience, y = Total, fill = value)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Number Of Participant", fill = NULL) +
  coord_flip() +
  theme_minimal() + 
  scale_fill_brewer(palette= "Pastel2") +
  scale_x_discrete(labels = wrap_format(40)) +
  theme(legend.title = element_blank()) 


#Views on social distancing
Views_Social_Distance_Plot <- data %>%
  select ("Stay-at-home order is a necessary precaution right now" = nec_stay_home,
          "Social Distancing is a necessary precaution right now" = nec_social_dist,
          "People in my community are practicing social distancing" = com_social_dist,
          "People in my community are following stay-at-home order" = com_stay_home,
          "Overal, my response to COVID-19 has been appropriate" = my_response ) %>%
  pivot_longer(1:5, names_to = 'Experience', values_to = "value") %>%
  mutate(Agree = ifelse(str_detect(value, "Agree"),1,0),
         Disagree = ifelse(str_detect(value, "Disagree"),1,0),
         NeitherNor = ifelse(str_detect(value, "Neither Agree nor Disagree"),1,0),
         Not_applicable = ifelse(str_detect(value, "Not applicable"),1,0)) %>%
  group_by(Experience, value ) %>%
  summarise(Agree = sum(Agree),
            Disagree = sum(Disagree),
            NeitherNor = sum(NeitherNor),
            Not_applicable = sum(Not_applicable)) %>%
  mutate(Total = Agree + Disagree + NeitherNor + Not_applicable) %>%
  mutate(Percent = round(100*(Total/sum(Total)),2))

Views_Social_Distance_Plot <- Views_Social_Distance_Plot %>%
  ggplot(aes(x = factor(Experience), y = Total, fill = value)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Number Of Participants" , fill = NULL) +
  coord_flip() +
  theme_minimal() + 
  scale_fill_brewer(palette= "Pastel2") +
  scale_x_discrete(labels = wrap_format(40)) +
  theme(legend.title = element_blank())

#--------------------------------Finances--------------------------------------#
#------------------------------------------------------------------------------#

#Education Funding changes
pct_t <- function(x) { round(100*(x/sum(x)), 2) } 

funds <- data %>% 
  group_by(fund_parents) %>% 
  tally() %>% 
  mutate(n = pct_t(n)) %>% 
  pivot_wider(1:2, names_from = "fund_parents", values_from = n) %>% 
  mutate(f_source = "Parents/Guardians") %>% 
  bind_rows(data %>% 
              group_by(fund_partner) %>% 
              tally() %>% 
              mutate(n = pct_t(n)) %>% 
              pivot_wider(1:2, names_from = "fund_partner", values_from = n) %>% 
              mutate(f_source = "Partner")) %>% 
  bind_rows(data %>% 
              group_by(fund_gov) %>% 
              tally() %>% 
              mutate(n = pct_t(n)) %>% 
              pivot_wider(1:2, names_from = "fund_gov", values_from = n) %>% 
              mutate(f_source = "Government loans/grants")) %>% 
  bind_rows(data %>% 
              group_by(fund_private) %>% 
              tally() %>% 
              mutate(n = pct_t(n)) %>% 
              pivot_wider(1:2, names_from = "fund_private", values_from = n) %>% 
              mutate(f_source = "Private loans")) %>% 
  bind_rows(data %>% 
              group_by(fund_scholarship) %>% 
              tally() %>% 
              mutate(n = pct_t(n)) %>% 
              pivot_wider(1:2, names_from = "fund_scholarship", values_from = n) %>% 
              mutate(f_source = "Scholarships"))  %>% 
  bind_rows(data %>% 
              group_by(fund_income) %>% 
              tally() %>% 
              mutate(n = pct_t(n)) %>% 
              pivot_wider(1:2, names_from = "fund_income", values_from = n) %>% 
              mutate(f_source = "Personal Income"))

funds %>% 
  select(`Funding Source` = f_source, Decreased, 
         `No change`, Increased, `Not Applicable`) %>% 
  arrange(desc(Decreased))


#Difficulty Paying Expenses
Difficulty_Paying_Expenses_Plot <- data %>%
  select (`Healthcare` = dif_healthcare,
          `Groceries` = dif_groceries,
          `Transportation` = dif_trans,
          `Rent and/or Utilities` = dif_rent) %>%
  pivot_longer(1:4, names_to = 'Experience', values_to = "value") %>%
  mutate(Less_difficulty = ifelse(str_detect(value, "Less difficulty"),1,0),
         No_change = ifelse(str_detect(value, "No change"),1,0),
         More_difficulty = ifelse(str_detect(value, "More difficulty"),1,0),
         Not_Applicable = ifelse(str_detect(value, "Not Applicable"),1,0)) %>%
  group_by(Experience, value) %>%
  summarise(Less_difficulty = sum(Less_difficulty),
            No_change = sum(No_change),
            More_difficulty = sum(More_difficulty),
            Not_Applicable = sum(Not_Applicable)) %>%
  mutate(Total = Less_difficulty + No_change + More_difficulty + Not_Applicable) %>%
  select(Experience, value, Total)

Difficulty_Paying_Expenses_Plot <- Difficulty_Paying_Expenses_Plot %>%
  ggplot(aes(x = factor(Experience), y = Total, fill = value)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Number of participants", fill = NULL) + coord_flip() + 
  theme_minimal()+
  scale_x_discrete(labels = wrap_format(10)) +
  theme(legend.title = element_blank())

#More difficulty by change in income
Difficulty_By_IncomeChange <- data %>%
  select (`Healthcare` = dif_healthcare,
          `Groceries` = dif_groceries,
          `Transportation` = dif_trans,
          `Rent and/or Utilities` = dif_rent,
          personal_income = fund_income) %>%
  pivot_longer(1:4, names_to = 'Experience', values_to = "value") %>%
  mutate(Less_difficulty = ifelse(str_detect(value, "Less difficulty"),1,0),
         No_change = ifelse(str_detect(value, "No change"),1,0),
         More_difficulty = ifelse(str_detect(value, "More difficulty"),1,0),
         Not_Applicable = ifelse(str_detect(value, "Not Applicable"),1,0)) %>%
  group_by(Experience, value, personal_income) %>%
  summarise(Less_difficulty = sum(Less_difficulty),
            No_change = sum(No_change),
            More_difficulty = sum(More_difficulty),
            Not_Applicable = sum(Not_Applicable)) %>%
  mutate(Total = Less_difficulty + No_change + More_difficulty + Not_Applicable) %>%
  select(Experience, value, Total, personal_income)

Difficulty_By_IncomeChange <- Difficulty_By_IncomeChange %>%
  filter(value == "More difficulty") %>% 
  ggplot(aes(x = factor(Experience), y = Total, fill = personal_income)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Number of Participants Experincing More Difficulty", 
       fill = "Change In Personal Income") + 
  coord_flip() + 
  theme_minimal()+
  scale_x_discrete(labels = wrap_format(10)) 

#More difficulty by First Generation
Difficulty_By_Generation <- data %>%
  select (`Healthcare` = dif_healthcare,
          `Groceries` = dif_groceries,
          `Transportation` = dif_trans,
          `Rent and/or Utilities` = dif_rent,
          First_G = first_gen) %>%
  pivot_longer(1:4, names_to = 'Experience', values_to = "value") %>%
  mutate(Less_difficulty = ifelse(str_detect(value, "Less difficulty"),1,0),
         No_change = ifelse(str_detect(value, "No change"),1,0),
         More_difficulty = ifelse(str_detect(value, "More difficulty"),1,0),
         Not_Applicable = ifelse(str_detect(value, "Not Applicable"),1,0)) %>%
  group_by(Experience, value, First_G) %>%
  summarise(Less_difficulty = sum(Less_difficulty),
            No_change = sum(No_change),
            More_difficulty = sum(More_difficulty),
            Not_Applicable = sum(Not_Applicable)) %>%
  mutate(Total = Less_difficulty + No_change + More_difficulty + Not_Applicable) %>%
  select(Experience, value, Total, First_G)

Difficulty_By_Generation <- Difficulty_By_Generation %>%
  filter(value == "More difficulty") %>% 
  ggplot(aes(x = factor(Experience), y = Total, fill = First_G)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Number of Participants Experincing More Difficulty", 
       fill = "First Generation") + 
  coord_flip() + 
  theme_minimal()+
  scale_x_discrete(labels = wrap_format(10)) 


#-------------------------------Other Stuff------------------------------------#
#------------------------------------------------------------------------------#

#Percieved credability of Various news sources (summary table)
tab <- data %>% 
  group_by(cred_natl_news) %>% 
  tally() %>% 
  mutate(n = round(100*(n/sum(n)), 2)) %>% 
  rename(Response = cred_natl_news,
         `National News` = n) %>% 
  left_join(data %>% 
              group_by(cred_local_news) %>% 
              tally() %>% 
              mutate(n = round(100*(n/sum(n)), 2)) %>% 
              rename(Response = cred_local_news,
                     `Local News` = n)) %>% 
  left_join(data %>% 
              group_by(cred_city) %>% 
              tally() %>% 
              mutate(n = round(100*(n/sum(n)), 2)) %>% 
              rename(Response = cred_city,
                     City = n)) %>% 
  left_join(data %>% 
              group_by(cred_state) %>% 
              tally() %>% 
              mutate(n = round(100*(n/sum(n)), 2)) %>% 
              rename(Response = cred_state,
                     State = n)) %>% 
  left_join(data %>% 
              group_by(cred_federal) %>% 
              filter(leave_usa != "Yes") %>% 
              tally() %>% 
              mutate(n = round(100*(n/sum(n)), 2)) %>% 
              rename(Response = cred_federal,
                     Federal = n)) 

NewsCredabilityTable <- tab %>% 
  bind_rows(tibble(Response = "Net Credibility",
                   `National News` = as.numeric(tab[4,2] - tab[2,2]),
                   `Local News` =  as.numeric(tab[4,3] - tab[2,3]),
                   City = as.numeric(tab[4,4] - tab[2,4]),
                   State = as.numeric(tab[4,5] - tab[2,5]),
                   Federal = as.numeric(tab[4,6] - tab[2,6]))) %>% 
  mutate(Response = fct_relevel(Response, 
                                levels = c("Yes", "It depends", "No", 
                                           "Unsure/not applicable",
                                           "Net Credibility"))) %>% 
  arrange(Response)

#News source usage
pct_m <- function(x) { round(100*mean(x), 2) }

NewsSourceUsageTable <- data %>% 
  summarize(`National TV` = pct_m(ns_natl_tv),
            Newspapers = pct_m(ns_newspaper),
            `Local TV` = pct_m(ns_local_tv),
            Government = pct_m(ns_government),
            `Social Media` = pct_m(ns_social),
            `Other Sources` = pct_m(ns_other),
            Friends = pct_m(ns_friends),
            Family = pct_m(ns_family),
            Workplace = pct_m(ns_employer)) %>% 
  pivot_longer(1:9, 
               names_to = "Information Source", 
               values_to = "Percent of sample") %>% 
  arrange(desc(`Percent of sample`))

