
library(googledrive)
library(tidyverse)
library(lubridate) 

drive_download(file = as_id("13f2sQRjEvC1UYfhSWJBPC8gozGzv-4m_SPeb-gKQtww"),
               overwrite = T, type = "csv")

#_____________________________________________________________________________________

data <- read_csv("survey_responses.csv") 

q_list <- names(data)

codebook <- tibble(question = q_list) %>% 
  mutate(var_id = seq_along(1:length(q_list)))

data <- data %>% 
  rename(time = q_list[1],
         wa_undergrad = q_list[2],
         school = q_list[3],
         age = q_list[4],
         race = q_list[5],
         hispanic = q_list[6],
         gender = q_list[7],
         residency = q_list[8],
         par_bach = q_list[9],
         pol_views = q_list[10],
         
         before_secure = q_list[11],
         
         # bs_ for Before and Secure
         bs_zip = q_list[12],        
         bs_housing = q_list[13],   
         bs_roommates = q_list[14],
         bs_internet = q_list[15],
         bs_computer = q_list[16],
         
         # bi_ for Before and Insecure
         bi_zip = q_list[17],       
         bi_situation = q_list[18],
         bi_internet = q_list[19],
         bi_computer = q_list[20],
         
         housing_change = q_list[21],
         leave_usa = q_list[22],
         current_secure = q_list[23],
         
         # cs_ for Current and Secure
         cs_zip = q_list[24],         
         cs_housing = q_list[25],   
         cs_roommates = q_list[26],
         cs_internet = q_list[27],
         cs_computer = q_list[28],
         
         # ci_ for Current and Insecure
         ci_zip = q_list[29],       
         ci_situation = q_list[30],
         ci_internet = q_list[31],
         ci_computer = q_list[32], 
         
         # online learning 
         cancelled_course = q_list[33],
         vol_change = q_list[34],
         workspace_b = q_list[35],  # workspace before
         workspace_c = q_list[36],  # workspace currently
         devices = q_list[37],
         slow_tech = q_list[38],
         
         # compared to before was there a change in...
         edu_focus = q_list[39],
         edu_motivation = q_list[40],
         edu_time = q_list[41],
         edu_attendance  = q_list[42],
         edu_participate = q_list[43],
         
         edu_tutor = q_list[44],
         edu_office = q_list[45],
         edu_advising = q_list[46],
         edu_drs = q_list[47],
         edu_career = q_list[48],
         
         edu_comm = q_list[49],
         edu_issues = q_list[50],
         edu_needs = q_list[51],
         
         learning_long = q_list[52], # long answer on online learning 
         
         # education characteristics
         sem_qtr = q_list[53],
         grade = q_list[54],
         major = q_list[55],
         
         chng_grad = q_list[56],
         post_grad = q_list[57],
         chng_post_grad = q_list[58],
         grad_2020 = q_list[59],
         job_revoke = q_list[60],
         
         # mh_ for mental health
         mh_concentration = q_list[61],
         mh_motivation = q_list[62],
         mh_anxiety = q_list[63],
         mh_sad = q_list[64],
         
         # act_ for activities
         act_exercise = q_list[65],
         act_sleep = q_list[66],
         act_tv = q_list[67],
         act_games = q_list[68],
         act_read = q_list[69],
         act_chat = q_list[70],
         act_alcohol = q_list[71],
         act_cannabis = q_list[72],
         act_drugs = q_list[73],
         
         # covid-19 response questions
         com_social_dist = q_list[74],
         com_stay_home = q_list[75],
         nec_social_dist = q_list[76],
         nec_stay_home = q_list[77],
         my_response = q_list[78],
         
         # response-related activities 
         act_grocery = q_list[79],
         act_park = q_list[80],
         act_takeout = q_list[81],
         act_visit = q_list[82],
         act_news = q_list[83],
         
         news_source = q_list[84],   
         
         dif_healthcare = q_list[85],
         dif_groceries = q_list[86],
         dif_trans = q_list[87],
         dif_rent = q_list[88],
         
         cred_natl_news = q_list[89],
         cred_local_news = q_list[90],
         cred_city = q_list[91],
         cred_state = q_list[92],
         cred_federal = q_list[93],
         
         # employment
         employed_before = q_list[94],
         employ_chng = q_list[95],
         unem_insur = q_list[96],
         new_job = q_list[97],
         
         # education funding
         fund_parents = q_list[98],
         fund_partner = q_list[99],
         fund_gov = q_list[100],
         fund_private = q_list[101],
         fund_scholarship = q_list[102],
         fund_income = q_list[103],
         cares_act = q_list[104],
         
         # open-ended questions
         funding_long = q_list[105],
         other_long = q_list[106], 
         
         # email questions
         interview = q_list[107],
         gift_card = q_list[108]) 

codebook <- codebook %>% 
  bind_cols(variable = names(data)) %>% 
  select(var_id, variable, question) 

write_csv(codebook, "codebook.csv")

data <- data %>% 
  filter(wa_undergrad == "Yes") %>% 
  mutate(school = str_to_lower(school),
         school = case_when(str_detect(school, "(university of washington|uw)") & str_detect(school, "seattle") ~ "uw seattle",
                            str_detect(school, "washington") & str_detect(school, "tacoma") ~ "uw tacoma",
                            str_detect(school, "washington") & str_detect(school, "bothell") ~ "uw bothell",
                            str_detect(school, "uwb") ~ "uw bothell", 
                            str_detect(school, "washington") & str_detect(school, "state") ~ "wsu", 
                            str_detect(school, "western") ~ "wwu", 
                            str_detect(school, "(university of washington|uw)") ~ "uw (nonspecified)",
                            school %in% c("washington", 
                                          "art & sci college",
                                          "arts and sciences", 
                                          "school of public health", 
                                          "pre-major", 
                                          "college of education and college of american ethnic studies",
                                          "college of engineering",
                                          "college of arts and sciences",
                                          "political science and communication", 
                                          "las") ~ "uw (nonspecified)",
                            school == "highline college" ~ "highline community college",
                            school == "everett" ~ "everett community college", 
                            T ~ school),
         major = tolower(major), 
         more_alcohol = ifelse(act_alcohol == "More", 1, 0), 
         ideology = pol_views - 4,
         
         # new variables created for the "descriptive_stats.Rmd" file:
         
         race_white = ifelse(race == "White", 1, 0),
         race_asian = ifelse(race == "Asian", 1, 0),
         # if race is not in the list including white and asian it takes the value other
         race_other = ifelse(!race %in% c("White", "Asian"), 1, 0),
         race_3 = case_when(race_white == 1 ~ "White",
                            race_asian == 1 ~ "Asian",
                            race_other == 1 ~ "Other"),
         hisp = ifelse(hispanic == "Yes", 1, 0),
         man = ifelse(gender == "Man", 1, 0),
         citizen = ifelse(residency == "US Citizen", residency, "Non-US Citizen"),
         first_gen = ifelse(par_bach == "No", "Yes", "No"),
         
         # devices
         dev_laptop = ifelse(str_detect(devices, "Laptop"), 1, 0),
         dev_desktop = ifelse(str_detect(devices, "Desktop"), 1, 0),
         dev_tablet = ifelse(str_detect(devices, "Tablet"), 1, 0),
         dev_phone = ifelse(str_detect(devices, "Smart Phone"), 1, 0),
         
         # housing change variables 
         hc_to_parents = case_when(
           housing_change == "No" ~ 0,
           !str_detect(bs_housing, "parents") & str_detect(cs_housing, "parents") ~ 1, 
           T ~ 0),
         hc_leave_camp = case_when(
           housing_change == "No" ~ 0,
           str_detect(bs_housing, "On-campus") & !str_detect(cs_housing, "On-campus") ~ 1, 
           T ~ 0),
         hc_leave_greek = case_when(
           housing_change == "No" ~ 0,
           str_detect(bs_housing, "Fraternity") & !str_detect(cs_housing, "Fraternity") ~ 1, 
           T ~ 0),
         hc_more_mates = case_when(
           housing_change == "No" ~ 0,
           str_detect(bs_roommates, "alone") & str_detect(cs_roommates, "(other person|other people)") ~ 1,
           str_detect(bs_roommates, "person") & str_detect(cs_roommates, "other people") ~ 1,
           T ~ 0),
         hc_fewer_mates = case_when(
           housing_change == "No" ~ 0, 
           str_detect(bs_roommates, "(other person|other people)") & str_detect(cs_roommates, "alone") ~ 1,
           str_detect(bs_roommates, "other people") & str_detect(cs_roommates, "(other person|alone)") ~ 1,
           T ~ 0),
         hc_to_insecure = case_when(
           housing_change == "No" ~ 0,
           before_secure == "Yes" & current_secure == "No" ~ 1,
           T ~ 0),
         
         # this update may make *some* of the code above redundant 
         housing_b4 = ifelse(before_secure == "Yes", bs_housing, "Insecure Housing"),
         housing_after = case_when(
           housing_change == "No" & before_secure == "Yes" ~ bs_housing, 
           housing_change == "No" & before_secure == "No" ~ "Insecure Housing", 
           housing_change == "Yes" & current_secure == "Yes" ~ cs_housing,
           housing_change == "Yes" & current_secure == "No" ~ "Insecure Housing"),
         
         # this makes the categories consistent between before and after: 
         housing_b4 = case_when(str_detect(housing_b4, "parents or guardians") ~ "Parents/guardians",
                                str_detect(housing_b4, "Off-campus") ~ "Off-campus",
                                str_detect(housing_b4, "Fraternity") ~ "Greek",
                                str_detect(housing_b4, "On-campus") ~ "On-campus",
                                str_detect(housing_b4, "Insecure") ~ "Insecure"),
         housing_after = case_when(str_detect(housing_after, "parents or guardians") ~ "Parents/guardians",
                                   str_detect(housing_after, "Off-campus") ~ "Off-campus",
                                   str_detect(housing_after, "Fraternity") ~ "Greek",
                                   str_detect(housing_after, "On-campus") ~ "On-campus",
                                   str_detect(housing_after, "Insecure") ~ "Insecure"),
         mates_b4 = case_when(
           before_secure == "Yes" & str_detect(bs_roommates, "alone") ~ "Living Alone",
           before_secure == "Yes" & str_detect(bs_roommates, "one other") ~ "One other person",
           before_secure == "Yes" & str_detect(bs_roommates, "two or more") ~ "Two or more",
           T ~ "Insecure Housing"),
         
         mates_after = case_when(
           housing_change == "No" ~ mates_b4, 
           before_secure == "Yes" & str_detect(cs_roommates, "alone") ~ "Living Alone",
           before_secure == "Yes" & str_detect(cs_roommates, "one other") ~ "One other person",
           before_secure == "Yes" & str_detect(cs_roommates, "two or more") ~ "Two or more",
           T ~ "Insecure Housing"),
         
         internet_b4 = case_when(
           before_secure == "Yes" & str_detect(bs_internet, "not have") ~ "No access", 
           before_secure == "Yes" & !str_detect(bs_internet, "not have") ~ bs_internet, 
           bi_internet == "No" ~ "Stable and reliable",
           bi_internet == "Sometimes" ~ "Unreliable, unstable, or slow",
           bi_internet == "Yes" ~ "I did not have internet access"),
         
         internet_after = case_when(
           housing_change == "No" ~ internet_b4, 
           current_secure == "Yes" & str_detect(cs_internet, "not have") ~ "No access", 
           current_secure == "Yes" & !str_detect(cs_internet, "not have") ~ cs_internet, 
           ci_internet == "No" ~ "Stable and reliable",
           ci_internet == "Sometimes" ~ "Unreliable, unstable, or slow",
           ci_internet == "Yes" ~ "No access"),
         
         computer_b4 = case_when(
           before_secure == "Yes" & str_detect(bs_computer, "exclusive") ~ "Exclusive/reliable access",
           before_secure == "Yes" & str_detect(bs_computer, "share") ~ "Shared/occasional access",
           before_secure == "Yes" & str_detect(bs_computer, "did not have") ~ "No access/difficult access", 
           bi_computer == "No" ~ "Exclusive/reliable access",
           bi_computer == "Sometimes" ~ "Shared/occasional access",
           bi_computer == "Yes" ~ "No access/difficult access"), 
         
         computer_after = case_when(
           housing_change == "No" ~ computer_b4, 
           current_secure == "Yes" & str_detect(cs_computer, "exclusive") ~ "Exclusive/reliable access",
           current_secure == "Yes" & str_detect(cs_computer, "share") ~ "Shared/occasional access",
           current_secure == "Yes" & str_detect(cs_computer, "do not have") ~ "No access/difficult access", 
           ci_computer == "No" ~ "Exclusive/reliable access",
           ci_computer == "Sometimes" ~ "Shared/occasional access",
           ci_computer == "Yes" ~ "No access/difficult access"),
         date_time = mdy_hms(time),
         date = date(date_time),
         current_zip = case_when(
           housing_change == "Yes" & current_secure == "Yes" ~  cs_zip,
           housing_change == "Yes" & current_secure == "No" ~  ci_zip,
           housing_change == "No" & before_secure == "Yes" ~  bs_zip,
           housing_change == "No" & before_secure == "No" ~  bi_zip),
         current_zip = as.numeric(current_zip),
         workspace_b = case_when(str_detect(workspace_b, "friend") ~ "Friend's place",
                                 str_detect(workspace_b, "My place") ~ "My place",
                                 str_detect(workspace_c, "lol") ~ "NA",
                                 str_detect(workspace_b, "University") ~ "University space",
                                 str_detect(workspace_b, "Coffee") ~ "Private establishment",
                                 T ~ workspace_b),
         workspace_c = case_when(str_detect(workspace_c, "friend") ~ "Friend's place",
                                 str_detect(workspace_c, "My place") ~ "My place",
                                 str_detect(workspace_c, "University") ~ "University space",
                                 str_detect(workspace_c, "Coffee") ~ "Private establishment",
                                 str_detect(workspace_c, "lol") ~ "NA",
                                 T ~ workspace_c),
         # news source
         ns_natl_tv = ifelse(str_detect(news_source, "National"), 1, 0),
         ns_newspaper = ifelse(str_detect(news_source, "(Newspaper|euters)"), 1, 0),
         ns_local_tv = ifelse(str_detect(news_source, "Local TV"), 1, 0),
         ns_government = ifelse(str_detect(news_source, "Government"), 1, 0),
         ns_social = ifelse(str_detect(news_source, "(Social|itter)"), 1, 0),
         ns_other = ifelse(str_detect(news_source, "(Podcasts|eddit)"), 1, 0),
         ns_friends = ifelse(str_detect(news_source, "Friends"), 1, 0),
         ns_family = ifelse(str_detect(news_source, "Family"), 1, 0),
         ns_employer = ifelse(str_detect(news_source, "ork"), 1, 0)) %>% 
  filter(school != "oregon state university") 

# we need to remove multiple responses from the same people 

yes_email <- data %>% distinct(gift_card, .keep_all = T) %>% filter(!is.na(gift_card)) 
no_email <- data %>% filter(is.na(gift_card)) 
data <- yes_email %>% bind_rows(no_email)

# create a CSV for the results 

write_csv(data, "survey_data_renamed.csv") 

# create a CSV list with the emails for interviews

write_csv(data %>% 
            select(interview) %>% 
            distinct(interview),
          "interview_list.csv") 
