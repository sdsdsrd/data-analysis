# 1.
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)

k_wf_bsy <- read.csv("k_Wf.csv")
k_wf_bsy
cb <- read_excel("Codebook.xlsx", sheet = 2)
cb


# 2.
k_wf_bsy <- k_wf_bsy %>% select(h10_g3, h10_g4, h10_g10, h10_g11, p1002_8aq1, h10_eco9, h10_reg7)
k_wf_bsy

k_wf_bsy <- rename(k_wf_bsy, sex = h10_g3)
k_wf_bsy <- rename(k_wf_bsy, birth = h10_g4)
k_wf_bsy <- rename(k_wf_bsy, marriage = h10_g10)
k_wf_bsy <- rename(k_wf_bsy, religion = h10_g11)
k_wf_bsy <- rename(k_wf_bsy, income = p1002_8aq1)
k_wf_bsy <- rename(k_wf_bsy, code_job = h10_eco9)
k_wf_bsy <- rename(k_wf_bsy, code_region = h10_reg7)
k_wf_bsy


# 3.
k_wf_bsy <- left_join(k_wf_bsy, cb, by = "code_job")
k_wf_bsy

k_wf_bsy <- k_wf_bsy %>%
  mutate(sex = ifelse(sex != 1 & sex != 2 | sex == 9, NA, sex))
k_wf_bsy <- k_wf_bsy %>%
  mutate(birth = ifelse(birth < 1900 | birth > 2014 | birth == 9999, NA, birth))
k_wf_bsy <- k_wf_bsy %>%
  mutate(marriage = ifelse(marriage < 0 | marriage > 6 | marriage == 9, NA, marriage))
k_wf_bsy <- k_wf_bsy %>%
  mutate(religion = ifelse(religion != 1 & religion != 2 | religion == 9, NA, religion))
k_wf_bsy <- k_wf_bsy %>%
  mutate(income = ifelse(income < 1 | income > 9998 | income == 9999, NA, income))
k_wf_bsy <- k_wf_bsy %>%
  mutate(code_job = ifelse(!is.na(code_job) & is.na(job), NA, code_job))
k_wf_bsy <- k_wf_bsy %>%
  mutate(code_region = ifelse(code_region < 1 | code_region > 7, NA, code_region))

k_wf_bsy

table(is.na(k_wf_bsy$sex))
is.na(k_wf_bsy$sex)
table(is.na(k_wf_bsy$birth))
is.na(k_wf_bsy$birth)
table(is.na(k_wf_bsy$marriage))
is.na(k_wf_bsy$marriage)
table(is.na(k_wf_bsy$religion))
is.na(k_wf_bsy$religion)
table(is.na(k_wf_bsy$income))
is.na(k_wf_bsy$income)
table(is.na(k_wf_bsy$code_job))
is.na(k_wf_bsy$code_job)
table(is.na(k_wf_bsy$code_region))
is.na(k_wf_bsy$code_region)


# 4.
k_wf_bsy$sex_t <- ifelse(k_wf_bsy$sex == 1, "M", "F")
k_wf_bsy

k_wf_bsy$age <- 2018 - k_wf_bsy$birth
k_wf_bsy

k_wf_bsy$age_g <- ifelse(k_wf_bsy$age >= 110, "110",
                         ifelse(k_wf_bsy$age >= 100, "100",
                                ifelse(k_wf_bsy$age >= 90, "90",
                                       ifelse(k_wf_bsy$age >= 80, "80",
                                              ifelse(k_wf_bsy$age >= 70, "70",
                                                     ifelse(k_wf_bsy$age >= 60, "60",
                                                            ifelse(k_wf_bsy$age >=50, "50",
                                                                   ifelse(k_wf_bsy$age >= 40, "40",
                                                                          ifelse(k_wf_bsy$age >= 30, "30",
                                                                                 ifelse(k_wf_bsy$age >= 20, "20", "10"))))))))))
k_wf_bsy

k_wf_bsy <- k_wf_bsy %>%
  mutate(income_g = ifelse(income >= 1000, "1000이상",
                              ifelse(income >= 900, "900이상 1000미만",
                                     ifelse(income >= 800, "800이상 900미만",
                                            ifelse(income >= 700, "700이상 800미만",
                                                   ifelse(income >= 600, "600이상 700미만",
                                                          ifelse(income >= 500, "500이상 600미만",
                                                                 ifelse(income >= 400, "400이상 500미만",
                                                                        ifelse(income >= 300, "300이상 400미만",
                                                                               ifelse(income >= 200, "200이상 300미만",
                                                                                      ifelse(income >= 100, "100이상 200미만", "1이상 100미만")))))))))))
k_wf_bsy

k_wf_bsy <- k_wf_bsy %>%
  mutate(region = ifelse(code_region == 1, "서울",
                         ifelse(code_region == 2, "수도권(인천/경기)",
                                ifelse(code_region == 3, "부산/경남/울산",
                                       ifelse(code_region == 4, "대구/경북",
                                              ifelse(code_region == 5, "대전/충남",
                                                     ifelse(code_region == 6, "강원/충북",
                                                            ifelse(code_region == 7, "광주/전남/전북/제주도", NA))))))))
k_wf_bsy


# 5.
sage_g_ic <- k_wf_bsy %>%
  filter(region == "서울") %>%
  filter(!is.na(income)) %>%
  group_by(age_g, sex_t) %>%
  summarise(mean_income = mean(income))
sage_g_ic

ggplot(data = sage_g_ic, aes(x = age_g, y = mean_income, fill = sex_t)) + geom_col()


# 6.
age_ic <- k_wf_bsy %>%
  filter(!is.na(income)) %>%
  group_by(age, sex_t) %>%
  summarise(mean_income = mean(income))
age_ic

ggplot(data = age_ic, aes(x = age, y = mean_income, color = sex_t)) + geom_line()


# 7.
f_job_ic <- k_wf_bsy %>%
  filter(sex_t == "F") %>%
  filter(!is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income)) %>%
  arrange(desc(mean_income)) %>%
  head(10)
f_job_ic


# 8.
m_job_mr <- k_wf_bsy %>%
  filter(sex_t == "M") %>%
  filter(!is.na(income_g)) %>%
  mutate(is_mr = ifelse(marriage == 1 | marriage == 2 | marriage == 3 | marriage == 4, 1, 0)) %>%
  group_by(income_g) %>%
  summarise(total = n(),
            mr = sum(is_mr),
            ratio_mr = mr / total)
m_job_mr

# 8. (2)
k_wf_bsy$group_marriage <- ifelse(k_wf_bsy$marriage %in% c(1, 2, 3, 4), "o", "x")

m_job_mr <- k_wf_bsy %>%
  filter(sex_t=="M") %>%
  filter(!is.na(group_marriage)) %>%
  filter(!is.na(income_g)) %>%
  group_by(income_g,group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(ratio = n/tot_group) %>%
  filter(group_marriage=="o")
m_job_mr


# 9.
k_wf_bsy <- k_wf_bsy %>%
  mutate(age_group = ifelse(age >= 60, "old",
                            ifelse(age >= 30, "middle",
                                   ifelse(age >= 15, "young", "child"))))
k_wf_bsy

rg_group <- k_wf_bsy %>%
  group_by(region, age_group) %>%
  summarise(count = n()) %>%
  group_by(region) %>%
  mutate(total = sum(count),
         ratio = count / total)
rg_group

ggplot(data = rg_group, aes(x = region, y = ratio, fill = age_group)) + geom_col() + coord_flip()
