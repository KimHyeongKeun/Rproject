install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)


raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",to.data.frame = T)
welfare<- raw_welfare

View(welfare)
head(welfare)
dim(welfare)

welfare <- rename(welfare,
                  sex = h10_g3,  #성별 
                  birth = h10_g4,  # 태어난 연도
                  marriage = h10_g10,  # 혼인상태
                  religion = h10_g11,  #종교
                  income = p1002_8aq1,  #월급
                  code_job = h10_eco9,   #직업 코드
                  code_region = h10_reg7)  #지역 코드
head(welfare,3)
welfare$age <-2015 - welfare$birth +1
 head(welfare,3)
summary(welfare$age)
qplot(welfare$age)


#성별에 따른 월급 차이

class(welfare$sex)
class(welfare$income)
table(welfare$sex)  #빈도테이블 

welfare$sex <- ifelse(welfare$sex ==9, NA, welfare$sex)
table(is.na(welfare$sex))

welfare$sex <- ifelse(welfare$sex ==1, "male", "female")
table(welfare$sex)

qplot(welfare$sex)  #여자의 비율이 남자의 비율보다 높다.

#변수 검토및 전처리 
ggplot(data = welfare, aes(x=sex))+geom_bar()

table(welfare$sex)

class(welfare$income)
summary(welfare$income)

#연봉이 얼마나 쏠려있는지 
qplot(welfare$income) + xlim(0,1000) 
#값이 0인 것이 존재한다는 것은 이상치가 존재한다는 것이기 때문에 결측치로 바꾸어 줘야한다.모름 9999도 결측치로 바꿔줘야한다.
welfare$income <- ifelse(welfare$income %in% c(0,9999),NA,welfare$income)
table(is.na(welfare$income))

sex_income <- welfare %>%
  filter(!is.na(income))%>%
  group_by(sex)%>%
  summarise(mean_income = mean(income))

sex_income

#geom_bar()
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

#나이와 월급의 관계

class(welfare$birth)
#가장 어린 사람 2014년생 가장 나이 많은 사람 1907년생
summary(welfare$birth)
qplot(welfare$birth)
table(is.na(welfare$birth))
welfare$birth <- ifelse(welfare$birth == 9999, NA,welfare$birth)

welfare$age <- 2015 - welfare$birth +1
summary(welfare$age)
qplot(welfare$age)

age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))
age_income

ggplot(data = age_income, aes(x =age, y = mean_income))+geom_line()

welfare$age <- 2015 - welfare$birth +1       
head(welfare,3)
summary(welfare$age)
qplot(welfare$age)

#연령대에 따른 임금 
welfare <- welfare%>%
  mutate(ageg = ifelse(age <30, "young", ifelse(age <=59, "middle", "old")))
summary(welfare$ageg)
table(welfare$ageg)
qplot(welfare$ageg)

class(welfare$income)
summary(welfare$income)
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA,welfare$income)
table(is.na(welfare$income))

ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarize(mean_income = mean(income))
ageg_income
ggplot(data = ageg_income, aes(x = ageg , y = mean_income)) + geom_col()+
  scale_x_discrete(limits = c("young", "middle", "old"))

#연령대 및 성별 월급 차이

class(welfare$sex)
table(welfare$sex)
welfare$sex <- ifelse(welfare$sex == 9, NA ,welfare$sex)
welfare$sex <- ifelse(welfare$sex ==1, "male", "female")
table(welfare$sex)

sex_income <-welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg,sex) %>%
  summarise(mean_income = mean(income))

sex_income

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex))+geom_col(position = "dodge")+
scale_x_discrete(limits = c("young", "middle","old"))

sex_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age,sex) %>%
  summarise(mean_income = mean(income))
sex_age
ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex))+geom_line()


#직업별 월급 차이
class(welfare$code_job)
table(welfare$code_job)

library(readxl)
list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
list_job

welfare <- left_join(welfare, list_job, id = "code_job")
welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job, job) %>%
  head(6)

job_income <- welfare %>%
  filter (!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income))
job_income

# 월급을 내림차순 정력, 상위  10개 추출

top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
top10

ggplot(data = top10, aes(x = reorder(job, mean_income),y= mean_income)) +geom_col()+
coord_flip()

#하위 10개 
bottom10 <- job_income %>%
  arrange(mean_income) %>%
  head(10)
bottom10

ggplot(data = bottom10, aes(x = reorder(job, -mean_income), y = mean_income))+geom_col()+
  coord_flip() +ylim(0,850)
#성별 직업 빈도
job_female <- welfare %>%
  filter(!is.na(job) & sex =="female") %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_female

ggplot(data = job_male, aes(x = reorder(job,n),y = n)) +geom_col() + coord_flip()




#프로젝트 (1)  지역별 월급 차이

class(welfare$code_region)
table(welfare$code_region)
list_region <- data.frame(code_region = c(1:7), region = c("서울", "수도권(인천/경기)","부산/경남/울산","대구/경북","대전/충남","강원,충북","광주/전남/전북/제주도"))
list_region


welfare <-left_join(welfare, list_region, id = "code_region")
welfare %>%
  select(code_region, region) %>%
  head


region_income <- welfare %>%
  group_by(region,income) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100,2))
region_income
ggplot(data = region_income, aes(x= region, y = pct, fill = income)) +geom_col()+coord_flip()

list_order_income <- region_income %>%
  filter(income =="")









#프로젝트(2)  나이에 따른 이혼율 

class(welfare$birth)
class(welfare$marriage)
qplot(welfare$marriage)

table(welfare$group_marriage)
welfare$group_marriage <- ifelse(welfare$marriage ==1, "marraige", ifelse(welfare$marriage ==3, "divorce",NA))
table(welfare$group_marriage)
welfare$group_marriage

table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

birth_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(birth,group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group= sum(n)) %>%
  mutate(pct = round(n/tot_group*100,1))
birth_marriage

divorce <- birth_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(birth,pct)
divorce
ggplot(data = divorce, aes(x = birth, y = pct)) + geom_col()



#프로젝트 (3) 지역별 성별 비율

class(welfare$code_region)
table(welfare$code_region)
list_region <- data.frame(code_region = c(1:7), region = c("서울", "수도권(인천/경기)","부산/경남/울산","대구/경북","대전/충남","강원,충북","광주/전남/전북/제주도"))
list_region

welfare <-left_join(welfare, list_region, id = "code_region")
welfare %>%
  select(code_region, region) %>%
  head

region_sex <- welfare %>%
  group_by(region,sex) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100,2))
region_sex
ggplot(data = region_sex, aes(x= region, y = pct, fill = sex)) +geom_col()+coord_flip()


#프로젝트 (4) 성별, 종교 유무, 결혼비율

class(welfare$sex)
class(welfare$religion)
class(welfare$marriage)

welfare$sex <- ifelse(welfare$sex == 9, NA ,welfare$sex)
welfare$sex <- ifelse(welfare$sex ==1, "male", "female")

welfare$religion <- ifelse(welfare$religion ==1, "yes", "no")

welfare$group_marriage <- ifelse(welfare$marriage ==1, "marraige", ifelse(welfare$marriage ==3, "divorce",NA))

religion_marriage <-welfare %>%
  filter(!is.na(marriage)) %>%
  group_by(sex,religion) %>%
  summarise(mean_marriage = mean(marriage))

religion_marriage

ggplot(data = religion_marriage, aes(x = sex, y = mean_marriage, fill = religion))+geom_col(position = "dodge")+
  scale_x_discrete(limits = c("male", "female"))

