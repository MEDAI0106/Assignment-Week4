raw_suicide <- read.csv(file = "SDGSUICIDE.csv", stringsAsFactors = F)

library(ggplot2)
library(dplyr)
suicide <- raw_suicide

summary(suicide_young)
View(suicide_young)
#wide pivoting해도 됨!
suicide_b <- suicide %>% 
  filter(Sex == " Both sexes") %>% 
  select(Country, X20.24.years)
suicide_b <- rename(suicide_b, both = X20.24.years)
View(suicide_b)

suicide_m <- suicide %>% 
  filter(Sex == " Male") %>% 
  select(Country, X20.24.years)
suicide_m <- rename(suicide_m, male = X20.24.years)
View(suicide_m)

suicide_f <- suicide %>% 
  filter(Sex == " Female") %>% 
  select(Country, X20.24.years)
suicide_f <- rename(suicide_f, female = X20.24.years)
View(suicide_f)

suicide_tot <- left_join(suicide_b, suicide_m, by = "Country")
suicide_tot1 <- left_join(suicide_tot, suicide_f, by = "Country")
View(suicide_tot1)
summary(suicide_tot1)

suicide_tot2 <- suicide_tot1 %>% 
  filter(both >= 13.5) %>% 
  mutate(differ = male / female) %>% 
  arrange(desc(differ)) %>% 
  head(7)
View(suicide_tot2)

ggplot(data = suicide_tot2, aes(x = reorder(Country, differ), y = differ))+
  geom_col()+
  coord_flip()+
  ggtitle("20-24 years old Suicide Sex Ratio, Top 7 Countries \n Analyzed by Kim")+
  xlab("Country")+
  ylab("Suicide sex ratio")

# 문제 2번
install.packages("tidyr")
library(tidyr)
raw_continent_suicide <- read.csv(file = "countryContinent.csv", stringsAsFactors = F)
continent_suicide <- raw_continent_suicide
View(continent_suicide)

continent <- continent_suicide %>% 
  select(country, continent)
View(continent)

suicide_continent <- left_join(continent, suicide_tot1, by = "country")
View(suicide_continent)

suicide_continent2 <- suicide_continent %>% filter(!is.na(both)& !is.na(male)& !is.na(female))
suicide_continent <- suicide_continent[ ,c(2, 1, 3, 4, 5)]
View(suicide_continent2)

suicide_continent3 <- suicide_continent2 %>% 
  pivot_longer(c("both", "male", "female"), names_to = "Sex", values_to = "suicide_ratio") 
View(suicide_continent3)

suicide_continent4 <- suicide_continent3 %>% 
  group_by(continent, Sex) %>%
  summarise(mean_suicide = mean(suicide_ratio))

head(suicide_continent4)  

ggplot(data = suicide_continent4, aes(x = continent, y = mean_suicide, fill = Sex))+
  geom_col(position = "dodge")+
  scale_x_discrete(limits = c("Africa", "Asia", "Europe", "Americas", "Oceania"))+
  ggtitle("Age 20-24 Suicide Rate By Continent \n Analyzed by Kim")+
  xlab("Continent")+
  ylab("Crude suicide rates(per 100 000 population")

# 문제 3번
raw_HR <- read.csv(file = "Human_resources.csv", stringsAsFactors = F)
HR <- raw_HR
View(HR)
head(HR)
HR1 <- rename(HR, psy = Psychiatrists.working.in.mental.health.sector..per.100.000.population.,
              nur =  Nurses.working.in.mental.health.sector..per.100.000.population.,
              soc =  Social.workers.working.in.mental.health.sector..per.100.000.population.,
              psycho = Psychologists.working.in.mental.health.sector..per.100.000.population.)
View(HR1)
is.na(HR1)

HR1$psy <- ifelse(is.na(HR1$psy), 0, HR1$psy)  
HR1$nur <- ifelse(is.na(HR1$nur), 0, HR1$nur) 
HR1$soc <- ifelse(is.na(HR1$soc), 0, HR1$soc)
HR1$psycho <- ifelse(is.na(HR1$psycho), 0, HR1$psycho) 
View(HR1)

HR_tot <- HR1 %>% 
  mutate(total = psy+nur+soc+psycho)
HR_tot1 <- HR_tot %>% 
  select(Country, total)
View(HR_tot1)

head(suicide)
suicide_HR <- suicide %>% 
  filter(Sex == " Both sexes") %>% 
  mutate(mean_suicide = (X15.19.years+X10.14.years+X25.29.years+X20.24.years)/4) %>% 
  select(Country, mean_suicide)

View(suicide_HR)

suicide_HR <- left_join(HR_tot1, suicide_HR, by = "Country")
View(suicide_HR)

ggplot(data = suicide_HR, aes(x = total, y=mean_suicide ))+
  geom_point()+
  ggtitle("Is there any correlation? \n Hmm..")+
  xlab("HR working in mental health sector(per100 000 popylation)")+
  ylab("10-29 year old suicide rate per 100 000")

# 4번 문제 
suicide_differ <- suicide %>% 
  filter(Sex == " Male" | Sex == " Female") %>% 
  mutate(mean_suicide = (X15.19.years+X10.14.years+X25.29.years+X20.24.years)/4) %>% 
  select(Country, Sex, mean_suicide)
View(suicide_differ)
View(suicide)

suicide_differ2 <- suicide_differ %>% 
  pivot_wider(names_from = Sex, values_from = mean_suicide) 

names(suicide_differ2)<- c("Country","Male", "Female") 
View(suicide_differ2)

suicide_differ3 <- suicide_differ2 %>% 
  mutate(differ = abs(Male - Female)) %>% 
  select(Country, differ)
View(suicide_differ3)

suicide_differ_HR <-left_join(HR_tot1, suicide_differ3, by = "Country")

View(suicide_differ_HR)

ggplot(data = suicide_differ_HR, aes(x = total, y=differ ))+
  geom_point()+
  ggtitle("HR working & difference of sex")+
  xlab("HR working in mental health sector(per100 000 popylation)")+
  ylab("difference of 10-29 year old suicide rate ")

