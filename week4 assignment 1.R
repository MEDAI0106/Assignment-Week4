install.packages("ggplot2")
library(ggplot2) # 20~24세 자살률 상위 25% 국가 중, 20~24세 자살률의 남여비(남자 자살률/여자 자살률)가 가장 큰 국가 7개의 그래프를 그리기
install.packages("dplyr")
library(dplyr)
suicide_data <- read.csv("SDGSUICIDE.csv", stringsAsFactors = F)

suicide_data_1 <- suicide_data
suicide_data_1 <- rename(suicide_data_1, Crude.suicide.rates..per.100.000.population. = suicide_rates_per_100.000_25_29, Crude.suicide.rates..per.100.000.population..1 = suicide_rates_per_100.000_20_24, Crude.suicide.rates..per.100.000.population..2 = suicide_rates_per_100.000_15_19, Crude.suicide.rates..per.100.000.population..3 = suicide_rates_per_100.000_10_14, Crude.suicide.rates..per.100.000.population..4 = suicide_rates_per_100.000_5_9)# 왜 열 이름이 안 바뀌지?에러: Can't rename columns that don't exist. x Column `suicide_rates_per_100.000_25_29` doesn't exist.

suicide_data_1 <- ifelse(suicide_data_1$Crude.suicide.rates..per.100.000.population..1>100|is.numeric(suicide_data_1$Crude.suicide.rates..per.100.000.population..1)==FALSE, NA, suicide_data_1$Crude.suicide.rates..per.100.000.population..1)
suicide_data_1 <- na.omit(suicide_data_1)#조금 이상하게 잘리는 것 같은데...

suicide_data_bothsex <- suicide_data_1 %>% filter(suicide_data_1$X.1 == Both sexes)#both sexes만 추출하기
suicide_data_sex <- suicide_data_1 %>% filter(suicide_data_1$X.1 == Male|Female)
Q3 = quantile(suicide_data_bothsex$Crude.suicide.rates..per.100.000.population..1, prob=c(0.75)) #Q3찾기


ggplot(data = suicide_data, aes(x = "Suicide sex ratio", y = "Country")) + geom_col() #그래프 그리기
