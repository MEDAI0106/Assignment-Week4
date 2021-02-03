# Assignment-Week4: 세계 청소년/청년 자살률 데이터 다루기
library(dplyr)

data_countrycontinent <- read.csv("countryContinent.csv")
data_suicide <- read.csv("SDGSUICIDE.csv")
data_human <- read.csv("Human_resources.csv")

summary(data_countrycontinent)
summary(data_suicide)
View(data_suicide)
### 문제 1번
# 20~24세 자살률 상위 25% 국가 중, 20-24세 자살률의 남여비(남자 자살률/여자 자살률! 비율)가 가장 큰 국가 7개 그래프를 그려주세요.  

# 20~24세 자살률 상위 25% 국가->data_suicide의 4번째 col(..1) // 국가의 both sexes 행(3~551 간격 3) // 상위 25% => 3/4 이상 
#charater -> numeric으로 
data_suicide$Crude.suicide.rates..per.100.000.population..1 <- as.numeric(data_suicide$Crude.suicide.rates..per.100.000.population..1)

# 인덱스 번호로 추출을 못하겠으니까 True False를 써볼까? X.1에서 both sexes이면 True
data_suicide$Both <- ifelse(data_suicide$X.1 == " Both sexes", "True","False" )
new_suicideboth <- data_suicide %>% filter(Both == "True")

summary(new_suicideboth$Crude.suicide.rates..per.100.000.population..1)
# 3rd Qu. 13.50 이상인 행 추출
data_suicide$q3 <- ifelse(data_suicide$Both == "True" & data_suicide$Crude.suicide.rates..per.100.000.population..1 >= 13.50, "q3", "n")

#남녀 자살률 비
# data_suicide$X.1 == "Male"의 Crude.suicide.rates..per.100.000.population..1 나누기 data_suicide$X.1 == "Female"의 Crude.suicide.rates..per.100.000.population..1

male <- data_suicide %>% filter(X.1 == " Male")
female <- data_suicide %>% filter(X.1 == " Female")

new_suicideboth$mf_rate <- male$Crude.suicide.rates..per.100.000.population..1 / female$Crude.suicide.rates..per.100.000.population..1

q3_mf_rate <- new_suicideboth %>% filter(q3 == "q3")
order(q3_mf_rate$mf_rate)
# index : 17 29 24 42 46 32 35 ???

q3_mf_rate_order <- q3_mf_rate[c(order(-q3_mf_rate$mf_rate)),]

head(q3_mf_rate_order, 7)

library(ggplot2)

ggplot(data = q3_mf_rate_order, aes(x = mf_rate, y=X)) +geom_col()


# 대륙별 20-24세 자살률 평균 그래프를 그려주세요. (성별 포함)  
#country변수 -> data_countrycontinent 변수 이용해서 continent 열 추가 => 평균 구하기 => 그래프

# 100 000명당 정신보건인력 수와 국가별 10세~29세 자살률의 상관관계를 알 수 있는 산점도를 그려주세요. (10세 이하 데이터는 제외해주세요./연령별 인구 비율은 무시합니다. 단순히 연령별 자살률 평균만 내어주시면 되어요 )  
View(data_human)
# data_human의 열 다 더해서 새로운 열(정신보건인력 수)



data_human <- rename(data_human, v1 = Psychiatrists.working.in.mental.health.sector..per.100.000.population., v2 = Nurses.working.in.mental.health.sector..per.100.000.population., v3 = Social.workers.working.in.mental.health.sector..per.100.000.population.,
v4 = Psychologists.working.in.mental.health.sector..per.100.000.population.)
#결측값 0으로 바꾸기
data_human$v1 <- ifelse(is.na(data_human$v1), 0, data_human$v1)
data_human$v2 <- ifelse(is.na(data_human$v2), 0, data_human$v2)
data_human$v3 <- ifelse(is.na(data_human$v3), 0, data_human$v3)
data_human$v4 <- ifelse(is.na(data_human$v4), 0, data_human$v4)

data_human$v5 <- data_human$v1+data_human$v2+data_human$v3+data_human$v4

# x축: data_human 정신보건인력수
# y축: 10~29세 자살률(10~29세, both sexes 행 총합 더한 열 새로 만들기)

# 새로운 데이터 프레임 만들기
new_sh <- data.frame()
new_sh$human <- data_human$v5


library(ggplot2)
ggplot(data = new_sh)

### 앗.. 정신보건분야 종사자 수와 청소년,청년  자살률 사이에 큰 상관관계가 보이지 않는 것 같네요. 오기가 생긴 당신은 관련 있는 데이터를 꼭 찾기로 마음먹었습니다. 문제 2번. 주어진 데이터와 관련있을 것 같은 데이터를 스스로 찾아 그래프를 그려주세요. 
# 상관관계가 나타나지 않아도 좋습니다. 또는 모든 국가의 데이터를 이용하지 않아도 됩니다. 궁금한 점을 데이터로 풀어보세요!

