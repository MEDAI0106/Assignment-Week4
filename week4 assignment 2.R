#대륙별 20-24세 자살률 평균 그래프를 그려주세요. (성별 포함)
suicide_data <- read.csv("SDGSUICIDE.csv", stringsAsFactors = F)
country_data <- read.csv("countryContinent.csv", stringsAsFactors = F)
country_data_Af <- country_data %>% filter(country_data$continent == "Africa") 
suicide_data_Af <- structure(list(character()), class = "data.frame")#빈 데이터 프라임 만들고 행 더하기
for(i in length(suicide_data)){
  if(suicide_data[i,1] %in% country_data_Af){
    suicide_data_Af <- rbind(suicide_data_Af, suicide_data_Af[i,])
  }
}#나라이름이 Africa 대륙 데이터 안에 있으면 그 행을 빈 데이터 프라임에 넣기 --- 그런데 빈 데이터프레임이 여전히 비었네...

country_data_As <- country_data %>% filter(country_data$continent == Asia)
country_data_Eu <- country_data %>% filter(country_data$continent == Europe)
country_data_Am <- country_data %>% filter(country_data$continent == Americas)
country_data_Oc <- country_data %>% filter(country_data$continent == Oceania)