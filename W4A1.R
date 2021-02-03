install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
sc <- read.csv("SDGSUICIDE.csv", stringsAsFactors = F)
sc %>% 
  filter(Sex == 1) %>%
  select(S, m_F)   #여기까지하면 나라별로 m/f한값이 잘 나옴 ㅠ
#엑셀에서 그냥 20~24 m/f 값을 m_F값으로 열 추가해서 구해버림
csc <- sc %>% arrange(desc(m_F)) %>% 
  head(7)
csc
library(ggplot2)
ggplot(data = csc, aes(x = reorder(S, m_F), y = m_F)) + geom_col()
#뭔가 그럴듯 한데? 왜 위에서 head(7)했을 땐 이런값이 안나왔을까?
#그리고 왜 reorder값을 씹을까?
#왜 민정이의 국가와 다를까?
#왜 막줄 실행하면 14개의 경고를 발견했다할까? ㅋㅋㅋㅋ