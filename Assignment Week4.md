# Assignment-Week4: ���� û�ҳ�/û�� �ڻ�� ������ �ٷ��
library(dplyr)

data_countrycontinent <- read.csv("countryContinent.csv")
data_suicide <- read.csv("SDGSUICIDE.csv")
data_human <- read.csv("Human_resources.csv")

summary(data_countrycontinent)
summary(data_suicide)
View(data_suicide)
### ���� 1��
# 20~24�� �ڻ�� ���� 25% ���� ��, 20-24�� �ڻ���� ������(���� �ڻ��/���� �ڻ��! ����)�� ���� ū ���� 7�� �׷����� �׷��ּ���.  

# 20~24�� �ڻ�� ���� 25% ����->data_suicide�� 4��° col(..1) // ������ both sexes ��(3~551 ���� 3) // ���� 25% => 3/4 �̻� 
#charater -> numeric���� 
data_suicide$Crude.suicide.rates..per.100.000.population..1 <- as.numeric(data_suicide$Crude.suicide.rates..per.100.000.population..1)

# �ε��� ��ȣ�� ������ ���ϰ����ϱ� True False�� �Ẽ��? X.1���� both sexes�̸� True
data_suicide$Both <- ifelse(data_suicide$X.1 == " Both sexes", "True","False" )
new_suicideboth <- data_suicide %>% filter(Both == "True")

summary(new_suicideboth$Crude.suicide.rates..per.100.000.population..1)
# 3rd Qu. 13.50 �̻��� �� ����
data_suicide$q3 <- ifelse(data_suicide$Both == "True" & data_suicide$Crude.suicide.rates..per.100.000.population..1 >= 13.50, "q3", "n")

#���� �ڻ�� ��
# data_suicide$X.1 == "Male"�� Crude.suicide.rates..per.100.000.population..1 ������ data_suicide$X.1 == "Female"�� Crude.suicide.rates..per.100.000.population..1

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


# ����� 20-24�� �ڻ�� ��� �׷����� �׷��ּ���. (���� ����)  
#country���� -> data_countrycontinent ���� �̿��ؼ� continent �� �߰� => ��� ���ϱ� => �׷���

# 100 000��� ���ź����η� ���� ������ 10��~29�� �ڻ���� ������踦 �� �� �ִ� �������� �׷��ּ���. (10�� ���� �����ʹ� �������ּ���./���ɺ� �α� ������ �����մϴ�. �ܼ��� ���ɺ� �ڻ�� ��ո� �����ֽø� �Ǿ�� )  
View(data_human)
# data_human�� �� �� ���ؼ� ���ο� ��(���ź����η� ��)



data_human <- rename(data_human, v1 = Psychiatrists.working.in.mental.health.sector..per.100.000.population., v2 = Nurses.working.in.mental.health.sector..per.100.000.population., v3 = Social.workers.working.in.mental.health.sector..per.100.000.population.,
v4 = Psychologists.working.in.mental.health.sector..per.100.000.population.)
#������ 0���� �ٲٱ�
data_human$v1 <- ifelse(is.na(data_human$v1), 0, data_human$v1)
data_human$v2 <- ifelse(is.na(data_human$v2), 0, data_human$v2)
data_human$v3 <- ifelse(is.na(data_human$v3), 0, data_human$v3)
data_human$v4 <- ifelse(is.na(data_human$v4), 0, data_human$v4)

data_human$v5 <- data_human$v1+data_human$v2+data_human$v3+data_human$v4

# x��: data_human ���ź����η¼�
# y��: 10~29�� �ڻ��(10~29��, both sexes �� ���� ���� �� ���� �����)

# ���ο� ������ ������ �����
new_sh <- data.frame()
new_sh$human <- data_human$v5


library(ggplot2)
ggplot(data = new_sh)

### ��.. ���ź��Ǻо� ������ ���� û�ҳ�,û��  �ڻ�� ���̿� ū ������谡 ������ �ʴ� �� ���׿�. ���Ⱑ ���� ����� ���� �ִ� �����͸� �� ã��� �����Ծ����ϴ�. ���� 2��. �־��� �����Ϳ� �������� �� ���� �����͸� ������ ã�� �׷����� �׷��ּ���. 
# ������谡 ��Ÿ���� �ʾƵ� �����ϴ�. �Ǵ� ��� ������ �����͸� �̿����� �ʾƵ� �˴ϴ�. �ñ��� ���� �����ͷ� Ǯ�����!

