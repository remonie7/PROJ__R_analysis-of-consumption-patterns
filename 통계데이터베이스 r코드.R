###[ R 코드 ]###
install.packages("lubridate")
install.packages("dplyr")
install.packages("data.table")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("chron")
install.packages("cowplot")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
library(cowplot)
library(lubridate)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#데이터읽어오기
demo=fread("C:/Users/korea/Desktop/2/제4회 Big Data Competition-분석용데이터-01.고객DEMO.txt")
inshop=fread("C:/Users/korea/Desktop/2/제4회 Big Data Competition-분석용데이터-02.쇼핑업종 상품구매.txt")
outshop=fread("C:/Users/korea/Desktop/2/제4회 Big Data Competition-분석용데이터-03.쇼핑외 업종 상품구매.txt")
shopcate=fread("C:/Users/korea/Desktop/2/제4회 Big Data Competition-분석용데이터-04.쇼핑업종 상품분류.txt", encoding="UTF-8")
demo$GENDER <- ifelse(demo$GENDER==1 , "M", "F")
inshop$DE_DT<- ymd(inshop$DE_DT)
#데이터병합
d_inshop = merge(demo, inshop,by = "ID")
d_outshop = merge(demo, outshop,by = "ID")
#데이터정리
genderindex=c("남","여")
ageindex=c("20대","30대","40대","50대","60대")
inshopunitindex=c("백화점","대형마트","슈퍼마켓","편의점","드러그스토어")
outshopunitindex=c("호텔","여행사","면세점","영화관","테마파크","야구관람","패스트푸드","페밀리레스토랑","카페")
monthindex=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
dayindex=c("일","월","화","수","목","금","토")


## 1번. 연령대별 쇼핑 빈도수
## => 40대 50대 30대 60대 20대 순임을 보여준다.
ageindex=c("20대","30대","40대","50대","60대")
inshopage=d_inshop[, .N, by=AGE_PRD][order(AGE_PRD)]
ggplot(inshopage, aes(x=factor(AGE_PRD), y=(N/10000)))+geom_bar(stat = "identity", color="black",aes(fill=AGE_PRD))+xlab("연령대")+ylab("빈도수(단위 : 만)")+scale_x_discrete(labels=ageindex)+ggtitle("연령대별 쇼핑 이용 수")+scale_fill_manual("연령",labels=ageindex, values=c("indianred", "goldenrod", "forestgreen", "darkcyan", "darkviolet"))+geom_text(aes(x=factor(AGE_PRD), y = N/10000, label = round(N/10000, 0)),vjust=1.6, size=3)

##  2번. 연령대별 쇼핑 총 금액
## => 50대 40대 30대 60대 20대 순임을 보여준다.
d11 <- d_inshop %>% group_by(AGE_PRD=as.factor(AGE_PRD),BUY_AM)%>% summarise(n = sum(BUY_AM))
d12 <- d11 %>% select(-BUY_AM) %>% summarise(k = sum(as.numeric(n)))
ggplot(d12, aes(x=AGE_PRD, y= k/100000000))+geom_bar(stat="identity", color="black",aes(fill=AGE_PRD))+xlab("연령대")+ylab("빈도수(단위 : 억)")+scale_x_discrete(labels=ageindex)+ggtitle("연령대별 쇼핑 이용 총금액")+scale_fill_manual("연령",labels=ageindex, values=c("indianred", "goldenrod", "forestgreen", "darkcyan", "darkviolet"))+geom_text(aes(x=AGE_PRD, y = k/100000000, label = round(k/100000000,0)),vjust=1.6, size=3)

## => 40대와 50대에 Focus !! (왜 빈도수에 비해 금액 차이가 많이 나는 걸까?)
## => 빈도는 40대가 크나, 금액은 50대가 더 크다
## => 쇼핑 어느 분야에서 금액 차이가 크게 나는지 분석


## 3번. 특정 연령대별 쇼핑 분야 총 금액
## => 백화점 대형마트 슈퍼마켓 편의점 드러그스토어 순임을 보여준다.
inshopagebizunit=d_inshop[AGE_PRD =="40PRD"|AGE_PRD =="50PRD", sum(BUY_AM, na.rm=TRUE), by=.(AGE_PRD, BIZ_UNIT)][order(AGE_PRD, BIZ_UNIT)]
ggplot(inshopagebizunit, aes(x=factor(BIZ_UNIT), y=round(V1/100000000,0), fill=factor(AGE_PRD))) +geom_bar(stat="identity", position="dodge", color="black")+xlab("업종")+ylab("총금액(단위 : 억)")+scale_x_discrete(labels=inshopunitindex)+ggtitle("40대 50대 쇼핑 업종 이용 총금액")+scale_fill_manual("연령",labels=c("40대","50대"), values=c("steelblue","lightcoral"))+geom_text(aes(x=BIZ_UNIT, y=round(V1/100000000,0), label=round(V1/100000000,0)),size=5,vjust=1.7,position=position_dodge(0.9))

## 4번. 특정 연령대별 쇼핑 분야 빈도수
## => 백화점의 빈도수가 50대가 40대보다 더 크다.
age4050 <- d_inshop %>% filter(AGE_PRD == c("40PRD","50PRD"))%>%group_by(AGE_PRD=as.factor(AGE_PRD), BIZ_UNIT,)%>% summarise(count = n())
bizlabel = c("백화점","대형마트","슈퍼마켓","편의점","드러그스토어")
age4050%>% ggplot(aes(x=BIZ_UNIT, y = count/1000, fill = AGE_PRD))+geom_bar(stat="identity", position = "dodge", color="black") +xlab("업종")+ylab("빈도수(단위 : 천)")+scale_x_discrete(labels=bizlabel)+ggtitle("40대 50대 쇼핑 업종 이용 수")+scale_fill_manual("연령",labels=c("40대","50대"), values=c("steelblue","lightcoral"))+geom_text(aes(x=BIZ_UNIT, y=count/1000, label=round(count/1000,0)),size=5,vjust=1.7,position=position_dodge(0.9))

## => 40대 50대 구매 금액 차이가 백화점에서 많이 난다
## => 백화점에 Focus !!
## => 50대가 40대보다 백화점 이용수가 더 많은데, 단순히 이용수가 많아서인지, 
1인당 상품을 많이 구매해서 인지, 상품 금액이 커서 인지분석


## 5번. 특정 연령대별 백화점 1인당 하루 구매 상품수
## => 40대 50대 1인당 구매 상품수가 비슷하다.
todaybuyfreq40=d_inshop[BIZ_UNIT=="A01"&AGE_PRD=="40PRD",sum(BUY_CT), by=.(DE_DT, ID)][order(DE_DT, ID)]
todaybuyfreq50=d_inshop[BIZ_UNIT=="A01"&AGE_PRD=="50PRD",sum(BUY_CT), by=.(DE_DT, ID)][order(DE_DT, ID)]
todaybuyfreq40V1=as.vector(todaybuyfreq40$V1)
todaybuyfreq50V1=as.vector(todaybuyfreq50[todaybuyfreq50$V1<4000]$V1)
todaybuyfreq40num=rep("40대", length(todaybuyfreq40V1))
todaybuyfreq50num=rep("50대", length(todaybuyfreq50V1))
todaybuyfreqbox=data.frame(age=c(todaybuyfreq40num, todaybuyfreq50num), count=c(todaybuyfreq40V1, todaybuyfreq50V1))
boxplot(log(todaybuyfreqbox$count)~factor(todaybuyfreqbox$age), main="1인당 백화점에서 하루 구매수량")
summary(todaybuyfreq40V1)
summary(todaybuyfreq50V1)

## 6번. 특정 연령대별 백화점 1인당 하루 구매 금액
## => 40대 50대 1인당 구매 금액이 50대가 더 크다
## => 1인당 구매 상품수는 비슷하나, 1인당 구매 금액이 50이 더 큰 걸 보아
50대가 구매하는 상품 금액이 더 크다
todayprice40=d_inshop[AGE_PRD=="40PRD"&BIZ_UNIT=="A01", .(sum=sum(BUY_AM), count=sum(BUY_CT)), by=.(ID, DE_DT)][order(DE_DT, ID)]
todayprice50=d_inshop[AGE_PRD=="50PRD"&BIZ_UNIT=="A01",.(sum=sum(BUY_AM), count=sum(BUY_CT)), by=.(ID, DE_DT)][order(DE_DT, ID)]
todayprice40v1=as.vector(todayprice40[todayprice40$count<990]$sum)
todayprice50v1=as.vector(todayprice50[todayprice50$count<4000]$sum)
todayprice40age=rep("40대", length(todayprice40v1))
todayprice50age=rep("50대", length(todayprice50v1))
todayprice4050=data.frame(age=c(todayprice40age, todayprice50age), count=c(todayprice40v1, todayprice50v1))
rownames(todayprice4050)=1:nrow(todayprice4050)
boxplot(log(todayprice4050$count)~factor(todayprice4050$age), main="1인당 하루 백화점에서 소비금액")
summary(todayprice40v1)
summary(todayprice50v1)

## => 40대 50대의 금액 차이가 나는 이유는 50대가 구매하는 상품 금액이 크고,
백화점 이용수가 많기 때문이다.


## 7번. 특정 연령대별 백화점 top 상품
## => 특정 상품 pick !! (백화점 40대 “아동”, 백화점 50대 “보석”)
buyproduct <- data.table(merge(d_inshop,shopcate, by = c("BIZ_UNIT", "PD_S_C")))
A1product40=buyproduct[AGE_PRD=="40PRD"&BIZ_UNIT=="A01", .N, by=PD_M_NM][order(N)]
wordcloud(words=A1product40$PD_M_NM, freq=A1product40$N, max.words = 50, min.freq = 20, scale=c(5,1),random.order = FALSE, rot.per = 0.25, colors=brewer.pal(8,"Dark2"))
A1product50=buyproduct[AGE_PRD=="50PRD"&BIZ_UNIT=="A01", .N, by=PD_M_NM][order(N)]
wordcloud(words=A1product50$PD_M_NM, freq=A1product50$N, max.words = 50, min.freq = 20, scale=c(5,1),random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## 8번. 특정 연령대별 특정 상품이 구매되는 날짜
url="https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R"  # 함수 웹 주소
source(url)
data_e <- as.data.frame(table(e$PD_M_NM))
colnames(data_e)<- c("word","freq")
data_e <- data_e[order(-data_e$freq),]
data_e$freq
# 40대 백화점 아동 구매되는 날짜
agea140 <- d_inshop%>%filter(BIZ_UNIT=="A01",AGE_PRD=="40PRD")%>%select(ID,DE_DT,PD_S_C)
aga1402 <- inner_join(agea140, shopcate, by = "PD_S_C")
juel2 <- aga1402%>%filter(PD_M_NM=="아동")%>%group_by(DE_DT)%>%summarise(n=n())
calendarHeat(dates=juel2$DE_DT, values=juel2$n, color="g2r", varname="40대 백화점 아동 날짜별 구매 빈도")
#50대 백화점 보석 구매되는 날짜
agea150 <- d_inshop%>%filter(BIZ_UNIT=="A01",AGE_PRD=="50PRD")%>%select(ID,DE_DT,PD_S_C)
aga1502 <- inner_join(agea150, shopcate, by = "PD_S_C")
juel <-aga1502%>%filter(PD_M_NM=="보석")%>%group_by(DE_DT)%>%summarise(n=n())
calendarHeat(dates=juel$DE_DT, values=juel$n, color="g2r", varname="50대 백화점 보석 날짜별 구매 빈도")

## 9번. 특정 연령대별 특정 상품과 같이 구매되는 상품
#50대 보석과 같이 구입한 품목
witha15<- d_inshop%>%filter(BIZ_UNIT=="A01",AGE_PRD=="50PRD")
witha150 <- d_inshop%>%filter(BIZ_UNIT=="A01",AGE_PRD=="50PRD")%>%select(ID,DE_DT,PD_S_C,RCT_NO)
witha1502 <- inner_join(witha150, shopcate, by = "PD_S_C")
dtwi20 <- witha1502%>%filter(PD_M_NM=="보석")%>%group_by(ID,RCT_NO,DE_DT)
kk <-  unique(dtwi20$RCT_NO)
yy <-  unique(dtwi20$DE_DT)
c <-  witha15%>%filter(RCT_NO==kk[1])
for( i in 1:length(kk)) {
  b <- witha15%>%filter(RCT_NO==kk[i+1])
  c <- rbind(c,b)
}
c1 <- c%>%group_by(PD_S_C,DE_DT)%>%summarise()%>%inner_join(shopcate, by = "PD_S_C")%>%filter(BIZ_UNIT=="A01")

e <- c1%>%filter(DE_DT==yy[1])
for( i in 1:length(yy)) {
  d <- c1%>%filter(DE_DT==yy[i+1])
  e <- rbind(e,d)
}
unique(e$PD_M_NM) 
data_e1 <- as.data.frame(table(e$PD_M_NM))
colnames(data_e1)<- c("word","freq")
data_e1$freq
wordcloud(words=data_e1$word, freq=data_e1$freq, max.words = 100, random.order = FALSE, rot.per = 0.1,min.freq = 3,
          colors=brewer.pal(3, "Dark2"))
#40대 아동과 같이 구입한 품목
witha14<- d_inshop%>%filter(BIZ_UNIT=="A01",AGE_PRD=="40PRD")
witha140 <- d_inshop%>%filter(BIZ_UNIT=="A01",AGE_PRD=="40PRD")%>%select(ID,DE_DT,PD_S_C,RCT_NO)
witha1402 <- inner_join(witha140, shopcate, by = "PD_S_C")
dtwi30 <- witha1402%>%filter(PD_M_NM=="아동")%>%group_by(ID,RCT_NO,DE_DT)
kk1 <-  unique(dtwi30$RCT_NO)
yy1 <-  unique(dtwi30$DE_DT)
cc <-  witha14%>%filter(RCT_NO==kk1[1])
for( i in 1:length(kk1)) {
  b <- witha14%>%filter(RCT_NO==kk1[i+1])
  cc <- rbind(cc,b)
}
cc2 <- cc%>%group_by(PD_S_C,DE_DT)%>%summarise()%>%inner_join(shopcate, by = "PD_S_C")%>%filter(BIZ_UNIT=="A01")
e1 <- cc2%>%filter(DE_DT==yy[1])
for( i in 1:length(yy)) {
  d <- cc2%>%filter(DE_DT==yy[i+1])
  e1 <- rbind(e1,d)
}
unique(e1$PD_M_NM)
data_e1e2 <- as.data.frame(table(e1$PD_M_NM))
colnames(data_e1e2)<- c("word","freq")
data_e1e2 <- data_e1e2[order(-data_e1e2$freq),]
data_e1e2$freq
wordcloud(words=data_e1e2$word, freq=data_e1e2$freq, max.words = 100, random.order = FALSE, rot.per = 0.1,min.freq = 3,
          colors=brewer.pal(3, "Dark2"))
