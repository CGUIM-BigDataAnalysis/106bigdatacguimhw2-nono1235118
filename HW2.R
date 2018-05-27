library(readr)
C103 <- read_csv("103_ab103_C.csv")
C104 <- read_csv("104_ab104_C.csv")
C105 <- read_csv("105_ab105_C.csv")
C106 <- read_csv("106_ab105_C.csv")

#1.請問哪些國家來台灣唸書的學生最多呢？請取出前十名的國家與總人數，由大到小排序(5分)。
library(dplyr)
C103$total<-rowSums(C103[,3:11],na.rm = T)
C104$total<-rowSums(C104[,3:11],na.rm = T)
C105$total<-rowSums(C105[,3:11],na.rm = T)
C106$total<-rowSums(C106[,3:11],na.rm = T)

CStudent<-merge(select(C103,"國別",total),
                select(C104,"國別",total),
                by = "國別",all=T)
CStudent<-merge(CStudent,
                select(C105,"國別",total),
                by = "國別",all=T)
CStudent<-merge(CStudent,
                select(C106,"國別",total),
                by = "國別",all=T)
names(CStudent)<-c("國別","total103","total104","total105","total106")

CStudent<-CStudent%>%
  mutate(總人數 =rowSums(CStudent[,2:5]))%>%
  select(國別,總人數)%>%
  arrange(desc(總人數))%>%
  head(10)

#又哪間大學的境外生最多呢？請取出前十名的大學與總人數，由大到小排序(5分)。
S103<- read_csv("103_ab103_S.csv")
S104<- read_csv("104_ab104_S.csv")
S105<- read_csv("105_ab105_S.csv")
S106<- read_csv("106_ab105_S.csv")

S103$`非學位生-大陸研修生`<-as.numeric(gsub("…",NA,S103$`非學位生-大陸研修生`))
S104$`非學位生-大陸研修生`<-as.numeric(gsub("…",NA,S104$`非學位生-大陸研修生`))


S103$total<-rowSums(S103[,4:12],na.rm = T)
S104$total<-rowSums(S104[,4:12],na.rm = T)
S105$total<-rowSums(S105[,4:12],na.rm = T)
S106$total<-rowSums(S106[,4:12],na.rm = T)

SStudent<-merge(select(S103,"學校名稱",total),
                select(S104,"學校名稱",total),
                by = "學校名稱")
SStudent<-merge(SStudent,
                select(S105,"學校名稱",total),
                by = "學校名稱")
SStudent<-merge(SStudent,
                select(S106,"學校名稱",total),
                by = "學校名稱")
names(SStudent)<-c("學校名稱","total103","total104","total105","total106")

SStudent<-SStudent%>%
  mutate(總人數=rowSums(SStudent[,2:5]))%>%
  select(學校名稱,總人數)%>%
  arrange(desc(總人數))%>%
  head(10)

#承1，請用bar chart呈現各個國家(全部)來台灣唸書的學生人數(10分)。
install.packages("ggplot2")
library(ggplot2)

ggplot()+geom_bar(data=CStudent,
                  aes(x=國別,y=總人數),
                  stat = "identity") 

#承1，請用面量圖呈現各個國家來台灣唸書的學生人數，人數越多顏色越深(10分)。
install.packages(c("choroplethr","choroplethrMaps")) 
library(choroplethr)
library(choroplethrMaps)
data(country.names)
df = data.frame(region=country.names, value=sample(1:length(country.names)))
choroplethr(df, lod="world")

#4.台灣大專院校的學生最喜歡去哪些國家進修交流呢？請取出前十名的國家與總人數，由大到小排序(5分)。
library(readxl)
library(dplyr)
TWstudent<- read_excel("Student_RPT_07.xlsx")
TCstudent<-TWstudent%>%
  group_by(`對方學校(機構)國別(地區)`)%>%
  summarise(總人數=sum(小計))%>%
  arrange(desc(總人數))%>%
  head(10)

#又哪間大學的出國交流學生數最多呢？請取出前十名的大學與總人數，由大到小排序(5分)。
TSstudent<-TWstudent%>%
  group_by(學校名稱)%>%
  summarise(總人數=sum(小計))%>%
  arrange(desc(總人數))%>%
  head(10)

#承4，請用bar chart呈現台灣大專院校(全部)的學生去各國家進修交流人數(10分)。
ggplot()+geom_bar(data=TSstudent,
                  aes(x=學校名稱,y=總人數),
                  stat = "identity") 

#承4，請用面量圖呈現台灣大專院校的學生去各國家進修交流人數，人數越多顏色越深(10分)。

#台灣學生最喜歡去哪些國家留學呢？請取出前十名的國家與總人數，由大到小排序(5分)。
library(readr)
twc<- read_csv("105fuck.csv")
twc[,4:6]<-NULL
twc10<-twc%>%
  select("國別","總人數")%>%
  arrange(desc(總人數))%>%
  head(10)

#承7，請用面量圖呈現台灣學生去各國家留學人數，人數越多顏色越深(10分)。
#請問來台讀書與離台讀書的來源國與留學國趨勢是否相同(5分)？

ggplot(dtanalysis, 
       aes(x =x, 
           y =y)) + 
  geom_point()+facet_grid(Species~.)+
  geom_smooth()
#想來台灣唸書的境外生，他們的母國也有很多台籍生嗎？請圖文並茂說明你的觀察(10分)。
#以上程式碼清晰程度與排版彈性給10分
