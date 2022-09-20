library("dplyr")
library("forcats")
library("scatterplot3d")
library("ggplot2")
library("ggcorrplot")
library("superheat")
library("ggpubr")
library("RColorBrewer")
library("readxl")
library("writexl")
library("xlsx")
library("HistData")
library("DECIPHER")
library("mosaicData")
df<-read.csv("data.csv",sep=",",dec=".",header=T,nrows=5)
df
df2<-read.xlsx("Covid Dashboard.xlsx",sheetIndex = "Data")
df2
View(df2)

str(df2)
summary(df2)
Active_Ratio<-df2$Active.Ratio
Dish_rat<-df2$Discharge.Ratio
death_rat<-df2$Death.Ratio
cor(Active_Ratio,Dish_rat,method="pearson")
cor.test(Active_Ratio,Dish_rat,method="pearson")
dim(df2)
dim(df2)[1]
df3<-read.csv2("data.csv",dec=",",nrows=5,sep=",")
df3
print(df$id)
table_id=table(df$id)
table_id
barplot(table_id,main="id in data",xlab="id",ylab="frequency",col="blue")
hist(df2$Discharge.Ratio,co="chocolate")
boxplot(df2$Population)
data_f<-read.delim("data.csv",header=T,nrows=3,dec=".",sep=",")
data_f[3,3]
ggqqplot(df$texture_mean,ylab="texture_mean")
ggqqplot(df$radius_mean,ylab="radius_mean")
m<-df2$Discharge.Ratio

par(mfrow=c(2,3))

hist(m,breaks=10,col=brewer.pal(3,"Set3"),main="set 3 colors")
hist(m,breaks=3,col=brewer.pal(3,"Set2"),main="set 3 color")
hist(m,breaks=7,col=brewer.pal(3,"Set1"),main="set 3 color")
hist(m,breaks=2,col=brewer.pal(8,"Set3"),main="set 3 color")

hist(m,col=brewer.pal(8,"Greys"),main="Gray color")
hist(m,col=brewer.pal(8,"Greens"),main="Green color")
RowLabels<-df2$State.UTs
TotalCases<-df2$Total.Cases
Discharged<-df2$Discharged
cases_recovered<-data.frame(RowLabels,TotalCases
,Discharged)
cases_recovered[nrow(cases_recovered)+1,]<-c("Grand Total",sum(TotalCases),sum(Discharged))
cases_recovered
write_xlsx(cases_recovered,"Cases and Recovered.xlsx")
Cases_and_Recovered<-read_excel("Cases and Recovered.xlsx")
View(Cases_and_Recovered)

File<-read_excel("Covid Dashboard.xlsx")
View(File)
Zone<-df2$Zone
TotalCasesbyzone<-df2$Total.Cases
Zone
TotalCasesbyzone

Total<-aggregate(df2$Total.Cases,by=list(Labels=Zone),FUN=sum)
Total
class(Total)
Stases_zones<-write_xlsx(Total,"States and Zones.xlsx")
dg<-read_excel("States and Zones.xlsx")
View(dg)
sum(Total$x)
Total[nrow(Total)+1,]<-c("Grand Total",sum(Total$x))
hist(df2$Discharge.Ratio,breaks=14,xlim=range_ratio)
range_ratio<-range(df2$Discharge.Ratio)
class(range_ratio)
barplot(df2$Discharge.Ratio,names.arg=df2$State.UTs,main="Recovered Average",col="red")
Dish_rat
df200<-data.frame(st,rat)
df200
install.packages("forcats")
library("forcats")
st<-sample(RowLabels[1:10])
rat<-sample(Dish_rat[1:10])
st
rat
df200%>%
  mutate(st=fct_reorder(st,rat))%>%
  ggplot(aes(st,rat))+
  geom_bar(stat="identity",width=.4,alpha=.5,fill="blue")+
  coord_flip()+
  xlab("")+
  theme_bw()
df300<-data.frame(df2$State.UTs,df2$Discharge.Ratio)
write_xlsx(df300,"Book1.xlsx")
File
df400<-select_if(File,is.numeric)
corelation<-cor(df400,use="complete.obs")
ggcorrplot(corelation)
data("mtcars")
superheat(mtcars,scale=TRUE,left.label.text.size = 3,
bottom.label.text.size = 3,bottom.label.size = .05)

View(mtcars)
with(df2,{scatterplot3d(x=df2$Active.Ratio,y=df2$Discharge.Ratio,z=df2$Death.Ratio)})
with(mtcars,{scatterplot3d(x=disp,y=wt,z=mpg)})


d10<-function(a,b,c)
{
  print(a)
  print(a*b)
  print(a*c)
}
d10(10,5)
c<-20
c<-15