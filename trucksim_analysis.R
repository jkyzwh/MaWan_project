#####################################################
# 深圳妈湾项目分析
#####################################################

# 程序加载package的初始化
library(psych)
library(ggplot2)
library(data.table)
library (stats)
library(plyr)

#制定ggplot2实验的主题
#theme_set(theme_gray())

#标准调用函数
source("D:/PROdata/R/Work project/MaWan_shenzhen/Mawan_function.R")

#初始化工作环境，数据工作目录
setwd("D:/PROdata/Data/MaWan/all")


# 1.将sim数据导入R工作环境，利用data.table包----
temp<- list.files(pattern="*.txt")#查询当前目录下包含的所有csv文件，将文件名存入temp列表
data_name<-gsub('.txt','',temp)#将文件名的扩展名去掉，即将文件名中的.csv替换为空，存入data_name列表
AllData<-data.frame()

# 依次读入驾驶模拟器数据，并汇总为一张大表
for (i in 1:length(temp))
{
  #列名命名为（Station，数据文件名字）
  col_name<-c("Station","Speed")
  #利用data.table包的fread函数
  FileToData<-fread(temp[i],header=T,sep="auto",stringsAsFactors=FALSE,data.table=F,skip = "Station",col.names =col_name )
  # 按照桩号排序
  FileToData<-Order.dis(FileToData,1)
  #增加平滑后的速度项
  speed_loess<-loess(Speed~Station,data=FileToData,span=0.1,degree = 2) #loess 数据平滑
  FileToData$Speed.loess<-speed_loess$fitted #平滑后的结果
  #增加冲坡初始速度
  FileToData$Speed.begin<-substr(data_name[i],0,nchar(data_name[i])-8)
  #增加坡度
  FileToData$grade<-substr(data_name[i],nchar(data_name[i])-6,nchar(data_name[i])-4)
  #增加车辆比功率
  FileToData$weight.power<-substr(data_name[i],nchar(data_name[i])-2,nchar(data_name[i]))
  
  FileToData$ID<-data_name[i]
  
  
  # 将调整过的FileToData组合成一个大表
  AllData<-rbind(FileToData,AllData)
}

rm(FileToData,speed_loess,col_name)

################################################################1. 冲坡速度与平衡坡长的关系---------------
# 利用2.5%坡度，比功率4.5的数据，分析冲坡速度与平衡坡长的关系
# 与实车实验的结果进行比对
##桩号与速度关系描述性绘图

Speed_data<-subset(AllData,grade==2.5 & weight.power==4.5 )

ggplot(data=Speed_data,aes(x=Station,y=Speed,colour=Speed.begin))+
  geom_line(size=0.5)+
  labs(x="坡长（m） ",y="初始速度(km/h)",title="不同冲坡速度与坡长关系（2.5%）")+
  scale_x_continuous(breaks=seq(0,11000,1000))+
  scale_y_continuous(breaks=seq(30,100,10))+
  theme_light()

BeginSpeed<-Speed_data$Speed.begin
BeginSpeed<-BeginSpeed[!duplicated(BeginSpeed)]#删除列表中重复项 ，使用 duplicated（）函数

# 计算不同冲坡速度对应的平衡速度和平衡坡长
Grade_2.5<-Station.minSpeed(Speed_data,BeginSpeed) #调用函数

Grade_2.5$Speed.begin<-as.double(Grade_2.5$Speed.begin)
Grade_2.5$min.Speed<-as.double(Grade_2.5$min.Speed)
Grade_2.5$Grade.Length<-as.double(Grade_2.5$Grade.Length)

ggplot(data=Grade_2.5,aes(x=Speed.begin,y=Grade.Length))+
  geom_point(size=1.5)+geom_smooth(method = lm)+
  labs(x="冲坡速度 ",y="平衡速度对应坡长",title="不同冲坡速度与坡长关系")+
  scale_x_continuous(breaks=seq(40,110,10))+
  scale_y_continuous(breaks=seq(0,3000,200))

#建立线性回归模型
grade.lm<-lm(Grade.Length~Speed.begin,data =Grade_2.5,na.action = na.omit)# 线性回归模型

#############################################！1.冲坡速度与平衡坡长的关系完成---------------

#############################################2.2.5%坡度分析------------------

Speed_data<-subset(AllData,grade==2.5)

ggplot(data=Speed_data,aes(x=Station,y= Speed,colour=weight.power))+
  geom_line(size=0.75)+
  labs(x="坡长（m） ",y="Speed(km/h)",title="不同冲坡速度与坡长关系")+
  scale_x_continuous(breaks=seq(0,11000,1000))+
  scale_y_continuous(breaks=seq(30,100,10))+
  facet_wrap(~Speed.begin)

ggplot(data=Speed_data,aes(x=Station,y= Speed,colour=ID))+
  geom_line(size=0.75)+
  labs(x="坡长（m） ",y="Speed(km/h)",title="不同冲坡速度与坡长关系")+
  scale_x_continuous(breaks=seq(0,11000,1000))+
  scale_y_continuous(breaks=seq(30,100,10))+
  facet_wrap(~weight.power)

ggplot(data=AllData,aes(x=Station,y= Speed,colour=ID))+
  geom_line(size=0.75)+
  labs(x="坡长（m） ",y="Speed(km/h)",title="不同冲坡速度与坡长关系")+
  scale_x_continuous(breaks=seq(0,11000,1000))+
  scale_y_continuous(breaks=seq(30,100,10))

# 分析纵坡坡度为3.5%时速度变化
grade_3.5<-subset(AllData,grade==3.5)

ggplot(data=grade_3.5,aes(x=Station,y= Speed.loess,colour=weight.power))+
  geom_line(size=0.75)+
  labs(x="坡长（m） ",y="Speed(km/h)",title="不同冲坡速度与坡长关系")+
  scale_x_continuous(breaks=seq(0,11000,1000))+
  scale_y_continuous(breaks=seq(30,100,10))+
  facet_wrap(~Speed.begin)

xx<-Station.minSpeed(AllData,data_name)



