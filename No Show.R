rm(list=ls())
library(ggplot2)
library(lubridate)

setwd("C:/Users/cuisa/Desktop/PCCP/No Show")

noshow<-read.csv("Copy of Daily No Shows.csv",header = T)
noshow<-noshow[-1,]
levels(noshow$Barrier)
noshow$Barrierclean<-rep(0,length(noshow$Barrier))
noshow$Barrierclean[c(which(noshow$Barrier==""),which(noshow$Barrier=="NA (Not applicable) "),which(noshow$Barrier=="N (Not obtained)"))]<-NA
noshow$Barrierclean[which(noshow$Barrier=="A (Appointment conflict)")]<-"A (Appointment conflict)"
noshow$Barrierclean[which(noshow$Barrier=="C (Childcare)")]<-"C (Childcare)"
noshow$Barrierclean[which(noshow$Barrier=="F (Forgot)" )]<-"F (Forgot)" 
noshow$Barrierclean[which(noshow$Barrier=="F (Forgot) U (Unaware of appt) T (Transportation) W (Work conflict)" )]<-"F (Forgot)" 
noshow$Barrierclean[c(which(noshow$Barrier=="H (Hospitalized)"),which(noshow$Barrier=="H (Hospitalized) ") )]<-"H (Hospitalized)"
noshow$Barrierclean[c(which(noshow$Barrier=="O (Other)"),which(noshow$Barrier=="O (Other) ") )]<-"O (Other)"
noshow$Barrierclean[which(noshow$Barrier=="S (Sick)" )]<-"S (Sick)" 
noshow$Barrierclean[which(noshow$Barrier=="T (Transportation)" )]<-"T (Transportation)"
noshow$Barrierclean[which(noshow$Barrier=="U (Unaware of appt)" )]<-"U (Unaware of appt)"
noshow$Barrierclean[which(noshow$Barrier=="W (Weather)" )]<-"W (Weather)"
noshow$Barrierclean[which(noshow$Barrier=="W (Work conflict)" )]<-"W (Work conflict)"




sum(1*is.na(noshow$Barrierclean))
length(which(noshow$Barrierclean=="A (Appointment conflict)"))
length(which(noshow$Barrierclean=="C (Childcare)"))
length(which(noshow$Barrierclean=="F (Forgot)"))
length(which(noshow$Barrierclean=="H (Hospitalized)"))
length(which(noshow$Barrierclean=="O (Other)"))
length(which(noshow$Barrierclean=="S (Sick)"))
length(which(noshow$Barrierclean=="T (Transportation)"))
length(which(noshow$Barrierclean=="U (Unaware of appt)"))
length(which(noshow$Barrierclean=="W (Weather)"))
length(which(noshow$Barrierclean=="W (Work conflict)"))

noshow$No.Show.Date<-as.character(noshow$No.Show.Date)
noshow$New.appt.Date<-as.character(noshow$New.appt.Date)
noshow$No.Show.Date[387]<-"2018/3/9"


noshow$days<-as.numeric(difftime(ymd(noshow$New.appt.Date),ymd(noshow$No.Show.Date), units="days"))

noshow<-data.frame(noshow[,1:13],noshow[,c(50,51)],noshow[,14:49])


### Calculate time interval between no show date and new appointment date ###


median(noshow$days,na.rm = T)
quantile(noshow$days,c(0.25,0.75),na.rm = T)

median(noshow$days[is.na(noshow$Barrierclean)],na.rm = T)
quantile(noshow$days[is.na(noshow$Barrierclean)],c(0.25,0.75),na.rm = T)
kruskal.test(list(noshow$days[is.na(noshow$Barrierclean)],
                  noshow$days[!is.na(noshow$Barrierclean)]))


median(noshow$days[which(noshow$Barrierclean=="A (Appointment conflict)")],na.rm = T)
quantile(noshow$days[which(noshow$Barrierclean=="A (Appointment conflict)")],c(0.25,0.75),na.rm = T)
kruskal.test(list(noshow$days[which(noshow$Barrierclean=="A (Appointment conflict)")],
                  noshow$days[which(noshow$Barrierclean!="A (Appointment conflict)")]))

median(noshow$days[which(noshow$Barrierclean=="C (Childcare)")],na.rm = T)
quantile(noshow$days[which(noshow$Barrierclean=="C (Childcare)")],c(0.25,0.75),na.rm = T)
kruskal.test(list(noshow$days[which(noshow$Barrierclean=="C (Childcare)")],
                  noshow$days[which(noshow$Barrierclean!="C (Childcare)")]))

median(noshow$days[which(noshow$Barrierclean=="F (Forgot)")],na.rm = T)
quantile(noshow$days[which(noshow$Barrierclean=="F (Forgot)")],c(0.25,0.75),na.rm = T)
kruskal.test(list(noshow$days[which(noshow$Barrierclean=="F (Forgot)")],
                  noshow$days[which(noshow$Barrierclean!="F (Forgot)")]))



median(noshow$days[which(noshow$Barrierclean=="H (Hospitalized)")],na.rm = T)
quantile(noshow$days[which(noshow$Barrierclean=="H (Hospitalized)")],c(0.25,0.75),na.rm = T)
kruskal.test(list(noshow$days[which(noshow$Barrierclean=="H (Hospitalized)")],
                  noshow$days[which(noshow$Barrierclean!="H (Hospitalized)")]))

median(noshow$days[which(noshow$Barrierclean=="O (Other)")],na.rm = T)
quantile(noshow$days[which(noshow$Barrierclean=="O (Other)")],c(0.25,0.75),na.rm = T)
kruskal.test(list(noshow$days[which(noshow$Barrierclean=="O (Other)")],
                  noshow$days[which(noshow$Barrierclean!="O (Other)")]))



median(noshow$days[which(noshow$Barrierclean=="S (Sick)")],na.rm = T)
quantile(noshow$days[which(noshow$Barrierclean=="S (Sick)")],c(0.25,0.75),na.rm = T)
kruskal.test(list(noshow$days[which(noshow$Barrierclean=="S (Sick)")],
                  noshow$days[which(noshow$Barrierclean!="S (Sick)")]))


median(noshow$days[which(noshow$Barrierclean=="T (Transportation)")],na.rm = T)
quantile(noshow$days[which(noshow$Barrierclean=="T (Transportation)")],c(0.25,0.75),na.rm = T)
kruskal.test(list(noshow$days[which(noshow$Barrierclean=="T (Transportation)")],
                  noshow$days[which(noshow$Barrierclean!="T (Transportation)")]))


median(noshow$days[which(noshow$Barrierclean=="U (Unaware of appt)")],na.rm = T)
quantile(noshow$days[which(noshow$Barrierclean=="U (Unaware of appt)")],c(0.25,0.75),na.rm = T)
kruskal.test(list(noshow$days[which(noshow$Barrierclean=="U (Unaware of appt)")],
                  noshow$days[which(noshow$Barrierclean!="U (Unaware of appt)")]))


median(noshow$days[which(noshow$Barrierclean=="W (Weather)")],na.rm = T)
quantile(noshow$days[which(noshow$Barrierclean=="W (Weather)")],c(0.25,0.75),na.rm = T)
kruskal.test(list(noshow$days[which(noshow$Barrierclean=="W (Weather)")],
                  noshow$days[which(noshow$Barrierclean!="W (Weather)")]))


median(noshow$days[which(noshow$Barrierclean=="W (Work conflict)")],na.rm = T)
quantile(noshow$days[which(noshow$Barrierclean=="W (Work conflict)")],c(0.25,0.75),na.rm = T)
kruskal.test(list(noshow$days[which(noshow$Barrierclean=="W (Work conflict)")],
                  noshow$days[which(noshow$Barrierclean!="W (Work conflict)")]))



boxplot(noshow$days,
        noshow$days[which(noshow$Barrierclean=="A (Appointment conflict)")],
        noshow$days[which(noshow$Barrierclean=="C (Childcare)")],
        noshow$days[which(noshow$Barrierclean=="F (Forgot)")],
        noshow$days[which(noshow$Barrierclean=="FUTW")],
        noshow$days[which(noshow$Barrierclean=="H (Hospitalized)")],
        noshow$days[which(noshow$Barrierclean=="O (Other)")],
        noshow$days[which(noshow$Barrierclean=="S (Sick)")],
        noshow$days[which(noshow$Barrierclean=="T (Transportation)")],
        noshow$days[which(noshow$Barrierclean=="U (Unaware of appt)")],
        noshow$days[which(noshow$Barrierclean=="W (Weather)")],
        noshow$days[which(noshow$Barrierclean=="W (Work conflict)")])


kruskal.test(list(
                  noshow$days[which(noshow$Barrierclean=="A (Appointment conflict)")],
                  noshow$days[which(noshow$Barrierclean=="C (Childcare)")],
                  noshow$days[which(noshow$Barrierclean=="F (Forgot)")],
                  noshow$days[which(noshow$Barrierclean=="H (Hospitalized)")],
                  noshow$days[which(noshow$Barrierclean=="O (Other)")],
                  noshow$days[which(noshow$Barrierclean=="S (Sick)")],
                  noshow$days[which(noshow$Barrierclean=="T (Transportation)")],
                  noshow$days[which(noshow$Barrierclean=="U (Unaware of appt)")],
                  noshow$days[which(noshow$Barrierclean=="W (Weather)")],
                  noshow$days[which(noshow$Barrierclean=="W (Work conflict)")]))







startdate<-ymd(rep("2018/3/1",length(noshow$days)))
enddate<-ymd(rep("2019/2/27",length(noshow$days)))

logic1<-as.character(as.numeric(difftime(ymd(noshow$New.appt.Date),startdate,units="days"))>=0)
logic2<-as.character(as.numeric(difftime(enddate,ymd(noshow$New.appt.Date),units="days"))>=0)

logic1[is.na(logic1)]<-"False"
logic2[is.na(logic2)]<-"False"

sort=unique(which(logic1=="TRUE"),which(logic2=="TRUE"))



noshow$Appt.Out.come=as.character(noshow$Appt.Out.come)
sum(1*(noshow$Appt.Out.come[sort]=="NOS"))
sum(1*(noshow$Appt.Out.come[sort]=="ARR"))
sum(1*(noshow$Appt.Out.come[sort]=="NOS"))/length(sort)
sum(1*(noshow$Appt.Out.come[sort]=="ARR"))/length(sort)



call1<-length(sort)-sum(1*(noshow$Call.Dates.1[sort]==""))
call2<-length(sort)-sum(1*(noshow$Call.Dates.2[sort]==""))
call3<-length(sort)-sum(1*(noshow$Call.Dates.3[sort]==""))

sum(call1,call2,call3)

length(noshow$New.appt.Date[sort])-sum(1*(is.na(ymd(noshow$New.appt.Date[sort]))))
(length(noshow$New.appt.Date[sort])-sum(1*(is.na(ymd(noshow$New.appt.Date[sort])))))/length(noshow$New.appt.Date[sort])


### Calculate no show rate ###

length(which(noshow$Appt.Out.come == "ARR"))
length(which(noshow$Appt.Out.come == "NOS"))
length(which(noshow$Appt.Out.come == "CAN"))
length(which(noshow$Appt.Out.come == "RSC"))
length(which(noshow$Appt.Out.come == "BUM"))
length(which(noshow$Appt.Out.come == ""))





length(which(noshow$days<=180 & noshow$Appt.Out.come == "ARR"))


for (i in 22:51) {
        noshow[,i]=as.character(noshow[,i])
        noshow[,i][noshow[,i]==""]=NA
}

missing_label_barrier = which(1*is.na(noshow$Barrierclean)==1)


results = rep(1,7511)
for (i in 22:51) {
        results = results&is.na(noshow[missing_label_barrier,i])      
}

comment=is.na(noshow[missing_label_barrier,22])&is.na(noshow[missing_label_barrier,23])&
is.na(noshow[missing_label_barrier,24])&is.na(noshow[missing_label_barrier,25])&
is.na(noshow[missing_label_barrier,26])&is.na(noshow[missing_label_barrier,27])&
is.na(noshow[missing_label_barrier,28])&is.na(noshow[missing_label_barrier,29])&
is.na(noshow[missing_label_barrier,30])&is.na(noshow[missing_label_barrier,31])&
is.na(noshow[missing_label_barrier,32])&is.na(noshow[missing_label_barrier,33])&
is.na(noshow[missing_label_barrier,34])&is.na(noshow[missing_label_barrier,35])&
is.na(noshow[missing_label_barrier,36])&is.na(noshow[missing_label_barrier,37])&
is.na(noshow[missing_label_barrier,38])&is.na(noshow[missing_label_barrier,39])&
is.na(noshow[missing_label_barrier,40])&is.na(noshow[missing_label_barrier,41])&
is.na(noshow[missing_label_barrier,42])&is.na(noshow[missing_label_barrier,43])&
is.na(noshow[missing_label_barrier,44])&is.na(noshow[missing_label_barrier,45])&
is.na(noshow[missing_label_barrier,46])&is.na(noshow[missing_label_barrier,47])&
is.na(noshow[missing_label_barrier,48])&is.na(noshow[missing_label_barrier,49])&
is.na(noshow[missing_label_barrier,50])&is.na(noshow[missing_label_barrier,51])




## comment == 1 means true missing, comment == 0 means false missing (Or say other)##
noshow$Barrierclean[missing_label_barrier][which(1*comment==0)]="O (Other)"
sum(1*is.na(noshow$Barrierclean))

length(which(noshow$Barrierclean=="O (Other)"))


3353/7511
2515/7511

5376-3353


retention_start<-ymd(rep("2019/3/1",length(noshow$days)))
retention_end<-ymd(rep("2019/8/31",length(noshow$days)))

retention_label<-which(as.character(as.numeric(difftime(ymd(noshow$New.appt.Date),retention_start,units="days"))>=0 & as.numeric(difftime(retention_end,ymd(noshow$New.appt.Date),units="days"))>=0)=="TRUE")

## how many patients retented in care #
length(which(noshow$Appt.Out.come[retention_label]=="ARR"))

744/7511
744/(7511-467)






## Find unique MRN ##



length(which(freq_table[,2]==1))
length(which(freq_table[,2]==2))
length(which(freq_table[,2]==3))
length(which(freq_table[,2]>=10))
max(freq_table[,2])
quantile(freq_table[,2],c(0.25,0.5,0.75))
mean(freq_table[,2])
sd(freq_table[,2])
hist(freq_table[,2],breaks = c(0:25),col = "black",border = "red",  xlab = "No show times", main = "Frequency of no show times")


### Time interval between noshow date and arrival ##
arrive_label = which(noshow$Appt.Out.come=='ARR')



subnoshow = noshow[arrive_label,]

quantile(subnoshow$days,c(0.25,0.5,0.75),na.rm = T)

kruskal.test(list(
        subnoshow$days[which(subnoshow$Barrierclean=="A (Appointment conflict)")],
        subnoshow$days[which(subnoshow$Barrierclean=="C (Childcare)")],
        subnoshow$days[which(subnoshow$Barrierclean=="F (Forgot)")],
        subnoshow$days[which(subnoshow$Barrierclean=="H (Hospitalized)")],
        subnoshow$days[which(subnoshow$Barrierclean=="O (Other)")],
        subnoshow$days[which(subnoshow$Barrierclean=="S (Sick)")],
        subnoshow$days[which(subnoshow$Barrierclean=="T (Transportation)")],
        subnoshow$days[which(subnoshow$Barrierclean=="U (Unaware of appt)")],
        subnoshow$days[which(subnoshow$Barrierclean=="W (Weather)")],
        subnoshow$days[which(subnoshow$Barrierclean=="W (Work conflict)")]))



sum(1*is.na(subnoshow$Barrierclean))

quantile(subnoshow$days[is.na(subnoshow$Barrierclean)],c(0.25,0.5,0.75),na.rm = T)
kruskal.test(list(subnoshow$days[is.na(subnoshow$Barrierclean)],
                  subnoshow$days[!is.na(subnoshow$Barrierclean)]))


length(which(subnoshow$Barrierclean=="A (Appointment conflict)"))
quantile(subnoshow$days[which(subnoshow$Barrierclean=="A (Appointment conflict)")],c(0.25,0.5,0.75),na.rm = T)
kruskal.test(list(subnoshow$days[which(subnoshow$Barrierclean=="A (Appointment conflict)")],
                  subnoshow$days[which(subnoshow$Barrierclean!="A (Appointment conflict)")]))


length(which(subnoshow$Barrierclean=="C (Childcare)"))
quantile(subnoshow$days[which(subnoshow$Barrierclean=="C (Childcare)")],c(0.25,0.5,0.75),na.rm = T)
kruskal.test(list(subnoshow$days[which(subnoshow$Barrierclean=="C (Childcare)")],
                  subnoshow$days[which(subnoshow$Barrierclean!="C (Childcare)")]))


length(which(subnoshow$Barrierclean=="F (Forgot)"))
quantile(subnoshow$days[which(subnoshow$Barrierclean=="F (Forgot)")],c(0.25,0.5,0.75),na.rm = T)
kruskal.test(list(subnoshow$days[which(subnoshow$Barrierclean=="F (Forgot)")],
                  subnoshow$days[which(subnoshow$Barrierclean!="F (Forgot)")]))


length(which(subnoshow$Barrierclean=="H (Hospitalized)"))
quantile(subnoshow$days[which(subnoshow$Barrierclean=="H (Hospitalized)")],c(0.25,0.5,0.75),na.rm = T)
kruskal.test(list(subnoshow$days[which(subnoshow$Barrierclean=="H (Hospitalized)")],
                  subnoshow$days[which(subnoshow$Barrierclean!="H (Hospitalized)")]))


length(which(subnoshow$Barrierclean=="O (Other)"))
quantile(subnoshow$days[which(subnoshow$Barrierclean=="O (Other)")],c(0.25,0.5,0.75),na.rm = T)
kruskal.test(list(subnoshow$days[which(subnoshow$Barrierclean=="O (Other)")],
                  subnoshow$days[which(subnoshow$Barrierclean!="O (Other)")]))


length(which(subnoshow$Barrierclean=="S (Sick)"))
quantile(subnoshow$days[which(subnoshow$Barrierclean=="S (Sick)")],c(0.25,0.5,0.75),na.rm = T)
kruskal.test(list(subnoshow$days[which(subnoshow$Barrierclean=="S (Sick)")],
                  subnoshow$days[which(subnoshow$Barrierclean!="S (Sick)")]))


length(which(subnoshow$Barrierclean=="T (Transportation)"))
quantile(subnoshow$days[which(subnoshow$Barrierclean=="T (Transportation)")],c(0.25,0.5,0.75),na.rm = T)
kruskal.test(list(subnoshow$days[which(subnoshow$Barrierclean=="T (Transportation)")],
                  subnoshow$days[which(subnoshow$Barrierclean!="T (Transportation)")]))


length(which(subnoshow$Barrierclean=="U (Unaware of appt)"))
quantile(subnoshow$days[which(subnoshow$Barrierclean=="U (Unaware of appt)")],c(0.25,0.5,0.75),na.rm = T)
kruskal.test(list(subnoshow$days[which(subnoshow$Barrierclean=="U (Unaware of appt)")],
                  subnoshow$days[which(subnoshow$Barrierclean!="U (Unaware of appt)")]))


length(which(subnoshow$Barrierclean=="W (Weather)"))
quantile(subnoshow$days[which(subnoshow$Barrierclean=="W (Weather)")],c(0.25,0.5,0.75),na.rm = T)
kruskal.test(list(subnoshow$days[which(subnoshow$Barrierclean=="W (Weather)")],
                  subnoshow$days[which(subnoshow$Barrierclean!="W (Weather)")]))



length(which(subnoshow$Barrierclean=="W (Work conflict)"))
quantile(subnoshow$days[which(subnoshow$Barrierclean=="W (Work conflict)")],c(0.25,0.5,0.75),na.rm = T)
kruskal.test(list(subnoshow$days[which(subnoshow$Barrierclean=="W (Work conflict)")],
                  subnoshow$days[which(subnoshow$Barrierclean!="W (Work conflict)")]))




freq_table_barrier = as.data.frame(table(noshow$Barrierclean))
freq_table_barrier[,1]=as.character(freq_table_barrier[,1])
need_to_add_barrier= c("Missing", 3353)
freq_table_barrier = rbind(freq_table_barrier,need_to_add_barrier)
freq_table_barrier[,1]=as.factor(freq_table_barrier[,1])
freq_table_barrier[,2]=as.numeric(freq_table_barrier[,2])
colnames(freq_table_barrier)[1]="Barrier"
colnames(freq_table_barrier)[2]="Frequency"

ggplot(freq_table_barrier, aes(x = Barrier, y = Frequency))+
        geom_bar(stat = "identity",fill = "lightblue", colour = "black")+
        theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
        ggtitle('Frequency of Barrier (Focus on all observations)')+theme(plot.title=element_text(hjust=0.5))









freq_table_barrier_arrival = as.data.frame(table(subnoshow$Barrierclean))
freq_table_barrier_arrival[,1]=as.character(freq_table_barrier_arrival[,1])
need_to_add_barrier_arrival = c("Missing", 1582)
freq_table_barrier_arrival = rbind(freq_table_barrier_arrival,need_to_add_barrier_arrival)
freq_table_barrier_arrival[,1]=as.factor(freq_table_barrier_arrival[,1])
freq_table_barrier_arrival[,2]=as.numeric(freq_table_barrier_arrival[,2])
colnames(freq_table_barrier_arrival)[1]="Barrier"
colnames(freq_table_barrier_arrival)[2]="Frequency"

ggplot(freq_table_barrier_arrival, aes(x = Barrier, y = Frequency))+
geom_bar(stat = "identity",fill = "lightblue", colour = "black")+
theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
ggtitle('Frequency of Barrier (Focus on the patients who arrived)')+theme(plot.title=element_text(hjust=0.5))


#################################################
rm(list=ls())
library(lubridate)
library(ggplot2)

setwd("C:/Users/cuisa/Desktop/PCCP/No Show")
noshow2<-read.csv("No_Show_2.csv",header = T)
demo<- read.csv('demographics.csv',header = T)
colnames(demo)[1]="mrn"

## Data cleaning ##
length(which(noshow2$arr==0&noshow2$bmp==0&noshow2$can==0&noshow2$nos==0))
noshow2 = noshow2[-which(noshow2$arr==0&noshow2$bmp==0&noshow2$can==0&noshow2$nos==0),]


##  find unique mrn ###

uniq = unique(noshow2$mrn)
length(uniq)

extract= rep(0,length(uniq))
for (i in 1:length(uniq)){
  a=which(noshow2$mrn == uniq[i])
  extract[i]=a[1]
}

noshow2ext = noshow2[extract,]



## Merging data sets ###

merged_data=merge(demo, noshow2ext, by = "mrn", all = TRUE)

merged_data= merged_data[,-c(2,3,4,7,8,14,15,16,17,18,19,30,31,32,33,34)]

cleaning=is.na(merged_data$dob)

merged_data =merged_data[!cleaning,]

## basic statistics of 1316 patients ###


### fill missing race/housing/insurance/ethinicity

missing_race<-read.csv("missing_race.csv",header = F)
missing_race$V1 = as.character(missing_race$V1)
missing_race$V1[1]=2287
missing_race$V1=as.numeric(missing_race$V1)
missing_race$V2 = as.character(missing_race$V2)


missing_housing = read.csv("missing_housing.csv", header = F)
missing_housing$V1 = as.character(missing_housing$V1)
missing_housing$V1[1]=3396
missing_housing$V1 =as.numeric(missing_housing$V1)
missing_housing$V2 = as.character(missing_housing$V2)


missing_insurance = read.csv("missing_insurance.csv", header = F)
missing_insurance$V1 = as.character(missing_insurance$V1)
missing_insurance$V1[1]=28701
missing_insurance$V1 =as.numeric(missing_insurance$V1)
missing_insurance$V2 = as.character(missing_insurance$V2)


missing_race2<-read.csv("missing_race2.csv",header = F)
missing_race2$V1 = as.character(missing_race2$V1)
missing_race2$V1[1]=3396
missing_race2$V1=as.numeric(missing_race2$V1)
missing_race2$V2 = as.character(missing_race2$V2)

missing_ethnicity<-read.csv("missing_ethnicity.csv",header = F)
missing_ethnicity= missing_ethnicity[-1,]
missing_ethnicity$V1 = as.character(missing_ethnicity$V1)
missing_ethnicity$V1=as.numeric(missing_ethnicity$V1)
missing_ethnicity$V2 = as.character(missing_ethnicity$V2)






### HIV risk factor ##

merged_data$HIV.Risk.Factor = as.character(merged_data$HIV.Risk.Factor)
merged_data$HIV.Risk.Factor[which(merged_data$HIV.Risk.Factor=="Not Specified")]= "Missing"
merged_data$HIV.Risk.Factor[is.na(merged_data$HIV.Risk.Factor)]="Missing"

risk_factors = read.csv(file="Risk_factors.csv", header = T)
risk_factors$X= as.character(risk_factors$X)


for (i in 1:length(risk_factors$risk_factor_missing_mrn)) {
  label=which(merged_data$mrn==risk_factors$risk_factor_missing_mrn[i]) 
  merged_data$HIV.Risk.Factor[label]=risk_factors$X[i]
}



merged_data$HIV.Risk.Factor=as.factor(merged_data$HIV.Risk.Factor)
levels(merged_data$HIV.Risk.Factor)

table(merged_data$HIV.Risk.Factor)


merged_data_new = merged_data[-which(merged_data$HIV.Risk.Factor=="Missing"),]

ggplot(merged_data_new, mapping =  
         aes(x=factor(HIV.Risk.Factor,levels=c("Heterosexual", "MSM","IDU", "MSM and IDU", "Perinatal", "Transfusion")  )))+
  geom_bar(stat="count", width = 0.7, fill="steelblue")+
  xlab("HIV risk factors") + ylab("Number of patients")+  
  geom_text(stat="count",aes(label=..count..), vjust=-0.5, color="black", size=3.5)+
  geom_text(stat="count",
            label=c("45.4%","38.6%","12.5%","2.2%","0.8%","0.5%"),
            colour = "black",  vjust=c(2,2,2,2.7,1.8,1.6), size=3.5)+
  theme(axis.title.x = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =90))+
  scale_y_continuous(breaks=seq(0,600,100),
                     limits=c(0,600)) 


### Insurance status ##


merged_data$Last.Primary.Insurance.In.Span = as.character(merged_data$Last.Primary.Insurance.In.Span)


for (i in 1:length(missing_insurance$V1)) {
  merged_data$Last.Primary.Insurance.In.Span[which(merged_data$mrn == missing_insurance$V1[i])]=missing_insurance$V2[i]
}


merged_data$Last.Primary.Insurance.In.Span[which(merged_data$Last.Primary.Insurance.In.Span=="Medicare (unspecified)")]="Medicare"
merged_data$Last.Primary.Insurance.In.Span[which(merged_data$Last.Primary.Insurance.In.Span=="Medicare Part A/B")]="Medicare"
merged_data$Last.Primary.Insurance.In.Span[which(merged_data$Last.Primary.Insurance.In.Span=="Private - Employer")]="Private"
merged_data$Last.Primary.Insurance.In.Span[which(merged_data$Last.Primary.Insurance.In.Span=="Private - Individual")]="Private"
merged_data$Last.Primary.Insurance.In.Span[which(merged_data$Last.Primary.Insurance.In.Span=="Private Employer")]="Private"


table(merged_data$Last.Primary.Insurance.In.Span)


ggplot(merged_data, mapping =  
         aes(x=factor(Last.Primary.Insurance.In.Span,
                      levels=c("Medicaid","Medicare","Private","No Insurance")  )))+
  geom_bar(stat="count", width = 0.5, fill="steelblue")+
  xlab("Insurance status") + ylab("Number of patients")+  
  geom_text(stat="count",aes(label=..count..), vjust=-0.5, color="black", size=3.5)+
  geom_text(stat="count",
            label=c("49.5%", "23.4%", "24.8%", "2.2%"),
            colour = "black",  vjust=c(2,2,2,2.4), size=3.5)+
  theme(axis.title.x = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =90))+
  scale_y_continuous(breaks=seq(0,700,100),
                     limits=c(0,700)) 




## Housing status ###




merged_data$Last.Housing.Arr.In.Span = as.character(merged_data$Last.Housing.Arr.In.Span)


for (i in 1:length(missing_housing$V1)) {
  merged_data$Last.Housing.Arr.In.Span[which(merged_data$mrn == missing_housing$V1[i])]=missing_housing$V2[i]
}




table(merged_data$Last.Housing.Arr.In.Span)


ggplot(merged_data, mapping =  
         aes(x=factor(Last.Housing.Arr.In.Span,
                      levels=c("Stable/Permanent","Temporary","Unstable")  )))+
  geom_bar(stat="count", width = 0.5, fill="steelblue")+
  xlab("Housing status") + ylab("Number of patients")+  
  geom_text(stat="count",aes(label=..count..), vjust=-0.5, color="black", size=3.5)+
  geom_text(stat="count",
            label=c("86.7%", "4.2%", "9.1%"),
            colour = "black",  vjust=c(1.7,2.6,1.6), size=3.5)+
  theme(axis.title.x = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =90))+
  scale_y_continuous(breaks=seq(0,1200,100),
                     limits=c(0,1200)) 








## gender ##
table(merged_data$gender)

## age ##
mean(merged_data$age)
sd(merged_data$age)
quantile(merged_data$age,c(0.25,0.5,0.75))



### Last VL ##
mean(merged_data$Last.VL,na.rm = T)
sd(merged_data$Last.VL,na.rm = T)
quantile(merged_data$Last.VL,na.rm = T,c(0.25,0.5,0.75))



### Last CD4+ ##
mean(merged_data$Last.CD4.Count,na.rm = T)
sd(merged_data$Last.CD4.Count,na.rm = T)
quantile(merged_data$Last.CD4.Count,na.rm = T,c(0.25,0.5,0.75))



## race ##


merged_data$race = as.character(merged_data$race)
merged_data$ethnicity = as.character(merged_data$ethnicity)

for (i in 1:length(missing_race$V1)) {
  merged_data$race[which(merged_data$mrn == missing_race$V1[i])]=missing_race$V2[i]
}



merged_data$race[which(merged_data$race=="ASIAN")]="Asian"
merged_data$race[which(merged_data$race=="BLACK")]="Black"
merged_data$race[which(merged_data$race=="DECLINED")]="Missing"
merged_data$race[which(merged_data$race=="Hispanic")]="Hispanic or Latino"
merged_data$race[which(merged_data$race=="HISPANIC OR LATINO")]="Hispanic or Latino"
merged_data$race[which(merged_data$race=="NOT REPORTED")]="Missing"
merged_data$race[which(merged_data$race=="UNKNOWN")]="Missing"
merged_data$race[which(merged_data$race=="WHITE")]="White"
merged_data$race[which(merged_data$race=="white")]="White"

merged_data$ethnicity[which(merged_data$ethnicity=="")]="Missing"
merged_data$ethnicity[which(merged_data$ethnicity=="Declined")]="Missing"
merged_data$ethnicity[which(merged_data$ethnicity=="Not Reported")]="Missing"
merged_data$ethnicity[which(merged_data$ethnicity=="Pt Unavailable")]="Missing"
merged_data$ethnicity[which(merged_data$ethnicity=="Unknown")]="Missing"


for (i in 1:length(missing_race2$V1)) {
  merged_data$race[which(merged_data$mrn == missing_race2$V1[i])]=missing_race2$V2[i]
}


for (i in 1:length(missing_ethnicity$V1)) {
  merged_data$ethnicity[which(merged_data$mrn == missing_ethnicity$V1[i])]=missing_ethnicity$V2[i]
}



table(merged_data$race)
table(merged_data$ethnicity)


merged_data$eth_race = rep("0", length(merged_data$mrn))
merged_data$eth_race[merged_data$race=="AMERICAN INDIAN OR ALASKA NATIVE"]="Other"
merged_data$eth_race[merged_data$race=="Asian"]="Other"
merged_data$eth_race[merged_data$race=="NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"]="Other"
merged_data$eth_race[merged_data$race=="Black"&merged_data$ethnicity=="Hispanic or Latino"]="Hispanic or Latino"
merged_data$eth_race[merged_data$race=="Black"&merged_data$ethnicity=="Not Hispanic or Latino"]="NH Black"
merged_data$eth_race[merged_data$race=="White"&merged_data$ethnicity=="Hispanic or Latino"]="Hispanic or Latino"
merged_data$eth_race[merged_data$race=="White"&merged_data$ethnicity=="Not Hispanic or Latino"]="NH White"
merged_data$eth_race[merged_data$race=="Hispanic or Latino"]="Hispanic or Latino"

table(merged_data$eth_race)


ggplot(merged_data, mapping =  
         aes(x=factor(eth_race,
                      levels=c("NH Black","NH White","Hispanic or Latino", "Other")  )))+
  geom_bar(stat="count", width = 0.5, fill="steelblue")+
  xlab("Race/Ethnicity") + ylab("Number of patients")+  
  geom_text(stat="count",aes(label=..count..), vjust=-0.5, color="black", size=3.5)+
  geom_text(stat="count",
            label=c("76.3%", "12.5%", "9.9%", "1.2%"),
            colour = "black",  vjust=c(1.5,1.5,1.5,1.5), size=3.5)+
  theme(axis.title.x = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =90))+
  scale_y_continuous(breaks=seq(0,1100,100),
                     limits=c(0,1100)) 




## statistics of outcome ###


noshowclean = noshow2[-extract,]
table(noshowclean$status)

## ARR ##
arrdata = noshowclean[ which(noshowclean$status == "ARR") , ]
arrdatatable = as.data.frame( table(arrdata$mrn))
arr=c(rep(0,(1268-length(arrdatatable[,1]))),arrdatatable[,2])
sum(arr)
mean(arr)
sd(arr)
quantile(arr,c(0.25,0.5,0.75))
min(arr)
max(arr)

arr_frame = as.data.frame (table(arr))
ggplot(arr_frame,aes(arr,Freq))+geom_bar(stat="identity",fill="steelblue")+
  xlab("Arrival times") + ylab("Frequency") 





## BMP ##
bmpdata = noshowclean[ which(noshowclean$status == "BMP") , ]
bmpdatatable = as.data.frame( table(bmpdata$mrn))
bmp=c(rep(0,(1268-length(bmpdatatable[,1]))),bmpdatatable[,2])
sum(bmp)
mean(bmp)
sd(bmp)
quantile(bmp,c(0.25,0.5,0.75))
min(bmp)
max(bmp)

bmp_frame = as.data.frame (table(bmp))
ggplot(bmp_frame,aes(bmp,Freq))+geom_bar(stat="identity",fill="steelblue")+
  xlab("Bumped times") + ylab("Frequency") 

## CAN ##
candata = noshowclean[ which(noshowclean$status == "CAN") , ]
candatatable = as.data.frame( table(candata$mrn))
can=c(rep(0,(1268-length(candatatable[,1]))),candatatable[,2])
sum(can)
mean(can)
sd(can)
quantile(can,c(0.25,0.5,0.75))
min(can)
max(can)

can_frame = as.data.frame (table(can))
ggplot(can_frame,aes(can,Freq))+geom_bar(stat="identity",fill="steelblue")+
  xlab("Cancelled times") + ylab("Frequency") 


## NOS ##
nosdata = noshowclean[ which(noshowclean$status == "NOS") , ]
nosdatatable = as.data.frame( table(nosdata$mrn))

nos=c(rep(0,(1268-length(nosdatatable[,1]))), nosdatatable[,2])
sum(nos)
mean(nos)
sd(nos)
quantile(nos,c(0.25,0.5,0.75))
min(nos)
max(nos)
1268-length(nosdatatable[,1])
length(which(nosdatatable[,2]==1))
length(which(nosdatatable[,2]==2))
length(which(nosdatatable[,2]==3))
length(which(nosdatatable[,2]>=10))
hist(nos,breaks = c(0:20),col = "grey",border="black",xlab = "No show times", main = "Frequency of no show times")

nos_frame = as.data.frame (table(nos))

ggplot(nos_frame,aes(nos,Freq))+geom_bar(stat="identity",fill="steelblue")+
  geom_text(aes(label=Freq), vjust=-0.5, color="black", size=3.5)+
  xlab("Number of No Show Appointments") + ylab("Number of Patients")+
  theme(axis.title.x = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =90))+
  scale_y_continuous(breaks=seq(0,300,50),
                     limits=c(0,300)) 

## No show rate ##

3848/(5863+817+2537+3848)



## retention in care rate ##

noshowclean$appt_dt<- as.character(noshowclean$appt_dt)

startdate<-mdy(rep("3/1/2019",length(noshowclean[,1])))
diff1<-as.numeric(difftime(mdy(noshowclean$appt_dt),startdate, units="days"))
enddate<-mdy(rep("8/31/2019",length(noshowclean[,1])))
diff2<-as.numeric(difftime(enddate,mdy(noshowclean$appt_dt), units="days"))

intime=noshowclean[(diff1>=0&diff2>=0),]
rention=intime[which(intime$status=="ARR"),]
uniq2 = unique(rention$mrn)
length(uniq2)

1017/1268



## Time interval between noshow date and attend date ###



blankmrn = c()
blanknoshodate = c()
blankarrdate = c()

for (i in 1:length(uniq)) {
  eachmrn = which(noshowclean$mrn == uniq[i])
  each_instance = length(eachmrn)
  Noshowlabel = which(noshowclean$status[eachmrn]=="NOS")
  if (is.na(Noshowlabel[1])){
    next}
  
  for (j in 1:length(Noshowlabel)){
    start = Noshowlabel[j]+1
    if (start>each_instance){
      blankmrn= append(blankmrn,noshowclean$mrn[eachmrn][Noshowlabel[j]])
      blanknoshodate=append(blanknoshodate,noshowclean$appt_dt[eachmrn][Noshowlabel[j]])
      blankarrdate=append(blankarrdate,NA)
    }else{
      
      while (noshowclean$status[eachmrn][start] != "ARR" ) {
        
        start = start+1
        if (start>each_instance){
          start = start-1
          blankmrn=append(blankmrn,noshowclean$mrn[eachmrn][start])
          blanknoshodate=append(blanknoshodate,noshowclean$appt_dt[eachmrn][Noshowlabel[j]])
          blankarrdate=append(blankarrdate,NA)         
          break}
        
      }
      blankmrn=append(blankmrn,noshowclean$mrn[eachmrn][start])
      blanknoshodate=append(blanknoshodate,noshowclean$appt_dt[eachmrn][Noshowlabel[j]])
      blankarrdate=append(blankarrdate,noshowclean$appt_dt[eachmrn][start])    
    }
    
  }
  
  
  
  
}

intervaldata= data.frame(mrn=blankmrn, noshowdate = blanknoshodate, arrdate = blankarrdate)



blanklogic = c()
uniq3 = unique(intervaldata$mrn)

for (i in 1:length(uniq3)){
  eachmrn = which(intervaldata$mrn == uniq3[i])
  blanklogic = append(blanklogic,duplicated(intervaldata$arrdate[eachmrn]))
}

intervalclean = intervaldata[!blanklogic,]
intervalclean =intervalclean[!is.na(intervalclean$arrdate),]


interval1<-as.numeric(difftime(mdy(intervalclean$arrdate),mdy(intervalclean$noshowdate), units="days"))


mean(interval1)
sd(interval1)
quantile(interval1, c(0.25,0.5,0.75))
min(interval1)
max(interval1)

interval_noshow_attend = data.frame(time = interval1)

ggplot(interval_noshow_attend, aes(x=time)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+
  theme(axis.title.x = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =90))+
  xlab("Time interval between no-show date and next attended date (Days)") + ylab("Density")+
  geom_vline(aes(xintercept=median(time, na.rm=T)),   
             color="red", linetype="dashed", size=1)+annotate("text", x=55, y=-0.004, label="Median = 42")





## Time interval between noshow date and next appointment date ###





blankmrn2 = c()
blanknoshodate2 = c()
blankappdate = c()

for (i in 1:length(uniq)) {
  eachmrn = which(noshowclean$mrn == uniq[i])
  each_instance = length(eachmrn)
  Noshowlabel = which(noshowclean$status[eachmrn]=="NOS")
  if (is.na(Noshowlabel[1])){
    next}
  
  for (j in 1:length(Noshowlabel)){
    start = Noshowlabel[j]+1
    if (start>each_instance){
      blankmrn2= append(blankmrn2,noshowclean$mrn[eachmrn][Noshowlabel[j]])
      blanknoshodate2=append(blanknoshodate2,noshowclean$appt_dt[eachmrn][Noshowlabel[j]])
      blankappdate=append(blankappdate,NA)
    }else{
      
      
      blankmrn2=append(blankmrn2,noshowclean$mrn[eachmrn][start])
      blanknoshodate2=append(blanknoshodate2,noshowclean$appt_dt[eachmrn][Noshowlabel[j]])
      blankappdate=append(blankappdate,noshowclean$appt_dt[eachmrn][start])    
    }
    
  }
  
  
  
  
}

intervaldata2= data.frame(mrn=blankmrn2, noshowdate = blanknoshodate2, nextappdate = blankappdate)




intervalclean2 =intervaldata2[!is.na(intervaldata2$nextappdate),]


interval2<-as.numeric(difftime(mdy(intervalclean2$nextappdate),mdy(intervalclean2$noshowdate), units="days"))


mean(interval2)
sd(interval2)
quantile(interval2, c(0.25,0.5,0.75))
min(interval2)
max(interval2)



interval_noshow_app = data.frame(time = interval2)

ggplot(interval_noshow_app, aes(x=time)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+
  theme(axis.title.x = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =90))+
  xlab("Time interval between no-show date and next appointment date (Days)") + ylab("Density")+
  geom_vline(aes(xintercept=median(time, na.rm=T)),   
             color="red", linetype="dashed", size=1)+annotate("text", x=55, y=-0.004, label="Median = 24")





### Analysis ###


## 1.Years of HIV diagnosis ###


todaydate<-mdy(rep("8/31/2019",length(merged_data[,1])))
years_of_HIV<-round(as.numeric(difftime(todaydate,mdy(merged_data$HIV.Date), units="days"))/365)

merged_data= cbind(merged_data,years_of_HIV)

mean(merged_data$years_of_HIV,na.rm = T)
sd(merged_data$years_of_HIV, na.rm = T)
quantile(merged_data$years_of_HIV,c(0.25,0.5,0.75), na.rm = T)





## response variable 1: no show rate ##


noshowrate = merged_data$nos/(merged_data$arr + merged_data$bmp + merged_data$can + merged_data$nos)

merged_data= cbind(merged_data,noshowrate)



## response variable 2: whether retention or not ##

retention_in_care = 1*merged_data$mrn%in%uniq2

retention_in_care = as.numeric(retention_in_care)


merged_data= cbind(merged_data,retention_in_care)



## response variable 3: whether VL suppression or not ##

VL_supp=rep(0, length(merged_data[,1]))
VL_supp[which(merged_data$Last.VL<200)]=1

merged_data= cbind(merged_data,VL_supp)

length(which(merged_data$VL_supp==1))

## LM outcome 1: no show rate ##


noshowrate_LM=lm(merged_data$noshowrate ~ merged_data$age + factor(merged_data$gender, levels = c("M","F",""))+ merged_data$years_of_HIV + 
                   factor(merged_data$eth_race, levels = c("NH Black", "NH White", "Hispanic or Latino", "Other")) + 
                   factor(merged_data$Last.Housing.Arr.In.Span, levels = c("Unstable","Temporary", "Stable/Permanent"))+
                   factor(merged_data$Last.Primary.Insurance.In.Span, levels = c("Medicare", "Medicaid", "Private", "No Insurance")))
summary(noshowrate_LM)





## GLM outcome 2: retention in care rate##

retention_GLM=glm(merged_data$retention_in_care ~ merged_data$age +merged_data$noshowrate+merged_data$years_of_HIV + 
                    factor(merged_data$gender, levels = c("M","F",""))+ 
                    factor(merged_data$eth_race, levels = c("NH Black", "NH White", "Hispanic or Latino", "Other")) + 
                    factor(merged_data$Last.Housing.Arr.In.Span, levels = c("Unstable","Temporary", "Stable/Permanent"))+
                    factor(merged_data$Last.Primary.Insurance.In.Span, levels = c("Medicare", "Medicaid", "Private", "No Insurance")), family=binomial(link="logit"))
summary(retention_GLM)




exp(2.041855-1.96*0.58711-3.586*0.75-1.96*0.75*0.364920-0.81-1.96*0.224158)/(1+exp(2.041855-1.96*0.58711-3.586*0.75-1.96*0.75*0.364920-0.81-1.96*0.224158))
exp(2.041855+1.96*0.58711-3.586*0.75+1.96*0.75*0.364920-0.81+1.96*0.224158)/(1+exp(2.041855+1.96*0.58711-3.586*0.75+1.96*0.75*0.364920-0.81+1.96*0.224158))





exp(2.041855-1.96*0.58711-3.586*0.75-1.96*0.75*0.364920)/(1+exp(2.041855-1.96*0.58711-3.586*0.75-1.96*0.75*0.364920))
exp(2.041855+1.96*0.58711-3.586*0.75+1.96*0.75*0.364920)/(1+exp(2.041855+1.96*0.58711-3.586*0.75+1.96*0.75*0.364920))



## GLM outcome 3: VL suppression##


VL_supp_GLM=glm(merged_data$VL_supp~ merged_data$age + merged_data$years_of_HIV + factor(merged_data$gender, levels = c("M","F","")) + 
                  factor(merged_data$eth_race, levels = c("NH Black", "NH White", "Hispanic or Latino", "Other")) + 
                  factor(merged_data$Last.Housing.Arr.In.Span, levels = c("Unstable","Temporary", "Stable/Permanent"))+
                  factor(merged_data$Last.Primary.Insurance.In.Span, levels = c("Medicare", "Medicaid", "Private", "No Insurance")) + 
                  merged_data$noshowrate+factor(merged_data$retention_in_care),family=binomial(link="logit"))
summary(VL_supp_GLM)






### Number of patients vs No show rate (categorical) ###

noshowrate_cat = rep(1,length(merged_data$mrn))


noshowrate_cat[merged_data$noshowrate==0]="0%"
noshowrate_cat[merged_data$noshowrate>0&merged_data$noshowrate<=0.25]="1%-25%"
noshowrate_cat[merged_data$noshowrate>0.25&merged_data$noshowrate<=0.5]="26%-50%"
noshowrate_cat[merged_data$noshowrate>0.5&merged_data$noshowrate<=0.75]="51%-75%"
noshowrate_cat[merged_data$noshowrate>0.75]=">76%"

merged_data= cbind(merged_data,noshowrate_cat)


merged_data$noshowrate_cat = factor(merged_data$noshowrate_cat,levels = c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%"))





ggplot(data=merged_data,mapping=aes(x=  factor(noshowrate_cat, levels = c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%"))         ))+
  geom_bar(stat="count",width=0.5, fill="steelblue")+
  geom_text(stat='count',aes(label=..count..), vjust=-0.5, color="black", size=3.5)+
  geom_text(stat="count",
            label=paste(round(table(merged_data$noshowrate_cat)/sum(table(merged_data$noshowrate_cat))*100),'%',sep=''),
            colour = "black", vjust=c(8,14,15,4,1.5), size=4.7)+
  xlab("No Show Rate") + ylab("Number of Patients")+
  theme(axis.title.x = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =90))+
  scale_y_continuous(breaks=seq(0,450,50),
                     limits=c(0,450)) 




### Mean number of attended appointments vs No show rate  ###

mean_num_arr=c(
  round(sum(merged_data$arr[merged_data$noshowrate_cat=="0%"])/length(merged_data$arr[merged_data$noshowrate_cat=="0%"]),1),
  round(sum(merged_data$arr[merged_data$noshowrate_cat=="1%-25%"])/length(merged_data$arr[merged_data$noshowrate_cat=="1%-25%"]),1),
  round(sum(merged_data$arr[merged_data$noshowrate_cat=="26%-50%"])/length(merged_data$arr[merged_data$noshowrate_cat=="26%-50%"]),1),
  round(sum(merged_data$arr[merged_data$noshowrate_cat=="51%-75%"])/length(merged_data$arr[merged_data$noshowrate_cat=="51%-75%"]),1),
  round(sum(merged_data$arr[merged_data$noshowrate_cat==">76%"])/length(merged_data$arr[merged_data$noshowrate_cat==">76%"]),1)
)

noshowrate_level = c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%")


mean_vs_noshowrate = data.frame(mean_num_arr,noshowrate_level)






ggplot(mean_vs_noshowrate,aes(factor(noshowrate_level, levels = c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%")),mean_num_arr))+geom_bar(stat="identity",fill="steelblue",width=0.5)+
  geom_text(aes(label=mean_num_arr), vjust=-0.5, color="black", size=3.5)+
  xlab("No Show Rate") + ylab("Mean # of Attended Appointments ")+
  theme(axis.title.x = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =90))+
  scale_y_continuous(breaks=seq(0,6,1),
                     limits=c(0,6)) 




### Retention in care vs No show rate ###

table(merged_data$noshowrate_cat,merged_data$retention_in_care)

df_re_vs_no = data.frame(noshowrate=c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%","0%", "1%-25%", "26%-50%", "51%-75%", ">76%"),
                         retention_status=c("Out of care","Out of care","Out of care","Out of care","Out of care","In care","In care","In care","In care","In care"),
                         value= c(16,18,33,23,11,24,34,32,9,0.5),
                         value_p= c("16%","18%","33%","23%","11%","24%","34%","32%","9%","0.5%"))

df_re_vs_no$noshowrate= factor(df_re_vs_no$noshowrate,levels = c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%") )


df_re_vs_no$number = c(40,44,83,57,27,249,349,327,87,5)


ggplot(data=df_re_vs_no,mapping=aes(x=noshowrate, fill=retention_status, y=value))+geom_col(width=0.6,position = "dodge")+
  geom_text(aes(label=value_p), vjust=-0.5, color="black", size=3.5,position=position_dodge(0.6))+
  geom_text(aes(label=number), vjust=1.5, color="black", size=3.5,position=position_dodge(0.5))+
  xlab("No Show Rate") + ylab("Percentage of Patients")+
  theme(axis.title.x = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =90))+
  scale_y_continuous(breaks=seq(0,40,5),
                     limits=c(0,40))+annotate("text", x=3, y=40, label="p<0.001")





df_re_vs_no2 = data.frame(noshowrate=c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%","0%", "1%-25%", "26%-50%", "51%-75%", ">76%"),
                          retention_status=c("Out of care","Out of care","Out of care","Out of care","Out of care","In care","In care","In care","In care","In care"),
                          value_p= c("14%", "11%" , "20%", "40%", "84%", "86%", "89%", "80%", "60%", "16%"),
                          value= c(14, 11 , 20, 40, 84, 86, 89, 80, 60, 16))


df_re_vs_no2$noshowrate= factor(df_re_vs_no2$noshowrate,levels = c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%") )

df_re_vs_no2$number = c(40,44,83,57,27,249,349,327,87,5)

ggplot(data=df_re_vs_no2,mapping=aes(x=noshowrate, fill=retention_status, y=value))+geom_col(width=0.6,position = "dodge")+
  geom_text(aes(label=value_p), vjust=-0.5, color="black", size=3.5,position=position_dodge(0.65))+
  geom_text(aes(label=number), vjust=1, color="black", size=3.5,position=position_dodge(0.5))+
  xlab("No Show Rate") + ylab("Percentage of Patients")+scale_fill_brewer(palette = 'Accent')+
  theme(axis.title.x = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =90))+
  scale_y_continuous(breaks=seq(0,100,5),
                     limits=c(0,100)) 



### Viral suppression vs No show rate ###


table(merged_data$noshowrate_cat,merged_data$VL_supp)

df_vs_vs_no = data.frame(noshowrate=c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%","0%", "1%-25%", "26%-50%", "51%-75%", ">76%"),
                         vs_status=c("VS","VS","VS","VS","VS","VL>200","VL>200","VL>200","VL>200","VL>200"),
                         value= c(25,32,32,9,2,10,23,38,25,5),
                         value_p= c("25%","32%","32%","9%","2%","10%","23%","38%","25%","5%"))

df_vs_vs_no$noshowrate= factor(df_vs_vs_no$noshowrate,levels = c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%") )
df_vs_vs_no$vs_cat= factor(df_vs_vs_no$vs_status,levels = c("VS","VL>200") )


df_vs_vs_no$number = c(273,357,350,104,24,16,36,60,40,8)



ggplot(data=df_vs_vs_no,mapping=aes(x=noshowrate, fill=vs_status, y=value))+geom_col(width=0.6,position = "dodge")+
  geom_text(aes(label=value_p), vjust=-0.5, color="black", size=3.5,position=position_dodge(0.6))+
  geom_text(aes(label=number), vjust=1, color="black", size=3.5,position=position_dodge(0.5))+
  xlab("No Show Rate") + ylab("Percentage of Patients")+
  theme(axis.title.x = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =90))+
  scale_y_continuous(breaks=seq(0,45,5),
                     limits=c(0,45)) +annotate("text", x=3, y=45, label="p<0.001")



df_vs_vs_no2 = data.frame(noshowrate=c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%","0%", "1%-25%", "26%-50%", "51%-75%", ">76%"),
                          vs_status=c("VS","VS","VS","VS","VS","VL>200","VL>200","VL>200","VL>200","VL>200"),
                          value= c(94,91,85,72,75,6,9,15,28,25),
                          value_p= c("94%","91%","85%","72%","75%","6%","9%","15%","28%","25%"))

df_vs_vs_no2$noshowrate= factor(df_vs_vs_no2$noshowrate,levels = c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%") )
df_vs_vs_no2$vs_cat= factor(df_vs_vs_no2$vs_status,levels = c("VS","VL>200") )
df_vs_vs_no2$number = c(273,357,350,104,24,16,36,60,40,8)



ggplot(data=df_vs_vs_no2,mapping=aes(x=noshowrate, fill=vs_status, y=value))+geom_col(width=0.6,position = "dodge")+
  geom_text(aes(label=value_p), vjust=-0.5, color="black", size=3.5,position=position_dodge(0.6))+
  geom_text(aes(label=number), vjust=1, color="black", size=3.5,position=position_dodge(0.5))+
  xlab("No Show Rate") + ylab("Percentage of Patients")+scale_fill_brewer(palette = 'Accent')+
  theme(axis.title.x = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", face = "bold", vjust = 0.5, hjust = 0.5, angle =90))+
  scale_y_continuous(breaks=seq(0,100,5),
                     limits=c(0,100)) 


library(DescTools)


ftable_vs =matrix(c(273,357,350,104,24,16,36,60,40,8), byrow=TRUE, nrow=2, dimnames=list(vs_cate=c("VS","VL>200"), no_show_rate=c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%")))
CochranArmitageTest(ftable_vs,"increasing")



ftable_re =matrix(c(249,349,327,87,5,40,44,83,57,27), byrow=TRUE, nrow=2, dimnames=list(re_cate=c("In care","Out of care"), no_show_rate=c("0%", "1%-25%", "26%-50%", "51%-75%", ">76%")))
CochranArmitageTest(ftable_re,"increasing")



control_intervention = matrix(data= c(2826,9461,3848,9217), nrow = 2)

control_intervention
chisq.test(control_intervention)






write.csv(missing_df, file = "C:/Users/cuisa/Desktop/missing.csv")



set.seed(123)
sample_data=sample(1:1268,500,replace = F)
mock=merged_data[sample_data,c('noshowrate','retention_in_care',"VL_supp")]


write.csv(mock, file = "C:/Users/cuisa/Desktop/deidentified_data.csv")



