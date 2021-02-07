################################################################################
# Project name: Mortality analysis - delivery required in the course 
#               HarvardX PH125.9x Data Science.
# Objective: Analyze Spanish mortality data. 
#            Build a prediction method before COVID-19, 
#            and another system after COVID-19 (most recent one) and compare how
#            the pandemic impacts prediction accuracy of the system.
# Data source: INE  (https://ine.es/) National Institute of Statistics (Spain)
# Authors: Anayansi Olivares (Based on Data Science DS09-Capstone and series)
################################################################################


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Section 1: DATA PREPARATION
# Objective: Extraction of Spain mortality data.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Initiation set up

#  Library calls: Boolean function <<require>> is used to determine if the 
#                 library has been installed. In case it has not, the proper 
#                 <<install.package>> function is call to add it.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)
library(gam)

#  Data visualization parameter
options(digits = 4)

# Data extraction and transformation.

#  Extraction:  Part 1 - Spain Mortality by Year-Month and Region.
#  Source:      INE (Instituto Nacional de Estadisticas)
#  Description: Spain public and official monthly mortality data  from 1975 to 
#               2019 (2020 official data is not yet available). Option selected:
#               "Por lugar de residencia. (Serie desde 1975). Total nacional y 
#               comunidades autónomas", which means by Region. Notice that Sex 
#               and Age are not available with this level of month granularity.
#               The source data was originally extracted from:
#  https://www.ine.es/dynt3/inebase/es/index.htm?padre=1132&capsel=1134
#  Data file:    Data file will be extracted from Github.

#   Open file connection 
file <- tempfile()

#   Download the file input data and keep it as an R object. 
download.file("https://raw.githubusercontent.com/anastoll/SpainMortalityData/main/6562.csv", 
              file)

#   Reading file
spain_raw_ine_monthly <- read.csv(file, header = FALSE, sep = ";", 
                                  col.names = c("Region", "Period", "Deaths"), 
                                  dec=",",fileEncoding = "UTF-8")

#   Review of first rows
head(spain_raw_ine_monthly)

#   Eliminate column names in spanish from file.
spain_raw_ine_monthly<-spain_raw_ine_monthly[-1,]
head(spain_raw_ine_monthly)

#   Review of data type
str(spain_raw_ine_monthly)

#   Change European decimal standard to American standard in R
spain_raw_ine_monthly<- spain_raw_ine_monthly %>% 
  mutate(Deaths= as.integer(str_replace_all(Deaths, "\\.", "")))

#   There are no Na values in Deaths.
sum(is.na(spain_raw_ine_monthly$Deaths))
sum(spain_raw_ine_monthly$Deaths)

#   Review of Region values
spain_raw_ine_monthly %>% distinct(Region)

#   Separate Region code from Region Name
spain_raw_ine_monthly <- spain_raw_ine_monthly %>% 
  mutate(RegionCode = str_sub(Region, 1, 2), 
         RegionName = ifelse(Region=="Extranjero","Extranjero", 
                             str_sub(Region, 4, str_length(Region))))

#   Verification of transformation
spain_raw_ine_monthly %>% distinct(Region, RegionCode, RegionName)

#   Eliminate "Total" Values in Region to avoid duplicates
spain_raw_ine_monthly %>% filter(Region=="Total") %>% summarise(Deaths=sum(Deaths))
spain_raw_ine_monthly %>% filter(Region!="Total") %>% summarise(Deaths=sum(Deaths))

#   Same sum value for "Total" and for  detailed data. There are 11340 records 
#   before transformation.
spain_raw_ine_monthly<- spain_raw_ine_monthly %>% filter(Region!="Total") 

#   Same total Deaths value is kept. And number of records downs to 10800 
spain_raw_ine_monthly %>% summarise(Deaths=sum(Deaths))


#   Review of Period values
head(spain_raw_ine_monthly %>% distinct(Period))

#   Separate Year and Month from Period
spain_raw_ine_monthly <- spain_raw_ine_monthly %>% 
  mutate(Year = as.integer(str_sub(Period, 1, 4)), 
         Month = as.integer(str_sub(Period, 6, 7))) 

#   Review of separation results
str(spain_raw_ine_monthly)

#   Review of separation results Year
spain_raw_ine_monthly %>% distinct(Year)

#   Review of separation results Month
spain_raw_ine_monthly %>% distinct(Month)


#  Extraction:  Part 2 - Spain Mortality PROVISIONAL data for First Semester 
#               2020 by Month, Sex and Region.
#  Source:      INE (Instituto Nacional de Estadisticas)
#  Description: Spain public of 2020 provisional Monthly mortality data. Please 
#               notices that official (final) data is not yet available, and 
#               provisional data is only available for first semester of 2020 by
#               the time this project was executed. Option selected: 
#               "Defunciones por comunidad autónoma de residencia del fallecido 
#                y mes", which means by Region and month. Notice that Age is not
#               available in this data set. The source data was extracted from:
#  https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177008&menu=resultados&idp=1254735573002#!tabs-1254736195546
#  Data file:    Data file will be extracted from Github.

#   Open file connection 
file2 <- tempfile()

#   Download the file input data and keep it as an R object. 
download.file("https://raw.githubusercontent.com/anastoll/SpainMortalityData/main/02003.csv", 
              file2)

#   Reading file
spain_raw_ine_2020 <- read.csv(file2, 
                                  header = FALSE, 
                                  sep = ";", 
                                  col.names = c("Region", "MonthESP", "Sex","Deaths"), 
                                  dec=",",
                                  fileEncoding = "UTF-8")

#   Review of first rows
head(spain_raw_ine_2020)

#   Eliminate column names in Spanish from file.
spain_raw_ine_2020<-spain_raw_ine_2020[-1,]
head(spain_raw_ine_2020)

#   Review of data types
str(spain_raw_ine_2020)

#   Change European decimal standard to American standard in R
spain_raw_ine_2020<- spain_raw_ine_2020 %>% 
  mutate(Deaths= as.integer(str_replace_all(Deaths, "\\.", "")))

summary(spain_raw_ine_2020)

#   There are no Na values in Deaths.
sum(is.na(spain_raw_ine_2020$Deaths))
sum(spain_raw_ine_2020$Deaths)

#   Review of Region values
spain_raw_ine_2020 %>% distinct(Region)

#   In this case, there is not Region Code.

#   Eliminate "Total" Values in Region to avoid duplicates
spain_raw_ine_2020 %>% filter(Region=="Total") %>% summarise(Deaths=sum(Deaths))
spain_raw_ine_2020 %>% filter(Region!="Total") %>% summarise(Deaths=sum(Deaths))

#   Same sum value for "Total" and fo  details data. There are 441 records 
#   before transformation.
spain_raw_ine_2020<- spain_raw_ine_2020 %>% filter(Region!="Total") 

#   Same total Deaths value is kept. And number of records downs to 420 
spain_raw_ine_2020 %>% summarise(Deaths=sum(Deaths))

#   Review of Month values
spain_raw_ine_2020 %>% 
  group_by(MonthESP) %>%
  summarise(n=n())

#   Eliminate "Total" Values in MonthESP to avoid duplicates
spain_raw_ine_2020 %>% filter(MonthESP=="Total") %>% summarise(Deaths=sum(Deaths))
spain_raw_ine_2020 %>% filter(MonthESP!="Total") %>% summarise(Deaths=sum(Deaths))

#   Same sum value for "Total" and fo  details data. There are 420 records 
#   before transformation.
spain_raw_ine_2020<- spain_raw_ine_2020 %>% filter(MonthESP!="Total") 

#   Same total Deaths value is kept. And number of records downs to 360 
spain_raw_ine_2020 %>% summarise(Deaths=sum(Deaths))

#   We need to transform Spanish month name into numbers
spain_raw_ine_2020 <- spain_raw_ine_2020 %>% 
  mutate( Month = ifelse(MonthESP=="Enero",1,
                         ifelse(MonthESP=="Febrero",2,
                                ifelse(MonthESP=="Marzo",3,
                                        ifelse(MonthESP=="Abril",4,
                                               ifelse(MonthESP=="Mayo",5,6))))))


#   Verify of Month values
spain_raw_ine_2020 %>% 
  group_by(Month,MonthESP) %>%
  summarise(n=n())

str(spain_raw_ine_2020)

#   Review of Sex values
spain_raw_ine_2020 %>% 
  group_by(Sex) %>%
  summarise(n=n())


#   Eliminate "Total" Values in Sex to avoid duplicates
spain_raw_ine_2020 %>% filter(Sex=="Total") %>% summarise(Deaths=sum(Deaths))
spain_raw_ine_2020 %>% filter(Sex!="Total") %>% summarise(Deaths=sum(Deaths))
#   Same sum value for "Total" and fo  details data. There are 360 records 
#   before transformation.
spain_raw_ine_2020<- spain_raw_ine_2020 %>% filter(Sex!="Total") 

#   Same total Deaths value is kept. And number of records downs to 240 
spain_raw_ine_2020 %>% summarise(Deaths=sum(Deaths))

#   Change values "Varones" by "m" (male) and "Mujeres" by "f" (female)
spain_raw_ine_2020<- spain_raw_ine_2020 %>% 
  mutate(Sex=ifelse(Sex=="Varones","m","f")) 

#   Verification
spain_raw_ine_2020 %>% 
  group_by(Sex) %>%
  summarise(n=n())

#   Review if Region Name are the same. We should have 20 items
spain_raw_ine_2020 %>% mutate(RegionName=Region) %>%
  distinct(RegionName) %>% inner_join(spain_raw_ine_monthly%>%distinct(RegionName),by ="RegionName")

#   Value related to Region Castilla-La Mancha is missing. There is blank space
#   defference that should be transformed.
spain_raw_ine_2020<- spain_raw_ine_2020 %>% 
  mutate(RegionName=ifelse(Region=="Castilla-La Mancha",
                           "Castilla - La Mancha",
                           Region)) 

#   Verify if Region Name are the same. We should have 20 items
spain_raw_ine_2020 %>% distinct(RegionName) %>% 
  inner_join(spain_raw_ine_monthly%>%distinct(RegionName),by ="RegionName")

#  Let's unify monthly data from 1975 until 2019 with the first semester of 2020 
#  and call it Covid impact (since it is the only data we have that includes 
#  deaths for Covid). Fields to be combined: RegionName, Year, Month and Deaths. 

#   First steps is add Year to 2020 data
spain_raw_ine_2020<- spain_raw_ine_2020 %>% 
  mutate(Year=2020)
                   
#   Bind all rows for year-months combinations avalaible    
spain_covid_impact <- bind_rows(spain_raw_ine_monthly%>%select(RegionName, Year, Month, Deaths),
                                spain_raw_ine_2020%>%select(RegionName, Year, Month, Deaths))


#   Review bind result
str(spain_covid_impact)

#  Since qe have only first semester, let's add the semester to help analysis.
spain_covid_impact <- spain_covid_impact %>% 
  mutate(Semester = ifelse(Month<7,1,2))


# Data sets:    Training and Test Data set building for each group of model:
#               1.- Monthly data (1975-2019)  with Region only.
#               2.- Covid: Monthly data (1975- 1st sem 2020) with Region only.
# Description:  With the hypotheses that mortality data is quite stable (until 
#               covid!) let's try a typical 80/20.

#  Models with Monthly Data until 2019
#    Setting seed 1. if you are not using R 3.6 or later, please remove 
#    sample.kind parameter in seed definition
set.seed(1, sample.kind = "Rounding") 

#  Variable to be predicted
y1 <- spain_raw_ine_monthly$Deaths

# Creation of 2 partitions, 80% of data (training) and the test one with 20% 
test_index1 <- createDataPartition(y1, times = 1, p = 0.2, list = FALSE)

train_set_monthly <- spain_raw_ine_monthly %>% slice(-test_index1)
test_set_monthly <- spain_raw_ine_monthly %>% slice(test_index1)

#  Models with Monthly Data until First semester 2020

#    Setting seed 1. if you are not using R 3.6 or later, please remove 
#    sample.kind parameter in seed definition
set.seed(1, sample.kind = "Rounding") 

#  Variable to be predicted
y2 <- spain_covid_impact$Deaths

# Creation of 2 partitions, 80% of data (training) and the test one with 20% 
test_index2 <- createDataPartition(y2, times = 1, p = 0.2, list = FALSE)

train_set_covid <- spain_covid_impact %>% slice(-test_index2)
test_set_covid <- spain_covid_impact %>% slice(test_index2)

# End of Data Preparation Phase.


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Section 2: DATA ANALYSIS
# Objective: Data exploration to guide the Model approach
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Only training sets will be used in this section.
# Analysis 1: Monthly data (1975-2019).

#  Let's start by reviewing the mean and quartiles
summary(train_set_monthly)

#  Standar deviation
sd(train_set_monthly$Deaths)

#   Distribution graph
train_set_monthly %>% 
  ggplot(aes(x=Deaths)) + 
  geom_density(color="cyan3", fill="cyan3") + 
  xlab("Number of deaths by groups of year, month and region")+
  ggtitle("Distribution of number of deaths")


#   Year Analysis
#    Correlation between Deaths and Year
cor(train_set_monthly$Deaths, train_set_monthly$Year)

#    Evolution graph - Total deaths
train_set_monthly%>%
  group_by(Year) %>%
  summarise(Deaths=sum(Deaths)) %>%
  ggplot(aes(x =Year, y=Deaths)) + 
  geom_point(color="cyan3",size=1.5)  +
  geom_line(color="cyan3") +
  ylab("Number of Deaths") +
  ggtitle("Spain total number of deaths evolution from 1975-2019")

#    Evolution graph - Mean deaths
train_set_monthly%>%
  group_by(Year) %>%
  summarise(avg_Deaths=mean(Deaths)) %>%
  ggplot(aes(x =Year, y=avg_Deaths)) + 
  geom_point(color="cyan3",size=1.5)  +
  geom_line(color="cyan3") +
  ylab("Number of Deaths") +
  ggtitle("Spain average of deaths evolution from 1975-2019")

#    Ranking by year - 10 highest
train_set_monthly%>%
  group_by(Year) %>%
  summarise(Deaths=sum(Deaths)) %>%
  mutate(YearRanking=fct_reorder(factor(Year), Deaths))  %>%
  arrange(desc(Deaths)) %>%
  slice(1:10) %>%
  ggplot(aes(x =YearRanking, y=Deaths)) + 
  geom_bar(stat = "identity", fill="cyan3") +
  xlab("Year") +
  ylab("Number of deaths") +
  coord_flip() +  
  ggtitle("Ranking: Highest number of deaths from 1975-2019 by year")

#    Ranking by year - 10 lowest
train_set_monthly%>%
  group_by(Year) %>%
  summarise(Deaths=sum(Deaths)) %>%
  mutate(YearRanking=fct_reorder(factor(Year), Deaths))  %>%
  arrange(Deaths) %>%
  slice(1:10) %>%
  ggplot(aes(x =YearRanking, y=Deaths)) + 
  geom_bar(stat = "identity", fill="cyan3") +
  xlab("Year") +
  ylab("Number of deaths") +
  coord_flip() +  
  ggtitle("Ranking: Lowest number of deaths from 1975-2019 by year")



#   Months Analysis
#    Seasonal review
train_set_monthly%>%
  group_by(Month) %>%
  summarise(Deaths=sum(Deaths)) %>%
  ggplot(aes(x =factor(Month), y=Deaths)) + 
  geom_bar(stat = "identity", fill="cyan3") +
  xlab("Month") +
  ylab("Number of Deaths") +
  ggtitle("Spain number of deaths from 1975-2019 by month")


#   Ranking by month graph
train_set_monthly%>%
  group_by(Month) %>%
  summarise(avg_Deaths=mean(Deaths), sd_Deaths=sd(Deaths),Deaths=sum(Deaths)) %>%
  mutate(MonthRanking=fct_reorder(month(Month, label=TRUE), Deaths))  %>%
  ggplot(aes(x =MonthRanking, y=Deaths)) + 
  geom_bar(stat = "identity", fill="cyan3") +
  xlab("Month") +
  ylab("Number of deaths") +
  coord_flip() +  
  ggtitle("Spain total number of deaths from 1975-2019 by region and month")

#   Ranking by month table
train_set_monthly%>%
  group_by(Month) %>%
  summarise(avg_Deaths=mean(Deaths), n=n(), Deaths=sum(Deaths)) %>%
  arrange(desc(avg_Deaths))


#   Region Analysis

#    Total deaths ranking by region
train_set_monthly%>%
  group_by(RegionName) %>%
  summarise(Deaths=sum(Deaths)) %>%
  mutate(RegionRanking=fct_reorder(RegionName, Deaths))  %>%
  ggplot(aes(x =RegionRanking, y=Deaths)) + 
  geom_bar(stat = "identity", fill="cyan3") +
  xlab("Region name") +
  ylab("Number of deaths") +
  coord_flip() +  
  ggtitle("Spain total number of deaths from 1975-2019 by region")

#   Ranking by month table
train_set_monthly%>%
  group_by(RegionName) %>%
  summarise(avg_Deaths=mean(Deaths), sd_Deaths=sd(Deaths), Deaths=sum(Deaths)) %>%
  arrange(desc(avg_Deaths))

#    Region by months
train_set_monthly%>%
  group_by(RegionName, Month) %>%
  summarise(Deaths=sum(Deaths)) %>%
  ggplot(aes(x =RegionName, y=Deaths, fill=Month)) + 
  geom_bar(stat = "identity") +
  xlab("Region name") +
  ylab("Number of deaths") +
  ggtitle("Spain total number of deaths from 1975-2019 by region and month")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#    Region by year
train_set_monthly%>%
  group_by(RegionName, Year) %>%
  summarise(Deaths=sum(Deaths)) %>%
  ggplot(aes(x =RegionName, y=Deaths, fill=Year)) + 
  geom_bar(stat = "identity") +
  xlab("Region name") +
  ylab("Number of deaths") +
  ggtitle("Spain total number of deaths from 1975-2019 by region and year")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#   Evolution by region
train_set_monthly%>%
  group_by(Year, RegionName) %>%
  summarise(Deaths=sum(Deaths)) %>%
  ggplot(aes(x =Year, y=Deaths, color=RegionName)) + 
  geom_point(size=0.75)  +
  geom_line() +
  ylab("Number of deaths") +
  ggtitle("Spain number of deaths evolution from 1975-2019 by region")



# Analysis 2: Covid: Monthly data (1975- 1st sem 2020) with Region only.

#  Let's start by reviewing distribution
summary(train_set_covid)
summary(train_set_monthly)


train_set_covid %>% 
  ggplot(aes(x=Deaths)) + 
  geom_density(color="cyan3", fill="cyan3") + 
  xlab("Number of deaths by groups of year, month and region")+
  ggtitle("Distribution of number of deaths")

#   Year Analysis
train_set_covid%>%
  group_by(Year) %>%
  summarise(Deaths=sum(Deaths)) %>%
  ggplot(aes(x =Year, y=Deaths)) + 
  geom_point(color="cyan3",size=1.5)  +
  geom_line(color="cyan3") +
  ylab("Number of Deaths") +
  ggtitle("Spain number of deaths evolution from 1975-2019")

#   Evolution Semester analysis
train_set_covid %>%
  filter(Semester==1) %>%
  group_by(Year) %>%
  summarise(Deaths=sum(Deaths)) %>%
  ggplot(aes(x =Year, y=Deaths)) + 
  geom_point(color="cyan3",size=1.5)  +
  geom_line(color="cyan3") +
  ylab("Number of Deaths") +
  ggtitle("Spain  number of deaths evolution for 1st semester from 1975-2020/1")

#   Ranking Semester analysis
train_set_covid%>%
  mutate(YearSem = paste(Year,Semester)) %>%
  group_by(YearSem) %>%
  summarise(Deaths=sum(Deaths)) %>%
  mutate(YearSemRanking=fct_reorder(YearSem, Deaths))  %>%
  arrange(desc(Deaths)) %>%
  slice(1:15) %>%
  ggplot(aes(x =YearSemRanking, y=Deaths)) + 
  geom_bar(stat = "identity", fill="cyan3") +
  coord_flip() +  
  xlab("Year and semester") +
  ylab("Number of Deaths") +
  ggtitle("Spain ranking number of deaths by semester from 1975-2020/1") 


#    Region by semester
train_set_covid%>%
  group_by(Semester, RegionName, Year) %>%
  summarise(Deaths=sum(Deaths)) %>%
  ggplot(aes(x =RegionName, y=Deaths, fill=Year)) + 
  geom_bar(stat = "identity") +
  xlab("Region name") +
  ylab("Number of deaths") +
  ggtitle("Spain number of deaths from 1975-2020/1 by region and semester")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(.~Semester)

#   Semester analysis - data dispersion
train_set_covid %>%
  ggplot(aes(x= factor(Semester), y= Deaths)) + 
  geom_boxplot(fill="cyan3") +
  xlab("Semester") +
  ylab("Number of deaths") +
  ggtitle("Spain number of deaths from 1975-2020/1 by semester")
  
#   Semester analysis with jitter
train_set_covid %>%
  ggplot(aes(x= factor(Semester), y= Deaths)) + 
  geom_boxplot(fill="cyan3") +
  geom_jitter(size=0.1) +
  xlab("Semester") +
  ylab("Number of deaths") +
  ggtitle("Spain number of deaths from 1975-2020/1 by semester")

#   Month analysis post covid

#   Ranking by month table
train_set_covid%>%
  group_by(Month) %>%
  summarise(avg_Deaths=mean(Deaths), n=n(), Deaths=sum(Deaths)) %>%
  arrange(desc(avg_Deaths))

#   Ranking by total death by month graph post covid
train_set_covid%>%
  filter(Semester==1) %>%
  mutate(YearMonth = paste(Year,Month)) %>%
  group_by(YearMonth) %>%
  summarise(Deaths=sum(Deaths)) %>%
  mutate(YearMonth=fct_reorder(YearMonth, Deaths))  %>%
  arrange(desc(Deaths)) %>%
  slice(1:15) %>%
  ggplot(aes(x =YearMonth, y=Deaths)) + 
  geom_bar(stat = "identity", fill="cyan3") +
  coord_flip() +  
  xlab("Year and month") +
  ylab("Number of Deaths") +
  ggtitle("Spain ranking number of deaths by year-month from 1975-2020/1") 


#   Ranking by total death by month graph post covid. 

#    January
train_set_covid%>%
  filter(Semester==1 & Year > 2000 & Month==1) %>%
  mutate(YearMonth = paste(Year,Month)) %>%
  group_by(YearMonth) %>%
  summarise(Deaths=sum(Deaths)) %>%
  mutate(YearMonth=fct_reorder(YearMonth, Deaths))  %>%
  arrange(desc(Deaths)) %>%
  slice(1:10) %>%
  ggplot(aes(x =YearMonth, y=Deaths)) + 
  geom_bar(stat = "identity", fill="cyan3") +
  coord_flip() +  
  xlab("Year and month") +
  ylab("Number of Deaths") +
  ggtitle("January Spain ranking number of deaths by year from 2001-2020/1") 

#    February
train_set_covid%>%
  filter(Semester==1 & Year > 2000 & Month==2) %>%
  mutate(YearMonth = paste(Year,Month)) %>%
  group_by(YearMonth) %>%
  summarise(Deaths=sum(Deaths)) %>%
  mutate(YearMonth=fct_reorder(YearMonth, Deaths))  %>%
  arrange(desc(Deaths)) %>%
  slice(1:10) %>%
  ggplot(aes(x =YearMonth, y=Deaths)) + 
  geom_bar(stat = "identity", fill="cyan3") +
  coord_flip() +  
  xlab("Year and month") +
  ylab("Number of Deaths") +
  ggtitle("February Spain ranking number of deaths by year from 2001-2020/1") 


#    March - first wave starts
train_set_covid%>%
  filter(Semester==1 & Year > 2000 & Month==3) %>%
  mutate(YearMonth = paste(Year,Month)) %>%
  group_by(YearMonth) %>%
  summarise(Deaths=sum(Deaths)) %>%
  mutate(YearMonth=fct_reorder(YearMonth, Deaths))  %>%
  arrange(desc(Deaths)) %>%
  slice(1:10) %>%
  ggplot(aes(x =YearMonth, y=Deaths)) + 
  geom_bar(stat = "identity", fill="cyan3") +
  coord_flip() +  
  xlab("Year and month") +
  ylab("Number of Deaths") +
  ggtitle("March Spain ranking number of deaths by year from 2001-2020/1") 


#    April - first wave peak on daily deaths
train_set_covid%>%
  filter(Semester==1 & Year > 2000 & Month==4) %>%
  mutate(YearMonth = paste(Year,Month)) %>%
  group_by(YearMonth) %>%
  summarise(Deaths=sum(Deaths)) %>%
  mutate(YearMonth=fct_reorder(YearMonth, Deaths))  %>%
  arrange(desc(Deaths)) %>%
  slice(1:10) %>%
  ggplot(aes(x =YearMonth, y=Deaths)) + 
  geom_bar(stat = "identity", fill="cyan3") +
  coord_flip() +  
  xlab("Year and month") +
  ylab("Number of Deaths") +
  ggtitle("April Spain ranking number of deaths by year from 2001-2020/1") 


#    May - first wave ends
train_set_covid%>%
  filter(Semester==1 & Year > 2000 & Month==5) %>%
  mutate(YearMonth = paste(Year,Month)) %>%
  group_by(YearMonth) %>%
  summarise(Deaths=sum(Deaths)) %>%
  mutate(YearMonth=fct_reorder(YearMonth, Deaths))  %>%
  arrange(desc(Deaths)) %>%
  slice(1:10) %>%
  ggplot(aes(x =YearMonth, y=Deaths)) + 
  geom_bar(stat = "identity", fill="cyan3") +
  coord_flip() +  
  xlab("Year and month") +
  ylab("Number of Deaths") +
  ggtitle("May Spain ranking number of deaths by year from 2001-2020/1") 

#    June 
train_set_covid%>%
  filter(Semester==1 & Year > 2000 & Month==6) %>%
  mutate(YearMonth = paste(Year,Month)) %>%
  group_by(YearMonth) %>%
  summarise(Deaths=sum(Deaths)) %>%
  mutate(YearMonth=fct_reorder(YearMonth, Deaths))  %>%
  arrange(desc(Deaths)) %>%
  slice(1:10) %>%
  ggplot(aes(x =YearMonth, y=Deaths)) + 
  geom_bar(stat = "identity", fill="cyan3") +
  coord_flip() +  
  xlab("Year and month") +
  ylab("Number of Deaths") +
  ggtitle("June Spain ranking number of deaths by year from 2001-2020/1") 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Section 3: Model building
# Objective: Build a model based on 1975-2019 (pre-COVID-19), and then train 
#            another model with the same parameters and design and test it with 
#            2020 (post-COVID-19) data.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Function to compare methods

rmse_f <- function(y, y_hat){
  sqrt(mean((y - y_hat)^2))
}
accuracy_f<-function(y, y_hat){
  mean(y==y_hat)
}

# Monthly data set
#  Model lm method. We need to decide which predictors (Region, Year, Month and
#  Semester) builds the better model with this method. For that we will used
#  choose the lowest RMSE value produce by in the train function.

#   Lm: Including all predictors 
#    Seed setting for sampling replication
set.seed(1, sample.kind = "Rounding") 

#    Training algorithm
train_lm_monthly <- train(Deaths ~ RegionName + Year + Month, 
                          method = "lm", 
                          data = train_set_monthly)
#    Review of training results
train_lm_monthly

#    Keep RMSE results 
result_lm <- data.frame(Type = "Monthly 1975-2019", 
                     Method = "lm: all predictors", 
                     RMSE_training = train_lm_monthly$results$RMSE)
result_lm

#   Lm: using Region 
#    Seed setting for sampling replication
set.seed(1, sample.kind = "Rounding") 

#    Training algorithm
train_lm_monthly <- train(Deaths ~ RegionName , 
                          method = "lm", 
                          data = train_set_monthly)
#    Review of training results
train_lm_monthly

#    Keep RMSE results 
result_lm<-bind_rows(result_lm,
                  data_frame(Type = "Monthly 1975-2019", 
                             Method = "lm: Region", 
                             RMSE_training = train_lm_monthly$results$RMSE))

result_lm

#   Lm: using Year 
#    Seed setting for sampling replication
set.seed(1, sample.kind = "Rounding") 

#    Training algorithm
train_lm_monthly <- train(Deaths ~ Year , 
                          method = "lm", 
                          data = train_set_monthly)

#    Review of training results
train_lm_monthly

#    Keep RMSE results 
result_lm<-bind_rows(result_lm,
                  data_frame(Type = "Monthly 1975-2019", 
                             Method = "lm: Year", 
                             RMSE_training = train_lm_monthly$results$RMSE))

result_lm

#   Lm: using Month 
#    Seed setting for sampling replication
set.seed(1, sample.kind = "Rounding") 

#    Training algorithm
train_lm_monthly <- train(Deaths ~ Month , 
                          method = "lm", 
                          data = train_set_monthly)

#    Review of training results
train_lm_monthly

#    Keep RMSE results 
result_lm<-bind_rows(result_lm,
                  data_frame(Type = "Monthly 1975-2019", 
                             Method = "lm: Month", 
                             RMSE_training = train_lm_monthly$results$RMSE))

result_lm

#   Lm: using Year + Month 
#    Seed setting for sampling replication
set.seed(1, sample.kind = "Rounding") 

#    Training algorithm
train_lm_monthly <- train(Deaths ~ Year + Month , 
                          method = "lm", 
                          data = train_set_monthly)
#    Review of training results
train_lm_monthly

#    Keep RMSE results 
result_lm<-bind_rows(result_lm,
                     data_frame(Type = "Monthly 1975-2019", 
                                Method = "lm: Year + Month", 
                                RMSE_training = train_lm_monthly$results$RMSE))

result_lm 

#   Lm: using Region + Year 
#    Seed setting for sampling replication
set.seed(1, sample.kind = "Rounding") 

#    Training algorithm
train_lm_monthly <- train(Deaths ~ RegionName + Year , 
                          method = "lm", 
                          data = train_set_monthly)
#    Review of training results
train_lm_monthly

#    Keep RMSE results 
result_lm<-bind_rows(result_lm,
                     data_frame(Type = "Monthly 1975-2019", 
                             Method = "lm: Region + Year", 
                             RMSE_training = train_lm_monthly$results$RMSE))

result_lm


#   Lm: using Region + Month 
#    Seed setting for sampling replication
set.seed(1, sample.kind = "Rounding") 

#    Training algorithm
train_lm_monthly <- train(Deaths ~ RegionName + Month , 
                          method = "lm", 
                          data = train_set_monthly)
#    Review of training results
train_lm_monthly

#    Keep RMSE results 
result_lm<-bind_rows(result_lm,
                     data_frame(Type = "Monthly 1975-2019", 
                                Method = "lm: Region + Month", 
                                RMSE_training = train_lm_monthly$results$RMSE))

result_lm %>% arrange(RMSE_training)

#   Selected model for lm is all predictors with RMSE 302.3866.
#   Calculate accuracy and RMSE againts test data
#    Seed setting for sampling replication
set.seed(1, sample.kind = "Rounding") 

#    Training algorithm  Including all predictors 
train_lm_monthly <- train(Deaths ~ RegionName + Year + Month, 
                          method = "lm", 
                          data = train_set_monthly)

#   Review of training results
train_lm_monthly

#   Calculate y_hat
y_hat_lm_monthly <- predict(train_lm_monthly, test_set_monthly)

#  Quick review of results
y_hat_lm_monthly[1:10]
test_set_monthly$Deaths[1:10]

#  Deaths is a discrete variable, and prediction values are integer since we 
#  can not have "0.5" deaths, we have 0, 1, 2 ... 
#  However, since y_hat has decimal values, we are rounding it to avoid decimals
y_hat_lm_monthly <- round(y_hat_lm_monthly)

# Result table to keep Model results
result <- data.frame(Type = "Monthly 1975-2019", 
                     Method = "lm: all predictors", 
                     Accuracy = accuracy_f(test_set_monthly$Deaths,y_hat_lm_monthly),
                     RMSE_test = rmse_f(test_set_monthly$Deaths,y_hat_lm_monthly),
                     RMSE_training = train_lm_monthly$results$RMSE)

result


#  Model Loess method. We will cross-validation in the train model to find the  
#  best span parameter value. We will use degree=1 as fixed value. We will also 
#  use all predictors in the model.
#  For Region we use RegionCode, since RegionName produces error, likely due to 
#  special characters

#   Seed setting for sampling replication
set.seed(1, sample.kind = "Rounding") 

#   Definition of span parameters to be tested in the model.
grid <- expand.grid(span = seq(0.25, 0.95, len = 10), degree = 1)

date()
#   Training algorithm
train_loess <- train(Deaths ~ RegionCode+ Year + Month, 
                             method = "gamLoess", 
                             tuneGrid = grid,
                             data = train_set_monthly)
date()

#   Review training results
train_loess

#   Graphic cross validation
train_loess$results %>% ggplot(aes(x=span, y=RMSE)) +
  geom_point(color="cyan3") +
  geom_line(color="cyan3") +
  xlab("Span")+
  ylab("RMSE")+
  ggtitle("Cross validation: best span for lowest RMSE")

#   Obtain value of the lowest RMSE in the model
lowest_rmse<-train_loess$results$RMSE[which.min(train_loess$results$RMSE)]
span <- train_loess$results$span[which.min(train_loess$results$RMSE)]

#   Calculate y_hat
y_hat_loess <- predict(train_loess, test_set_monthly)

#   Quick review of results
y_hat_loess[1:10]

#   Transform decimal to integer values
y_hat_loess <- round(y_hat_loess)

#   Keep result table
result<-bind_rows(result,
                  data_frame(Type = "Monthly 1975-2019", 
                             Method = paste("loess: span =",round(span,4)) , 
                             Accuracy = accuracy_f(test_set_monthly$Deaths,
                                                   y_hat_loess),
                             RMSE_test  = rmse_f(test_set_monthly$Deaths,
                                                 y_hat_loess),
                             RMSE_training = lowest_rmse))

result

#  Model KNN method. We will cross-validation in the train model to find the  
#  best K (number of neighbor). All predictors are used.

#   Seed setting for sampling replication
set.seed(1, sample.kind = "Rounding") 

date()
#   Training algorithm
train_knn <- train(Deaths ~ RegionName+ Year + Month, 
                   method = "knn", 
                   tuneGrid = data.frame(k = seq(3, 21, 2)), 
                   data = train_set_monthly)
date()

#   Review of cross-validation results. Best K =5
train_knn

#   Graphic cross validation
train_knn$results %>% ggplot(aes(x=k, y=RMSE)) +
  geom_point(color="cyan3") +
  geom_line(color="cyan3") +
  xlab("K neighbors")+
  ylab("RMSE")+
  ggtitle("Cross validation: best k for lowest RMSE")

#   Obtain value of the lowest RMSE in the model
lowest_rmse<-train_knn$results$RMSE[which.min(train_knn$results$RMSE)]
k_value <- train_knn$results$k[which.min(train_knn$results$RMSE)]

#   Calculate y_hat
y_hat_knn <- predict(train_knn, test_set_monthly)

#   Transform decimal to integer values
y_hat_knn <- round(y_hat_knn)

#  Keep result table
result<-bind_rows(result,
                  data_frame(Type = "Monthly 1975-2019", 
                             Method = paste("knn with k= ",k_value),  
                             Accuracy = accuracy_f(test_set_monthly$Deaths,
                                                   y_hat_knn),
                             RMSE_test = rmse_f(test_set_monthly$Deaths,
                                           y_hat_knn),
                             RMSE_training = lowest_rmse))


result


#  Model Random Forest method. We will cross-validation in the train model to find the  best mtry parameter.

#   Seed setting for sampling replication
set.seed(1, sample.kind = "Rounding") 

date()
#   Training algorithm
train_rf <- train(Deaths ~ RegionName+ Year + Month, 
                  method = "rf", 
                  tuneGrid = data.frame(mtry = c(5,10,15)), 
                  data = train_set_monthly)
date()

#   Review of cross-validation results. Best K =5
train_rf

#   Graphic cross validation
train_rf%>% ggplot(aes(x=k, y=RMSE)) +
  geom_point(color="cyan3") +
  geom_line(color="cyan3") +
  xlab("K neighbours")+
  ylab("RMSE")+
  ggtitle("Cross validation: best k for lowest RMSE")

#   Obtain value of the lowest RMSE in the model
lowest_rmse<-train_rf$results$RMSE[which.min(train_rf$results$RMSE)]
mtry_value <- train_rf$results$mtry[which.min(train_rf$results$RMSE)]


#   Review best tune
train_rf$bestTune

#   Obtain variable importance in the model
imp<- varImp(train_rf)
imp

#   Calculate y_hat
y_hat_rf <- predict(train_rf, test_set_monthly)

#   Transform decimal to integer values
y_hat_rf <- round(y_hat_rf)

#  Keep result table
result<-bind_rows(result,
                  data_frame(Type = "Monthly 1975-2019", 
                             Method = paste("rf with mtry= ",mtry_value),  
                             Accuracy = accuracy_f(test_set_monthly$Deaths,
                                                   y_hat_rf),
                             RMSE_test = rmse_f(test_set_monthly$Deaths,
                                                y_hat_rf),
                             RMSE_training = lowest_rmse))


result

#  Model Random Forest with rf achieve 13% accuracy and RMSE 183. Best results.


# Compere model with Covid data (first semester 2020).
#  Model Lm 
#    Seed setting for sampling replication
set.seed(1, sample.kind = "Rounding") 

#    Training algorithm  Including all predictors 
train_lm_covid <- train(Deaths ~ RegionName + Year + Month, 
                          method = "lm", 
                          data = train_set_covid)

#   Review of training results
train_lm_covid

#   Calculate y_hat
y_hat_lm_covid <- predict(train_lm_covid, test_set_covid)

#  Quick review of results
y_hat_lm_covid[1:10]

#   Transform decimal to integer values
y_hat_lm_covid <- round(y_hat_lm_covid)

# Result table to keep Model results
result<-bind_rows(result,
                  data_frame(Type = "Monthly 1975-2020/1", 
                             Method = "lm: all predictors",  
                             Accuracy = accuracy_f(test_set_covid$Deaths,y_hat_lm_covid),
                             RMSE_test = rmse_f(test_set_covid$Deaths,y_hat_lm_covid),
                             RMSE_training = train_lm_covid$results$RMSE))

result

#  Model Random Forest method "rf" with Covid Data and mtry=15 found in previous model.

#   Seed setting for sampling replication
set.seed(1, sample.kind = "Rounding") 

date()
#   Training algorithm
train_rf_covid <- train(Deaths ~ RegionName+ Year + Month, 
                  method = "rf", 
                  tuneGrid = data.frame(mtry = 15), 
                  data = train_set_covid)
date()

#   Review of cross-validation results.
train_rf_covid

#   Obtain value of the lowest RMSE in the model
lowest_rmse<-train_rf_covid$results$RMSE[which.min(train_rf_covid$results$RMSE)]
mtry_value <- train_rf_covid$results$mtry[which.min(train_rf_covid$results$RMSE)]

#   Calculate y_hat
y_hat_rf_covid <- predict(train_rf_covid, test_set_covid)

#   Transform decimal to integer values
y_hat_rf_covid <- round(y_hat_rf_covid)

#  Keep result table
result<-bind_rows(result,
                  data_frame(Type = "Monthly 1975-2020/1", 
                             Method = paste("rf with mtry= ",mtry_value),  
                             Accuracy = accuracy_f(test_set_covid$Deaths,
                                                   y_hat_rf_covid),
                             RMSE_test = rmse_f(test_set_covid$Deaths,
                                                y_hat_rf_covid),
                             RMSE_training = lowest_rmse))

result





  