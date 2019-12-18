# To analyse comcasr telecom company complaints, to find out the problem of their customer service

# setting working directory
setwd("C:\\Users\\admin\\Desktop\\DS with R\\Projects\\Comcast Telecom complaints")
getwd()

# Importing data
compdata <- read.csv("Comcast Telecom Complaints data.csv")
head(compdata,3)

# structure of data
str(compdata)  # 2224 obs  & 10 variables , only zip code is numerical variable, rest all variables are categorical

summary(compdata) # There are no missing values

#Trend charts also known as run charts, are used to show trends in data over time
# monthly trend chart of complaints

typeof(compdata$Date[2])
head(compdata$Date)

install.packages("lubridate")
library("lubridate")

# Adding new variable, month to the dataset
compdata$month <-month(parse_date_time(as.character(compdata$Date),order= c("ymd","mdy","dmy")))
head(compdata)

unique(compdata$month)
table(compdata$month)

compdata$month <- ifelse(compdata$month==4, "April", ifelse(compdata$month==5,"May","June"))
complaints_per_month <-table(compdata$month)
plot(complaints_per_month,
     type='b',
     main ="Trend chart for monthly complaints",
     col ="red",
     xlab="Month",
     ylab="Frequency of complaints")

# complaint Trend chart on a daily basis
compdata$Day <- day(parse_date_time(as.character(compdata$Date),order=c("ymd","mdy","dmy")))
head(compdata$Day)

#compdata %>% group_by (Day) %>% summarise(count(as.numeric(Customer.Complaint)

w <- as.data.frame(table(compdata$Day))
names(w) <- c("Days","ComplaintFreq")
head(w)
plot(w,
     type='l', 
     main="Trend chart for Daily complaints",
     col ="red",
     xlab="Month",
     ylab="Frequency of complaints")     

# Frequency table for complaint types

compdata$complaint_type <- ifelse(grepl("Internet", compdata$Customer.Complaint),"Internet issue",
                     ifelse(grepl("service", compdata$Customer.Complaint),"Network service issue",
                     "others"))
head(compdata$complaint_type,10)
freqtable <-table(compdata$complaint_type)
freqtable  # Other domain issues are reported more

barplot(freqtable, main="Frequency table for complaints",
        xlab="complaints", ylab="Frequency",
        col=rainbow(3))

# Creating new categorical variable with value open and closed
#Open & Pending is to be categorized as Open and Closed & Solved is to be categorized as Closed
unique(compdata$Status)
sum(is.na(compdata$Status)==TRUE) # No missing values

compdata$open_close_status <- ifelse(compdata$Status == "Open" ,"Open", 
                               ifelse(compdata$Status == "Pending", "Open","Closed"))
head(compdata$open_close_status)
table(compdata$open_close_status)

tb <-table(compdata$open_close_status,compdata$State)
barplot(tb,
        main="State wise complaint chart",
        xlab ="States", ylab="Open & closed status",
        col= c("red","blue"),
        legends= rownames(tb))

# State with maximum complaints
state_complaints <-as.data.frame(tb)
names(state_complaints) <- c("status","state","Freq")

state_complaints %>% group_by (state) %>% summarise(totalcomplaintcount=sum(Freq)) %>% 
  arrange(desc(totalcomplaintcount))
 # Georgia has maximum complaints

#state which has the highest percentage of unresolved complaints
state_complaints %>% filter(status =="Open") %>% group_by(state) %>% summarise(totalopencount=sum(Freq)) %>%
   arrange(desc(totalopencount))
 # Georgia has the highest total open complaints

#Provide the percentage of complaints resolved till date, which were received through theInternet and customer care calls
solved_complaints <-as.data.frame(table(compdata$Received.Via,compdata$Status))
colnames(solved_complaints) <- c("Received_via", "Status", "Freq")

Total_compaints <- sum(solved_complaints$Freq)
Total_compaints

#About 21% of customercall complaints and 22% of complaints received via Internet are resolved

solved_complaints
solved_complaints <-mutate(solved_complaints,solvpct= Freq/Total_compaints)
plot(solved_complaints$Status, solved_complaints$solvpct,
     type="b", col=rainbow(4),
     main="Complaint status pct",
     xlab="complaint status",
     ylab="Freq pct",
)

dev.off()
