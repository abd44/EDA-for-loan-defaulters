##################
# EDA CASE STUDY #
##################
library(dplyr)
library(ggplot2)
library(scales)

setwd("C:/Users/BALI/UPGRAD/Course 2 Exploratory Data Analysis/Module 4 Exloratory Data Analysis/2. CaseStudy EDA")
getwd()
loan <- read.csv("loan.csv", stringsAsFactors = F)
class(loan)
str(loan)
summary(loan)
colSums(is.na(loan))
#****************************************  DATA CLEANING  *************************************************
# COLUMN DELETION
# Remove all columns populated only with NA and zeros values 

loan.data <- loan[, apply(loan, 2, function(x) !any(is.na(x)))]
loan.data <- loan.data[,colSums(loan.data != 0,na.rm = T) > 0]



# Remove columns that will not play significant role in the analysis and/or have incomplete information
# All the applications are type "Individual", removing the variable from annalysis

loan.data <- loan.data[ ,-which(names(loan.data) %in% c("pymnt_plan", "url", "desc", "sub_grade", "emp_title", 
                                                        "policy_code", "zip_code","total_rec_late_fee", "initial_list_status", 
                                                        "collection_recovery_fee", "next_pymnt_d", "acc_now_delinq","delinq_amnt", 
                                                        "out_prncp_inv", "total_pymnt_inv", "application_type", "funded_amnt_inv",
                                                        "funded_amnt", "installment", "out_prncp","total_pymnt","total_pymnt_inv",
                                                        "total_rec_int","total_rec_prncp"))]
# Checking unique
sapply(loan.data, function(x) {length(unique(x))})

#Elimainating coulmns with only 1 Unique value as we do not need a cloulmn for only 1 unique value

loan.data <- loan.data[sapply(loan.data, function(x) {!length(unique(x))== 1})]

#**********************************************FORMATING DATA***********************************************

# DATE COLUMN CONVERSION - CHAR TO DATE

loan.data$issue_d <- paste("01-", loan.data$issue_d, sep="")
loan.data$issue_d <- as.Date(loan.data$issue_d, format = "%d-%b-%y")

loan.data$earliest_cr_line <- paste("01-", loan.data$earliest_cr_line, sep="")
loan.data$earliest_cr_line <- as.Date(loan.data$earliest_cr_line, format = "%d-%b-%y")

loan.data$last_pymnt_d <- paste("01-", loan.data$last_pymnt_d, sep="")
loan.data$last_pymnt_d <- as.Date(loan.data$last_pymnt_d, format = "%d-%b-%y")

loan.data$last_credit_pull_d <- paste("01-", loan.data$last_credit_pull_d, sep="")
loan.data$last_credit_pull_d <- as.Date(loan.data$last_credit_pull_d, format = "%d-%b-%y")

View(loan.data)

# PERCENTAGE TO NUMERIC CONVERSION

loan.data$revol_util <- as.numeric(sub("%", "", loan.data$revol_util, fixed=TRUE))
loan.data$int_rate <- as.numeric(sub("%", "", loan.data$int_rate, fixed=TRUE))

# Store data set to new interim variable in case to restore back
loan.data.clean <- loan.data

# *****************************************   DERIVED METRICS  *********************************************
# EXTRACT YEARS & MONTHS
loan.data$issue.d.year <- format(loan.data$issue_d, "%Y")
loan.data$issue.d.month <- format(loan.data$issue_d, "%m")

loan.data$earliest_cr_line.year <- format(loan.data$earliest_cr_line, "%Y")
loan.data$earliest_cr_line.month <- format(loan.data$earliest_cr_line, "%m")

loan.data$last_pymnt_d.year <- format(loan.data$last_credit_pull_d, "%Y")
loan.data$last_pymnt_d.month <- format(loan.data$last_credit_pull_d, "%m")

loan.data$last_credit_pull_d.year <- format(loan.data$last_credit_pull_d, "%Y")
loan.data$last_credit_pull_d.month <- format(loan.data$last_credit_pull_d, "%m")

# BINNING
#Income range defining and creating Bins:
summary(loan.data$annual_inc)
quantile(loan.data$annual_inc,probs = seq(0.1, 1, 0.1))

income_slots <- function(i) {
  if(i <= 30000){
    return('Till 30K')
  }else if(i >30000 & i<= 50000){
    return('30K-50K')
  }else if(i>50000 & i<=60000 ){
    return('50K-60K')
  }else if(i > 60000 & i<= 90000){
    return('60K-90K')
  }else if(i >90000 & i<= 150000){
    return('90K-150K')
  }else if(i>150000 & i<= 300000){
    return('150K-300K')
  }else if(i >300000 & i<= 6000000){
    return('300K-600K')
  }else{
    return(NA)
  }
}

loan.data$income_bins <- sapply(as.numeric(as.vector(loan.data$annual_inc)),income_slots)
loan.data$income_bins <- factor(loan.data$income_bins,levels = c('Till 30K','30K-50K','50K-60K','60K-90K','90K-150K','150K-300K','300K-600K'))
summary(loan.data$income_bins)


# *************************************** DRIVER VARIABLES IDENTIFICATION ********************************************
# 
# Possible application status - loan defaulted, and loan not defaulted
# Derogatory Public Records - pub_rec, this indicates that the applicant had issues with delinqency before.
# Utilization of the available credit lines to max - revol_util
# Member ever had a recovery from collections - recoveries
# Grades are given based on the credit and financial histroy of the applicant, A being best to G as worst

loan.default <- subset(loan.data, loan.data$loan_status == "Charged Off")
loan.paid <- subset(loan.data, loan.data$loan_status == "Fully Paid")
loan.curr <- subset(loan.data, loan.data$loan_status == "Current")

# Total number applicants (1140) with current status is insignificant

# Considering loan status - Fully Paid, and Charged Off

loan.data <- subset(loan.data, loan.data$loan_status != "Current")
risk.var1 <- subset(loan.data, loan.data$pub_rec !=0)
risk.var2 <- subset(loan.data, loan.data$revol_util > 0.95)
risk.var3 <- subset(loan.data, loan.data$recoveries !=0)
risk.var4 <- subset(loan.data, loan.data$delinq_2yrs !=0 )
risk.var5 <- subset(loan.data, loan.data$verification_status != "Not Verified")
  
#df %>% group_by(group, var1) %>% tally()

loan.new <- as.data.frame(loan.data %>% group_by(grade, loan_status, verification_status, purpose, term, delinq_2yrs,home_ownership) %>% tally())

#aggregate(Frequency ~ Category, x, sum)
aggregate(n ~ grade, loan.new, sum)
aggregate(n ~ verification_status, loan.new, sum)
aggregate(n ~ loan_status, loan.new, sum)
aggregate(n ~ delinq_2yrs, loan.new, sum)
aggregate(n ~ purpose, loan.new, sum)
aggregate(n ~ term, loan.new, sum)
aggregate(n ~ home_ownership, loan.new, sum)

# *************************************** DRIVER VARIABLES IDENTIFICATION ********************************************
# 
# Possible application status - loan defaulted, and loan not defaulted
# Derogatory Public Records - pub_rec, this indicates that the applicant had issues with delinqency before.
# Utilization of the available credit lines to max - revol_util
# Member ever had a recovery from collections - recoveries
# Grades are given based on the credit and financial histroy of the applicant, A being best to G as worst

loan.default <- subset(loan.data, loan.data$loan_status == "Charged Off")
loan.paid <- subset(loan.data, loan.data$loan_status == "Fully Paid")
loan.curr <- subset(loan.data, loan.data$loan_status == "Current")
risk.cat1 <- subset(loan.data, loan.data$loan_status != "Fully Paid" | loan.data$pub_rec !=0)
risk.cat2 <- subset(risk.cat1, risk.cat1$revol_util > 0.95)

#**********************************************************PLOTS*************************************
# LOAN STATUS VS OTHER VARIABLES FOR ANALYSIS AND DEPENDENCY UNDERSTANDING
#Vizualizing income_bins:
percent_income <- loan.data %>% group_by(income_bins) %>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))

ggplot(loan.data,aes(x=income_bins,fill=loan_status))+
  geom_bar(position = "fill")+geom_text(data=percent_income,aes(y=n,label=ratio),position=position_fill(vjust = 0.5)) + theme_bw() + 
  labs(x="Income Bins", y="Number of Requests", title = "Income Vs Loan Status")





#Vizualizing grade:

percent_grade <- loan.data %>% group_by(grade) %>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))


ggplot(loan.data,aes(x=grade,fill=loan_status)) + geom_bar(stat = "count",position = "fill")+
  geom_text(data=percent_grade,aes(y=n,label=ratio),position=position_fill(vjust =0.9)) + theme_bw()+
  labs(x="Grade",y="Count",fill="Status",title="Grade % for each Loan Staus")

#Vizualizing verification_status:

percent_grade2 <- loan.data %>% group_by(verification_status,grade) %>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))

ggplot(loan.data,aes(x=verification_status,fill=loan_status))+geom_bar(stat = "count",position = "fill")+
  geom_text(data=percent_grade2,aes(y=n,label=ratio),position=position_fill(vjust=0.9))+facet_wrap(~grade)


#Vizualizing delinq_2yrs:

percent_delinquent <- loan.data %>% group_by(delinq_2yrs,income_bins) %>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))

ggplot(loan.data, aes(x=factor(delinq_2yrs), fill=factor(loan_status))) + geom_bar(position = "fill") + 
  facet_wrap(~income_bins) + geom_text(data=percent_delinquent,aes(y=n,label=ratio),position=position_fill(vjust=0.9))
labs(x="Delinquent", y="Number of Requests", fill= "Income" ,title = "Delinquent Vs Loan Status")

#Vizualizing purpose:

percent_purpose2 <- loan.data %>% group_by(purpose) %>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))
ggplot(loan.data, aes(x=factor(purpose), fill=factor(loan_status))) + geom_bar(position = "dodge") + 
  labs(x="Purpose", y="Number of Requests", fill = "Status", title = "Loan Purpose Vs Loan Status")  +
  geom_text(data=percent_purpose2,aes(y=n,label=ratio),position=position_dodge(width =0.9),vjust=0.5)




#Vizualizing dti:


loan.data$dti <- round(loan.data$dti)


percent_dti <- loan.data %>% group_by(dti)%>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))

ggplot(loan.data,aes(x=factor(dti),fill=loan_status))+geom_bar(stat = "count") +
  geom_text(data=percent_dti,aes(y=n,label=ratio),position=position_stack(vjust = 0.5)) + theme_bw() +
  labs(x="Debt To Income Ratio",y="Count",title="Debt to Income Ratio % count for each status of loan",fill="Loan Status")


#Vizualizing Term:

percentdata <- loan.data %>% group_by(term) %>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))  


ggplot(loan.data,aes(x=term,fill=loan_status)) + geom_bar(position = "stack") +
  geom_text(data=percentdata, aes(y=n,label=ratio),position=position_stack(vjust=0.8))+theme_bw()



#******************************UNIVARIATE ANALYSIS FOR DEFAULTED AND FULLY PAID************************
#Univariate of Term of loan.data:  
prop.table(table(loan.data$term))*100
prop.table(table(loan.default$term))*100

#Vizualizing Term :
ggplot(loan.data,aes(term)) + 
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat="count",position=position_stack(0.5),vjust=-0.2) + theme_bw()
#For Defaulters:
ggplot(loan.default,aes(term)) + 
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat="count",position=position_stack(0.5),vjust=-0.2) + theme_bw()

#Insights: Overall 73% of loan.data term is 36 Months and 27% of loan.data is 60 MOnths  
#       :  But For defaulters("Charged Off") 57% of loan.data term is 36 Months and 43% of loan.data is 60 Months 



#Univariate on home_ownership:
  
  prop.table(table(loan.paid$home_ownership)) * 100  
  prop.table(table(loan.default$home_ownership))*100  
  
  
  # Vizualizing  home_ownership :
  ggplot(loan.data,aes(home_ownership)) + geom_bar(stat = "count",color=I('black'),fill=I('#56B4E9'))+
    geom_text(aes(label=scales::percent(..count../sum(..count..))),stat="count",position=position_stack(0.5),vjust=-0.2) + theme_bw()
  
  
  ggplot(loan.default,aes(home_ownership)) + geom_bar(stat = "count",color=I('black'),fill=I('#56B4E9'))+
    geom_text(aes(label=scales::percent(..count../sum(..count..))),stat="count",position=position_stack(0.5),vjust=-0.2) + theme_bw()+
    labs(x="Home Ownership",y="Count",title="Home Ownership % of Count")
  
  #Insight:Mortgage and Rent Home ownership are more likely to be defaulter as amongst defaulters are 41% are under Mortagae and 51% are under Rent ownership.

  
#For loan.data Paid Applicants:
  ggplot(loan.paid,aes(emp_length)) + geom_bar(stat = "count",color=I('black'),fill=I('#56B4E9'))+
    geom_text(aes(label=scales::percent(..count../sum(..count..))),stat="count",position=position_stack(0.5),vjust=-0.2) + theme_bw()+
    labs(x="Employee job Length",y="Count",title="Employee Job Length Vs count of loan.data Paid Applicant")
  
  #10+ Years of Employee length are more prone to Defaulter,as in this data overall 23.8% of the defaulters are 10 + years Experienced 
#Vizualizing grade:
ggplot(loan.data, aes(grade)) + geom_bar() + labs(title = "Grade Plot")
#Vizualizing int_rate
ggplot(loan.data, aes(int_rate)) + geom_bar() + labs(x= "Rate of Interest", title = "Interest Rate")

#Univariate of purpose of loan.data:
ggplot(loan.data,aes(purpose)) + geom_bar(stat = "count")+labs(x="Purpose",y="Count",title="Purpose of Overall loan.data Count")
ggplot(loan.default,aes(purpose)) + geom_bar(stat = "count")+labs(x="Purpose",y="Count",title="Purpose of Defaulter loan.data Count")
ggplot(loan.paid,aes(purpose)) + geom_bar(stat = "count")+labs(x="Purpose",y="Count",title="Purpose of Paid loan.data Count")
#Majority of loan.data Applicant have taken for debt consolidation purpose 


#Number of Delinquencies
ggplot(loan.data, aes(pub_rec)) + geom_bar() + labs(x= "Number of Delinquencies", title = "Public Records - Delinquncy Plot")
#Vizualization revol_util
ggplot(subset(loan.data, loan.data$revol_util > 0.95), aes(revol_util)) + geom_bar() + labs(x="Credit Utilization", title = "Maximum Credit Utilization")

#vizualization revol_util:
ggplot(risk.cat2, aes(x=factor(revol_util), fill=factor(loan_status))) + geom_bar () + labs(x="Revolving Utilzation", y="Number of Records", fill="Load Status", title = "Impact of High Revolving Utilization")




#**************************** BIVARIATE ANALYSIS FOR DEFAULTED AND FULLY PAID****************************
# Save original csv

loan.orig <- loan
# Read recend loan.data into loan variable

loan <- loan.data
loan_defaulters <- loan.default
loan_Paid <- loan.paid

#********************Bivariate: 



#Bivariate on employee length and Loan Status:

loan %>% group_by(emp_length,loan_status) %>% summarise(n())

percent <- loan %>% group_by(emp_length) %>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))

ggplot(loan,aes(x=emp_length,fill=factor(loan_status))) + geom_bar(stat = "count")+
  geom_text(data=percent,aes(y=n,label=ratio),position=position_stack(vjust=0.5)) + theme_bw()+
  labs(x="Employee job Length",y="Count",title="Employee Job length Vs Count of Loan for Each Loan Status")



#Bivariate on Annual income and status of Loan using box plot:
ggplot(loan, aes(loan_status, annual_inc)) + geom_boxplot() + ylim(0,200000) +
  labs(x = "Loan Status",y = "Annual income",title = "Loan staus for Annual Income")
#Median annual income is more for the "Fully Paid " Applicants.
#  That says that lower income on an average leads to Default the loan.




prop.table(table(loan$purpose)) * 100
barchart(table(loan$purpose))
#Majority of loans are taken for "Debt Consolidation"  




#Loan amount and loan status vizualization:

ggplot(loan, aes(x = funded_amnt_inv,fill=loan$loan_status)) +
  geom_histogram(aes(y = ..count..), binwidth = 300,colour = barlines) +
  scale_x_continuous(name = "Funded Amount",breaks = seq(0, 35000, 1000),limits=c(0, 35000)) +
  scale_y_continuous(name = "Count") +
  ggtitle("Status for each Loan amount")  


#    Loan amount vizualization for defaulters "Charged Off" candidate:

ggplot(loan_defaulters, aes(x = funded_amnt_inv)) +
  geom_histogram(aes(y = ..count..), binwidth = 300,colour = barlines) +
  scale_x_continuous(name = "Funded Amount",breaks = seq(0, 35000, 1000),limits=c(0, 35000)) +
  scale_y_continuous(name = "Count") +
  ggtitle("Status for each Loan amount for Defaulters") + facet_grid(grade~.)


#    Loan amount and employee length trend vizualization for defaulters "Charged Off" candidate:

ggplot(loan_defaulters, aes(x = funded_amnt_inv)) +
  geom_histogram(aes(y = ..count..), binwidth = 300,colour = barlines) +
  scale_x_continuous(name = "Funded Amount",breaks = seq(0, 35000, 1000),limits=c(0, 35000)) +
  scale_y_continuous(name = "Count") +
  ggtitle("Status for each Loan amount") + facet_grid(emp_length~.)




#Term and loan status:

percentdata <- loan %>% group_by(term) %>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))  


ggplot(loan,aes(x=term,fill=loan_status)) + geom_bar(position = "stack") +
  geom_text(data=percentdata, aes(y=n,label=ratio),position=position_stack(vjust=0.8))+theme_bw()

# 10.9 % of Applicants have charged off if they took Loan for 36 Months
# 22.6 % of Applicants have charged off if they too Loan for 60 Months.
# Number of Term Months Increase has more likely that Applicant will Charge off(Default) 


#Bivariate of Verification status
ggplot(loan,aes(x=verification_status,fill=loan_status)) + geom_bar(position = "stack") +
  geom_text(data=percent_verification, aes(y=n,label=ratio),position=position_stack(vjust=0.8)) + theme_bw()

percent_verification <-loan %>% group_by(verification_status) %>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))

#  
summary(loan$emp_title) 

# Bivariate on Loan Grade :

loan %>% group_by(grade) %>% summarise(n())
loan_defaulters %>% group_by(grade) %>% summarise(n=n()) %>% arrange(-n)
loan_Paid %>% group_by(grade) %>% summarise(n=n()) %>% arrange(-n)

percent_grade <- loan %>% group_by(grade) %>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))


ggplot(loan,aes(x=grade,fill=loan_status)) + geom_bar(stat = "count",position = "fill")+
  geom_text(data=percent_grade,aes(y=n,label=ratio),position=position_fill(vjust =0.9)) + theme_bw()+
  labs(x="Grade",y="Count",fill="Status",title="Grade % for each Loan Staus")

#Insight: With Increase of Grade from A To G,the defaulter's perceent count also increase,ie for G grade the defaulters are 32%.
#Hence its a strong variable  

##Bivariate on Annual Income for each loan status
summary(loan$annual_inc) 

percent_income <- loan %>% group_by(Income_Bins) %>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))

ggplot(loan, aes(x = Income_Bins)) +geom_bar(stat = "count",color=I('black'),fill=I('#56B4E9')) +
  geom_text(data=percent_income,aes(y=n,label=ratio),vjust=-0.2) + theme_bw() + facet_grid(loan_status~.)


ggplot(loan,aes(x=Income_Bins,fill=loan_status))+
  geom_bar()+geom_text(data=percent_income,aes(y=n,label=ratio),position=position_stack(vjust = 0.5)) + theme_bw() + theme_bw()

ggplot(loan,aes(x=Income_Bins,fill=loan_status))+
  geom_bar(position = "fill")+geom_text(data=percent_income,aes(y=n,label=ratio),position=position_fill(vjust = 0.5)) + theme_bw() + 
  labs(x="Income Bins", y="Number of Requests", title = "Income Vs Loan Status")



#Majority of the Borrower have income from 30K to 150K with median income of 59K 
#Majority of defaulters have annual income till 90K


#Bivariate on dti and loan status:

#Rounding the value of dti:
loan$dti <- round(loan$dti)


percent_dti <- loan %>% group_by(dti)%>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))
ggplot(loan,aes(x=factor(dti),fill=loan_status))+geom_bar(stat = "count") +
  geom_text(data=percent_dti,aes(y=n,label=ratio),position=position_stack(vjust = 0.5)) + theme_bw() +
  labs(x="Debt To Income Ratio",y="Count",title="Debt to Income Ratio % count for each status of loan",fill="Loan Status")


#As the dti increases the % of defaulters are too increasing dti 26 as after 26 dti getting loan gets tough ad hence charge off defaulters are too getting less after dti>26
#But Till dti 26 ..dti is Increase is strong indicator of defaulters.

#Bivariate on the number of loans issued by loan origination date:

ggplot(loan,aes(loan$issue_d_year,fill=factor(loan$term))) + 
  geom_bar( colour="darkgreen")+labs(x="Issue Year",y="Count",title="Number of loans issued by loan origination date",fill="Term")

#The number of loans issued by loan orgination date have highly increased between 2007 to 2011 .
#Most of the loans issued had a term of 36 months.Company issued 60-month loans for the first time in 2010.



ggplot(data=loan,aes(funded_amnt_inv,installment)) +
  geom_jitter(aes(color=term),alpha=1/20) +
  scale_color_brewer(type = 'qual')


#Bivariate on Loan Purpose:

percent_purpose <- loan %>% group_by(purpose)%>% count(loan_status) %>% mutate(ratio=scales::percent(n/sum(n)))
ggplot(loan,aes(x=purpose,fill=loan_status)) + geom_bar(stat = "count",position = "fill")+
  labs(x="Purpose",y="Count",title="Purpose of Loan Count") +
  geom_text(data=percent_purpose,aes(y=n,label=ratio),position=position_fill(vjust=0.5)) + theme_bw()

#25% of Small Business Purpose category defaults loan.So beaware o small business owner before granting them loan.

#Correlation Matrix in defaulters:
ggcorr(loan_defaulters, label = TRUE, label_size = 3,
       hjust = 0.8, size = 2.5, color="black", layout.exp = 2)

#Bivariate on addr_state of Applicant:

ggplot(loan,aes(x=addr_state,fill=loan_status)) + geom_bar(stat = "count",position = "fill") + labs(x="Addres",y="loan status",title="Bivariate on addr_state of Applicant")


#Place with code "NE" Nebraska state in USA has almost half of the loan applicants  are defaulters



