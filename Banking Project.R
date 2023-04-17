library(dplyr)
library(tidyr)
library(visdat)


setwd('D:\\Edvancer\\R\\Banking Project')

bank_train=read.csv('bank-full_train.csv', stringsAsFactors = FALSE)
bank_test=read.csv('bank-full_test.csv', stringsAsFactors= FALSE)

bank_test$y= NA

bank_train$data='train'
bank_test$data='test'

bank_all=rbind(bank_train, bank_test)

vis_dat(bank_all)


#Convert character to dummies 
#job, marital, education, default, housing, loan, contact, month, poutcome

View(bank_all)
glimpse(bank_all)

table(bank_all$data)

#function for dummy vaiables

CreateDummies=function(data,var,freq_cutoff=0                                                                                                      ){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}



# Creating Dummies


bank_all=CreateDummies(bank_all,'job')
bank_all=CreateDummies(bank_all, 'marital')
bank_all=CreateDummies(bank_all,'education')
bank_all=CreateDummies(bank_all, 'default')

table(bank_all$housing)
bank_all=CreateDummies(bank_all,'housing')

table(bank_all$loan)
bank_all=CreateDummies(bank_all,'loan')

table(bank_all$contact)
bank_all=CreateDummies(bank_all,'contact')

table(bank_all$month)
bank_all=CreateDummies(bank_all,'month')

xyz=as.data.frame(bank_all$poutcome)
bank_all=CreateDummies(bank_all,'poutcome')

table(bank_all$y)

vis_dat(bank_all, warn_large_data= FALSE)
glimpse(bank_all)


#splitting data

bank_train=bank_all %>% filter(data=='train') %>% select(-data)
vis_dat(bank_train, warn_large_data= FALSE)

bank_test=bank_all %>% filter(data=='test')%>% select(-data,-y)

vis_dat(bank_test)

#starting library(h2o)
library(h2o)
h2o.init(nthreads= 6)

train= as.h2o(bank_train)
test= as.h2o(bank_test)

y= 'y'

x=setdiff(names(train), y)

train[,y]= as.factor(train[,y])

aml= h2o.automl(x= x, y= y, training_frame = train, max_models = 10)

glimpse(bank_train)

lb=aml@leaderboard

print(lb, n=nrow(lb))

m= h2o.get_best_model(aml)
print(m)

x=as.data.frame(h2o.predict(m,test))

library(dgof)
ks.test(m,test)



colnames(x)[1]="y"

x=x %>% select(-no, -yes)

write.csv(x, 'D:/Edvancer/R/Banking Project/Vibhu_Nigam_P5_part2.csv', row.names = F)


glimpse(bank_train)
mean(bank_train$age, round=2)
