
#install.packages("data.table")
library(data.table)
loan=fread("E:/Aegis/Jobs/ABF/lending-club-loan-data/loan.csv")

#summary(loan)

#nrow(loan)#887379
#ncol(loan)#74

#for id
loan=loan[,-"id"]

#for member_id
loan=loan[,-"member_id"]

#for loan_amnt
#summary(loan$loan_amnt)

#for funded_amnt
#summary(loan$funded_amnt)

#for funded_amnt_inv
#summary(loan$funded_amnt_inv)

#for term
#unique(loan$term)

#for grade
#unique(loan$grade)

#for sub_grade
#unique(loan$sub_grade)

#for emp_title
loan=loan[,-"emp_title"]

#for emp_length
#unique(loan$emp_length)
loan$emp_length=ifelse(loan$emp_length=="n/a","0 year",loan$emp_length)
#loan$emp_length=ordered(loan$emp_length, levels = c("0 year","< 1 year","1 year","2 years","3 years","4 years","5 years","6 years","7 years","8 years","9 years","10+ years"))

#for home_ownership
#unique(loan$home_ownership)

#for annual_inc
#summary(loan$annual_inc)
a=which(is.na(loan$annual_inc)==T)
#loan$loan_status[a]
loan$annual_inc[is.na(loan$annual_inc)]=median(loan$annual_inc,na.rm = TRUE)

#for verification_status
#unique(loan$verification_status)

#for issue_d
#unique(loan$issue_d)
loan=loan[,-"issue_d"]

#for loan_status
#unique(loan$loan_status)


#classifying loan_status
#unique(loan$loan_status)
loan$loan_status=ifelse(loan$loan_status=="Fully Paid" | loan$loan_status=="Does not meet the credit policy. Status:Fully Paid","Good",loan$loan_status)
loan$loan_status=ifelse(loan$loan_status=="Charged Off"|loan$loan_status=="Default"|loan$loan_status=="Late (31-120 days)"|loan$loan_status=="Late (16-30 days)"|loan$loan_status=="Does not meet the credit policy. Status:Charged Off","Bad",loan$loan_status)
#unique(loan$loan_status)
#typeof(loan$loan_status)

#For payment_plan
#unique(loan$pymnt_plan)
#table(loan$pymnt_plan)

#for url
#loan$url[5]
loan=loan[,-"url"]

#for desc
#unique(loan$desc)
loan=loan[,-"desc"]

#For purpose
#unique(loan$purpose)

#for title
#unique(loan$title)
loan=loan[,-"title"]

#for zip_code
#unique(loan$zip_code)

#For addr_state
#unique(loan$addr_state)

#for dti
#summary(loan$dti)
#unique(loan$dti)

#for delinq_2yrs
#unique(loan$delinq_2yrs)
#summary(loan$delinq_2yrs)
#table(loan$delinq_2yrs) #29 NAs
b=which(is.na(loan$delinq_2yrs)==T)
loan=loan[-b,]

#for earliest_cr_line
#unique(loan$earliest_cr_line)#29 Blanks as dealinq_2yrs
#table(loan$earliest_cr_line)
#which(loan$earliest_cr_line=="")
loan=loan[,-"earliest_cr_line"]

#for inq_last_6mths
#unique(loan$inq_last_6mths)
#which(is.na(loan$inq_last_6mths)==T)#29 NAs as dealinq_2yrs and earliest_cr_line


#for mths_since_last_delinq
#summary(loan$mths_since_last_delinq)#454312 NAs
#unique(loan$mths_since_last_delinq)
#sort(-table(loan$mths_since_last_delinq))
loan$mths_since_last_delinq[is.na(loan$mths_since_last_delinq)]=6

#for mths_since_last_record
#unique(loan$mths_since_last_record)
#summary(loan$mths_since_last_record)#750326 NAs
#sort(-table(loan$mths_since_last_record))
loan$mths_since_last_record[is.na(loan$mths_since_last_record)]=71


#for open_acc
#unique(loan$open_acc)
#summary(loan$open_acc)
#which(is.na(loan$open_acc)==T)#29 NAs as dealinq_2yrs and earliest_cr_line and inq_last_6mths

#for pub_rec
# unique(loan$pub_rec)
# summary(loan$pub_rec)
# which(is.na(loan$pub_rec)==T)#29 NAs as dealinq_2yrs and earliest_cr_line and inq_last_6mths and open_acc

#for revol_bal
# summary(loan$revol_bal)

#for revol_util
# summary(loan$revol_util)
# loan$revol_util[1:10]
# plot(density(loan$revol_util,na.rm = T))
# hist(loan$revol_util)
# sum(loan$revol_util>127.45,na.rm = TRUE)
loan$revol_util[is.na(loan$revol_util)]=median(loan$revol_util,na.rm = TRUE)


#for total_acc
# unique(loan$total_acc)
# summary(loan$total_acc)
# which(is.na(loan$total_acc)==T)#29 NAs as dealinq_2yrs and earliest_cr_line and inq_last_6mths and open_acc

#for initial_list_status
# unique(loan$initial_list_status)

#for out_prncp
# summary(loan$out_prncp)

#for out_prncp
# summary(loan$out_prncp_inv)

#for total_pymnt
# summary(loan$total_pymnt)

#for total_pymnt_inv
# summary(loan$total_pymnt_inv)

#for total_rec_prncp
# summary(loan$total_rec_prncp)

#for total_rec_int
# summary(loan$total_rec_int)

#for total_rec_late_fee
# summary(loan$total_rec_late_fee)

#for recovries
# summary(loan$recoveries)

#for collection_recovery_fee
# summary(loan$collection_recovery_fee)

#for last_pymnt_d
# unique(loan$last_pymnt_d)
# which(loan$last_pymnt_d=="")# "" blank in dates
# table(loan$last_pymnt_d)
loan=loan[,-"last_pymnt_d"]

#for last_pymnt_amnt
# summary(loan$last_pymnt_amnt)

#for next_pymnt_d
# unique(loan$next_pymnt_d)
# table(loan$next_pymnt_d)# "" blank in dates
loan=loan[,-"next_pymnt_d"]

#for last_credit_pull_d
# unique(loan$last_credit_pull_d)# "" blank in dates
loan=loan[,-"last_credit_pull_d"]

#for collections_12_mths_ex_med
# unique(loan$collections_12_mths_ex_med)
# table(loan$collections_12_mths_ex_med)
# summary(loan$collections_12_mths_ex_med)
# sum(is.na(loan$collections_12_mths_ex_med)==T)
loan$collections_12_mths_ex_med[is.na(loan$collections_12_mths_ex_med)]=0

#for mths_since_last_major_derog
# unique(loan$mths_since_last_major_derog)
# sum(is.na(loan$mths_since_last_major_derog)==T)
# table(loan$mths_since_last_major_derog)
# summary(loan$mths_since_last_major_derog)# 665676 NAs
# loan$mths_since_last_major_derog
loan=loan[,-"mths_since_last_major_derog"]

#for policy_code
# unique(loan$policy_code)

#for application_type
# unique(loan$application_type)
# typeof(loan$application_type)

#for annual_inc_joint
# unique(loan$annual_inc_joint)
# summary(loan$annual_inc_joint)
# sum(loan$application_type=="JOINT" & !is.na(loan$annual_inc_joint))
# sum(loan$application_type=="JOINT")
loan$annual_inc_joint[is.na(loan$annual_inc_joint)]=0

#for dti_joint
# unique(loan$dti_joint)
# table(loan$dti_joint)
# summary(loan$dti_joint)
loan$dti_joint[is.na(loan$dti_joint)]=0

#for verification_status_joint
# unique(loan$verification_status_joint)
# table(loan$verification_status_joint)

#for acc_now_delinq
# unique(loan$acc_now_delinq)
# table(loan$acc_now_delinq)
# summary(loan$acc_now_delinq)
# which(is.na(loan$acc_now_delinq))#29 NAs as dealinq_2yrs and earliest_cr_line and inq_last_6mths and open_acc

#for tot_coll_amt

# summary(loan$tot_coll_amt)
# unique(loan$tot_coll_amt)
# plot(density(loan$tot_coll_amt,na.rm = T))
# head(loan$tot_coll_amt)
# summary(loan$tot_coll_amt)
loan$tot_coll_amt[is.na(loan$tot_coll_amt)]=0


#for tot_cur_bal
# summary(loan$tot_cur_bal)
loan$tot_cur_bal[is.na(loan$tot_cur_bal)]=0

#for open_acc_6m
# unique(loan$open_acc_6m)
# table(loan$open_acc_6m)
# summary(loan$open_acc_6m)
loan$open_acc_6m[is.na(loan$open_acc_6m)]=0

#for open_il_6m
# unique(loan$open_il_6m)
# table(loan$open_il_6m)
# summary(loan$open_il_6m)
loan$open_il_6m[is.na(loan$open_il_6m)]=10

#for open_il_12m
# unique(loan$open_il_12m)
# table(loan$open_il_12m)
# summary(loan$open_il_12m)
loan$open_il_12m[is.na(loan$open_il_12m)]=3


#for open_il_24m 
# unique(loan$open_il_24m)
# table(loan$open_il_24m)
# summary(loan$open_il_24m)
loan$open_il_24m[is.na(loan$open_il_24m)]=5


#for mths_since_rcnt_il
# summary(loan$mths_since_rcnt_il)
# sort(-table(loan$mths_since_rcnt_il))
loan$mths_since_rcnt_il[is.na(loan$mths_since_rcnt_il)]=0

#for total_bal_il
# summary(loan$total_bal_il)
loan$total_bal_il[is.na(loan$total_bal_il)]=0

#for il_util
# summary(loan$il_util)
# unique(loan$il_util)
# sort(-table(loan$il_util))
# plot(density(loan$il_util,na.rm = T))
loan$il_util[is.na(loan$il_util)]=100

#for open_rv_12m
# summary(loan$open_rv_12m)
# unique(loan$open_rv_12m)
# table(loan$open_rv_12m)
loan$open_rv_12m[is.na(loan$open_rv_12m)]=10

#for open_rv_24m
# summary(loan$open_rv_24m)
# table(loan$open_rv_24m)
loan$open_rv_24m[is.na(loan$open_rv_24m)]=15

#for max_bal_bc
# summary(loan$max_bal_bc)
# unique(loan$max_bal_bc)
# table(loan$max_bal_bc)
# plot(density(loan$max_bal_bc,na.rm = T))
loan$max_bal_bc[is.na(loan$max_bal_bc)]=median(loan$max_bal_bc,na.rm=T)

#for all_util
# summary(loan$all_util)
# table(loan$all_util)
# plot(density(loan$all_util,na.rm = T))
loan$all_util[is.na(loan$all_util)]=median(loan$all_util,na.rm = T)

#for total_rev_hi_lim
# summary(loan$total_rev_hi_lim)
# table(loan$total_rev_hi_lim)
# plot(density(loan$total_rev_hi_lim,na.rm = T))
loan$total_rev_hi_lim[is.na(loan$total_rev_hi_lim)]=median(loan$total_rev_hi_lim,na.rm = T)

#for inq_fi
# summary(loan$inq_fi)
# unique(loan$inq_fi)
# table(loan$inq_fi)
loan$inq_fi[is.na(loan$inq_fi)]=7

#for total_cu_tl
# summary(loan$total_cu_tl)
# unique(loan$total_cu_tl)
# table(loan$total_cu_tl)
loan$total_cu_tl[is.na(loan$total_cu_tl)]=10

#for inq_last_12m
# summary(loan$inq_last_12m)
# table(loan$inq_last_12m)
loan$inq_last_12m[is.na(loan$inq_last_12m)]=9
loan$inq_last_12m[loan$inq_last_12m %in% -4]=9


############################# Further Preprocessing ##################################

# cor(loan$out_prncp,loan$out_prncp_inv)
loan=loan[,-"out_prncp"]

# cor(loan$total_pymnt,loan$total_pymnt_inv)
loan=loan[,-"total_pymnt"]

# cor(loan$open_il_6m,loan$open_il_12m)
loan=loan[,-"open_il_6m"]
#cor(loan$open_il_6m,loan$open_il_24m)
# cor(loan$open_il_12m,loan$open_il_24m)
loan=loan[,-"open_il_12m"]

# cor(loan$open_rv_12m,loan$open_rv_24m)
loan=loan[,-"open_rv_12m"]

# cor(loan$loan_amnt,loan$funded_amnt)
# cor(loan$loan_amnt,loan$funded_amnt_inv)
# loan=loan[,-"funded_amnt"]
# loan=loan[,-"funded_amnt_inv"]

loan_cor=data.frame(loan$loan_amnt,loan$int_rate,loan$annual_inc,loan$dti,loan$delinq_2yrs,loan$inq_last_6mths,loan$mths_since_last_delinq
           ,loan$mths_since_last_record,loan$open_acc,loan$pub_rec,loan$revol_bal,loan$revol_util,loan$total_acc,loan$out_prncp_inv
           ,loan$total_rec_int,loan$total_rec_late_fee,loan$recoveries
           ,loan$last_pymnt_amnt,loan$collections_12_mths_ex_med,loan$dti_joint,loan$acc_now_delinq,loan$tot_coll_amt,loan$tot_cur_bal,
           loan$open_acc_6m,loan$mths_since_rcnt_il,loan$total_bal_il,loan$il_util,loan$max_bal_bc
           ,loan$all_util,loan$total_cu_tl)

#install.packages("corrplot")
library(corrplot)
corrplot(cor(loan_cor))

# cor(loan$loan_amnt,loan$installment)
loan=loan[,-"installment"]

# cor(loan$open_rv_24m,loan$inq_last_12m)
loan=loan[,-"open_rv_24m"]

# cor(loan$inq_fi,loan$inq_last_12m)
loan=loan[,-"inq_fi"]

# cor(loan$open_il_24m,loan$inq_last_12m)
loan=loan[,-"open_il_24m"]

# cor(loan$inq_last_12m,loan$total_cu_tl)
loan=loan[,-"inq_last_12m"]

# cor(loan$total_cu_tl,loan$open_acc_6m)#-0.6098
# cor(loan$total_cu_tl,loan$il_util)#0.6788
# cor(loan$open_acc_6m,loan$total_cu_tl)#-0.6098

# cor(loan$loan_amnt,loan$out_prncp_inv)#6468
# cor(loan$open_acc,loan$total_acc)#0.6950

# cor(loan$total_pymnt_inv,loan$total_rec_prncp)
loan=loan[,-"total_pymnt_inv"]

# cor(loan$last_pymnt_amnt,loan$total_rec_prncp)
loan=loan[,-"total_rec_prncp"]

# cor(loan$recoveries,loan$collection_recovery_fee)
loan=loan[,-"collection_recovery_fee"]

# cor(loan$dti_joint,loan$annual_inc_joint)
loan=loan[,-"annual_inc_joint"]

# cor(loan$total_rev_hi_lim,loan$revol_bal)
loan=loan[,-"total_rev_hi_lim"]

#corrplot(cor(loan_cor))
#str(loan)

################################################################################
# table(loan$loan_status)
b1=subset(x = loan,loan$loan_status=="Bad")
g1=subset(x = loan,loan$loan_status=="Good")

########################### Bad Loan Distribution ###############################
ind_bd=sample(x = nrow(b1),size = round(0.8*nrow(b1)))  #80% indices of  Bad Loan
train_bad=b1[ind_bd,]   #80%Train Data(Bad)

train_bd1=b1[-ind_bd,]  #20% of Remaining Data(For Validation And test Data)

b1_v_ind=sample(x = nrow(train_bd1),size = round(0.5*nrow(train_bd1)))

valid_bd1=train_bd1[b1_v_ind,]        #10% of Validation Data

test_bd1=train_bd1[-b1_v_ind,]        #10% of Test Data

########################### Good Loan Distribution ###############################
ind_gd=sample(x = nrow(g1),size = round(0.8*nrow(g1)))
train_good=g1[ind_gd,]

train_good1=g1[-ind_gd,]

g1_v_ind=sample(x = nrow(train_good1),size = round(0.5*nrow(train_good1)))

valid_gd1=train_good1[g1_v_ind,]

test_gd1=train_good1[-g1_v_ind,]

######################### Final Merging of Data ####################################
train=rbind(train_bad,train_good)
valid=rbind(valid_bd1,valid_gd1)
test=rbind(test_bd1,test_gd1)

########################### Model ######################################################

######################################### Random Forest ##########################################

#install.packages("randomForest")
# library(randomForest)
# fit_random=randomForest(loan_status~loan_amnt+term+int_rate+grade+sub_grade
#                                       +emp_length+home_ownership+annual_inc+verification_status+pymnt_plan
#                                       +dti+purpose+addr_state+initial_list_status+
#                                         delinq_2yrs+inq_last_6mths+mths_since_last_delinq+mths_since_last_record+open_acc+pub_rec+
#                                         revol_bal+revol_util+total_acc+out_prncp_inv
#                                       +total_rec_int+total_rec_late_fee+recoveries+
#                                         last_pymnt_amnt+collections_12_mths_ex_med+policy_code+application_type+dti_joint+
#                                         acc_now_delinq+tot_coll_amt+tot_cur_bal+
#                                         open_acc_6m+mths_since_rcnt_il+total_bal_il+il_util+max_bal_bc
#                                       +all_util+total_cu_tl,data=train,importance=TRUE,proximity=TRUE,ntree=100,strata=loan$loan_status)

######################################### Logistic ###########################################
# fit_logistic=glm(as.factor(loan_status)~loan_amnt+as.factor(term)+int_rate+as.factor(grade)+as.factor(sub_grade)
#                  +as.factor(emp_length)+as.factor(home_ownership)+annual_inc+as.factor(verification_status)+as.factor(pymnt_plan)
#                  +dti+as.factor(purpose)+as.factor(zip_code)+as.factor(addr_state)+as.factor(initial_list_status)+
#                    delinq_2yrs+inq_last_6mths+mths_since_last_delinq+mths_since_last_record+open_acc+pub_rec+
#                    revol_bal+revol_util+total_acc+out_prncp_inv
#                  +total_rec_int+total_rec_late_fee+recoveries+
#                  last_pymnt_amnt+collections_12_mths_ex_med+as.factor(policy_code)+as.factor(application_type)+dti_joint+
#                    as.factor(verification_status_joint)+acc_now_delinq+tot_coll_amt+tot_cur_bal+
#                  open_acc_6m+mths_since_rcnt_il+total_bal_il+il_util+max_bal_bc
#                  +all_util+total_cu_tl, family = binomial("logit"),train)
# 
# summary(fit_logistic)


############################################ Decision #########################################################

#install.packages("rpart")
library(rpart)

fit_decision=rpart(formula =  loan_status~loan_amnt+as.factor(term)+int_rate+as.factor(grade)+as.factor(sub_grade)
                          +as.factor(emp_length)+annual_inc+as.factor(verification_status)+as.factor(pymnt_plan)
                          +dti+as.factor(purpose)+as.factor(addr_state)+as.factor(initial_list_status)+
                            delinq_2yrs+inq_last_6mths+mths_since_last_delinq+mths_since_last_record+open_acc+pub_rec+
                            revol_bal+revol_util+total_acc+out_prncp_inv
                          +total_rec_int+total_rec_late_fee+recoveries+
                            last_pymnt_amnt+collections_12_mths_ex_med+as.factor(policy_code)+as.factor(application_type)+dti_joint+
                            acc_now_delinq+tot_coll_amt+tot_cur_bal+
                            open_acc_6m+mths_since_rcnt_il+total_bal_il+il_util+max_bal_bc
                          +all_util+total_cu_tl ,method ="class",data =  train,control = rpart.control(minsplit = 10,xval=20,maxsurrogate = 10),parms =list(split="gini"))




fit_decision
summary(fit_decision)

fit_decision$variable.importance

#install.packages("rpart.plot")
library("rpart.plot")
prp(fit_decision)

plotcp(fit_decision)
fit_decision$cptable
mincp=fit_decision$cptable[which.min(fit_decision$cptable[,"xerror"]),"CP"]
prune(fit_decision,mincp)

predict_valid=predict(fit_decision,valid,type="class")
#install.packages("caret")
library(caret)
confusionMatrix(predict_valid, valid$loan_status) 

predict_test=predict(fit_decision,test,type="class")
confusionMatrix(predict_test,test$loan_status)

##################################### Current ###########################################

c1=subset(x = loan,loan$loan_status=="Current")
predict_Current=predict(fit_decision,c1,type="class")
table(predict_Current)


