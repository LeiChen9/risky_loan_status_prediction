loan = read.csv("loan_2016q1.csv", stringsAsFactors = F) # factor convert string to numeric when new feature comes in
loanT = loan

# check data
summary(loan)
str(loan)
# We got numerical variables, categorical variables, and also categorical variables that supposed to be numerical
head(loan) # lots of NA
dim(loan)
colnames(loan)


# check date related features 
head(loan[, c("issue_d", "last_pymnt_d", "next_pymnt_d" )])
dim(subset(loan, next_pymnt_d == ''))
with(subset(loan, next_pymnt_d == ''), table(loan_status)) # either charged off or fully paid
with(subset(loan, next_pymnt_d == '' & last_pymnt_d == ''), table(loan_status)) # all charged off

with(subset(loan, last_pymnt_d == ''), table(loan_status))

# one variable
## numerical
### distribution plot, bar plot
colnames(loan[which(sapply(loan, function(x){is.numeric(x)}))]) # got numeric variables
length(loan$annual_inc[which(loan$annual_inc == '')]) # annual_inc should be numeric, but not
loan$annual_inc <- as.numeric(as.factor(loan$annual_inc))
summary(loan$annual_inc)
plot(density(log(loan$annual_inc))) # log normal in origin plot

boxplot(loan$annual_inc)
boxplot(log(loan$annual_inc)) # has zero in data
boxplot(log(loan$annual_inc+1))
boxplot(log(annual_inc+ 1) ~ grade, data = loan)

# top and bottom line to define outlier --- Q3 + 1.5IQR, Q1 - 1.5IQR
# remove outlier -- 0.3% - 99.7%    3 sigma principle
# process outlier -- discritimize
# box means 1st quantile, median and 3rd quantile

# Clear features that should be numeric, but not
head(loan$int_rate)
which(sapply(loan[1, ], function(x){return(grepl('%', x))})) 
loan$int_rate <- as.numeric(sapply(strsplit(loan$int_rate, '%'), '[', 1)) 
loan$revol_util <- as.numeric(sapply(strsplit(loan$revol_util, '%'), '[', 1))

loan$dti <- loanT$dti
loan$dti[which(loan$dti == '')] <- NA
loan$dti <- as.numeric(as.factor(loan$dti))

length(loan$total_acc[which(loan$total_acc == '')])
loan$total_acc <- as.numeric(as.factor(loan$total_acc))

length(loan$annual_inc_joint[which(loan$annual_inc_joint == '')])
loan$annual_inc_joint[which(loan$annual_inc_joint == '')] <- NA
loan$annual_inc_joint <- as.numeric(as.factor(loan$annual_inc_joint))


## categorical

sort(table(loan$loan_status))
round(sort(table(loan$loan_status)) / dim(loan)[1] * 100, 2)
barplot(sort(table(loan$loan_status), decreasing = T))

sort(table(loan$purpose))
round(sort(table(loan$purpose)) / dim(loan)[1] * 100, 2)
barplot(sort(table(loan$purpose), decreasing = T))
# two variable
## categorical vs categorical -- confusion matrix
## categorical vs numerical -- boxplot
## numerical vs numerical -- scatter plot

# multiple -- corvarience matrix

# Numeric variable with numerical response, interest rate
with(loan[1:10000, ], plot(log(annual_inc + 1), int_rate))
library(corrplot)
correlations <- cor(loan[, c('int_rate', 'total_acc', 'acc_now_delinq', 'annual_inc', 'dti',
                             'loan_amnt')],
                    use = 'pairwise.complete.obs') # pairwise deletion
corrplot(correlations, method = 'square', tl.cex = 1, type = 'lower')

# Categorical variable with numerical response
boxplot(subset(loan, term == ' 36 months')$int_rate,
        subset(loan, term == ' 60 months')$int_rate)
boxplot(int_rate ~ purpose, data=loan)
with(loan, round(table(term, loan_status) / dim(loan)[1] * 100), 2)

# Dealing with the date related feature
library(zoo)
head(loan$issue_d)
loan$issue_d_1 <- as.Date(as.yearmon(loan$issue_d, '%b-%Y'))
loan$issue_year <- as.character(format(loan$issue_d_1, '%Y'))
loan$issue_mon <- as.character(format(loan$issue_d_1, '%m'))

loan$last_pymnt_d_1 <- as.Date(as.yearmon(loan$last_pymnt_d, '%b-%Y'))
loan$last_pymnt_year <- as.character(format(loan$last_pymnt_d_1, '%Y'))
loan$last_pymnt_mon <- as.character(format(loan$last_pymnt_d_1, '%m'))

loan$last_pymnt_from_issue <- with(loan, last_pymnt_d_1 - issue_d_1)
head(loan$last_pymnt_from_issue)
table(loan$last_pymnt_from_issue) # too many feature

loan$pay_gap <- with(loan, as.character(cut(as.numeric(last_pymnt_from_issue),
                                            c(-1,0, 92, 184, 275, 366, 457, 549, 639, 730, 822, 914, 975, 1096))))
table(loan$pay_gap)
sum(table(loan$pay_gap)) # there exists NA

# check NA out
table(subset(loan, is.na(pay_gap))$last_pymnt_d)
# there is 148 na shows in last_pymnt_d
# missing value imputation
loan$last_pymnt_from_issue[which(is.na(loan$last_pymnt_from_issue))] <- 2000
loan$pay_gap[which(is.na(loan$pay_gap))] <- 'no pymnt'

# test if pay_gap could be a useful feature
by_pymnt_gap <- with(loan, table(pay_gap, loan_status))
by_pymnt_gap
round(100 * by_pymnt_gap / apply(by_pymnt_gap, 1, sum), 3)
loan$pay_gap <- relevel(as.factor(loan$pay_gap), ref = 'no pymnt')
table(loan$pay_gap)
loan$loan_status_binary <- as.factor(ifelse(loan$loan_status %in% c('Fully Paid', 'Current'),
                                            'okay', 'past_due'))
# categorical variables show better performance than numeric. As numeric assume a linear relationship
mod1 <- glm(loan_status_binary ~ pay_gap, data = loan, family = 'binomial') # binomial for logit
summary(mod1)
mod2 <- glm(loan_status_binary ~ last_pymnt_from_issue, loan, family = 'binomial')
summary(mod2)
# mod1 p-value relatively high
# complete or more accuartely quasi complete separatable data
# MLE for logistic regression doesn't exist in the case of complete or more accurately quasi
# log likelihood doesn't exist
# when a feature can totally classify a category, don't use it in modeling, take it as a rule

with(loan, table(loan_status_binary, pay_gap))

mod1 <- glm(loan_status_binary ~ pay_gap, subset(loan, !pay_gap %in% c('no pymnt', '(-1,0]')),
            family = 'binomial')
summary(mod1)

# process other date related features
date_cols <- colnames(loan)[c(which(grepl('_d$', colnames(loan))),
                              which(grepl('_date$', colnames(loan))))]
date_cols
for (col_i in date_cols) {
  loan[, col_i] <- as.Date(as.yearmon(loan[, col_i], '%b-%Y'))
}
# convert date to gap
loan$mon_since_issue <- as.integer((as.Date('2017-11-01') - loan$issue_d) / 30)
loan$mon_since_credit_pull <- as.integer((as.Date('2017-11-01') - loan$last_credit_pull_d) / 30)
TransformToLengthFromIssueDate <- function(loan, col.name, new.col.name, other.level) {
  # get difference in months.
  loan[, new.col.name] <-
    ifelse(is.na(loan[, col.name]), other.level,
           as.character(cut(as.integer((loan[, col.name] - loan$issue_d) /30), 
                            c(min(as.integer((loan[, col.name] - loan$issue_d) /30), na.rm = T) - 1,
                              quantile(as.integer((loan[, col.name] - loan$issue_d) /30), c(0.1, 0.9), na.rm = T),
                              max(as.integer((loan[, col.name] - loan$issue_d) /30), na.rm = T)))))
  return(loan)
}
loan <- TransformToLengthFromIssueDate(loan, 'hardship_start_date' ,'hardship_since_issue', 'no_hs')
loan <- TransformToLengthFromIssueDate(loan, 'settlement_date' ,'settlement_since_issue', 'no_settle')
table(loan$hardship_since_issue)
loan <- loan[, -which(colnames(loan) %in% date_cols)]

# Treat categorical features with too many values
num.value <- sapply(loan, function(x){return(length(unique(x)))})
colnames(loan)[intersect(which(sapply(loan, function(x){return(is.character(x))})),
                         which(num.value >= 50))]

loan$earliest_cr_line <- as.Date(as.yearmon(loan$earliest_cr_line, '%b-%Y'))
loan$mths_since_crline <- as.integer((as.Date('2017-11-01') -loan$earliest_cr_line)/30)

loan <- loan[, -which(colnames(loan) %in% c("emp_title", "zip_code", "addr_state", 'earliest_cr_line'))]

# Treat features which is joint oriented
colnames(loan)[which(grepl('joint', colnames(loan)))]
loan$dti <- ifelse(!is.na(loan$dti_joint), loan$dti_joint, loan$dti)
loan$annual_inc <- ifelse(!is.na(loan$annual_inc_joint), loan$annual_inc_joint, loan$annual_inc)
loan$verification_status <- ifelse(!is.na(loan$verification_status_joint), loan$verification_status_joint, loan$verification_status)
loan$revol_bal <- ifelse(!is.na(loan$revol_bal_joint), loan$revol_bal_joint, loan$revol_bal)

loan <- loan[, -which(grepl('joint', colnames(loan)))]

# missing value

num.NA = sort(sapply(loan, function(x) {sum(is.na(x))}), decreasing = T)
num.NA
sum(is.na(loan)) / (nrow(loan)* ncol(loan))
sort(num.NA, decreasing = TRUE)[1:10]
left <- colnames(loan[, names(num.NA[which(num.NA != dim(loan)[1])])])
length(left)
loan <- loan[, left]

# check columns with a lot of NA, for example, hardship related features
colnames(loan)[which(grepl('hardship', colnames(loan)))]
summary(loan$orig_projected_additional_accrued_interest)
loan$orig_projected_additional_accrued_interest[which(is.na(loan$orig_projected_additional_accrued_interest))] <- 0

# check some columns and find out not only NA but also empty value.
loan$hardship_reason <- ifelse(loan$hardship_reason == '', 'no_hs', loan$hardship_reason)
loan$hardship_status <- ifelse(loan$hardship_status == '', 'no_hs', loan$hardship_status)
loan$hardship_loan_status <- ifelse(loan$hardship_loan_status == '', 'no_hs', loan$hardship_loan_status)
loan$hardship_amount[which(is.na(loan$hardship_amount))] <- 0
loan$hardship_dpd[which(is.na(loan$hardship_dpd))] <- 0
loan$hardship_payoff_balance_amount[which(is.na(loan$hardship_payoff_balance_amount))] <- 0
loan$hardship_last_payment_amount[which(is.na(loan$hardship_last_payment_amount))] <- 0


loan <- loan[, -which(colnames(loan) %in% c('deferral_term',
                                            'hardship_length', 'hardship_type'))]

num.empty <- sapply(loan[, colnames(loan)[which(sapply(loan, function(x){return(is.character(x))}))]],
                    function(x){return(length(which(x == "")))})
num.empty[which(num.empty > 0)]

loan <- loan[, -which(colnames(loan) %in% c('verification_status ', 'url', 'desc', 'title'))]

# Similarly for settlement; third party solution for close the loan
loan$settlement_amount[which(is.na(loan$settlement_amount))] <- 0
loan$settlement_percentage[which(is.na(loan$settlement_percentage))] <- 0
loan$settlement_term[which(is.na(loan$settlement_term))] <- 0
loan$settlement_status <- ifelse(is.na(loan$settlement_status), 'no_settlement', loan$settlement_status)

num.NA <- sort(sapply(loan, function(x) { sum(is.na(x))} ), decreasing = TRUE)
sort(num.NA, decreasing = TRUE)[1:10]

# categorize mths related features
for(col_i in setdiff(names(num.NA)[which(grepl('mths_since', names(num.NA))& num.NA > 0)],
                     c('mths_since_issue', 'mths_since_crline', 'mths_since_last_credit_pull'))) {
  breaks <- quantile(loan[, col_i], c(0.1, 0.5, 0.9), na.rm = T)
  breaks <- c(min(loan[, col_i], na.rm = T) - 1, breaks, max(loan[, col_i], na.rm = T))
  loan[, col_i] <- ifelse(is.na(loan[, col_i]),
                          'not_avail', as.character(cut(loan[, col_i], breaks = breaks)))
}

# il_util: credit line to loan
plot(density(log(loan$il_util), na.rm = T))

# there got 17877 missing il_util
# is it bcuz no open account? Not for all the cases.
summary(subset(loan, is.na(il_util))$open_act_il) # 61 missing open_act_il
# is it becuz of the limit is 0? since open_act_il = total_bal_il / total_il_high_credit_limit
# not for all the cases.
with(subset(loan, is.na(il_util)), summary(total_il_high_credit_limit))

head(loan[which(is.na(loan$il_util) & loan$total_il_high_credit_limit != 0),
          c('il_util', 'total_bal_il', 'total_il_high_credit_limit')])
loan$il_util <- ifelse(is.na(loan$il_util) & loan$total_il_high_credit_limit != 0, 
                       loan$total_bal_il/ loan$total_il_high_credit_limit, loan$il_util)
summary(subset(loan, is.na(il_util) & total_il_high_credit_limit == 0)$open_act_il)
loan$il_util <-  ifelse(is.na(loan$il_util), 'no_il',
                        as.character(cut(loan$il_util, 
                                         c(min(loan$il_util, na.rm = T) - 0.01,
                                           quantile(loan$il_util, na.rm = T, c(0.1, 0.9)),
                                           max(loan$il_util, na.rm = T)))))
table(loan$il_util)

loan$mo_sin_old_il_acct <-  ifelse(is.na(loan$mo_sin_old_il_acct), 'no_il',
                                   as.character(cut(loan$mo_sin_old_il_acct, 
                                                    c(min(loan$mo_sin_old_il_acct, na.rm = T) - 0.01,
                                                      quantile(loan$mo_sin_old_il_acct, na.rm = T, c(0.1, 0.9)),
                                                      max(loan$mo_sin_old_il_acct, na.rm = T)))))
table(loan$mo_sin_old_il_acct)
# is it bcuz there is no open account? No.
summary(subset(loan, is.na(num_tl_120dpd_2m))$open_acc)
with(subset(loan, is.na(num_tl_120dpd_2m)), summary(num_tl_30dpd))
loan$num_tl_120dpd_2m <- ifelse(is.na(loan$num_tl_120dpd_2m), 0, loan$num_tl_120dpd_2m)

num.NA <- sort(sapply(loan, function(x) { sum(is.na(x))} ), decreasing = TRUE)
num.NA
for(col_i in names(num.NA)[num.NA > 0]) {
  loan[, col_i] <- ifelse(is.na(loan[, col_i]), median(loan[, col_i], na.rm = T), loan[, col_i])
}

num.unique <- sort(sapply(loan, function(x){return(length(unique(x)))}), decreasing = F)
num.unique

loan <- loan[, -which(colnames(loan) %in% c('grade', 'int_rate', 'sub_grade', 'policy_code'))]

table(loan$loan_status_binary)

# checking the features; could use l1-norm to select the feature
numeric.feats <- colnames(loan)[which(sapply(loan, function(x){return(is.numeric(x))}))]
for(col_i in numeric.feats) {
  formula = paste(col_i, " ~ loan_status_binary")
  p.val <- t.test(as.formula(formula), data = loan)$p.value
  if(p.val >= 0.05) {
    loan[, col_i] <- NULL
  }
}

cat.feats <- colnames(loan)[which(sapply(loan, function(x){return(is.character(x))}))]
cat.feats <- setdiff(cat.feats, 'loan_status_binary')

for(col_i in cat.feats) {
  cate_num <- length(table(loan[, col_i]))
  if(cate_num == 1) {
    loan[, col_i] <- NULL
  }
}

for(col_i in cat.feats) {
  p.val <- chisq.test(x = loan[, col_i], y = loan$loan_status_binary)$p.value
  if(p.val >= 0.05) {
    loan[, col_i] <- NULL
  }
}

str(loan$emp_length)
loan$emp_length <- ifelse(loan$emp_length == 'n/a', loan$emp_length,
                          ifelse(loan$emp_length %in% c('< 1 year', '1 year', '2 years', '3 years'),
                                 '< 3 years', ifelse(loan$emp_length %in% c('4 years', '5 years', '6 years', '7 years'), 
                                                     '4-7 years', '> 8 years')))

# modeling
set.seed(1)

loan_goal <- loan$loan_status_binary
loan_goal_df <- as.data.frame(loan_goal)
loan_goal <- loan_goal_df
loan_data <- loan[, -which(colnames(loan) %in% c('loan_status_binary', 'loan_status'))]
loan_sparse <- sparse.model.matrix( ~. , loan_data)
dim(loan_sparse)

train.ind <- sample(1:dim(loan)[1], 0.7 * dim(loan)[1])
train <- loan_sparse[train.ind, ]
test <- loan_sparse[-train.ind, ]
train_goal <- loan_goal[train.ind, ]
test_goal <- loan_goal[-train.ind, ]

library(glmnet)

# ind <- sparse.model.matrix( ~. , train[, -which(colnames(train) %in% c('loan_status_binary', 'loan_status'))])
# dep <- train$loan_status_binary
mod1_data <- cbind(loan, loan_goal)
mod1_data$loan_goal <- NULL
mod1_data$loan_status <- NULL
str(mod1_data)
mod1_train <- mod1_data[train.ind, ]
mod1_test <- mod1_data[-train.ind, ]
mod1 <- glm(loan_status_binary ~ ., data = mod1_train, family = "binomial")

# residual = observed - fitted
head(sort(mod1$res))
mod1$res[which.min(mod1$res)]
mod1$res[which.max(mod1$res)]
plot(mod1$fit, mod1$res, xlab = 'Fitted', ylab = 'residual')
summary(mod1)
plot(mod1)

Sys.time()
cv.mod <- cv.glmnet(train[1:10000, ], train_goal[1:10000], family = 'binomial')
Sys.time()

plot(cv.mod)
cv.mod$lambda.1se
coef <- coef(cv.mod, s = 'lambda.1se') # make feature selection via l1 norm
coef

pred <- predict(cv.mod, newx = test, s = 'lambda.1se')

rmse <- (mean((as.numeric(pred) - as.numeric(test_goal))^2))^0.5
rmse

summary(cv.mod)

library(ggfortify)

autoplot(cv.mod)

library(plotmo) # for plotres
plotres(cv.mod)

