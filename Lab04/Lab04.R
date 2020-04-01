################################ Functions #####################################
make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}
################################################################################

library(kernlab)
data("spam")
tibble::as_tibble(spam)

is.factor(spam$type)
levels(spam$type)

set.seed(42)
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]

fit_caps = glm(type ~ capitalTotal,
               data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                   data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.),
               data = spam_trn, family = binomial, maxit = 50)

# training misclassification rate
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)



################################## Excersise 1 #################################

### 1 ###
# training misclassification rate
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)

# testing misclassification rate
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_tst$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_tst$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_tst$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_tst$type)

library(boot)
set.seed(1)
cv.glm(spam_trn, fit_caps, K = 5)$delta[1]
cv.glm(spam_trn, fit_selected, K = 5)$delta[1]
cv.glm(spam_trn, fit_additive, K = 5)$delta[1]
cv.glm(spam_trn, fit_over, K = 5)$delta[1]

# "A model that is underfit will have high training and high testing error while 
# an overfit model will have extremely low training error but a high testing error."
# https://towardsdatascience.com/overfitting-vs-underfitting-a-complete-example-d05dd7e19765

# Rating from most underfit to most overfit
# fit_caps, fit_selected, fit_additive, fit_over

### 2 ###
set.seed(100)
cv.glm(spam_trn, fit_caps, K = 100)$delta[1]
cv.glm(spam_trn, fit_selected, K = 100)$delta[1]
cv.glm(spam_trn, fit_additive, K = 100)$delta[1]
cv.glm(spam_trn, fit_over, K = 100)$delta[1]

# There were no differences, and the conclusions from part 1 do not change
################################################################################

spam_tst_pred = ifelse(predict(fit_additive, spam_tst) > 0,
                       "spam",
                       "nonspam")
spam_tst_pred = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")

(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))

table(spam_tst$type) / nrow(spam_tst)

################################## Excersise 1 #################################

### 3 ###
spam_tst_caps = ifelse(predict(fit_caps, spam_tst) > 0,
                       "spam",
                       "nonspam")

conf_mat_caps = make_conf_mat(predicted = spam_tst_caps, actual = spam_tst$type)

spam_tst_selected = ifelse(predict(fit_selected, spam_tst) > 0,
                       "spam",
                       "nonspam")

conf_mat_selected = make_conf_mat(predicted = spam_tst_selected, actual = spam_tst$type)

spam_tst_additive = ifelse(predict(fit_additive, spam_tst) > 0,
                       "spam",
                       "nonspam")

conf_mat_additive = make_conf_mat(predicted = spam_tst_additive, actual = spam_tst$type)

spam_tst_over = ifelse(predict(fit_over, spam_tst) > 0,
                       "spam",
                       "nonspam")

conf_mat_over = make_conf_mat(predicted = spam_tst_over, actual = spam_tst$type)

### 4 ###
# We determined that the fit_additive model is the best. We know that based on the rankings, 
# overfit models will have lower training error and high testing error as opposed to underfit 
# models which have both high testing and training error. Although having lower testing error 
# would provide more reliable predictions when applied to unexposed data, having a low training 
# error still helps increase testing accuracy.
# Sensitivity is the true positive rate, while specificity  is the true negative rate. Here, 
# sensitivity refers to nonspam ending up as spam, while specificity refers to spam ending up 
# an nonspam. As a result, we decided that the fit_additive model would be the best option. 


################################################################################

################################## Excersise 2 #################################

### 1 ###
bank <- read.csv("bank.csv")

set.seed(3)
bank_idx = sample(nrow(bank), 2100)
bank_trn = bank[bank_idx, ]
bank_tst = bank[-bank_idx, ]

### 2 ###
bank_fit <- glm(
  default ~ age + housing + loan + education,
  family = binomial,
  data = bank_trn 
  )

cv.glm(bank_trn, bank_fit, K = 10)$delta[1]

### 3 ###
# Based on our model, age has a positive coefficient. This means that older individuals are more 
# likely to have y = yes. Housing also has a positive coefficient. This means that individuals 
# with housing or homeowners are more likely to have y = yes. Loan has a positive coefficient. 
# This implies that individuals that took out loans are more likely to have y = yes. Finally, 
# education has a positive coefficient. This suggests that individuals with some education 
# level are more likely to have y = yes. 

### 4 ###
bank_tst_pred = ifelse(
  predict(bank_fit, bank_tst) > 0,
  "yes",
  "no"
  )

conf_mat_bank = make_conf_mat(predicted = bank_tst_pred, actual = bank_tst$default)
conf_mat_bank











