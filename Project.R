library(tidyverse)
library(caret)
library(leaps)
library(lattice)
library(e1071)
library(normtest)
library(MASS)
library(car)
install.packages("fBasics")

body = read.table('body.dat', header = T)
body$gender = as.factor(body$gender)
full_model = lm(height ~ ., data = body)

plot(body$pelvic.breadth , body$height)
boxplot(body$height~body$gender, main = "Factor variable Gender", xlab = "Gender", ylab = "Height")
boxplot(body$age, main = "Age")
#Transforming covariates to correct skewness
skewness(body$biacromial)
skewness(body$pelvic.breadth)
skewness(body$bitrochanteric)
skewness(body$chest.depth)
skewness(body$chest.diam)
skewness(body$elbow.diam)
skewness(body$wrist.diam)
skewness(body$knee.diam)
skewness(body$ankle.diam)
skewness(body$shoulder.girth)
skewness(body$chest.girth)
skewness(body$waist.girth)
skewness(body$navel.girth)
skewness(body$hip.girth)
skewness(body$thigh.girth)
skewness(body$bicep.girth)
skewness(body$forearm.girth)
skewness(body$knee.girth)
skewness(body$calf.girth)
skewness(body$ankle.girth)
skewness(body$wrist.girth)
skewness(body$age) #Skewness of age > 1
skewness(body$weight)
skewness(body)
library(fBasics)
B = basicStats(body)
C= data.frame(B[15,])
C = C[,-c(24,25)]
C_transpose = t(C)

skewness(log(body$age)) #Skewness reduced to 0.54 after taking log

body_log_transformed = body
body_log_transformed$age = log(body_log_transformed$age)

#Best subset variable selection using BIC
models = regsubsets(height ~ ., data = body_log_transformed , nvmax = 24)
summary(models)
best_sum = summary(models)
I = data.frame(
  Adj.R2 = which.max(best_sum$adjr2),
  CP = which.min(best_sum$cp),
  BIC = which.min(best_sum$bic)
)
coef(models, 11)
plot(models, scale = "bic", main = "BIC")
res.legend <-subsets(models, statistic="bic", legend = FALSE, min.size = 10, main = "BIC")
abline(a = 1, b = 1, lty = 2)


#Best subset variable selection using minimum average cross-validation error
# get_model_fomula function
# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
get_model_formula <- function(id, object, outcome){
# get models data
models <- summary(object)$which[id,-1]
# Get outcome variable
#form <- as.formula(object$call[[2]])
#outcome <- all.vars(form)[1]
# Get model predictors
predictors <- names(which(models == TRUE))
predictors <- paste(predictors, collapse = "+")
# Build model formula
as.formula(paste0(outcome, "~", predictors))
}
#get_cv_error function
get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 5)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$bic
}
# Compute cross-validation error
model.ids = 1:24
cv.errors =  map(model.ids, get_model_formula, models, "height") %>%
  map(get_cv_error, data = body) %>%
  unlist()
cv.errors
which.min(cv.errors)

#Model selected By best subset according to BIC
BIC_model = lm(height ~ weight+gender+biacromial+pelvic.breadth+knee.diam+ankle.diam+chest.girth+
                waist.girth+thigh.girth+bicep.girth+calf.girth, data = body)
summary(BIC_model)
vif(BIC_model)
plot(BIC_model)
jb.norm.test(residuals(BIC_model))
summary(aov(BIC_model))

#Box-Cox transformation
boxcox(height ~ weight+gender+biacromial+pelvic.breadth+
         waist.girth, data = body)
#close to 1, so no transformation

model_log = lm(log(height) ~ weight+gender+biacromial+pelvic.breadth+
                 waist.girth, data = body)
summary(model_log)
plot(model_log)
vif(model_log)
jb.norm.test(residuals(model_log))
jb.norm.test(residuals(model2))
aov(BIC_model)
summary(aov(model_no_chest.girth_calf.girth_bicep.girth_knee.diam_ankle.diam_thigh.girth))
model_no_chest.girth = lm(height ~ biacromial+pelvic.breadth+knee.diam+ankle.diam+
                            waist.girth+thigh.girth+bicep.girth+calf.girth+weight+gender, data = body)
model_no_chest.girth_waist.girth = lm(height ~ biacromial+pelvic.breadth+knee.diam+ankle.diam+
                                        thigh.girth+bicep.girth+calf.girth+weight+gender, data = body)
model_no_chest.girth_waist.girth_bicep.girth = lm(height ~ biacromial+pelvic.breadth+knee.diam+ankle.diam+
                                                    thigh.girth+calf.girth+weight+gender, data = body)
model_no_chest.girth_waist.girth_bicep.girth_calf.girth = lm(height ~ biacromial+pelvic.breadth+knee.diam+ankle.diam+
                                                               thigh.girth+weight+gender, data = body)
model_no_chest.girth_calf.girth = lm(height ~ biacromial+pelvic.breadth+knee.diam+ankle.diam+
                                       waist.girth+thigh.girth+bicep.girth+weight+gender, data = body)
model_no_chest.girth_calf.girth_bicep.girth_knee.diam_ankle.diam_thigh.girth = lm(height ~ weight+gender+biacromial+pelvic.breadth+
                                                   waist.girth, data = body)
model_no_chest.girth_calf.girth_bicep.girth_waist.girth = lm(height ~ biacromial+pelvic.breadth+knee.diam+ankle.diam+
                                                               thigh.girth+weight+gender, data = body)
vif(BIC_model3)
vif(model_no_chest.girth_calf.girth_bicep.girth_knee.diam_ankle.diam_thigh.girth)
summary(model_no_chest.girth_calf.girth_bicep.girth_knee.diam_ankle.diam_thigh.girth)
plot(model_no_chest.girth_calf.girth_bicep.girth_knee.diam_ankle.diam_thigh.girth)
jb.norm.test(residuals(model_no_chest.girth_calf.girth_bicep.girth_knee.diam_ankle.diam_thigh.girth))

#Removing points with large Cook's distance
cooksd = cooks.distance(model_no_chest.girth_calf.girth_bicep.girth_knee.diam_ankle.diam_thigh.girth)
sample_size = nrow(body)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
influential = as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
body_no_influential = body[-c(influential),]
model2 = lm(height ~ weight+gender+biacromial+pelvic.breadth+
              waist.girth, data = body_no_influential)
summary(model2)
jb.norm.test(residuals(model2))

jb.norm.test(residuals(model2))
summary(aov(model2))
vif(model2)

#Removing high leverage points
hatval = hatvalues(model_no_chest.girth_calf.girth_bicep.girth_knee.diam_ankle.diam_thigh.girth)
high_leverage = as.numeric(names(hatval)[(hatval > (10/sample_size))])

body_no_high_leverage = body[-c(high_leverage),]

body_no_both = intersect (body_no_influential, body_no_high_leverage)
model3 = lm(height ~ weight+gender+biacromial+pelvic.breadth+
              waist.girth, data = body_no_both)
jb.norm.test(residuals(model3))
summary(model3)

M = data.matrix(body[,-24])
#Lasso not useful
library(glmnet)
c = glmnet(M, body$height, standardize = TRUE, alpha = 1)
plot(c)
cv = cv.glmnet(M, body$height, standardize = TRUE, type.measure = "mse", nfolds = 5, alpha =1)
plot(cv)

install.packages("lars")
library(lars)
lars = lars(M, data$height, type = "lasso")
plot(lars)
summary(lars)
