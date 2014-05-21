#### create a data frame ####

library(data.table)
final <- read.csv(file="final.csv",header=TRUE,row.names=1)
final$Release_Date <- as.Date(final$Release_Date)

n <- nrow(final)
set.seed(7219)
in.train <- as.vector(replicate(n/5,sample(x=c(1,1,1,0,0),size=5,replace=FALSE)))

final$in_train[order(final$Revenue)] <- in.train

train <- data.table(final[final$in_train==1,])

test <- data.table(final[final$in_train==0,])

#### exploratory data analysis ####

library(ggplot2)

## exploring distributions

# histogram
ggplot(data= final, aes(x= Revenue)) + geom_bar()

# density plots
ggplot(data= final, aes(x= Revenue)) + geom_density()
ggplot(data= final, aes(x= logRev)) + geom_density() # log transformation

# scatter plots
base_plot <- ggplot(data= final, aes(y= logRev, x= N_Theaters))

base_plot + geom_point()
base_plot + geom_point() + stat_smooth()

# mind your data types!
base_plot2 <- ggplot(data= final, 
                    aes(y= logRev, x= N_Theaters, color= Wide_Release))

base_plot2 + geom_point()

final$Wide_Release2 <- as.factor(final$Wide_Release)
base_plot3 <- ggplot(data= final, 
                     aes(y= logRev, x= N_Theaters, color= Wide_Release2))

base_plot3 + geom_point()

base_plot3 + geom_point() + stat_smooth(method= lm)

#### building regression models ####

fit <- lm(data= train, logRev ~ N_Theaters)
summary(fit)
plot(fit)

fit2 <- update(fit, . ~ logThea) # example of update function
summary(fit2)
plot(fit2)

fit3 <- update(fit2, . ~ . + Wide_Release)
summary(fit3)
plot(fit3)

#### LASSO with glmnet ####

require(glmnet)

## Start with Day 60, but with Wide_Release:logThea, No_Article:logThea, etc

formula60 <- as.formula("logRev ~ logThea + Inception + No_Article*logThea + Wide_Release*logThea + is_sequel + is_3D_or_IMAX + Fri_Sat + in_Summer + Federal_Holiday + N_Other_Rel + No_Article*lV60 + Wide_Release*lV60 + Wide_Release*lU60 + Wide_Release*lR60 + Wide_Release*lE60") # fifteen predictors

## apply the lasso

grid <- 10^ seq(9, -2, length= 100) # lambda values

w1 <- train[,log(N_Theaters+1)] # weights
y1 <- train[,logRev]
X1 <- model.matrix(formula60, data= train)[,-1] # need to suppress intercept

lasso.mod60 <- glmnet(x= X1, y= y1, alpha= 1, lambda= grid, weights= w1)
cv60 <- cv.glmnet(x= X1, y= y1, weights= w1, lambda= grid)
plot(cv60)

# look at the coefficients at various values of lambda
eval.lasso <- function(cv.obj, lasso.obj, se.mult) {
  dt <- with(data= cv.obj, data.table(lambda, cvm , cvsd, cvup, cvlo))
  lambda.se <- max(dt[cvm <= min(cvm) + se.mult * dt[cvm == min(cvm), cvsd],
                      lambda])
  coef.se <- coef(lasso.obj, s= lambda.se)
  nonzero.coef <- coef.se[coef.se[,1] != 0, 1]
  cat("------------------------------------\n")
  cat(paste("Coefficients using largest lambda with MSE within", se.mult, "s.e. of smallest MSE\n"))
  print(nonzero.coef)
  form <- as.formula(
    paste("logRev ~",
          paste(names(nonzero.coef[-1]), collapse= " + ")))
  test.fit <- lm(data= test, formula= form, weights= log(test$N_Theaters+1))
  cat("Least squares model of test data using selected variables.\n")
  print(summary(test.fit))
  invisible(summary(test.fit)$adj.r.squared)
}

for (i in 0:3) {
  eval.lasso(cv.obj=cv60, lasso.obj=lasso.mod60, i)
}