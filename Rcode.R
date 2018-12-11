#PACKAGE-USED---------------------------------------------------------------------------------------------------
install.packages("GGally")
#Power cut code
#dat = read.table("~/Documents/Rcode/MATH3714/auto.csv", header = T,row.names = 1, stringsAsFactors = F, sep = ",")
#dat$int = NULL
#ERROR-CORRECTING-----------------------------------------------------------------------------------------------
dat = read.table("http://www1.maths.leeds.ac.uk/~charles/math3714/Auto.csv", header = T, stringsAsFactors = F)
#---Problem 1
#fixing the year problem for the golf
dat$year[dat$name=="vw golf estate S 1.4 TSI"] = 118

#---Problem 2
#In order to achieved this I need to add an extra tag into the dataframe which is "stringAsFactors=F".
#adding a extra entry called make which stands for the maker of the car.
dat$make = dat$name

#changing the string into the first word of the sring.
for(string in dat$make){
  substring = strsplit(string, " ")[[1]]
  maker = substring[1]
  #print(maker)
  dat$make[dat$make==string]=maker
}

#changing the string into every word apart from the first word.
for(string in dat$name){
  substring = strsplit(string, " ")[[1]]
  #print(paste(substring[-1], collapse=' ' ))
  dat$name[dat$name==string]=paste(substring[-1], collapse=' ' )
}

#---Problem 3
#Changing the make to a proper make.
for(string in dat$make){
  if(string=="chevroelt" | string=="chevy"){
    dat$make[dat$make==string]="chevrolet"
  }
  if(string=="maxda"){
    dat$make[dat$make==string]="mazda"
  }
  if(string=="mercedes-benz"){
    dat$make[dat$make==string]="mercedes"
  }
  if(string=="toyouta"){
    dat$make[dat$make==string]="toyota"
  }
  if(string=="vokswagen"|string=="vw"){
    dat$make[dat$make==string]="volkswagen"
  }
}

#---Problem 5
dat$name=NULL

#---Problem 4
i=1
for(string in unique(dat$make)){
  print(paste("changing",string,"to",as.character(i), sep = " "))
  for(make in dat$make){
    if(make == string){
      dat$make[dat$make==string]= as.integer(i)
    }
  }
  i= i + 1
}
#changing categorical data to factor
dat$make=factor(dat$make)

#change origin to factors
# dat$origin[dat$origin==1]="American"
# dat$origin[dat$origin==2]="European"
# dat$origin[dat$origin==3]="Japanese"
# is.factor(dat$origin)

#change origin into factor
dat$origin = factor(dat$origin)
is.factor(dat$origin)

#pair wise plot
plot_pair=function(dat){
  x = cbind(dat$mpg, dat$cylinders,dat$displacement,dat$weight,dat$acceleration,dat$year)
  colnames(x)=c("mpg","cylinders","dis","wieght","acceleration","year")
  #changing the year column so if the year the car was made was 70 then not it's 0 i.e car year starts at 1970 rather than 1900.
  dat$year = dat$year-70
  pairs(x)
}
#MODELS-------------------------------------------------------------------------------------------------------
#First function with no transformation or interactions
base_model = function(dat){
  return(lm(mpg~.,data = dat))
}

#square root model
sqrt_model = function(dat){
  return(lm(sqrt(mpg)~., data=dat))
}

#logarithmic model
log_model = function(dat){
  return(lm(log(mpg) ~ ., data = dat))
}

recip_y = function(dat){
  return(lm(1/mpg ~., data = dat))
}

#reciprocal of certain variable
model2 = function(dat){
  dattemp=dat
  dattemp$displacement=1/dattemp$displacement
  dattemp$weight=1/dattemp$weight
  return(lm(mpg~.,data = dattemp))
}

ridge=function(dat){
  #install.packages("ridge")
  library(ridge)
  return(linearRidge(formula = mpg ~ ., data = dat))
}

box_cox = function(dat){
  base = lm(mpg~., data = dat)
  boxcox.lm = boxcox(base)
  return(boxcox.lm)
  ## now consider more general transformations:
  
}
#DIAGNOSTICS----------------------------------------------------------------------------------------------------
Res_VS_value=function(model_no,model,dat){
  #saving plot
  png(paste(model_no,"res_vs_value.png",sep = "_"))
  #Residual vs variables not including the make of the car
  no_variable = 7
  res = model$residuals
  dattemp=cbind.data.frame(dat$cylinders,dat$displacement,dat$horsepower,dat$weight,dat$acceleration,dat$year,dat$origin)
  colnames(dattemp)=c("cylinders","displacement","horsepower","weight","acceleration","year","origin")
  par(mfrow=c(3,3))
  for(i in 1:no_variable){
    print(names(dattemp)[i])
    plot(dattemp[,i],res,ylab = "residual",xlab = toString(names(dattemp)[i]))
  }
  dev.off()
  
  #plotting the plot
  for(i in 1:no_variable){
    print(names(dattemp)[i])
    plot(dattemp[,i],res,ylab = "residual",xlab = toString(names(dattemp)[i]))
  }
}

Res_VS_value1=function(model_no,model,dat){
  #saving plot
  png(paste(model_no,"res_vs_value.png",sep = "_"))
  #Residual vs variables not including the make of the car
  no_variable = 3
  res = model$residuals
  dattemp=as.data.frame(cbind(dat$prime, dat$acceleration, dat$year,dat$origin))
  View(dattemp)
  colnames(dattemp)=c("prime","acceleration","year")
  par(mfrow=c(3,3))
  for(i in 1:no_variable){
    print(names(dattemp)[i])
    plot(dattemp[,i],res,ylab = "residual",xlab = toString(names(dattemp)[i]))
  }
  dev.off()
  
  #plotting the plot
  for(i in 1:no_variable){
    print(names(dattemp)[i])
    plot(dattemp[,i],res,ylab = "residual",xlab = toString(names(dattemp)[i]))
  }
}

Res_VS_fit=function(model_no,model){
  #save the plot
  png(paste(model_no,"res_vs_fitted.png",sep = "_"))
  plot(model,1)
  dev.off()
  
  #plot the plots
  plot(model,1)
  
}

Normal=function(model_no,model){
  #saving plot
  png(paste(model_no,"QQplot.png",sep = "_"),width = 1000,height = 1000)
  res = model$residuals
  qqnorm(res)
  qqline(res)
  dev.off()
  
  #plot the plot
  qqnorm(res)
  qqline(res)
}

HomoSce=function(model_no,model){
  #plotting
  plot(model,3)
  
  #saving plot
  png(paste(model_no,"Variance.png",sep = "_"),width = 1000, height = 1000)
  plot(model,3)
  dev.off()
}

check_assumption=function(model_no,model,dat){
  print("Variance")
  HomoSce(model_no,model)
  print("Q-Q")
  Normal(model_no,model)
  print("Res VS value")
  Res_VS_value(model_no,model, dat)
  print("Res VS fitted")
  Res_VS_fit(model_no,model)
}

check_assumption1=function(model_no,model,dat){
  print("Variance")
  HomoSce(model_no,model)
  print("Q-Q")
  Normal(model_no,model)
  print("Res VS fitted")
  Res_VS_fit(model_no,model)
  print("Res VS value")
  Res_VS_value1(model_no,model,dat)
}

check_multicollinearity=function(dat){
  #Check for none factors only
  X=as.matrix(dat)
  round(diag(solve(t(X)%*%X)),3) 
  v=eigen(t(X)%*%X)
  print("Eigenvalues")
  print(round(v$values,1))
  print("Max Eigenvalue/Eigenvalues")
  print(round(max(v$values)/v$values,0))
  print("Eigenvectors")
  print(round(v$vectors,1))
  S=svd(X)
  print(S$d)
  print(max(S$d)/S$d)
}

pairwise_cor=function(dat){
  library(GGally)
  ggpairs(dat)
}
#BENCHMARK------------------------------------------------------------------------------------------------------
#use to predict value from a given dataset
# predict = function(model,testData){
#   predicted <- predict(model, testData)
#   compare <- cbind(actual=testData$response, predicted)
#   return(mean(apply(compare, 1, min)/apply(compare, 1, max)))
# }
# 
# #Create model and plit data into training and testing data. 
# #Then fit the model to the training data set
# #Then pass it to test how well it predict against the testing data which the model haven't seen.
# bench_mark=function(inputData){
#   library(glmnet)
#   trainingIndex <- sample(1:nrow(inputData), 0.8*nrow(inputData)) # indices for 80% training data
#   trainingData <- inputData[trainingIndex, ] # training data
#   testData <- inputData[-trainingIndex, ] # test data
#   #print(trainingData)
#   #print(testData)
#   
#   
#   #MODEL  
#   base = base_model(trainingData)
#   square_root = sqrt_model(trainingData)
#   Log_model = log_model(trainingData)
#   rec_y = recip_y(trainingData)
#   model1 = model2(trainingData)
#   log_recip_model = lm(log(1/mpg)~., data = trainingData)
#   #LinRidge = ridge(trainingData)
#   
#   #TESTING HOW GOOD PREDICTIONS ARE.
#   predicted = cbind(predict(base, testData),predict(square_root, testData),predict(Log_model, testData),predict(rec_y, testData),
#                     predict(model1, testData),predict(log_recip_model, testData),predict(LinRidge, testData))
#   colnames(predicted) = c("base","sqrt","log","reciprocal","model1","log-reciprocal","Ridge")
#   print(predicted)
# }

#Testing for the residual of the model
res_bench=function(dat){
  base = base_model(dat)
  square_root = sqrt_model(dat)
  Log_model = log_model(dat)
  rec_y = recip_y(dat)
  model1 = model2(dat)
  log_recip_model = lm(log(1/mpg)~., data = dat)
  LinRidge = ridge(dat)
  res_summary=cbind(sum(base$residuals^2), sum(square_root$residuals^2),sum(Log_model$residuals^2),
                    sum(rec_y$residuals^2),sum(model1$residuals^2),sum(log_recip_model$residuals^2),33.53)
  colnames(res_summary)=c("base","square root","log","reciprocal","model1", "log reciprocal","Ridge")
  print(res_summary)
}

#Diagnostics-of-Model-------------------------------------------------------------------------------------------

#Initializing the models. These model uses the data that have multicollinearity involve.
base = base_model(dat)
square_root = sqrt_model(dat)
Log_model = log_model(dat)
rec_y = recip_y(dat)
model1 = model2(dat)
log_recip_model = lm(log(1/mpg)~., data = dat)
LinRidge = ridge(dat)

#Checking the assumption of models.
check_assumption("base",base,dat)
check_assumption("model1",model1,dat)
check_assumption("sqrt",square_root,dat)
check_assumption("log",Log_model,dat)
check_assumption("reciprocal",rec_y,dat)
check_assumption("log_reciprocal",log_recip_model,dat)

#Solving MultiCollinearity
prime = (dat$cylinders+dat$weight)/(dat$horsepower+dat$displacement)
check_multicollinearity(as.data.frame(cbind(prime,dat$acceleration,dat$year)))
# [1] "Eigenvalues"
# [1] 2423800.9    3567.0     688.2
# [1] "Max Eigenvalue/Eigenvalues"
# [1]    1  680 3522
# [1] "Eigenvectors"
# [,1] [,2] [,3]
# [1,] -0.1 -0.5  0.9
# [2,] -0.2 -0.8 -0.5
# [3,] -1.0  0.2  0.0
# [1] 1556.85611   59.72403   26.23276
# [1]  1.00000 26.06750 59.34779

vif(as.data.frame(cbind(prime,dat$acceleration,dat$year)))
# Variables      VIF
# 1     prime 2.103025
# 2        V2 1.873807
# 3        V3 1.175262


#Data with Multicollinear solved.
dat2=as.data.frame(cbind(dat$mpg,prime,dat$acceleration,dat$year,dat$origin,dat$make))
colnames(dat2)=c("mpg","prime","acceleration","year","origin","make")
base = base_model(dat2)
square_root = sqrt_model(dat2)
Log_model = log_model(dat2)
rec_y = recip_y(dat2)
log_recip_model = lm(log(1/mpg)~., data = dat2)
LinRidge = ridge(dat2)

#Checking the assumption of models.
check_assumption1("base_cor",base,dat2)
check_assumption1("model1_cor",model1,dat2)
check_assumption1("sqrt_cor",square_root,dat2)
check_assumption1("log_cor",Log_model,dat2)
check_assumption1("reciprocal_cor",rec_y,dat2)
check_assumption1("log_reciprocal_cor",log_recip_model,dat2)
#IGNORE
#Benchmarking the models. Unable to perform them due to R having limit on rams for stacks.
# trainingIndex <- sample(1:nrow(dat), 0.99*nrow(dat))
# trainingData <- dat[trainingIndex, ] 
# testData <- dat[-trainingIndex, ]
# base_test = base_model(trainingData)
# square_root_test = sqrt_model(trainingData)
# Log_model_test = log_model(trainingData)
# rec_y = recip_y(trainingData)
# model1 = model2(trainingData)
# log_recip_model = lm(log(1/mpg)~., data = trainingData)
# LinRidge = ridge(trainingData)
# predicted <- predict(model, testData)
# predicted = predict(base_test,testData)

#Checking which model has the best residual
res_bench(dat)
# base square root     log reciprocal   model1 log reciprocal Ridge
# [1,] 8129.847    83.09975 5.60372 0.03921142 3328.877       14.86362 33.53

#Checking-for-Outliers-----------------------------------------------------------------------------------------
#Cook's distance
check_cook=function(model_no,model,dat){
  par(mfrow=c(1,2))
  png(paste(model_no,"Cooks.png",sep = "_"),width = 1000, height = 1000)
  plot(model,4)
  plot(model,5)
  dev.off()
}

#Logarithmic Model
check_cook("log_cor",Log_model,dat)

#Reciprocal Model
check_cook("reciprocal_cor",rec_y,dat)

summary(Log_model)
summary(rec_y)

Log_model = log_model(dat[-9])
step(Log_model,direction = "both")

#AIC------------------------------------------------------------------------------------------------------------
#FINAL-TWO-MODEL------------------------------------------------------------------------------------------------
dat3=dat2[-6]
final_log = lm(log(mpg) ~ ., data = dat3)
final_rec = lm(1/mpg ~ ., data = dat3)

step(final_log, direction="both")
step(final_rec,direction = "both")
# > step(final_log, direction="forward")
# Start:  AIC=-1321.51
# log(mpg) ~ (prime + acceleration + year + origin)^2
# 
# 
# Call:
#   lm(formula = log(mpg) ~ (prime + acceleration + year + origin)^2, 
#      data = dat3)
# 
# Coefficients:
#   (Intercept)                prime         acceleration                 year               origin  
# 0.5409502            0.3444590           -0.1970143            0.0111617            0.2646668  
# prime:acceleration           prime:year         prime:origin    acceleration:year  acceleration:origin  
# -0.0030720           -0.0014064           -0.0739016            0.0021538            0.0399048  
# year:origin  
# 0.0008263  

# > step(final_rec, direction="forward")
# Start:  AIC=-3660.86
# 1/mpg ~ (prime + acceleration + year + origin)^2
# 
# 
# Call:
#   lm(formula = 1/mpg ~ (prime + acceleration + year + origin)^2, 
#      data = dat3)
# 
# Coefficients:
#   (Intercept)                prime         acceleration                 year               origin  
# 2.828e-01           -2.242e-02            7.727e-03           -1.930e-03           -3.296e-02  
# prime:acceleration           prime:year         prime:origin    acceleration:year  acceleration:origin  
# 1.324e-04            1.480e-04            3.557e-03           -8.655e-05           -1.526e-03  
# year:origin  
# 1.473e-04
final_log = lm(log(mpg)~., data = dat3)
final_rec = lm(1/mpg~., data = dat3)
step(final_log, direction="both")
final_rec = step(final_rec,direction = "both")
# > step(final_log, direction="both")
# Start:  AIC=-1275.53
# log(mpg) ~ prime + acceleration + year + origin
# 
# Df Sum of Sq    RSS     AIC
# - acceleration  1    0.0240 14.945 -1276.9
# <none>                      14.921 -1275.5
# - origin        1    1.2778 16.198 -1245.2
# - year          1    3.9017 18.822 -1186.2
# - prime         1    3.9600 18.881 -1185.0
# 
# Step:  AIC=-1276.9
# log(mpg) ~ prime + year + origin
# 
# Df Sum of Sq    RSS     AIC
# <none>                      14.945 -1276.9
# + acceleration  1    0.0240 14.921 -1275.5
# - origin        1    1.5229 16.467 -1240.8
# - year          1    4.0292 18.974 -1185.1
# - prime         1    6.8062 21.751 -1131.4
# 
# Call:
#   lm(formula = log(mpg) ~ prime + year + origin, data = dat3)
# 
# Coefficients:
#   (Intercept)        prime         year       origin  
# 0.11322      0.08052      0.02581      0.09420  

# > step(final_rec,direction = "both")
# Start:  AIC=-3607.76
# 1/mpg ~ prime + acceleration + year + origin
# 
# Df Sum of Sq      RSS     AIC
# - acceleration  1 0.0000098 0.039501 -3609.7
# <none>                      0.039491 -3607.8
# - origin        1 0.0022469 0.041738 -3588.0
# - year          1 0.0077182 0.047209 -3539.6
# - prime         1 0.0096978 0.049189 -3523.5
# 
# Step:  AIC=-3609.66
# 1/mpg ~ prime + year + origin
# 
# Df Sum of Sq      RSS     AIC
# <none>                      0.039501 -3609.7
# + acceleration  1 0.0000098 0.039491 -3607.8
# - origin        1 0.0025693 0.042070 -3586.9
# - year          1 0.0078913 0.047392 -3540.1
# - prime         1 0.0178094 0.057310 -3465.4
# 
# Call:
#   lm(formula = 1/mpg ~ prime + year + origin, data = dat3)
# 
# Coefficients:
#   (Intercept)        prime         year       origin  
# 0.185501    -0.004119    -0.001142    -0.003869  

#Predicting the new value
prime = (dat$cylinders+dat$weight)/(dat$horsepower+dat$displacement)
dat2=as.data.frame(cbind(dat$mpg,prime,dat$acceleration,dat$year,dat$origin,dat$make))
colnames(dat2)=c("mpg","prime","acceleration","year","origin","make")
dat3=dat2[-6]
final_log = lm(log(mpg) ~ ., data = dat3)
final_rec = lm(1/mpg ~ ., data = dat3)
final_rec = step(final_rec,direction = "both")
summary(final_rec)
new = as.data.frame(cbind(15.043956,100,3))
colnames(new)=c("prime","year","origin")
predict.lm(final_rec,newdata=new,interval="confidence",level=0.95)
# fit          lwr         upr
# 1 -0.002286896 -0.008114051 0.003540259
# fit      lwr      upr
# 1 4.187991 4.074648 4.301334