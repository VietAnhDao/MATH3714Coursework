#Power cut code
#dat = read.table("~/Documents/Rcode/MATH3714/auto.csv", header = T, stringsAsFactors = F, sep = ",")
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
#changing categorical data to factor
dat$make=factor(dat$make)
# i=1
# for(string in unique(dat$make)){
#   print(paste("changing",string,"to",as.character(i), sep = " "))
#   for(make in dat$make){
#     if(make == string){
#       dat$make[dat$make==string]= as.integer(i)
#     }
#   }
#   i= i + 1
# }

#change origin to factors
dat$origin[dat$origin==1]="American"
dat$origin[dat$origin==2]="European"
dat$origin[dat$origin==3]="Japanese"
is.factor(dat$origin)

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
  return(lm(log(mpg)~., data = dat))
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
  library(MASS)
  return(lm.ridge(mpg~.,data = dat, lambda = seq(0, 0.000001, 0.0000001)))
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

Res_VS_fit=function(model_no,model){
  #save the plot
  png(paste(model_no,"res_vs_fitted.png",sep = "_"))
  plot(model,1)
  dev.off()
  
  #plot the plots
  plot(preds,res,xlab = "fitted",ylab = "residuals")
  
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
  HomoSce(model_no,model)
  Normal(model_no,model)
  Res_VS_value(model_no,model, dat)
  Res_VS_fit(model_no,model)
}


check_multicollinearity=function(dat){
  #Check for none factors only
  X=as.matrix(cbind(dat$cylinders,dat$displacement,dat$horsepower,dat$weight,dat$acceleration,dat$year))
  round(diag(solve(t(X)%*%X)),3) 
  v=eigen(t(X)%*%X)
  round(v$values,1)
  round(max(v$values)/v$values,0)
  round(v$vectors,1)
  S=svd(X)
  S$d
  max(S$d)/S$d
}

#BENCHMARK------------------------------------------------------------------------------------------------------
bench_mark=function(dat){
}

#Diagnostics-of-Model-------------------------------------------------------------------------------------------
#Running these codes will force save the plots onto the disk.
base = base_model(dat)
square_root = sqrt_model(dat)
Log_model = log_model(dat)
rec_y = recip_y(dat)
model1 = model2(dat)
log_recip_model = lm(log(1/mpg)~., data = dat)

check_assumption("base",base,dat)
check_assumption("model1",model1,dat)
check_assumption("sqrt",square_root,dat)
check_assumption("log",log_model,dat)
check_assumption("reciprocal",rec_y,dat)
check_assumption("log_reciprocal",log_recip_model,dat)
