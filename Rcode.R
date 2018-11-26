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
  print(maker)
  dat$make[dat$make==string]=maker
}

#changing the string into every word apart from the first word.
for(string in dat$name){
  substring = strsplit(string, " ")[[1]]
  print(paste(substring[-1], collapse=' ' ))
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

#---Problem 4
dat$name=NULL

#---Problem 5
#changing categorical data to numerical value
i=1
for(string in unique(dat$make)){
  for(make in dat$make){
    if(make == string){
      paste("changing",make,as.character(i), sep = " ")
      dat$make[dat$make==string]= as.integer(i)
    }
  }
  i= i + 1
}
