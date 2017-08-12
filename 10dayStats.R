# d1--quantile

# Enter your code here. Read input from STDIN. Print output to STDOUT
con<-file('stdin','r')

stringTovector<-function(a) {
  return (as.numeric(unlist(strsplit(a," "))))
}

dataline<-readLines(con)[[2]]

v<-stringTovector(dataline)

get_q<- function(x) {
  x<-sort(x)
  n<-length(x)
  m<-(n+1)/2
  if (n%%2 ==1) {
    l<-(m-1);u<-(m+1)
  } else {
    l<-floor(m);u<-ceiling(m)
  }
  result<-c(median(x[1:l]),median(x),median(x[u:n]))
  return (result)
}

x<-get_q(v)
cat(x,sep='\n')

# d1--Interquantile
# Enter your code here. Read input from STDIN. Print output to STDOUT

con<-file('stdin','r')

stringTovector<-function(a) {
  return (as.numeric(unlist(strsplit(a," "))))
}
input<-readLines(con)
n<-stringTovector(input[[2]])
f<-stringTovector(input[[3]])

total<-c()
for (i in 1:length(n)) {
  total<-c(total,rep(n[i],f[i]))
}


range<- function(data) {
  data<-sort(data)
  n<-length(data)
  m<-(n+1)/2
  if (n%%2 ==1) {
    l<-(m-1);u<-(m+1)
  } else {
    l<-floor(m);u<-ceiling(m)
  }
  result<-c(median(data[u:n])-median(data[1:l]))
  return (result)
}

cat(format(round(range(total), 1), nsmall=1))

#std
# Enter your code here. Read input from STDIN. Print output to STDOUT

con<-file('stdin','r')
inputs<-readLines(con)

string<-inputs[[2]]
number<-as.numeric(unlist(strsplit(string," ")))
s<-c()
std<- function (n) {
  m=mean(n)
  
  for (i in 1:length(n)) {
    s<-c(s,(sum(n[i]-m))^2)
  }
  return (sqrt(sum(s)/length(s)))
}

cat(format(round(std(number),1),nsmall=1))

#operator
input <- file('stdin','r')
mealcost <- as.double(readLines(input,n=1))
tipPercent <- as.double(readLines(input,n=1))
taxPercent <- as.double(readLines(input,n=1))
tip <- as.double((mealcost*tipPercent)/100)
tax <- as.double((mealcost*taxPercent)/100)
totalcost <- round(mealcost + tip + tax)

cat("The total meal cost is",totalcost ,"dollars.")

#conditional statements

# Enter your code here. Read input from STDIN. Print output to STDOUT

con<-file('stdin','r')
input<-as.numeric(readLines(con)[[1]][1])

print<- function (x) {
  if (x<6 || x>20) {
    ifelse (x%%2==1,"Weird","Not Weird")
  } else { return ("Weird")}
}

cat(print(input))