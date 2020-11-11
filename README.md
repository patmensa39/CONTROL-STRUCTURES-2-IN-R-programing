# CONTROL-STRUCTURES-2-IN-R-programing
Factorial functions, Fibonacci sequence, Taylor series and gamma function in R.
---
title: "Functions"
output:
  pdf_document: default
  html_document: default
  word_document: default
date: "November 9th, 2020"
theme: cerulean
---


## Name: Mensah Patrick

```{r}
### 1. Write a function to evaluate a^b given a and b. The function should have a default value of 3 for b.
patrick.power <- function(a, b= 3){
return (a^b);
}
### Example
patrick.power(4,3)
patrick.power(10)
patrick.power(2,.5)
```


```{r}
###. 2. Write a function that accepts two arguments, an integer and a vector, and returns TRUE if the integer is inside the vector. (Note: here we are using the mathematical meaning of “integer”. You do not have to use the integer type in R.)
my.function <- function(vec,num){
return (num %in% vec);
}
### Example
vec=c(1:20)
my.function(vec,5)
my.function(vec,32)

```


```{r}
### 3. Writing a function that, given an integer and a vector, will return the number of times the integer occurs in the vector.
my.function1 <- function(vec,num){
return (sum(vec==num));
}
### Examples
vect=rep(c(1,6,3), c(30,22,17))
my.function1(vect,1)
my.function1(vect,6)
my.function1(vect,12)
```


```{r}
### 4. Using the mtcars dataset for this question. Use one of the apply functions to find the mean of every column in mtcars.
data(mtcars)
apply(mtcars,2,mean)
```


```{r}
### 5. The Fibonacci sequence of numbers, Fn, n = 0, 1, ... is defined as follows:
###  F0 = 0, F1 = 1
###  Fn = Fn−1 + Fn−2 n = 2, 3, . . .
### Write a function in R that takes as input a non-negative integer n and returns Fn. Use a forloop in the function. Then use the function to find F20 and F100.

fibonacci <- function(n){
  if(n==0){
    return(0)
  }
  if(n==1){
    return(1)
  }
  first = 0
  second = 1
 for (i in 2:n){
     next_num = first+second
     first = second
     second = next_num
}
 return(next_num)
 }
print(fibonacci(20))
print(fibonacci(100))

```


```{r}
### 6. 

sumexp<-function(n,x){
sum=0
i=0
while(i<=n){
    sum=sum+(x^i)/factorial(i)
    i=i+1
}
remainder = exp(x)-sum
vec= c(sum,remainder)
return(vec) ##value and error
}

sumexp(10,3)
sumexp(3,4)


```


```{r}
### 7. A recursive function is a function that calls itself during execution. For example, n! can be defined recursively as
### n! = n × (n − 1)!
###Write two R functions to compute the factorial of any user-supplied positive integer n. The first function should calculate (and return) n! using for loop. The second function calculates and returns n! recursively.

###a. Recursive Function

patrick.factorial <- function(num) {
  factorial = 1
 
  if(number >= 0) {
    if (number == 0){
      return (1)
    }
    else {
      return (number * patrick.factorial(number-1))
    }
  }
}
patrick.factorial(3);

### b. For Loop 
patloop.factorial <- function(num) {
  factorial = 1

  if(num == 0) {
   return(0);
  } else {
    for(i in 1:num) {
      factorial = factorial * i
    }
    return(factorial);
  }
}
patloop.factorial(3)

```



```{r}
### 8. Write a function that accepts a vector of numbers (of length >= 3) and returns a vector of moving averages, i.e. with x = (x1, . . . , xn), the output is a vector with the values
### (x1 + x2 + x3)/3 , (x2 + x3 + x4)/3,... (xn−2, xn−1, xn)/3

input <- scan()                  
magic.numbers <-c()
for(i in 1:length(input)-2){
    temp=(input[i]+input[i+1]+input[i+2])/3
    magic.numbers[i]=temp
    }
magic.numbers
```


```{r}
### 9.
num<-function(x){
  if (x<0) {
    y=x^2+2*x+3
  } else if(x>=0 && x<2){
    y=x+3
  }else {
    y=x^2+4*x-7
  }
  return(y)
}
vec = list(-2:3)
plot(unlist(vec), unlist(lapply(vec,num)), type = 'l')

```


```{r}
### 10. 
make.NegLogLik <- function(data, fixed = c(FALSE, FALSE)) {
         params <- fixed
         function(p) {
                 params[!fixed] <- p
                 a<- params[1]
                 b<- params[2]
                 
                 ## Calculate the gamma distribution 
                 average=a*b
                 std.dv=sqrt(a*b^2)
                 range=seq(0,average + 5*std.dv,0.01)
                 y=dgamma(range, a, rate = 1/b)
                 plot(range, y, type='l', ylim=c(y)+0.01)
        } 
}
set.seed(20000)
mydata<-rgamma(200, shape = 10, scale = 2)
```
