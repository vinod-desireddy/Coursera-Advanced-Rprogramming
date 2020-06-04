#Part 1: Factorial Function
# The objective of Part 1 is to write a function that computes the factorial of an 
#       integer greater than or equal to 0. 

# For this Part you will need to write four different versions of the Factorial function:
# Factorial_loop: a version that computes the factorial of an integer using looping
# Factorial_reduce: a version that computes the factorial using the 
#     reduce() function in the purrr package. Alternatively, you 
#     can use the Reduce() function in the base package.
# Factorial_func: a version that uses recursion to compute the factorial.
# Factorial_mem: a version that uses memoization to compute the factorial.

# After writing your four versions of the Factorial function, 
#       use the microbenchmark package to time the operation of 
#       these functions and provide a summary of their performance. 
#       In addition to timing your functions for specific inputs, 
#       make sure to show a range of inputs in order to demonstrate 
#       the timing of each function for larger inputs.

library(purrr)
##1.Factorial_loop:
factorial_loop = function(n = 10){
      f = 1
      if(n < 0){
            stop('number shoud be a whole number')
      }
      else if(n == 0){
            f = 1
      } else{
      for(i in 1:n){
          f = f*i  
      }}
      paste('factorial', f, sep = '=')
}

#factorial_loop(100)

##2.Factorial_reduce:
factorial_reduce = function(n = 10){
      if(n < 0){
            stop('number shoud be a whole number')
      }
      else if(n == 0){
            f = 1
      } else{
         f = reduce(as.numeric(1:n), `*`)
            }
      paste('factorial', f, sep = '=')
}

#factorial_reduce(100)

##3.Factorial_func: a version that uses recursion to compute the factorial.
factorial_func = function(n = 10){
      if(n < 0){
            stop('number shoud be a whole number')
      }
      else if(n <= 1){
            return(1)
      } else {
            return(n * factorial_func(n-1))
      }
}

#factorial_func(100)

##4.Factorial_mem: a version that uses memoization to compute the factorial.
#creating a vector called mem to store the factorial values, which can be referenced later.
mem = c(1, rep(NA, 99))
factorial_mem = function(n = 10){
      if(n < 0){
            stop('number shoud be a whole number')
      }
      else if(n <= 1){
            return(1)
      } else {
            if(!is.na(mem[n])){
                  return(mem[n])
            } else {
                  mem[n] <<- n * factorial_mem(n-1)
                  return(mem[n])
            }
      }
}

#factorial_mem(12)

#creating a function bench, which uses a function called microbenchmark,
#     to calculate the time of operation and performance summary of above 4 functions.
library(microbenchmark)
bench = function(n){
      microbenchmark(factorial_loop(n),
               factorial_reduce(n),
               factorial_func(n),
               factorial_mem(n))
}

#checking and comparing the time taken by the above functions for different inputs.
bench(20)
bench(100)
bench(1000)
bench(2000)