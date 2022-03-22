## This script prints out the first n prime numbers
# The script is optimised for the number of trials before identifying a 
# number as prime
# Script by Arman Hassanniakalager GitHub @hkalager
now0=Sys.time()
n = 50

fun1 <- function(n)
{
  n <- as.integer(n)
  if (n<1)
  {
    stop(errorCondition("Wrong choice of n"))
  }
  
  list_prime <- list()
  v=1
  while (length(list_prime)<n) 
  {
    v <- v+1
    vv=1
    prev_divider=NA
    cap_vv=v
    while (is.na(prev_divider) && vv<cap_vv) 
    {
      vv=vv+1
      if (length(list_prime)>0)
      {
        cap_vv <- as.integer(v/2)
      }
      
      if (v%%vv==0)
      {
        prev_divider=TRUE
        vv=vv-1
      } 
    }
    if (vv>=cap_vv || v==2)
    {
      list_prime=append(list_prime,v)
    }
    
  }
  return (list_prime)
}

prime_list=fun1(n)

print(paste("The list of ",toString(n)," first prime numbers is in variable 'prime_list'"))
now1=Sys.time()
View(prime_list)
dt=now1-now0
print(dt)

