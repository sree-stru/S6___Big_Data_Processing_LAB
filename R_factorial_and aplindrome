num <- as.integer(readline(prompt="Enter the number"))
fact <- 1
if (num < 0)
{
  cat("no factorial for negative numbers")
}
else
{
  for (i in 1:num)
  {
    fact <- fact * i
  }
  cat(fact, "is the factorial of ", num)
}

temp <- num
rev <- 0
while(temp>0)
{
  rem <- temp %% 10
  rev <- rev * 10 + rem
  temp <- temp %/% 10
}
if (num == rev)
{
  cat(num ," is a palindrome")
}
else
{
  cat(num ,"is not a palindrome")
}
