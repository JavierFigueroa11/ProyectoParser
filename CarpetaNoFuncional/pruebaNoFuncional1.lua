function factorial(n){
   if n == 0 then
      return 1 end
   return n * factorial(n - 1)
end
print(factorial(5))