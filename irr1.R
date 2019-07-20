# This function reads in data from the user
# Also sets initial 'guess' for the IRR to the input coupon rate.
read<-function(){
  r_c<<-as.numeric(readline(prompt="Enter the coupon rate: "))
  N<<-as.numeric(readline(prompt="Enter the number of payments: "))
  f<<-as.numeric(readline(prompt="Enter the face value: "))
  V0<<-as.numeric(readline(prompt="Enter the initial payment amount: ")) 
  r<<-r_c
  d<<-(1/(1+r))
  IRR1(r_c, N, f, V0)
}
# This function represents the stream of payments for a bond.
NPV<-function(r_c, N, f, V0){
  total<-(-V0/f)+(d^N)
  for(i in 1:N) {
    total<-total+(r_c*(d^i))
  }
  return(total)
}
# Function for the derivative of NPV
NPV1<-function(r_c, N, f, V0){
  total1<-(N*(d^(N-1)))
  for(i in 1:N){
    total1<-total1+(r_c*(i*(d^(i-1))))
  }
  return(total1)
}
# This function updates d, using Newton's Method, until the result of the NPV
# function is very close to 0 (the IRR has been found).
# The IRR is then returned.
IRR1<-function(r_c, N, f, V0){
  while(NPV(r_c, N, f, V0) < -.0000001 || NPV(r_c, N, f, V0) > 0.0000001){
    d<<-(d-(NPV(r_c, N, f, V0)/NPV1(r_c, N, f, V0)))
  } 
  return((1/d)-1)
}