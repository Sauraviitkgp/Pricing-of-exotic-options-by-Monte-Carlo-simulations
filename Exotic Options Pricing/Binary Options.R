#NIFTY 50 CE, Expiry- 28/03/2019, Strike-10500, NIFTY Price-10912
#Market premium for the option- 559.05, Data date- 04/02/2019
#--------------------------Binary Option------------------------------

T= 53/365 #Time to maturity
S= 10912  #Current Nifty price
K= 11500  #Strike price
rf= 0.065
sig = 0.15
q=0       #Dividend yield assumption
n=100
delt=T/n
nsim=500  #Number of Simulations
sum_binary=0

#Variable declaration for Binary Option
p=readline(prompt = "Enter the binary payoff:")
p=as.integer(p) #Predecided payoff when the option is in the money
#If out of the money, payoff is 0


# Payoff for Binary Options
for (i in 1:nsim) {
  price=S
  for(j in 1:n)
  {
    z=rnorm(1)
    price=price * exp((rf-((sig^2)/2))*delt + z*sig*sqrt(delt))
  }
  if(price > K)
  {
    payoff_binary= p
  }
  else{payoff_binary= 0}
  sum_binary = sum_binary + payoff_binary
}

avg_payoff_binary=sum_binary/nsim #Payoff
print (avg_payoff_binary)