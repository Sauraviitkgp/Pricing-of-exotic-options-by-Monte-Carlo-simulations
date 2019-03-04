#NIFTY 50 CE, Expiry- 28/03/2019, Strike-10500, NIFTY Price-10912
#Market premium for the option- 559.05, Data date- 04/02/2019
#--------------------------Asian Option------------------------------


T= 53/365 #Time to maturity
S= 10912  #Current Nifty price
K= 10500  #Strike price
rf= 0.065
sig = 0.15
q=0       #Dividend yield assumption
n=100
delt=T/n
nsim=500  #Number of Simulations
#Variable declaration for Asian Option
sum_AE=0

# Payoff for Asian Options
for (i in 1:nsim) {
price=S
price_sum=price
    for(j in 1:n)
    {
    z=rnorm(1)
    price=price * exp((rf-((sig^2)/2))*delt + z*sig*sqrt(delt))
    price_sum=price + price_sum
    }
avg_price= price_sum/n #Average price at which option would be exercised
payoff_AE=max((avg_price-K),0)*exp(-rf*T)  
sum_AE= sum_AE + payoff_AE
}

avg_payoff_AE=sum_AE/nsim #Payoff
print(avg_payoff_AE)