#NIFTY 50 CE, Expiry- 28/03/2019, Strike-10500, NIFTY Price-10912
#Market premium for the option- 559.05, Data date- 04/02/2019
#--------------------------Shout Option------------------------------


#Variable declaration for Shout Option
So = 10912
K= 10500
sig = 0.15
rf= 0.065
T= 53/365 #Time to maturity
n = 100
nsim = 500
u =rf
delT = T/n
q=0       #Dividend yield assumption
sum=0

#Enter the day on which you want to shout
shout_day = readline(prompt="When do you want to shout(in days since contract initiation): ")
shout_t = as.integer(shout_day)/365

# Constants used in the loop
u1= (u-(sig^2)/2)*delT        
sig1=sig*((delT)^0.5) 
sum=0
for(sim in 1:nsim) #simulation loop
{
  price = So
  for(i in 1:n)   # A simulation loop in each time stamp delta T
      { 
    
# Check the shout day and store that day price into variable shout price

if((i*delT<=shout_t) && ((i+1)*delT>shout_t))
{Shout_price = price}
      price = price*exp(u1+sig1*rnorm(1,0,1))
      }
  
#Choose the price which is greater between Shout day price and price at the end
#Payoff Calculation
  
  if(price>Shout_price)
    payoff = max((price-K),0)*exp(-rf*T)
  else
    payoff = max((Shout_price-K),0)*exp(-rf*T)
  
  sum = sum+ payoff
  
}
Average_payoff = sum/nsim
print(Average_payoff)