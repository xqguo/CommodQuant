# Asian Option

In commodities, Asian option is often more suitable to hedge the underlying pricing risk on the OTC market. 
In many cases, the option is a more complex version of Asian spread basket.

For example, a oil refinary has a monthly production where the crude oil physical is linked to the average price of the month of dated brent, 
published daily by Platts, where the output is a basket of gasoline, diesel priced also using the average price of platts fixings.  

## Pricing method

Unlike the bullet option which uses the Black-Scholes formula as benchmark. Asian options ( and with the basket and spread version ) does not 
have a globally accepted pricing formula. [Choi's 2018 method](https://github.com/PyFE/SumBSM-R) turns out to be one generic pricing method that could potentially become the reference
in this space. I have reimplemented his method in a FSharp code in this project. 

A few extensions was made to take care of the perfect correlation case with cause the cholesky decomposition to fail. I used the moment matching to avoid this case. 

When the expiration date is before all the underlying fixings, this option is called swaption. 

In LNG market, there could be cases where that some of the fixings are before and others are after the option expiration. These cases become something
like a generic Asian-Swaption-Spread-basket option.

I uses the [Gabillon Model](Gabillon.html) to model the underlying asset price curves. The 2 factor model allows for features like earlier vol discounts and decaying correlation and provide a reasonable dynamics needed for most commodity options.   
