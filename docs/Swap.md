# Swaps

Swaps in commodity is different from fx or interest rate swaps. It is basically a strip of average forwards. 

The average is applied over each settlment periods, typically monthly, for a fixing frequency, typically daily/business days. 

The specs of common average swaps can be found on the ICE website under S2F ( swap to futures ). 

## Pricing 

Pricing of average swaps can be done by creating evaluation curves for the underlying price, ( either future nearby based, or publication 
based), and then put a theoretical value for each future fixing. Past fixings can be found from relevant sources. 

The net settlement amount for each period is then computed. Total amount is then used as swap market to market value. 

Day one pricing of swap from market maker is done similarly, where a fixed price or a spread over floating price is found to achieve zero or initial PL requirement. 

### Futures nearby

Futures are well documented on the futures exchanges. Futures nearby is a frequent underlying reference price of swaps. The 1st nearby referes to the current nearest to expire future. and the 2nd / 3rd etc nearby refer to the next to expire, and the following one etc. There can also be a roll adjust factor, which referes to alternative rules to decide on which nearby to take on the fixing date. One ICE, it is common to have a roll adjust of 1, which means that on the futures expiration date, the underlying future would be the next to expirate instead of the current expiring contract. 


## Discounting
It is market convention not to discount either cleared swap or futures. The reason is that these instrument has a daily settlement process, and has 0 mtm after the daily variation margin is settled. The actual process of settling the futures margin is however more complex ( total call in combination of initiaal and variation margin), and it is questionable whether there is a financing cost of these positions. For example, on LME, the forwards( futures) are actually discounted. 

For OTC market, market convention is often discount at more or less risk free rate. LIBOR curves were once popular, and later OIS and SOFA curves becomes more widely adopted. However, for a market maker to decide on the client unwind value, they need to consider their own financing cost, customer financing cost, changes of CVA etc. It is not purely a discounting issue. 
