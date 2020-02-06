# Swaps

Swaps in commodity is different from fx or interest rate swaps. It is basically a strip of average forwards. 

The average is applied over each settlment periods, typically monthly, for a fixing frequency, typically daily/business days. 

The specs of common average swaps can be found on the ICE website under S2F ( swap to futures ). 

## Pricing 

Pricing of average swaps can be done by creating evaluation curves for the underlying price, ( either future nearby based, or publication 
based), and then put a theoretical value for each future fixing. Past fixings can be found from relevant sources. 

The net settlement amount for each period is then computed. Total amount is then used as swap market to market value. 

Day one pricing of swap from market maker is done similarly, where a fixed price or a spread over floating price is found to achieve zero or initial PL requirement. 
