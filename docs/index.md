---
katex: True
---

# Project outline

The primary objective of this project is to provide an open-source reference for quantitative functions related to commodities. It includes a commodity pricing library [CommodQuant](https://github.com/xqguo/CommodQuant) written in F# ( which can be used in any dotnet developments ), as well as some guides to some related learning materials around technology and quantitative finance. Also with its azure DevOps pipeline [project](https://dev.azure.com/guoxiaoq/CommodQuant)

## Programming setup

* [CheatSheet](https://xqguo.github.io/CommodQuant/CheatSheet.html) for some usage reference cmds.

* Learn FSharp basics with free online resources.

  * [F# for fun and profit](https://fsharpforfunandprofit.com): a very good site to learn F#.

  * [My Azure notebook guide](https://xqguo.github.io/CommodQuant/aznotebook.html)

* Get your own dev environment

  * Install .net core SDK and a IDE or editor

    * VS Community for ease of use and windows environment

    * or Install VSCode for cross-platform

    * Vim + VimSharp plugin for cross-platform use minimal resources

* Contribute to open source project

  * Here is the [documentation](https://docs.microsoft.com/en-us/azure/devops/pipelines/?view=azure-devops) to azure devops pipeline  

  * here is the [azure pipeline yaml source code](https://github.com/microsoft/azure-pipelines-yaml/)

## Understand the markets

* [Intro to Money market](https://docs.google.com/presentation/d/e/2PACX-1vSBtq-1KcZtVHhFnpL0sCLaqKtg5m2FpPKly7bN6X6hPmg5T-Blxo3xD6PTeBFmQt1TJDlJ5x9pZXF0/pub?start=false&loop=false&delayms=3000)

* CME

* ICE

  * [Brent future](https://www.theice.com/products/219/Brent-Crude-Futures) is a benchmark price for global oil.

  * [Brent Futures Option](https://www.theice.com/products/218/Brent-Crude-American-style-Option) is the option on the brent future.  

* LME

* Fed

  * The Fed publishes selected rates, including the fed funds rate [here](https://www.federalreserve.gov/releases/h15/)

## Understand the Quantitative finance

* Notes for [Intro to Derivative Pricing](https://xqguo.github.io/CommodQuant/intro.pdf) and [source](https://github.com/xqguo/CommodQuant/tree/master/docs/intro.tex)

* Notes to [Practical Guide](https://docs.google.com/presentation/d/153lNoOKmLwy0k7gJmCkcio5nt2ejdLZ5dONjbfFM4f4/edit?usp=sharing), slides for Master of Quantitative Finance lectures. 

* Pricing and market instruments 
 
   * [Swap price](https://xqguo.github.io/CommodQuant/Swap.html)

   * [Asian option](https://xqguo.github.io/CommodQuant/Asian.html)

* [Risk](https://xqguo.github.io/CommodQuant/Risk.html)

* [Gabillon Model](https://xqguo.github.io/CommodQuant/Gabillon.html)

* Conversion Factors

Commodity prices are quoted in currency per unit. There are often a few different commonly used units and there is a need to convert between them. The convertion factors are typical based on the physical nature, but the for pricing and quoting purpose, actual market convention is used instead of actual physical conversion. For example, when converting crude oil from barrel to metric ton, 7.33 bbl/mt is used regardless of the actual grade density.

There is a CME conversion calculator [here](https://www.cmegroup.com/tools-information/calc_crude.html).

For gas and LNG, million british thermal unit (mmbtu) is the most common unit. Billion cubic meter (bcm) or billion cubic feet (bcf) is often used to measure global supply demand. Annual contract amount is often measured in million tons per annum (mtpa). Shipping of cargo is measured in cubic meters. 

1 mt = 48.7 bcf = 1.379 bcm = 55.57 mmbtu  
1 bcf (Gas) = 45000 $m^3$ (LNG)  
600 $m^3$ (Gas ) = 1 $m^3$ (LNG)    
1 mmbtu = 10 therms   
1 $m^3$ = 35.315 cf  
1 mmbtu = 0.293071 mwh  
