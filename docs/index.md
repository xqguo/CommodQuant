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

* [Gabillon Model](https://xqguo.github.io/CommodQuant/Gabillon.md)
