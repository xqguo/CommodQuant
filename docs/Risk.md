<script type="text/javascript" async
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML">
</script>

# Risk

Risk rewards are important decisions to make. There are different approaches and each with their own merit. 

## [Kelly's criterion](https://en.wikipedia.org/wiki/Kelly_criterion)

Investment and gambling share a lot of similarities. Kelly's ratio is a good measure
for one to deside how much to invest based on the risk and return charateristics. More details can be 
found via the link in the title to wikipedia. Basic idea is that you bet a fraction of your wealth equal to the
expected winning over net odds if you win.

  $$f = p - \frac{q}{b}$$

where:

$$f^{*}$$ is the fraction of the current bankroll to wager; (i.e. how much to bet, expressed in fraction)

$$b$$ is the net odds received on the wager; (e.g. betting $ 10, on win, rewards $ 14 (plus wager); then $$b=0.4$$

$$p$$ is the probability of win;

$$q$$ is the probability of lose. (which equates $$1-p$$)

This ratio gives you the fastest expected growth rate for the same number of bets. The more the expected winning, the more you better; the more unlikely the winning, the less you bet; if it is a sure win, bet all; if the expected win is negative, don't bet; etc etc ....

This method is in sharp contract with modern porfolio management method, which relies more on [CAPM](https://en.wikipedia.org/wiki/Capital_asset_pricing_model), beta, alpha etc. 

## Delta hedging

Delta hedging of options, or self-financed replication of payoff in general, are examples of almost-sure-win(loss) payoffs. For a market maker can charge enough for the hedging costs, according to Kelly's criterial, they are willing to bet all-in.
