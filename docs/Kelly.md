<script type="text/javascript" async
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML">
</script>

# [Kelly's criterion](https://en.wikipedia.org/wiki/Kelly_criterion)

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

This ratio gives you the fastest expected growth rate for the same number of bets. The more the expected winning, the more you better; the more unlikely the winning, the less you bet; if it is a sure win, bet all; etc etc ....

This method is in sharp contract with modern porfolio management method, which relies more on [CAPM](https://en.wikipedia.org/wiki/Capital_asset_pricing_model), beta, alpha etc. 
