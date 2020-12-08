<script>
MathJax = {
  tex: {
    inlineMath: [['$', '$'], ['\\(', '\\)']]
  },
  svg: {
    fontCache: 'global'
  }
};
</script>
<script type="text/javascript" id="MathJax-script" async
  src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js">
</script>

# Gabillon Model

## SDE
$$dF(t,T_i) = F(t,T_i)(e^{-k(T_i-t)} \sigma_s(t) d W_s(t) + (1- e^{-k(T_i-t)}) \sigma_l d W_l(t))$$

$$E(dW_s(t)dW_l(t)) = \rho dt$$
## Calibration

There are two part of model parameter fitting. Firstly, the structural parameters $k$, $rho$, $\sigma_l$ are chosen to be constants that reflect the desired level of mean reversion, curve intra pillar correlation, and long term vols. These can also be chosen according to observed market prices that are sensitive to them, e.g. swaptions, time spread options etc. 

In the second step, we choose to calibrate a piecewise constant time dependent $dW_s(t)$ to the market implied volatilities of listed options. These are pseudo-realtime and are always recalibrated on the fly when the market vol changes. 

The forward variance of $\ln F(t,T_i)$ is
$$\int_{t_m}^{t_n}[ e^{-2k(T_i-t)} \sigma_s^2(t) + ( 1 - e^{-k(T_i-t)})^2 \sigma_l^2 + 2 e^{-k(T_i-t)}( 1- e^{-k(T_i-t)}) \sigma_s(t) \sigma_l  \rho] dt$$

The integration results are:
$$\int_{t_m}^{t_n}e^{-2k(T_i-t)} \sigma_s^2(t) dt = \frac{\sigma^2_s(t_n)}{2k} \left( e^{-2k(T_i - t_n)} - e^{-2k(T_i - t_m)} \right )$$

$$\int_{t_m}^{t_n}( 1 - e^{-k(T_i-t)})^2 \sigma_l^2 dt = \sigma^2_l \left ( t_n - t_m -\frac{2}{k}\left(e^{-k(T_i-t_n)}-e^{-k(T_i-t_m)}\right) + \frac{1}{2k} \left( e^{-2k(T_i - t_n)} - e^{-2k(T_i - t_m)} \right) \right)$$

$$\int_{t_m}^{t_n} 2 e^{-k(T_i-t)}( 1- e^{-k(T_i-t)}) \sigma_s(t) \sigma_l  \rho]  dt = \rho \sigma_s(t_n)\sigma_l \left ( \frac{2}{k}\left(e^{-k(T_i-t_n)}-e^{-k(T_i-t_m)}\right) - \frac{1}{k} \left( e^{-2k(T_i - t_n)} - e^{-2k(T_i - t_m)} \right) \right)$$
