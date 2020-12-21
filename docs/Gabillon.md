<script>
MathJax = {
  tex: {
    inlineMath: [['$', '$'], ['\\(', '\\)']],
    displayMath: [ ['$$', '$$'], ['\\[', '\\]']
    ],
  },
  svg: {
    fontCache: 'global' 
  },
  jax: ["input/TeX","output/CommonHTML"]
};
</script>
<script type="text/javascript" id="MathJax-script" async
  src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js">
</script>

# Gabillon Model

## SDE

$$dF(t,T_i) = F(t,T_i)\left(e^{-k(T_i-t)} \sigma_s(t,T_i) d W_s(t) + \left(1- e^{-k(T_i-t)}\right) \sigma_l d W_l(t)\right)$$

$$E(dW_s(t)dW_l(t)) = \rho dt$$

## Calibration

There are two part of model parameter fitting. Firstly, the structural parameters $k$, $rho$, $\sigma_l$ are chosen to be constants that reflect the desired level of mean reversion, curve intra pillar correlation, and long term vols. These can also be chosen according to observed market prices that are sensitive to them, e.g. swaptions, time spread options etc. 

In the second step, we choose to calibrate a piecewise constant time dependent $\sigma_s(t)$ to the market implied volatilities of listed options. These are pseudo-realtime and are always recalibrated on the fly when the market vol changes. This is often used for oil contracts. 

Alternatively, we can also fit a constant $\sigma(t,T_i)$ in time dimension, and a different one per futures maturity. This is often more suitable for gas contracts. 

The forward covariance of two futures with different maturity $F(t,T_i)$, $F(t,T_j)$ is

$$\int_{t_m}^{t_n}\left[
\begin{aligned} e^{-k(T_i+T_j-2t )} \sigma_s(t,T_i) \sigma_s(t,T_j) \\
+\left( 1 - e^{-k(T_i-t)}\right) \left( 1 - e^{-k(T_j-t)}\right)\sigma_l^2 \\
+e^{-k(T_i-t)}\left( 1- e^{-k(T_j-t)}\right)\sigma_s(t,T_i)\sigma_l\rho \\ 
+e^{-k(T_j-t)}\left( 1- e^{-k(T_i-t)}\right)\sigma_s(t,T_j) \sigma_l\rho\\
\end{aligned}
\right] dt$$

The integration results are:

$$\begin{aligned}
&\int_{t_m}^{t_n}e^{-k(T_i+T_j-2t)} \sigma_s(t,T_i)\sigma_s(t,T_j) dt \\
=& \frac{\sigma_s(t_n,T_i)\sigma_s(t_n,T_j)}{2k} \left( e^{-k(T_i + T_j - 2t_n)} - e^{-k(T_i + T_j - 2t_m)} \right ) \\
&\int_{t_m}^{t_n}\left( 1 - e^{-k(T_i-t)}\right)\left( 1 - e^{-k(T_j-t)}\right) \sigma_l^2 dt \\
=& \sigma^2_l \left ( t_n - t_m -\frac{1}{k}\left(e^{-k(T_i-t_n)}-e^{-k(T_i-t_m)} + e^{-k(T_j-t_n)}-e^{-k(T_j-t_m)}\right) + \frac{1}{2k} \left( e^{-k(T_i + T_j - 2t_n)} - e^{-k(T_i + T_j - 2t_m)} \right) \right) \\
&\int_{t_m}^{t_n} e^{-k(T_i-t)}\left( 1- e^{-k(T_j-t)}\right)\sigma_s(t,T_i) \sigma_l  \rho  dt \\
=& \rho  \sigma_s(t_n, T_i) \sigma_l \left ( \frac{1}{k}\left(e^{-k(T_i-t_n)}-e^{-k(T_i-t_m)}\right)- \frac{1}{2k} \left( e^{-k(T_i + T_j - 2t_n)} - e^{-k(T_i + T_j - 2t_m)} \right) \right) \\
&\int_{t_m}^{t_n} e^{-k(T_j-t)}\left( 1- e^{-k(T_i-t)}\right)\sigma_s(t,T_j) \sigma_l  \rho  dt \\
=& \rho  \sigma_s(t_n, T_j) \sigma_l \left ( \frac{1}{k}\left(e^{-k(T_j-t_n)}-e^{-k(T_j-t_m)}\right)- \frac{1}{2k} \left( e^{-k(T_j + T_i - 2t_n)} - e^{-k(T_j + T_i - 2t_m)} \right) \right)
\end{aligned}$$

When $i = j$, the forward variance of $F(t,T_i)$ is

$$\int_{t_m}^{t_n}\left[ e^{-2k(T_i-t)} \sigma_s^2(t,T_i) + \left( 1 - e^{-k(T_i-t)}\right)^2 \sigma_l^2 + 2 e^{-k(T_i-t)}\left( 1- e^{-k(T_i-t)}\right) \sigma_s(t,T_i) \sigma_l  \rho \right] dt$$

The integration results are:

$$\int_{t_m}^{t_n}e^{-2k(T_i-t)} \sigma_s^2(t, T_i) dt 
= \frac{\sigma^2_s(t_n, T_i )}{2k} \left( e^{-2k(T_i - t_n)} - e^{-2k(T_i - t_m)} \right )$$

$$\int_{t_m}^{t_n}\left( 1 - e^{-k(T_i-t)}\right)^2 \sigma_l^2 dt 
= \sigma^2_l \left ( t_n - t_m -\frac{2}{k}\left(e^{-k(T_i-t_n)}-e^{-k(T_i-t_m)}\right) + \frac{1}{2k} \left( e^{-2k(T_i - t_n)} - e^{-2k(T_i - t_m)} \right) \right)$$

$$\int_{t_m}^{t_n} 2 e^{-k(T_i-t)}\left( 1- e^{-k(T_i-t)}\right) \sigma_s(t, T_i ) \sigma_l  \rho  dt 
= \rho \sigma_s( t_n, T_i ) \sigma_l \left ( \frac{2}{k} \left( e^{-k(T_i - t_n)} - e^{-k(T_i - t_m)} \right) - \frac{1}{k} \left( e^{-2k(T_i - t_n)} - e^{-2k(T_i - t_m)} \right) \right)$$
