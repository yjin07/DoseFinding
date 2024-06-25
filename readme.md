# Dose-Exposure-Response modeling

## 1. Exposure-Response model

Assuming that the median response $\mu$ as a function of exposure is represented by
$$\mu_c = f_m(c; \theta_m) $$
where $f_m$ denotes the exposure-response function, $c$ is the exposure, and $\theta_m$ is the vector of parameters of the exposure-response function.

> <u>ER functions</u>:
> +  **Emax model**: *monotone, concave expopsure-response shapes*
>    $$f(c, \theta) = E_0 + E_{\max}\frac{c}{EC_{50} + c}$$
>
>+ **Sigmoid Emax Model**: *adds a Hill coefficient $h$ to the Emax model to control the steepness of the curve at the EC50.*
>    $$f(c, \theta) = E_0 + E_{\max}\frac{c^h}{EC_{50}^h + c^h}$$
>
>+ **Exponential**: *captures a possible sub-linear or a convex shape*
>    $$f(c, \theta) = E_0 + E_1 (\exp(\frac{c}{\delta}) + 1)$$
>
>+ **Beta**: *captures non-monotone exposure-response relationships and is more flexible than the quadratic model*. <mark>$scal$ is not estimated</mark>
>    $$f(c, \theta) = E_0 + E_{\max}B(\delta_1, \delta_2)(c/scal)^{\delta_1}(1-c/scal)^{\delta_2}$$
>    where $B(\delta_1,\delta_2) = (\delta_1 + \delta_2)^{\delta_1+\delta_2}/(\delta_1^{\delta_1}\delta_2^{\delta_2})$
>
>+ **Linear**
>    $$f(c, \theta) = E_0 + \delta c$$
>
>+ **Linear in log**: <mark>$off$ is not estimated</mark>
>    $$f(c, \theta) = E_0 + \delta\log(c + off)$$
>
>
>+ **Logistic**: *captures general monotone, sigmoid exposure-response relationships*
>    $$f(c, \theta) = E_0 + E_{\max} / \{1 + \exp[(EC_{50} - c) / \delta]\}$$
>
>+ **Quadratic**: *captures a possible non-monotonic exposure-response relationship*
>    $$f(c, \theta) = E_0 + \beta_1 c + \beta_2 c^2$$


For the continous-response case, the response is assume to follow a lognormal distribution conditional on the exposure as follows:
$$log(Y) \sim N(\log(\mu_c), \sigma^2)$$

For the binary-response case, 


## 2. Dose-Exposure **model**