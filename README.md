# riskCommunicator
An R package for estimating risk differences and relative risk measures.

The `riskCommunicator` package facilitates the estimation of common epidemiological effect measures that are relevant to public health, but that are often not trivial to obtain from common regression models, like logistic regression. In particular, `riskCommunicator` estimates risk and rate differences, in addition to risk and rate ratios. The package estimates these effects using g-computation with the appropriate parametric model depending on the outcome (logistic regression for binary outcomes, Poisson regression for rate or count outcomes, and linear regression for continuous outcomes). Therefore, the package can handle binary, rate, count, and continuous outcomes and allows for dichotomous, categorical (>2 categories), or continuous exposure variables. Additional features include estimation of effects stratified by subgroup and adjustment of standard errors for clustering. Confidence intervals are constructed by bootstrap at the individual or cluster level, as appropriate. 

This package operationalizes g-computation, which has not been widely adopted due to computational complexity, in an easy-to-use implementation tool to increase the reporting of more interpretable epidemiological results. To make the package accessible to a broad range of health researchers, our goal was to design a function that was as straightforward as the standard logistic regression functions in R (e.g. glm) and that would require little to no expertise in causal inference methods or advanced coding.


# Installation
The `riskCommunicator` R package is available as a source package through GitHub. Installation requires the ability to compile R packages. This means that R and the R tool-chain must be installed, which requires the Xcode command-line tools on Mac and Rtools on Windows.

The easiest source installation method uses the devtools package:

```
library(devtools)
devtools::install_github("jgrembi/riskCommunicator")
```

# Other Resources

Bugs and difficulties in using `riskCommunicator` are welcome on the issue tracker.

Planned feature improvements are also publicly catalogued on the "Issues" page for `riskCommunicator`: https://github.com/jgrembi/riskCommunicator/issues
