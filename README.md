[![Coverage Status](https://coveralls.io/repos/dmenne/dlatex/badge.svg?branch=master&service=github)](https://coveralls.io/github/dmenne/dlatex?branch=master)

[![Coverage Status](https://coveralls.io/repos/github/dmenne/dlatex/badge.svg?branch=master)](https://coveralls.io/github/dmenne/dlatex?branch=master)

# Pretty-print lm, lme and glm tables

Generates a LaTex contrast table for `lme`, `glm` and `lm` models, with number of significant digits shown depending on standard deviation of the contrasts. Significant p-values can be shaded. Optionally suppresses p-values for `(Intercept)` because this is not a difference and tends to be grabbed as "nice p-value" by medical researches.

