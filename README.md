
# R Example Repository

## Barrier Hit Probability

This is an example R code which analyzes the impact of stop loss/ take profit on trading strategies. The approach taken here is to treat the problem as that of barrier hitting probabilities under geometric brownian motion. See <http://prodiptag.blogspot.com/2015/08/systematic-strategy-high-probability.html> for details.


## NIFTY Option Chain

This is an R program to download the current option chain trading on NSE NIFTY Index. This program accesses the data from a dynamic website, and uses the R Selenium package to download both the futures data as well as option chains. The resulting option chain table can be used for calibrating a suitable volatility model. For more on the later, see <https://github.com/prodipta/bsoption>

## Technical Pattern Back-testing

This example demonstrates a back-test of technical pattern performance for many different stock indices. This uses the R package techchart <https://github.com/prodipta/techchart> to define and extract technical pattern from time series data. See the discussions at <http://prodiptag.blogspot.com/2017/01/systematic-trading-back-testing.html> for further details.

## Wide and Deep in R

This is an example set-up for designing and training a wide-n-deep TensorFlow model in R (which is now a breeze thanks to the recently released Keras interface from R Studio). See the discussion for more details at <http://prodiptag.blogspot.com/2017/06/off-topic-wide-and-deep-learning-in-r.html>

## Auto-encoder

This is an set-up for extracting (time-series) momentum factors from a set of technical inputs using a 2 layer 3 latent dimension autoencoding based on the Keras/ TensorFlow framework. See the discussion for more details at <http://prodiptag.blogspot.co.uk/2017/10/systematic-trading-using-autoencoder.html>
