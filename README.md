# Lambdabox

__This library is heavily under construction, including the readme which is now more of a design document!__

_Lambdabox provides a generic embedded domain specific language (EDSL) in Haskell,
for algorithmic trading (also known as blackbox trading) of cryptocurrencies on
multiple exchanges._

## Feature overview

Lambabox provides a large range of features that can be used under a common EDSL for multiple
exchanges:

* Trading of cryptocurrencies
  * Trade creation (all kind of trades including Iceberging of trades)
  * Trade querying (last, all and open trades)
  * Trade cancellation
  * Trade history
* Market data
  * High level streams such as an asynchronous local `OrderBook`
  * Low level market data streams
  * Querying historical market data
* Statistical market data
  * Candlestick charting data
* Account management
* Exchange information

When required, the user can also leverage the individual low level API clients of
the underlying exchanges.

## Supported Exchanges

Table of the supported exchanges and what kind of features.

## EDSL

Lambdabox provides an EDSL that leverages Haskells strong typesystem providing that each action is checked for
correctness at compile time. For example, when creating a trade in Lambdabox, Haskell will check on
compile time wether an exchange actually provides a certain trading pair. Algorithmic trading systems can manage large amount of capital and this kind of safety is crucial in critical systems as such. For more information on the static
EDSL see the documentation.

An example of creating a simple trade:

```Haskell
import Lambdabox.Static.Currency
import Lambdabox.Static.Exchange
import Lambdabox.Static.Trade

main :: IO ()
main = let symbol = buy ADA/BTC `on` Binance
           order  = LimitOrder GoodTilCanceled 20000 0.13
       in $ do
       response <- trade symbol order
       print response
```

An example of the creating a trade based on a local orderbook with the static EDSL:

```Haskell
import Lambdabox.Static.Currency
import Lambdabox.Static.Exchange
import Lambdabox.Static.Trade

marketOrderAdaBtc :: Double -> Box TradeResponse
marketOrderAdaBtc = trade (ADA `for` BTC `on` Binance) . MarketOrder

-- | Buy 2000 ADA if the price is lower than 200 Satoshi via local order book
main :: IO ()
main = runBox configuration $ do
    ob <- orderBook ADA BTC
    r  <- register (marketOrderAdaBtc 2000) (< 200) ob
    print r
```

## Documentation

* For a full guide on using Lambdabox see [read the docs](link-to-read-the-docs).
* Lambda box on [Hackage](link-to-hackage).
* [API documentation](link-to-api-docs).

### Blogposts

The following lists is a collection of blogposts written about Lambdabox:

* Why use Haskell for Lambdabox? - [Blocknition.com](link-to-blocknition-blogpost)
* Creating your first algoryhtmic trade with Lambdabox and Haskell - [Blocknition.com](link-to-post)
* Local cryptocurrency order books with Lambdabox and Haskell - [Blocknition.com](link-to-post)
* Your blogpost? Create a PR!

## Contributing

Information that everyone is welcome to contribute! You can contribute in the form
of:

* Create an issue!
* Create a pull request (make sure it is linked to an issue)!
* Provide feedback on the implementation!
* Use Lambdabox!
* Write about Lambdabox!

## License

The use of Lambdabox is completely at your own risk, investing in
cryptocurrencies is a form of gambling and especially with algorithmic trading
you could potentially lose allot of money!

See the `LICENSE` file in the root folder.