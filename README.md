# Human Readable Duration

This is a minimal Haskell library to display duration.
It is mostly unsafe as it uses only `Int` and `String`s (not even `Text` nor `ByteString`).

## Install

Install [`stack`](http://github.com/commercialhaskell/stack).

~~~
stack build
~~~

## Usage

~~~ {.haskell}
> let duration = 2*ms + 3*oneSecond + 2*minute + 33*day + 2*year
> humanReadableDuration duration
"2 years 33 days 2 min 3s 2ms"
> getYears duration
2
> getDays duration
763
> getMs duration
65923323002
~~~
