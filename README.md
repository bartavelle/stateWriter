# stateWriter

[![Build Status](https://travis-ci.org/bartavelle/stateWriter.svg?branch=master)](https://travis-ci.org/bartavelle/stateWriter)
[![stateWriter on Stackage LTS 3](http://stackage.org/package/stateWriter/badge/lts-3)](http://stackage.org/lts-3/package/stateWriter)
[![stateWriter on Stackage Nightly](http://stackage.org/package/stateWriter/badge/nightly)](http://stackage.org/nightly/package/stateWriter)


A rewrite of the `RWS` monad, where the `Writer` part is actually handled like strict state so that it doesn't leak memory.
