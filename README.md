# stateWriter

[![Build Status](https://travis-ci.org/bartavelle/stateWriter.svg?branch=master)](https://travis-ci.org/bartavelle/stateWriter)

A rewrite of the `RWS` monad, where the `Writer` part is actually handled like strict state so that it doesn't leak memory.
