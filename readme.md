# Large safe-prime generator

This application will generate all 256 byte safe prime numbers. https://en.wikipedia.org/wiki/Safe_prime

## Usage
```erlang
application:start(safe_prime). %automatically starts processing, prints out safe primes to IO
application:stop(safe_prime).
```
