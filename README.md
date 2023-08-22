# pact-crypto

A collection of hashing, signing and verifying algorithms used in
[Pact](https://github.com/kadena-io/pact) and
[chainweb-node](https://github.com/kadena-io/chainweb-node).

## Goals

Pact and Chainweb rely on a lot of cryptography, and its easy to begin pulling
algorithms from lots of different sources, losing track of which ones are used
and where.

This repository consolidates all of the algorithms we would like to use into
a single place, so that if you encounter a named cryptographic algorithm in
either pact or chainweb, you can audit it by tracing it back to `kadena-crypto`.

## Example: Hashing

``` haskell
{-# LANGUAGE OverloadedStrings, TypeApplications #-}
import Kadena.Crypto.Hash (hash, toHexString)
import Kadena.Crypto.Hash.Sha3 (hashSha3_256, Sha3_512)

>>> toHexString $ hashSha3_256 "Hello Kadena!"
"c370fe9f42fd9539138eb809cb69e489567eca4e03ca162a6b1d640cadae8f5c"

>>> toHexString $ hash @Sha3_512 "With classes"
d9ef55b414163c83ae3408a983cec0b9b93ba2610ad0d08c4137f3fe5c836d7bd18dfca63a579256d32eb4b4ba416e8f7c862e1cac10b3baeeccc0927c290d0c
```

## Example: Signature verification

```haskell
{-# LANGUAGE OverloadedStrings, TypeApplications #-}
import Kadena.Crypto.Hash (hash, toHexString)
import Kadena.Crypto.Hash.Sha3 (hashSha3_256, Sha3_512)

>>> 
```
