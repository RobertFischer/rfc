RFC Redis
=============

These are the Robert Fischer Commons extensions to the [hedis](https://hackage.haskell.org/package/hedis)
library for [Redis](https://redis.io).  The major difference is making the Redis monad a lot more friendly
for monad transformer stacks, some naming aliases to bring the names in line with `rfc-psql`, and throwing
exceptions when they occur instead of returning them.
