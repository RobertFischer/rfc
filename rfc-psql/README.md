RFC PostgreSQL Support
=======================

This module provides various utility methods, a monad, and some instances to make operating with `psql` easier.
It specifically leverages [`postgresql-typed`](https://hackage.haskell.org/package/postgresql-typed), because that
provides some reasonable compile-time checking of SQL correctness and parameter/return value type conversions.
