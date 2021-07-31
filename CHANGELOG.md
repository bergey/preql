# 0.6 (2021-07-31)

- decode array types
- decode composite types with `Tuple` newtype
- cache OID lookups

# 0.5 (2021-01-10)

- fix `deriveFromSql` which was completely unusable
- add benchmark / exe for speeed testing
- 3x improvement in decoding speed for this benchmark

# 0.4 (2021-01-07)

- `select` quasiquoter that validates syntax
- tag number of columns consumed in `RowDecoder` type
- when decoder & number of returned columns are both known, ensure they match

# 0.3 (2020-06-18)

- lookup types by name when OID is not known statically
- specify isolation level when running a transaction
- stop trying to fake nested transactions
- provide direct access to a Connection when that control is needed

# 0.2 (2020-03-31)

- now we have documentation
- rearrange some code, re-export modules to support docs

# 0.1 (2020-02-29)

0.1 highlights include:

- binary wire format
- check that Postgres sent expected type before decoding
- Quasiquoter supports mixing numbered & antiquoted params
