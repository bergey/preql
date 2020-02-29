## PreQL

Before you Post(gres)QL, **preql**.

## Current Status

**preql** provides a low-level interface to PostgreSQL and a quasiquoter.  Higher-level
interfaces are planned.

The low-level interface (In `Preql.Wire`) sends query strings to Postgres without any
validation, and sends parameters using the binary encoder.  Parameters in the query string
are represented with the standard `$1, $2` syntax, and it is up to the application
developer to ensure that the provided tuple (or record) of parameters has enough elements.

### Effect Type Class

The `SQL` type class describes contexts in which SQL queries can be run.  This supports
running in various monads besides `IO`, and signatures like `SQL m => m ()` which permit
running SQL queries without permitting arbitrary effects.  A query can be run as:

``` haskell
    query [sql| SELECT name, age FROM cats|]
```

The variant `query_` is more convenient when no result is needed.

### Quasiquoter

The `sql` quasiquoter used above is defined in `Preql.QuasiQuoter.Raw` and exported from
the top-level module `Preql`.  It is intended to make parameter substitution less
error-prone.  `sql` supports both numbered parameters (with the standard syntax) and
antiquotes.  These can be freely mixed within the same query.  A query with an antiquote
is written `${varName}`.  The `sql` quasiquoter will replace antiquotes with numbered
parameters, and construct a matching tuple.

For example:
``` haskell
    [sql| SELECT name, age FROM cats WHERE age >= ${minAge} and age < ${maxAge} |]
```

is converted to:

``` haskell
    ("SELECT name, age FROM cats WHERE age >= $1 and age < $2", (minAge, maxAge))
```

This will only compile if `minAge` & `maxAge` are in scope, along with `ToSqlField`
instances that match their types.

When numbered paramaters and antiquotes are mixed, `sql` starts numbering the antiquotes
after the highest explictly numbered param.  The query above could instead be written:

``` haskell
    query $ [sql| SELECT name, age FROM cats WHERE age >= ${minAge} and age < $1 |] maxAge
```

Note the `$` after `query`; the quasiquote here expands to a function that takes `maxAge`
and produces the `(String, params)` pair as above.  Note also that `ToSql` has the
necessary instances to use either a tuple of parameters or a single parameter

## Vision: Parsing SQL in Haskell Quasiquotes

Can GHC check that a query is sytactically correct at compile time, without having
Postgres run it?

Can we teach GHC to do type inference for SQL queries?  That is, given the schema of the
database, and a SQL query written in the usual SQL syntax, can GHC check at compile time
that the query gets the right number & types of parameters, and returns the right number &
types of results?  This should be augmented by validating the actual schema when first
connecting to the database.

Given a table like

``` sql
CREATE TABLE users (id uuid, email text, last_login timestamptz);
```

I'd like to write Haskell like

``` haskell
staleUsers = do
    now <- getCurrentTime
    query [sql|SELECT uuid, email FROM users WHERE last_login > ${now}|]
```

and have GHC infer `staleUsers :: SQL m => m [(UUID, Text)]` or something similar.

### Prior Art

The most similar design that I've found in Haskell is
[hasql-th](http://hackage.haskell.org/package/hasql-th).

[postgresql-typed](https://hackage.haskell.org/package/postgresql-typed) also occupies a
similar point in the design space.  It provides a quasiquoter, and connects to Postgres at
compile time to validate the queries.  I haven't tried it out yet, but it seems like a
very promising approach.

If you know of other libraries with similar ambitions, please let me know.

Haskell libraries that I've used in production seem to fall into 3 categories:

- SQL as strings, with syntax & types only checked when the query runs
- simple EDSLs that are convenient for CRUD, but don't cover joins, aggregates, and other SQL features
- complex EDSLs that cover much of SQL, with a new syntax & lots of fancy type hackery

I'm delighted that so many people are exploring this design space, but I'm not entirely satisfied with any of these.
