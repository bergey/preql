## Parsing SQL in Haskell Quasiquotes

This is very experimental, and you definitely should not use it for anything important.

Can we teach GHC to do type inference for SQL queries?  That is, given the schema of the database, and a SQL query written in the usual SQL syntax, can GHC check at compile time that the query gets the right number & types of parameters, and returns the right number & types of results?  This should be augmented by validating the actual schema when first connecting to the database.

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

I hope to eventually support more complex queries.  I may also add multiple quasiquoters, to strike different tradeoffs between accurate type inference & working with a larger subset of SQL.

As of 2019-11-07 I can parse a very small subset of SQL, parse the `${}` antiquotes used in the example above, and type check that the *number* of columns returned matches.  (Also number of columns, which is useful if you are using numbered parameters instead / in addition to antiquotes.)  The library has no understanding of the *types* of SQL expressions yet, and the subset of SQL isn't really enough to be useful.

## Prior Art

The most similar design that I've found in Haskell is
[postgresql-typed](https://hackage.haskell.org/package/postgresql-typed).
It provides a quasiquoter, and connects to Postgres at compile time to
validate the queries.  I haven't tried it out yet, but it seems like a
very promising approach.

If you know of other libraries with similar ambitions, please let me know.

Haskell libraries that I've used in production seem to fall into 3 categories:

- SQL as strings, with syntax & types only checked when the query runs
- simple EDSLs that are convenient for CRUD, but don't cover joins, aggregates, and other SQL features
- complex EDSLs that cover much of SQL, with a new syntax & lots of fancy type hackery

I'm delighted that so many people are exploring this design space, but I'm not entirely satisfied with any of these.
