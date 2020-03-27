# PreQL

Before you Post(gres)QL, **preql**.

1. [Quickstart](#quickstart)
2. [Vision](#vision-parsing-sql-in-haskell-quasiquotes)

## Current Status

**preql** provides a low-level interface to PostgreSQL and a quasiquoter that converts
inline variable names to SQL parameters.  Higher-level interfaces, checking SQL syntax &
schema, are planned.

## Quickstart

```haskell
    import Preql -- The `Preql` module re-exports the interface useful to typical applications.
    import Control.Monad.Trans.Reader (runReaderT)

    main :: IO ()
    main = do
        conn <- connectdb "" -- Get a database connection with default connection string

        -- You can write a SQL instance to replace this ReaderT with
        -- your own application state, logging, error handling
        flip runReaderT conn $
            -- A simple query with no parameters
            cats <- query [sql| SELECT name, age FROM cats |]
            for_ cats \(cat :: (Text, Int)) -> print cat

            -- A query with parameters
            let minAge = 10
            oldCats <- query [sql| SELECT name, age FROM cats where age > ${minAge}|]
            for_ oldCats \(cat :: (Text, Int)) -> print cat

            -- A query that doesn't return rows
            query_ [sql| UPDATE cats SET age = 0 where age < 1 |]

            -- Two queries in a transaction
            moreOldCats <- runTransaction $ do
                maxAge <- V.head <$> query [sql| SELECT max(age) FROM cats |]
                query [sql| SELECT name FROM cats WHERE age = ${maxAge} |]
                -- Just an example; you could make this one DB roundtrip
            traverse_ putStrLn moreOldCats
```

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

As an intermediate stage, also useful to application authors migrating from other
libraries, I'd like to check syntax but not schema.

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
