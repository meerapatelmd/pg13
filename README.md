
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pg13 <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

<!-- badges: end -->

Functionalized rendering, reading, and building of PostgreSQL statements
using R. Other functions from the DatabaseConnector package are migrated
here with added conditional steps, such as warning the user that a
resultset has 0 rows, verbosity such that it is interactively known what
operations are taking place, or SQL rendering in the console before the
statement is sent and/or queried for troubleshooting in a separate
client.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("meerapatelmd/pg13")
```

## Example

pg13 provides functions that simplifies queries such as joins from the R
console.

First, a connection is made to Postgres. Here, I am connecting to my
test database `pg13_test`.

``` r
library(pg13)
conn <- local_connect(dbname = "pg13_test")
```

I write a target table in the test database with sample data:

``` r
test_table <- data.frame(A = 1:25, B = letters[1:25])
head(test_table)
#>   A B
#> 1 1 a
#> 2 2 b
#> 3 3 c
#> 4 4 d
#> 5 5 e
#> 6 6 f
```

The data is written to the target `schema` and `table_name` or
“test\_schema” and “test\_table2” respectively. Messages appear in the
console along with timestamp that elaborate on the connection status,
the data dimensions, and the passage of QA checks.

``` r
write_table(conn = conn,
            schema = "test_schema",
            table_name = "test_table2",
            drop_existing = TRUE,
            data = data.frame(A = 1:25, B = letters[1:25]))
#> [2021-03-28 17:17:26]    Dropping test_schema.test_table2...
#> [2021-03-28 17:17:26]    
#> ✓ Open connection
#> [2021-03-28 17:17:26]    
#> ✓ JDBC connection
#> [2021-03-28 17:17:26]    SQL: DROP TABLE IF EXISTS test_schema.test_table2;
#> [2021-03-28 17:17:26]    Sending...
#> [2021-03-28 17:17:26]    Sending...complete
#> [2021-03-28 17:17:26]    Dropping test_schema.test_table2...complete
#> [2021-03-28 17:17:26]    
#> ✓ Data 'data' has more than 0 rows
#> [2021-03-28 17:17:26]    
#> ✓ Table name 'test_table2' is not a reserved word
#> [2021-03-28 17:17:26]    
#> ✓ Field name 'a' is not a reserved word
#> [2021-03-28 17:17:26]    
#> ✓ Field name 'b' is not a reserved word
#> [2021-03-28 17:17:26]    Writing test_schema.test_table2...
#> [2021-03-28 17:17:26]    Writing test_schema.test_table2...complete
```

I create another dataframe in the R environment to serve as left-side
table for this demonstration.

``` r
test_data <-
        data.frame(A = 1:100, B = letters[1:100])
head(test_data)
#>   A B
#> 1 1 a
#> 2 2 b
#> 3 3 c
#> 4 4 d
#> 5 5 e
#> 6 6 f
```

I can then perform various joins between this dataframe and the
previously written table, such as an inner join

``` r
join1(conn = conn,
      write_schema = "public",
      data = test_data,
      column = "A",
      kind = "INNER",
      join_on_schema = "test_schema",
      join_on_table = "test_table2",
      join_on_column = "A")
#> [2021-03-28 17:17:26]    Dropping public.V20210328171726...
#> [2021-03-28 17:17:26]    
#> ✓ Open connection
#> [2021-03-28 17:17:26]    
#> ✓ JDBC connection
#> [2021-03-28 17:17:26]    SQL: DROP TABLE IF EXISTS public.V20210328171726;
#> [2021-03-28 17:17:26]    Sending...
#> [2021-03-28 17:17:26]    Sending...complete
#> [2021-03-28 17:17:26]    Dropping public.V20210328171726...complete
#> [2021-03-28 17:17:26]    
#> ✓ Data 'data' has more than 0 rows
#> [2021-03-28 17:17:26]    
#> ✓ Table name 'V20210328171726' is not a reserved word
#> [2021-03-28 17:17:26]    
#> ✓ Field name 'a' is not a reserved word
#> [2021-03-28 17:17:26]    
#> ✓ Field name 'b' is not a reserved word
#> [2021-03-28 17:17:26]    Writing public.V20210328171726...
#> [2021-03-28 17:17:26]    Writing public.V20210328171726...complete
#> [2021-03-28 17:17:26]    
#> ✓ Open connection
#> [2021-03-28 17:17:26]    
#> ✓ JDBC connection
#> [2021-03-28 17:17:26]    SQL: SELECT a.*, b.* FROM public.V20210328171726 a INNER JOIN test_schema.test_table2 b ON a.A = b.A
#> [2021-03-28 17:17:26]    Querying...
#> [2021-03-28 17:17:26]    Querying...complete
#> [2021-03-28 17:17:26]    
#> ✓ Returned data has more than 0 rows
#> [2021-03-28 17:17:26]    Dropping public.V20210328171726...
#> [2021-03-28 17:17:26]    
#> ✓ Open connection
#> [2021-03-28 17:17:26]    
#> ✓ JDBC connection
#> [2021-03-28 17:17:26]    SQL: DROP TABLE IF EXISTS public.V20210328171726;
#> [2021-03-28 17:17:26]    Sending...
#> [2021-03-28 17:17:26]    Sending...complete
#> [2021-03-28 17:17:26]    Dropping public.V20210328171726...complete
#>     a b  a b
#> 1   1 a  1 a
#> 2   2 b  2 b
#> 3   3 c  3 c
#> 4   4 d  4 d
#> 5   5 e  5 e
#> 6   6 f  6 f
#> 7   7 g  7 g
#> 8   8 h  8 h
#> 9   9 i  9 i
#> 10 10 j 10 j
#> 11 11 k 11 k
#> 12 12 l 12 l
#> 13 13 m 13 m
#> 14 14 n 14 n
#> 15 15 o 15 o
#> 16 16 p 16 p
#> 17 17 q 17 q
#> 18 18 r 18 r
#> 19 19 s 19 s
#> 20 20 t 20 t
#> 21 21 u 21 u
#> 22 22 v 22 v
#> 23 23 w 23 w
#> 24 24 x 24 x
#> 25 25 y 25 y
```

a right join

``` r
join1(conn = conn,
      write_schema = "public",
      data = test_data,
      column = "A",
      kind = "RIGHT",
      join_on_schema = "test_schema",
      join_on_table = "test_table2",
      join_on_column = "A")
#> [2021-03-28 17:17:26]    Dropping public.V20210328171726...
#> [2021-03-28 17:17:26]    
#> ✓ Open connection
#> [2021-03-28 17:17:26]    
#> ✓ JDBC connection
#> [2021-03-28 17:17:26]    SQL: DROP TABLE IF EXISTS public.V20210328171726;
#> [2021-03-28 17:17:26]    Sending...
#> [2021-03-28 17:17:26]    Sending...complete
#> [2021-03-28 17:17:26]    Dropping public.V20210328171726...complete
#> [2021-03-28 17:17:26]    
#> ✓ Data 'data' has more than 0 rows
#> [2021-03-28 17:17:26]    
#> ✓ Table name 'V20210328171726' is not a reserved word
#> [2021-03-28 17:17:26]    
#> ✓ Field name 'a' is not a reserved word
#> [2021-03-28 17:17:26]    
#> ✓ Field name 'b' is not a reserved word
#> [2021-03-28 17:17:26]    Writing public.V20210328171726...
#> [2021-03-28 17:17:26]    Writing public.V20210328171726...complete
#> [2021-03-28 17:17:27]    
#> ✓ Open connection
#> [2021-03-28 17:17:27]    
#> ✓ JDBC connection
#> [2021-03-28 17:17:27]    SQL: SELECT a.*, b.* FROM public.V20210328171726 a RIGHT JOIN test_schema.test_table2 b ON a.A = b.A
#> [2021-03-28 17:17:27]    Querying...
#> [2021-03-28 17:17:27]    Querying...complete
#> [2021-03-28 17:17:27]    
#> ✓ Returned data has more than 0 rows
#> [2021-03-28 17:17:27]    Dropping public.V20210328171726...
#> [2021-03-28 17:17:27]    
#> ✓ Open connection
#> [2021-03-28 17:17:27]    
#> ✓ JDBC connection
#> [2021-03-28 17:17:27]    SQL: DROP TABLE IF EXISTS public.V20210328171726;
#> [2021-03-28 17:17:27]    Sending...
#> [2021-03-28 17:17:27]    Sending...complete
#> [2021-03-28 17:17:27]    Dropping public.V20210328171726...complete
#>     a b  a b
#> 1   1 a  1 a
#> 2   2 b  2 b
#> 3   3 c  3 c
#> 4   4 d  4 d
#> 5   5 e  5 e
#> 6   6 f  6 f
#> 7   7 g  7 g
#> 8   8 h  8 h
#> 9   9 i  9 i
#> 10 10 j 10 j
#> 11 11 k 11 k
#> 12 12 l 12 l
#> 13 13 m 13 m
#> 14 14 n 14 n
#> 15 15 o 15 o
#> 16 16 p 16 p
#> 17 17 q 17 q
#> 18 18 r 18 r
#> 19 19 s 19 s
#> 20 20 t 20 t
#> 21 21 u 21 u
#> 22 22 v 22 v
#> 23 23 w 23 w
#> 24 24 x 24 x
#> 25 25 y 25 y
```

a left join

``` r
join1(conn = conn,
      write_schema = "public",
      data = test_data,
      column = "A",
      kind = "LEFT",
      join_on_schema = "test_schema",
      join_on_table = "test_table2",
      join_on_column = "A")
#> [2021-03-28 17:17:27]    Dropping public.V20210328171727...
#> [2021-03-28 17:17:27]    
#> ✓ Open connection
#> [2021-03-28 17:17:27]    
#> ✓ JDBC connection
#> [2021-03-28 17:17:27]    SQL: DROP TABLE IF EXISTS public.V20210328171727;
#> [2021-03-28 17:17:27]    Sending...
#> [2021-03-28 17:17:27]    Sending...complete
#> [2021-03-28 17:17:27]    Dropping public.V20210328171727...complete
#> [2021-03-28 17:17:27]    
#> ✓ Data 'data' has more than 0 rows
#> [2021-03-28 17:17:27]    
#> ✓ Table name 'V20210328171727' is not a reserved word
#> [2021-03-28 17:17:27]    
#> ✓ Field name 'a' is not a reserved word
#> [2021-03-28 17:17:27]    
#> ✓ Field name 'b' is not a reserved word
#> [2021-03-28 17:17:27]    Writing public.V20210328171727...
#> [2021-03-28 17:17:27]    Writing public.V20210328171727...complete
#> [2021-03-28 17:17:27]    
#> ✓ Open connection
#> [2021-03-28 17:17:27]    
#> ✓ JDBC connection
#> [2021-03-28 17:17:27]    SQL: SELECT a.*, b.* FROM public.V20210328171727 a LEFT JOIN test_schema.test_table2 b ON a.A = b.A
#> [2021-03-28 17:17:27]    Querying...
#> [2021-03-28 17:17:27]    Querying...complete
#> [2021-03-28 17:17:27]    
#> ✓ Returned data has more than 0 rows
#> [2021-03-28 17:17:27]    Dropping public.V20210328171727...
#> [2021-03-28 17:17:27]    
#> ✓ Open connection
#> [2021-03-28 17:17:27]    
#> ✓ JDBC connection
#> [2021-03-28 17:17:27]    SQL: DROP TABLE IF EXISTS public.V20210328171727;
#> [2021-03-28 17:17:27]    Sending...
#> [2021-03-28 17:17:27]    Sending...complete
#> [2021-03-28 17:17:27]    Dropping public.V20210328171727...complete
#>       a    b  a    b
#> 1     1    a  1    a
#> 2     2    b  2    b
#> 3     3    c  3    c
#> 4     4    d  4    d
#> 5     5    e  5    e
#> 6     6    f  6    f
#> 7     7    g  7    g
#> 8     8    h  8    h
#> 9     9    i  9    i
#> 10   10    j 10    j
#> 11   11    k 11    k
#> 12   12    l 12    l
#> 13   13    m 13    m
#> 14   14    n 14    n
#> 15   15    o 15    o
#> 16   16    p 16    p
#> 17   17    q 17    q
#> 18   18    r 18    r
#> 19   19    s 19    s
#> 20   20    t 20    t
#> 21   21    u 21    u
#> 22   22    v 22    v
#> 23   23    w 23    w
#> 24   24    x 24    x
#> 25   25    y 25    y
#> 26   26    z NA <NA>
#> 27   27 <NA> NA <NA>
#> 28   28 <NA> NA <NA>
#> 29   29 <NA> NA <NA>
#> 30   30 <NA> NA <NA>
#> 31   31 <NA> NA <NA>
#> 32   32 <NA> NA <NA>
#> 33   33 <NA> NA <NA>
#> 34   34 <NA> NA <NA>
#> 35   35 <NA> NA <NA>
#> 36   36 <NA> NA <NA>
#> 37   37 <NA> NA <NA>
#> 38   38 <NA> NA <NA>
#> 39   39 <NA> NA <NA>
#> 40   40 <NA> NA <NA>
#> 41   41 <NA> NA <NA>
#> 42   42 <NA> NA <NA>
#> 43   43 <NA> NA <NA>
#> 44   44 <NA> NA <NA>
#> 45   45 <NA> NA <NA>
#> 46   46 <NA> NA <NA>
#> 47   47 <NA> NA <NA>
#> 48   48 <NA> NA <NA>
#> 49   49 <NA> NA <NA>
#> 50   50 <NA> NA <NA>
#> 51   51 <NA> NA <NA>
#> 52   52 <NA> NA <NA>
#> 53   53 <NA> NA <NA>
#> 54   54 <NA> NA <NA>
#> 55   55 <NA> NA <NA>
#> 56   56 <NA> NA <NA>
#> 57   57 <NA> NA <NA>
#> 58   58 <NA> NA <NA>
#> 59   59 <NA> NA <NA>
#> 60   60 <NA> NA <NA>
#> 61   61 <NA> NA <NA>
#> 62   62 <NA> NA <NA>
#> 63   63 <NA> NA <NA>
#> 64   64 <NA> NA <NA>
#> 65   65 <NA> NA <NA>
#> 66   66 <NA> NA <NA>
#> 67   67 <NA> NA <NA>
#> 68   68 <NA> NA <NA>
#> 69   69 <NA> NA <NA>
#> 70   70 <NA> NA <NA>
#> 71   71 <NA> NA <NA>
#> 72   72 <NA> NA <NA>
#> 73   73 <NA> NA <NA>
#> 74   74 <NA> NA <NA>
#> 75   75 <NA> NA <NA>
#> 76   76 <NA> NA <NA>
#> 77   77 <NA> NA <NA>
#> 78   78 <NA> NA <NA>
#> 79   79 <NA> NA <NA>
#> 80   80 <NA> NA <NA>
#> 81   81 <NA> NA <NA>
#> 82   82 <NA> NA <NA>
#> 83   83 <NA> NA <NA>
#> 84   84 <NA> NA <NA>
#> 85   85 <NA> NA <NA>
#> 86   86 <NA> NA <NA>
#> 87   87 <NA> NA <NA>
#> 88   88 <NA> NA <NA>
#> 89   89 <NA> NA <NA>
#> 90   90 <NA> NA <NA>
#> 91   91 <NA> NA <NA>
#> 92   92 <NA> NA <NA>
#> 93   93 <NA> NA <NA>
#> 94   94 <NA> NA <NA>
#> 95   95 <NA> NA <NA>
#> 96   96 <NA> NA <NA>
#> 97   97 <NA> NA <NA>
#> 98   98 <NA> NA <NA>
#> 99   99 <NA> NA <NA>
#> 100 100 <NA> NA <NA>
```

or a full join:

``` r
join1(conn = conn,
      write_schema = "public",
      data = test_data,
      column = "A",
      kind = "FULL",
      join_on_schema = "test_schema",
      join_on_table = "test_table2",
      join_on_column = "A")
#> [2021-03-28 17:17:27]    Dropping public.V20210328171727...
#> [2021-03-28 17:17:27]    
#> ✓ Open connection
#> [2021-03-28 17:17:27]    
#> ✓ JDBC connection
#> [2021-03-28 17:17:27]    SQL: DROP TABLE IF EXISTS public.V20210328171727;
#> [2021-03-28 17:17:27]    Sending...
#> [2021-03-28 17:17:27]    Sending...complete
#> [2021-03-28 17:17:27]    Dropping public.V20210328171727...complete
#> [2021-03-28 17:17:27]    
#> ✓ Data 'data' has more than 0 rows
#> [2021-03-28 17:17:27]    
#> ✓ Table name 'V20210328171727' is not a reserved word
#> [2021-03-28 17:17:27]    
#> ✓ Field name 'a' is not a reserved word
#> [2021-03-28 17:17:27]    
#> ✓ Field name 'b' is not a reserved word
#> [2021-03-28 17:17:27]    Writing public.V20210328171727...
#> [2021-03-28 17:17:27]    Writing public.V20210328171727...complete
#> [2021-03-28 17:17:27]    
#> ✓ Open connection
#> [2021-03-28 17:17:27]    
#> ✓ JDBC connection
#> [2021-03-28 17:17:27]    SQL: SELECT a.*, b.* FROM public.V20210328171727 a FULL JOIN test_schema.test_table2 b ON a.A = b.A
#> [2021-03-28 17:17:27]    Querying...
#> [2021-03-28 17:17:27]    Querying...complete
#> [2021-03-28 17:17:27]    
#> ✓ Returned data has more than 0 rows
#> [2021-03-28 17:17:27]    Dropping public.V20210328171727...
#> [2021-03-28 17:17:27]    
#> ✓ Open connection
#> [2021-03-28 17:17:27]    
#> ✓ JDBC connection
#> [2021-03-28 17:17:27]    SQL: DROP TABLE IF EXISTS public.V20210328171727;
#> [2021-03-28 17:17:27]    Sending...
#> [2021-03-28 17:17:27]    Sending...complete
#> [2021-03-28 17:17:27]    Dropping public.V20210328171727...complete
#>       a    b  a    b
#> 1     1    a  1    a
#> 2     2    b  2    b
#> 3     3    c  3    c
#> 4     4    d  4    d
#> 5     5    e  5    e
#> 6     6    f  6    f
#> 7     7    g  7    g
#> 8     8    h  8    h
#> 9     9    i  9    i
#> 10   10    j 10    j
#> 11   11    k 11    k
#> 12   12    l 12    l
#> 13   13    m 13    m
#> 14   14    n 14    n
#> 15   15    o 15    o
#> 16   16    p 16    p
#> 17   17    q 17    q
#> 18   18    r 18    r
#> 19   19    s 19    s
#> 20   20    t 20    t
#> 21   21    u 21    u
#> 22   22    v 22    v
#> 23   23    w 23    w
#> 24   24    x 24    x
#> 25   25    y 25    y
#> 26   26    z NA <NA>
#> 27   27 <NA> NA <NA>
#> 28   28 <NA> NA <NA>
#> 29   29 <NA> NA <NA>
#> 30   30 <NA> NA <NA>
#> 31   31 <NA> NA <NA>
#> 32   32 <NA> NA <NA>
#> 33   33 <NA> NA <NA>
#> 34   34 <NA> NA <NA>
#> 35   35 <NA> NA <NA>
#> 36   36 <NA> NA <NA>
#> 37   37 <NA> NA <NA>
#> 38   38 <NA> NA <NA>
#> 39   39 <NA> NA <NA>
#> 40   40 <NA> NA <NA>
#> 41   41 <NA> NA <NA>
#> 42   42 <NA> NA <NA>
#> 43   43 <NA> NA <NA>
#> 44   44 <NA> NA <NA>
#> 45   45 <NA> NA <NA>
#> 46   46 <NA> NA <NA>
#> 47   47 <NA> NA <NA>
#> 48   48 <NA> NA <NA>
#> 49   49 <NA> NA <NA>
#> 50   50 <NA> NA <NA>
#> 51   51 <NA> NA <NA>
#> 52   52 <NA> NA <NA>
#> 53   53 <NA> NA <NA>
#> 54   54 <NA> NA <NA>
#> 55   55 <NA> NA <NA>
#> 56   56 <NA> NA <NA>
#> 57   57 <NA> NA <NA>
#> 58   58 <NA> NA <NA>
#> 59   59 <NA> NA <NA>
#> 60   60 <NA> NA <NA>
#> 61   61 <NA> NA <NA>
#> 62   62 <NA> NA <NA>
#> 63   63 <NA> NA <NA>
#> 64   64 <NA> NA <NA>
#> 65   65 <NA> NA <NA>
#> 66   66 <NA> NA <NA>
#> 67   67 <NA> NA <NA>
#> 68   68 <NA> NA <NA>
#> 69   69 <NA> NA <NA>
#> 70   70 <NA> NA <NA>
#> 71   71 <NA> NA <NA>
#> 72   72 <NA> NA <NA>
#> 73   73 <NA> NA <NA>
#> 74   74 <NA> NA <NA>
#> 75   75 <NA> NA <NA>
#> 76   76 <NA> NA <NA>
#> 77   77 <NA> NA <NA>
#> 78   78 <NA> NA <NA>
#> 79   79 <NA> NA <NA>
#> 80   80 <NA> NA <NA>
#> 81   81 <NA> NA <NA>
#> 82   82 <NA> NA <NA>
#> 83   83 <NA> NA <NA>
#> 84   84 <NA> NA <NA>
#> 85   85 <NA> NA <NA>
#> 86   86 <NA> NA <NA>
#> 87   87 <NA> NA <NA>
#> 88   88 <NA> NA <NA>
#> 89   89 <NA> NA <NA>
#> 90   90 <NA> NA <NA>
#> 91   91 <NA> NA <NA>
#> 92   92 <NA> NA <NA>
#> 93   93 <NA> NA <NA>
#> 94   94 <NA> NA <NA>
#> 95   95 <NA> NA <NA>
#> 96   96 <NA> NA <NA>
#> 97   97 <NA> NA <NA>
#> 98   98 <NA> NA <NA>
#> 99   99 <NA> NA <NA>
#> 100 100 <NA> NA <NA>
```

## Code of Conduct

Please note that the pg13 project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
