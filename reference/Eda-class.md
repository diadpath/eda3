# Eda: base class for exploratory data analysis on database tables

Mother class for EDA model objects. It stores the connection information
of a database table which is initialised from a credentials.yml at the
root of the project.

## Usage

``` r
# S4 method for class 'Eda'
initialize(.Object, base = list(), root_path = character(0L), ...)

# S4 method for class 'Eda'
show(object)
```

## Arguments

- .Object:

  An Eda object.

- base:

  `list`. base `list`. Named list with elements `database`, `host`,
  `port`, `user` and `password`. If empty (default), it is read from
  `credentials.yml` in `root_path`.#'

- root_path:

  `character(1)`. Project root. Defaults to
  [`here::here()`](https://here.r-lib.org/reference/here.html).

- ...:

  Passed to the next `initialize` method.

- object:

  An object of class eda

## Slots

- `base`:

  `list`. A list with names `database`, `host`, `port`, `user`,
  `password` that holds DBI connexion information.

- `root_path`:

  `character`. The path to root, `credentials.yml` is searched when
  `base` is not supplied. \#' @section Validity: `base` must return a
  string of length 5 and contain database, host, port, user, and
  password. and `port` must be numeric. `root_path` must have length 1.

## credentials.yml

Create a text file named `credentials.yml` at the root of the project:

    database: name_of_the_database
    host: 127.0.0.1
    port: 5432
    user: myuser
    password: mypassword

Do not commit this file to git.

## See also

[`loaddb()`](loaddb.md)

## Author

Cedric Briand <cedric.briand@eaux-et-vilaine.bzh>

## Examples

``` r
# An example setting initial string
obj <- new("Eda", list("database" = "my_database", "host" = "localhost",
"port" = 5432,"user" = "postgres", "password" = "Monpetitsecret" ))
obj
#> <Eda>
#>   root_path: /home/runner/work/eda/eda
#>   database: my_database
#>   host: localhost
#>   port: 5432
#>   user: postgres
#>   password: ****
# Reading a credentials file
if (requireNamespace("yaml", quietly = TRUE)) {
dir <-  withr::local_tempdir()
 base <- list(
   database = "my_database",
   host     = "localhost",
   port     = 5432,
   user     = "postgres",
   password = "mon_petit_secret_bien_garde"
   )
 yaml::write_yaml(base, file.path(dir,"credentials.yml"))
 obj <- new("Eda", root_path = dir)
 print(obj) # shows the content of the object
 }
#> <Eda>
#>   root_path: /tmp/RtmpMyO8w3/file182119ba352f
#>   database: my_database
#>   host: localhost
#>   port: 5432
#>   user: postgres
#>   password: ****
```
