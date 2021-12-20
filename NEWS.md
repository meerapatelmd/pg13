# pg13 (development version)  

## Fixes  

* Fixed `count` field datatypes when performing 
checks on source rows that are then bound into a single 
data frame  
* Add default `conn_fun` value to `send()` and changed logic to missing `conn` argument

## Features  

* Added `constraints` feature to automate ER diagram creation  
* Added a `total_rows` checkpoint that reports the row counts of 
source tables detected from the sql string served to the connection.  
* Removed legacy SQL queries in the installation directory  



# pg13 1.3.0  

* `query()` and `send()` can now accept custom values to 
render SQL with.  
* All `check_*` functions previously returned to the console 
when `verbose` is set to _TRUE_. Now they can be explicitly 
specified via the `checks` argument.  
* Added feature for logging console messages to a log file.  
* Added feature for concatenating SQL statements to SQL or 
Rmd files.  
* Deprecated redundant `check_*` functions.  


# pg13 1.1.1  

* Added `conn_fun` argument to `*_exists` functions  
* Added `rename_schema()` function  


# pg13 1.1.0.9000  

* Styled project according to the tidyverse style guide  
* Export `typewrite_*` functions   
* Remove cli package dependency  
* Add chunk style option to `typewrite_sql()`  
* Change `checks_*` to only return danger alerts so that 
console messages are less lengthy.  
* Add styling option and return `checks_*` in console as 
condition of `verbose` in `query()` and `send()`.  
* Expand default argument values in `conn_db()`  
* Silence `local_connect()` unless verbose is set to _TRUE_.  


# pg13 1.1.0

* Added join1 function


# pg13 1.0.0

* Change function naming conventions to snake case  
* Deprecated camel case functions  


# pg13 0.3.2

* Added a `NEWS.md` file to track changes to the package.  
* Added `log_*` functions (https://github.com/meerapatelmd/pg13/issues/13)  
* Added `migrate` function (https://github.com/meerapatelmd/pg13/issues/18) 
* Added `kill` and `getConnDB` functions  
* Added reserve word checks (https://github.com/meerapatelmd/pg13/issues/22)  






