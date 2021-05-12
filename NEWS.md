# pg13 1.1.0.9000  

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






