# COBOL-DB2-CICS-exercise

BOOK LIST AND DETAILS EXERCISE

Create a set of COBOL programs containing CICS and DB2 commands that will: 

* Display a list of books 
* Allow the user to: 
  * Select books to display their details
  * Delete books from the list
  * Update book details
  * Insert a new book into the list
  * Search for books with titles containing a string of characters
* 	Generate a report containing the list of books

Files and Directories:

* **cobol**
  * **bookrep.cbl**  - batch program to generate report
  * **cicsrjcl.cbl** - CICS program to submit job
  * **queuedb2.cbl** - CICS subprogram to create TSQs
  * **tran1db2.cbl** - CICS transaction for book list main menu
  * **tran2db2.cbl** - CICS transaction for book details display
* **copybooks**
  * **dclbooks.cbl** - DCLGEN for BOOKS table
  * **infoset.cbl**  - symbolic map for book details
  * **listset.cbl**  - symbolic map for book list
  * **repvars.cbl**  - copybook of report generation variables
* **bkrepop.txt**    - sample report
* **jbookrep.jcl**   - jcl for running report program
