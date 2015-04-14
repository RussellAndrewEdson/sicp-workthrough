;;; SICP Exercise 2.74
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we implemented a data-directed programming
;;; scheme for a company's file system.


; Basically, we want to use the technique of data-directed programming to
; handle everything here. That is, we want to make sure that each division 
; tags its data uniquely, so we can make use of the following table of 
; operations:
;
;              | get-record      | get-salary      |  ...
; ----------------------------------------------------------
;  Division 1  | get-record-div1 | get-salary-div1 |  ...
; ----------------------------------------------------------
;  Division 2  | get-record-div2 | get-salary-div2 |  ...
; ----------------------------------------------------------
;     ...              ...               ...
;     ...              ...               ...
; ----------------------------------------------------------
;  Division n  | get-record-divn | get-salary-divn |  ...
; ----------------------------------------------------------

; ..where get-record-div1, for instance, is the procedure that Division 1 uses
; to retrieve an employee's record from their own file (presumably they would
; already have such a procedure if they're already using their own system.)
;
; Note that the names need not be unique -- in fact, we can expect that
; each disivion probably calls their employee record retrieval procedure 
; 'get-record'. If we use the package technique as detailed in the book, this
; won't be a problem.

; Now the only other thing we need is to make sure that the data is tagged,
; so we can know which procedure to apply at each point.


; For the sake of example (and so we can see this data-driven programming
; in action!) let's make up some Division files and records using some
; different data structures.


; Suppose for division 1, they keep everything in one-level unordered lists. 
; So then their employee records look like:

(define employee-1a '((name john) (address 111) (salary 45000)))

(define employee-1b '((address 123) (name sally) (salary 47000)))

(define employee-1c '((salary 46000) (address 999) (name bob)))

; And their personnel file looks like:

(define file-div1 (list employee-1a employee-1c employee-1b))

; Then they would access their records and employee information
; with the following selectors (for simplicity, suppose they
; return the empty list in the case of missing data.)
;
; (Note again that the selectors would probably not specifically 
; mention the division number, but I'm including that here so that 
; there are no name clashes. In reality these name clashes would be 
; a concern, and part of the point of using the operation table
; is to fix exactly this name-clash issue.)

(define (get-attribute-div1 attribute record)
  (cond ((null? record) '())
        ((eq? attribute (caar record)) (cadar record))
        (else (get-attribute-div1 attribute (cdr record)))))

(define (get-name-div1 record)
  (get-attribute-div1 'name record))

(define (get-salary-div1 record)
  (get-attribute-div1 'salary record))

(define (get-address-div1 record)
  (get-attribute-div1 'address record))

(define (get-record-div1 employee)
  (define (search rest-of-file)
    (if (null? rest-of-file)
        '()
        (let ((record (car rest-of-file)))
          (if (eq? employee (get-name-div1 record))
              record
              (search (cdr rest-of-file))))))
  (search file-div1))

; We can test these quickly to make sure that they work as expected:

(get-record-div1 'john)
;> ((name john) (address 111) (salary 45000))

(get-record-div1 'sally)
;> ((address 123) (name sally) (salary 47000))

(get-address-div1 (get-record-div1 'bob))
;> 999

(get-salary-div1 (get-record-div1 'bob))
;> 46000


; And just for fun, suppose Division 2 uses -procedures- to
; store their data. So their employee records look like:

(define (employee-2a attribute)
  (cond ((eq? attribute 'name) 'harold)
        ((eq? attribute 'address) 445)
        ((eq? attribute 'salary) 78000)
        (else '())))

(define (employee-2b attribute)
  (cond ((eq? attribute 'name) 'samantha)
        ((eq? attribute 'address) 446)
        ((eq? attribute 'salary) 67000)
        (else '())))

; And their personnel file is also implemented as a procedure:

(define (file-div2 name)
  (cond ((eq? name 'harold) employee-2a)
        ((eq? name 'samantha) employee-2b)
        (else '())))

; Then obviously Division 2 would use a radically different
; set of selectors for accessing their data.

(define (get-name-div2 record)
  (record 'name))

(define (get-salary-div2 record)
  (record 'salary))

(define (get-address-div2 record)
  (record 'address))

(define (get-record-div2 employee)
  (file-div2 employee))

; We can also test this representation to make sure it works:

(get-record-div2 'harold)
;> #<procedure:employee-2a>

(get-salary-div2 (get-record-div2 'samantha))
;> 67000

(get-address-div2 (get-record-div2 'harold))
;> 445


; By using data-driven programming, we can provide a consistent
; interface for both (all) divisions, without having to modify
; any of this existing infrastructure (aside from the data tagging.)


; a)
; So suppose we have the 'get' and 'put' procedures defined as follows,
; to work with an operations table.

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))


; Then we can use the operations table idea, provided the divisions
; tag their file with a symbol that identifies the particular division. 
; That is, the files must also supply type information that indicates
; their division (so we can determine the procedures to use.)

(define tagged-file-div1 (cons 'div1 file-div1))

(define tagged-file-div2 (cons 'div2 file-div2))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

; Then we can populate the operations table with the existing
; procedures that each division uses to manage its own data,
; indexed by division:

(put 'div1 'get-record get-record-div1)
(put 'div1 'get-address get-address-div1)
(put 'div1 'get-name get-name-div1)
(put 'div1 'get-salary get-salary-div1)

(put 'div2 'get-record get-record-div2)
(put 'div2 'get-address get-address-div2)
(put 'div2 'get-name get-name-div2)
(put 'div2 'get-salary get-salary-div2)

; Then the general 'get-record' procedure simply looks up the
; correct procedure to use from the table, based on which division
; is responsible for the given data.

(define (get-record employee file)
  (let ((division (type-tag file)))
    ((get division 'get-record) employee)))

; And we can test that this general procedure works:

(get-record 'john tagged-file-div1)
;> ((name john) (address 111) (salary 45000))

(get-record 'harold tagged-file-div2)
;> #<procedure:employee-2a>


; b)
; Similarly, the general 'get-salary' procedure is just as
; easy to write, given that we've tagged the division file.
; (Note that we didn't actually need to tag the record data
; here -- we can tell which operation to use based on the
; file we're looking through.)

(define (get-salary employee file)
  (let ((division (type-tag file)))
    (let ((employee-record ((get division 'get-record) employee)))
      ((get division 'get-salary) employee-record))))

; And this one also works for both (all) divisions seamlessly.

(get-salary 'bob tagged-file-div1)
;> 46000

(get-salary 'samantha tagged-file-div2)
;> 67000
  

; c)
; Now suppose we want to implement a 'find-employee-record'
; procedure that searches a list of all the files from each
; division and returns the record for a given employee. We can
; code this up easily too, by simply using our 'get-record'
; procedure which should dispatch to the correct record-finding
; operation for each different division in the list.

; So we simply cdr down the division file list, calling the 
; correct 'get-record' procedure for each one.

(define (find-employee-record employee division-files)
  (if (null? division-files)
      '()
      (let ((record (get-record employee (car division-files))))
        (if (not (null? record))
            record
            (find-employee-record employee (cdr division-files))))))

; And this procedure works just fine, too.

(define files-list (list tagged-file-div1 tagged-file-div2))

(find-employee-record 'samantha files-list)
;> #<procedure:employee-2b>

(find-employee-record 'sally files-list)
;> ((address 123) (name sally) (salary 47000))
        

; d)
; So whenever there's a new division to be added, none of the existing
; structure for their data needs to be changed. We just need to tag the
; personnel file with a division identifier, and add the procedures for
; working with that file (which you'd assume already exist) into the
; operations table. And our general procedures should work correctly
; right away!

; For instance, suppose we have a new division, which had a pretty sloppy
; data-handling practice where they didn't bother using keys for their
; records at all; just lists, where the 1st element is the name, the
; 2nd element is the address, and the 3rd element is the salary.

; So their employee records and associated selectors look like:

(define employee-3a (list 'chris 456 37000))

(define employee-3b (list 'mary 789 45000))

(define (get-name-div3 record)
  (car record))

(define (get-address-div3 record)
  (cadr record))

(define (get-salary-div3 record)
  (caddr record))


; And their personnel file and 'get-record' selector look like this:

(define file-div3 (list employee-3a employee-3b))

(define (get-record-div3 employee)
  (define (search rest-of-file)
    (if (null? rest-of-file)
        '()
        (let ((record (car rest-of-file)))
          (if (eq? employee (get-name-div3 record))
              record
              (search (cdr rest-of-file))))))
  (search file-div3))

; Then we just tag the file and add the operations to the table:

(define tagged-file-div3 (cons 'div3 file-div3))

(put 'div3 'get-record get-record-div3)
(put 'div3 'get-address get-address-div3)
(put 'div3 'get-name get-name-div3)
(put 'div3 'get-salary get-salary-div3)


; And our existing general procedures will work perfectly!

(get-record 'mary tagged-file-div3)
;> (mary 789 45000)

(get-salary 'chris tagged-file-div3)
;> 37000

(define new-files-list (list tagged-file-div1
                             tagged-file-div2
                             tagged-file-div3))

(find-employee-record 'chris new-files-list)
;> (chris 456 37000)
