(unless
    (let
	((load-path load-path))

      (add-to-list 'load-path (expand-file-name "../site-lisp"))

      (load-library "cat-utils")

      (assert 
       (string= "zephirus eek with his sweete breeth"
		(chomp "zephirus eek with his sweete breeth
"))
       )

      (assert 
       (string= "zephirus eek with his sweete breeth"
		(chomp "zephirus eek with his sweete breeth
")
		)
       )

      (assert 
       (string= "zephirus eek with his sweete breeth"
		(chomp "zephirus eek with his sweete breeth
")
		)
       )


      (assert 
       (equal '(a b c d)
	      (let ((l '(a b c))) (unshift l 'd) l)
	      ))

      (assert 
       (equal '(a b c d)
	      (unshift '(a b c) 'd)
	      )
       )

      (assert
       (equal
	'("foo" "bar baz bo")
	(shift-word "foo;bar baz bo")
	)
       )

      (assert
       (equal 
	'(c a b)
	(let ((l '(a b c))) (nconc (list (shift l)) l))
	)
       )

      (assert
       (eq 'c (shift '(a b c)))
       )


      (assert 
       (equal
	(splice '(a b c) '(1 2 3))
	'((a . 1)
	  (b . 2)
	  (c . 3))
	)
       )

      (assert 
       (equal
	(split "abcd efgh, ijkl	mnop  " )
	'("abcd" "efgh," "ijkl" "mnop")
	)
       )

      (assert
       (equal
	(split "foo,bar,baz" ?,)
	'("foo" "bar" "baz"))
       )


      (assert
       (equal
	(split "-outline-Arial-bold-r-normal-normal-13-97-96-96-p-60-iso10646-1" "-" t)
	'("" "outline" "Arial" "bold" "r" "normal" "normal" "13" "97" "96" "96" "p" "60" "iso10646" "1"))
       )

      (assert
       (equal
	(join '("foo" "bar") " ")
	"foo bar")
       )

      (assert
       (equal
	(join (list "a" "b" "c") ":")
	"a:b:c"))

      (assert
       (equal
	(join (list "a" "b" "c") ?:)
	"a:b:c"))

      (assert
       (equal
	(join (vector "a" "b" "c") "-")
	"a-b-c"))

      (assert
       (equal
	(split "abcd efgh, ijkl	mnop  " )
	'("abcd" "efgh," "ijkl" "mnop")))

      (assert
       (equal
	(split "foo;bar;baz" ?\;)
	'("foo" "bar" "baz")))

      (assert
       (equal
	(split "-outline-Arial-bold-r-normal-normal-13-97-96-96-p-60-iso10646-1" "-" t)
	'("" "outline" "Arial" "bold" "r" "normal" "normal" "13" "97" "96" "96" "p" "60" "iso10646" "1")))

      (assert
       (equal
	(split "foo,\"Tuesday, Jan 25 17:25:26 CST 2011\",bar" ",")
	'("foo" "\"Tuesday, Jan 25 17:25:26 CST 2011\"" "bar")))

      (assert
       (equal
	(split "foo,bar,\"Tuesday, Jan 25 17:25:26 CST 2011\"" ",")
	'("foo" "bar" "\"Tuesday, Jan 25 17:25:26 CST 2011\"")))

      )
  (message "thunderbirds are go!")
  )
