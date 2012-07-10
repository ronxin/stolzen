#lang scheme


(define (get op type)
    (define (res . params)
        'whatever
    )

    res
)

; a: 
; for extracting, name of division (the type) must be supplied
; as well as the file itself

(define (division-name division-file)
    (car division-file)
)

(define (add-type type record)
    (car type record)
)

(define (get-record name personnel-file)
    (let
        ((get-record-in (get 'get-record (division-name personnel-file))))

        (add-type
            (division-name personnel-file)
            (get-record-in name)
        )
    )
)


; b.

(define (get-salary name record)
    (let
        ((get-salary-in (get 'get-salary (division-name record))))
        (get-salary-in name record)
    )
)

; c.

(define (find-employee-record name files)
    (if (null? files) 
        false

        (let 
            ((res (get-record (car files))))
            (if res 
                res
                (find-employee-record name (cdr files))
            )
        )
    )
)

; d.  When Insatiable takes over a new company, what changes must be made in
; order to incorporate the new personnel information into the central system?

; import procedure should be created for the new company