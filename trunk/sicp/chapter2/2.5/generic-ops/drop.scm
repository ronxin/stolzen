#lang scheme

(require "tags.scm")
(require "container.scm")


(define (drop param)
    (let 
        ((drop-proc (get 'drop (type-tag param))))
        (if drop-proc
            (let 
                ((res (drop-proc param)))
                (if res
                    (drop res)
                    param
                )
            )
            param
        )
    ) 
)

(provide drop)