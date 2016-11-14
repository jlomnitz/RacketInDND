#lang racket

(require web-server/templates
         web-server/servlet
         web-server/servlet-env
         html
         json
         xml
         db
         net/uri-codec
         )


(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define character-db
  (sqlite3-connect
   #:database "dand.db"))

(define (initialize-db)
  (query-exec character-db "CREATE TABLE characters (id, name, class, race, str, dex, con, int, wis, cha)"))

(define (character-db-search id)
  (query-rows character-db "SELECT * from characters where id=$1" id))

(define (character-db-delete id)
  (query-exec character-db "DELETE from characters where id=$1" id))

(define (character-db-all)
  (query-rows character-db "SELECT * from characters"))

(define (character-html c req)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list (string->bytes/utf-8
          (let
              ([name (character-name c)]
               [class (character-class c)]
               [race (character-race c)]
               [str (character-str c)]
               [dex (character-dex c)]
               [con (character-con c)]
               [int (character-int c)]
               [wis (character-wis c)]
               [cha (character-cha c)])
            (include-template "html/character.html"))))))

(define (character->jsexpr c)
  (let ([id (character-id c)]
        [name (character-name c)]
        [race (character-race c)]
        [str (character-str c)]
        [dex (character-dex c)]
        [con (character-con c)]
        [int (character-int c)]
        [wis (character-wis c)]
        [cha (character-cha c)])
    (hasheq `character
             (hasheq
              `id id
              `name name
              `race race
              `str str
              `dex dex
              `con con
              `int int
              `wis wis
              `cha cha))))

(define (character->xexpr-row c)
  (let
      ([id (number->string (character-id c))]
       [name (character-name c)]
       [class (character-class c)]
       [race (character-race c)]
       [str (number->string (character-str c))]
       [dex (number->string (character-dex c))]
       [con (number->string (character-con c))]
       [int (number->string (character-int c))]
       [wis (number->string (character-wis c))]
       [cha (number->string (character-cha c))]) 
    `(tr ([align "center"])
         (td 
          (a ([href ,(string-append "/characters/" id ".html")])
             (button "View"))
          (button ([onclick ,(string-append "delete_character(" id ")")])
                  "Delete")
          (a ([href ,(string-append "/characters/" id "/edit")])
             (button "Edit")))
         (td ,name)
         (td ,class)
         (td ,race)
         (td ,str)
         (td ,dex)
         (td ,con)
         (td ,int)
         (td ,wis)
         (td ,cha))))

(define (load-characters)
  (define all-characters (character-db-all))
  (for/list ([v all-characters])
    (create-endpoint (apply character (vector->list v)))))

(struct character (id name class race str dex con int wis cha))

(define (new-character data)
  (random-seed (current-seconds))
  (define id (random 1 1000000))
  (apply character (flatten (cons id data))))
  

(define (character-update c)
  (query-exec character-db "UPDATE characters set name=$1, class=$2, race=$3, str=$4, dex=$5, con=$6, int=$7, wis=$8, cha=$9 where id=$10"
                  (character-name c)
                  (character-class c)
                  (character-race c)
                  (character-str c)
                  (character-dex c)
                  (character-con c)
                  (character-int c)
                  (character-wis c)
                  (character-cha c)
                  (character-id c))
  (create-endpoint c))

(define (character-new c)
  (query-exec character-db "INSERT INTO characters values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)"
                  (character-id c)
                  (character-name c)
                  (character-class c)
                  (character-race c)
                  (character-str c)
                  (character-dex c)
                  (character-con c)
                  (character-int c)
                  (character-wis c)
                  (character-cha c))
  (create-endpoint c))           

(define (create-endpoint c)
  (define deleteEndpoint
     (string-join
       (list "DELETE->characters" (number->string (character-id c)))
       "/"))
  (define postEndpoint
     (string-join
       (list "POST->characters" (number->string (character-id c)))
       "/"))
  (define getEditEndpoint
     (string-join
       (list "GET->characters" (number->string (character-id c)) "edit")
       "/"))
  (define endpointString
    (string-join
     (list 
      (string-join
       (list "GET->characters" (number->string (character-id c)))
       "/")
      "html")
     "."))
  (define jsonEndpointString
    (string-join
     (list 
      (string-join
       (list "GET->characters" (number->string (character-id c)))
       "/")
      "json")
     "."))
  (set-add! endpoints (string->symbol deleteEndpoint))
  (set-add! endpoints (string->symbol postEndpoint))
  (set-add! endpoints (string->symbol getEditEndpoint))
  (set-add! endpoints (string->symbol endpointString))
  (set-add! endpoints (string->symbol jsonEndpointString))
  ;JSON-GET->characters
  (eval `(define ,(string->symbol deleteEndpoint)
        (λ (req)
          (set-remove! endpoints (string->symbol ,deleteEndpoint))
          (set-remove! endpoints (string->symbol ,postEndpoint))
          (set-remove! endpoints (string->symbol ,getEditEndpoint))
          (set-remove! endpoints (string->symbol ,endpointString))
          (set-remove! endpoints (string->symbol ,jsonEndpointString))
          (character-db-delete (character-id ,c))
          (character-html ,c req)))
        ns)
  (eval `(define ,(string->symbol postEndpoint)
        (λ (req)
          (EDIT-POST->characters ,c req)))
        ns)
  (eval `(define ,(string->symbol getEditEndpoint)
        (λ (req)
          (EDIT-GET->characters ,c req)))
        ns)
  (eval `(define ,(string->symbol endpointString)
        (λ (req)
          (character-html ,c req)))
        ns)
  (eval `(define ,(string->symbol jsonEndpointString)
        (λ (req)
          (JSON-GET->characters ,c req)))
        ns)
  (string->symbol endpointString))

(define root (path->string (current-directory)))

; path to the server certificate:
(define cert-path (string-append root "/server-cert.pem"))

; path to the private key:
(define key-path (string-append root "/private-key.pem"))

(define (hello-servlet req)
  (define method (request-method req))
  (define uri (request-uri req))
  (define path (map path/param-path (url-path uri)))
  (define endpoint
    (string->symbol (string-join (list (~a method) (string-trim (string-join path "/") "/")) "->")))
  (display endpoint)
  (if (set-member? endpoints endpoint)
      ((eval endpoint ns) req)
      (wrong-endpoint req)))


;; Endpoints

(define endpoints
    (mutable-set
     'GET->characters
     'GET->characters/create
     'PUT->characters/create
     'GET->hello/world))

(define (modified-die-roll)
  (define r (random 1 6))
  (if (eq? r 1)
      (modified-die-roll)
      r))

(define (standard-roll)
  (define (add-roll i)
    (define new-roll (modified-die-roll))
    (if (eq? i 3)
        (cons new-roll empty)
        (let ([next-roll (add-roll (+ i 1))])
           (if (< new-roll (car next-roll))
               (cons new-roll next-roll)
               (if (empty? (cdr next-roll))
                   (cons (car next-roll) new-roll)
                   (cons (car next-roll) (cons new-roll (cdr next-roll))))))))
  (define rolls (add-roll 0))
  (apply + (flatten (cdr rolls))))
  
(define (GET->characters/create req)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list (string->bytes/utf-8 (let ([Method "\"PUT\""]
                                    [Name ""]
                                    [Class ""]
                                    [Race ""]
                                    [Str (standard-roll)]
                                    [Dex (standard-roll)]
                                    [Con (standard-roll)]
                                    [Int (standard-roll)]
                                    [Wis (standard-roll)]
                                    [Cha (standard-roll)]
                                    [Target "\"/characters/create\""]) (include-template "html/create.html"))))))

(define (PUT->characters/create req)
  (define post-data (bytes->string/utf-8 (request-post-data/raw req)))
  (define form-data (form-urlencoded->alist post-data))
  (define name    (cdr (assq 'name form-data)))
  (define class    (cdr (assq 'class form-data)))
  (define race (cdr (assq 'race form-data)))
  (define str (string->number (cdr (assq 'str form-data))))
  (define dex (string->number (cdr (assq 'dex form-data))))
  (define con (string->number (cdr (assq 'con form-data))))
  (define int (string->number (cdr (assq 'int form-data))))
  (define wis (string->number (cdr (assq 'wis form-data))))
  (define cha (string->number (cdr (assq 'cha form-data))))
  (define char (new-character (list name class race str dex con int wis cha)))
  (define endpoint (character-new char))
  (GET->characters req))

(define (EDIT-POST->characters c req)
  (define post-data (bytes->string/utf-8 (request-post-data/raw req)))
  (define form-data (form-urlencoded->alist post-data))
  (define name    (cdr (assq 'name form-data)))
  (define class    (cdr (assq 'class form-data)))
  (define race (cdr (assq 'race form-data)))
  (define str    (string->number (cdr (assq 'str form-data))))
  (define dex    (string->number (cdr (assq 'dex form-data))))
  (define con    (string->number (cdr (assq 'con form-data))))
  (define int    (string->number (cdr (assq 'int form-data))))
  (define wis (string->number (cdr (assq 'wis form-data))))
  (define cha (string->number (cdr (assq 'cha form-data))))
  (define endpoint (character-update (apply character (list (character-id c) name class race str dex con int wis cha))))
  ((eval endpoint ns) req))

(define (EDIT-GET->characters c req)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list (string->bytes/utf-8 (let ([Method "\"POST\""]
                                    [Name (character-name c)]
                                    [Class (character-class c)]
                                    [Race (character-race c)]
                                    [Str (character-str c)]
                                    [Dex (character-dex c)]
                                    [Con (character-con c)]
                                    [Int (character-int c)]
                                    [Wis (character-wis c)]
                                    [Cha (character-cha c)]
                                    [Target (string-append "\"/characters/" (number->string (character-id c)) "\"")])
                                (include-template "html/create.html"))))))
(define (JSON-GET->characters c req)
  (response/full
   200 #"Okay"
   (current-seconds) #"text/json; charset=utf-8"
   empty
   (list (string->bytes/utf-8 (jsexpr->string (character->jsexpr c))))))

(define (GET->characters req)
  (define all-characters (character-db-all))
  (define (loop ls)
    (if (eq? ls empty)
        empty
        (cons (character->xexpr-row (apply character (vector->list (first ls)))) (loop (rest ls)))))  
  (define delete-js
    `(script
      ,(string-join
        (list
         "function delete_character(id) {"
         "var xhttp = new XMLHttpRequest();"
         "xhttp.onreadystatechange = function() {"
         "if (xhttp.readyState == 4) {"
         "location.reload();"
         "}"
         "};"
         "xhttp.open(\"DELETE\", \"/characters/\"+id, true);"
         "xhttp.setRequestHeader(\"Content-type\", \"application/x-www-form-urlencoded\");"	
         "xhttp.send();"
         "}")
        "\n")))
  (define rows
    (cons
     `table 
     (cons
      `([align "center"])
      (cons
       `(tr
         (th)
         (th "Name")
         (th "Class")
         (th "Race")
         (th "Str")
         (th "Dex")
         (th "Con")
         (th "Int")
         (th "Wis")
         (th "Cha"))
       (loop all-characters)))))
  (response/xexpr
   `(html
     (head)
     (body
      ,delete-js
      (center
       (div
        (h3 "List of characters")
        ,rows)
       (a ([href "/characters/create"])
          (button "New Character")))))))


(define (wrong-endpoint req)  
  (response/xexpr
   #:code 404
   #:message #"Route does not exist"
   `(html
     (head)
     (body
      (p "The page you requested does not exist")))))

;;Server

(define (run-server)
  (load-characters)
  (display endpoints)
  (serve/servlet hello-servlet
                 #:servlet-regexp #rx""
                 #:servlet-path "/characters"
                 #:port 8099
                 ))
  ;#:ssl? #t
  ;#:ssl-cert cert-path
  ;#:ssl-key  key-path)

(run-server)