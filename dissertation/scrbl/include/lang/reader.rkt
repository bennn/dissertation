(module reader syntax/module-reader
  #:read read-inside
  #:read-syntax read-syntax-inside
  #:whole-body-readers? #t
  #:language 'greenman-thesis/include

  (require (only-in scribble/reader read-inside read-syntax-inside)))
