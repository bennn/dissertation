https://groups.google.com/g/racket-users/c/jtmVDFCGL28/m/jwl4hsjtBQAJ

Hello,

I have one file called `type-test.rkt` with the following (notice that I discovered that there is a typed version of the web-server/http module, which solves another of my issues):

```
#lang typed/racket

(require (only-in typed/web-server/http response/xexpr response))

(provide f1 f2 f3 f4)

(: f1 (-> response))
(define (f1)
  (define x '(body (h1 "Try it")))
  (: resp response)
  (define resp (response/xexpr x))
  resp)

(: f2 (-> (U response Any)))
(define (f2)
  (define x '(body (h1 "Try it")))
  (: resp response)
  (define resp (response/xexpr x))
  resp)

(: f3 (-> (U response Number)))
(define (f3)
  (define x '(body (h1 "Try it")))
  (: resp response)
  (define resp (response/xexpr x))
  resp)

(: f4 (-> (U Any response)))
(define (f4)
  (define x '(body (h1 "Try it")))
  (: resp response)
  (define resp (response/xexpr x))
  resp)
```

Then I have another *untyped* file for a servlet:

```
#lang racket

(require "type-test.rkt"
         web-server/servlet
         web-server/servlet-env)

(define (start req)
  (f1)
  ; (f2)
  ; (f3)
  ; (f4)
  )

(serve/servlet start
               #:servlet-regexp #rx""
               #:launch-browser? #false
               #:port 8080)
```

Notice that I am telling all the f* functions that `resp` is of type `response`. Yet, when I run the server with `start` using `f1` through `f4` I get the following results:

(f1): All good
(f2): Error, see below. Unable to protect opaque value passed as `Any` 
(f3): All good 
(f4): Error, see below.

The error is:

```
f4: broke its own contract
  any-wrap/c: Unable to protect opaque value passed as `Any`
  value: #<response>
  in: the range of
      (-> Any)
```

First, I couldn't figure out how to replicate this in the REPL, and had to use the server to get the result. But I was able to figure out at the REPL that (f2) and (f4) return something of type `Any`, for some unknown reason. Clearly TR is smart enough to figure out that `resp` is not a Number, but a response, but then when I allow it to return both a type `response` and `Any`, it says that it's return value is `Any`. Why? Every function that can take `Any` can take `response`, and every function that expects response is now going to blow up (as now happens). 

At the REPL, I didn't manage to get this behavior, probably because `serve/servlet` has contracts around it's arguments. Thus:

- I pass all the typed racket tests
- I nonetheless return something of type `Any` (which is really guaranteed to be of type response, and I am already annotating, so this surprises me)
- When this hits the contract, it complains and tells me that f4 broke its own contract, which seems false, but this may be one of those 'Contract blaming is hard' moments

Long story short: Why do (f2) and (f4) return something of type `Any` rather than `response`? What is the logic for doing this and what use case am I missing where this is a feature (or maybe it's just hard to get right, but this seems vastly easier than other things Typed Racket does).

And yes, I should probably just use typed/web-server/servlet and friends (not sure if everything is covered, but probably) - but I discovered the typed/** modules only while trying to fix this. And the issue will come up with other non-typed modules or when/if I try to created typed versions of some module. 

Cheers,
Marc
