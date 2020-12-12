#lang greenman-thesis

@(require (only-in racket/string string-replace))

@; don't forget BKC despite waning influence
@; Leif, Max, MB, Sarah also critical need acks
@;  often discomfort, but also and more importantly growth

@; marrynig a software engineer? benefits marginal, never forget its her money

@; pre-phd 8 yrs for stock options 6 for phd not bad but oh no did not anticipate
@; the value added from mf (specifics)

@;christos ross zeina close research ties
@;max lief closest at neu

@; Sarah
@; No kidding, I spent hundreds of nights at Upson Hall.
@; There were three showers in the building.
@; and might have gone on to join the Free Software Foundation.
@; Thanks also for the sharp criticism. Are the papers I've written really worth the carbon cost? Alas

@(define-syntax-rule (define-href id url)
   (define id (hyperlink url (string-replace (symbol->string 'id) "-" " "))))

@(define-href Matthias-Felleisen "https://felleisen.org/matthias/")
@(define-href Jan-Vitek "http://janvitek.org/")
@(define-href Amal-Ahmed "http://www.ccs.neu.edu/home/amal/")
@(define-href Fritz-Henglein "http://hjemmesider.diku.dk/~henglein/")
@(define-href Shriram-Krishnamurthi "http://cs.brown.edu/~sk")
@(define-href Sam-Tobin-Hochstadt "http://samth.github.io")
@(define-href Pete-Migliorini "https://www.linkedin.com/in/peter-migliorini-0a770b125")
@(define-href Andrew-Hurd "https://www.albany.edu/computer-science/faculty/andrew-hurd")
@(define-href Hudson-Valley-Community-College "http://www.hvcc.edu/")
@(define-href ILR-School-at-Cornell "https://www.ilr.cornell.edu/")
@(define-href Kevin-Dolan "https://www.linkedin.com/in/kevin-j-dolan-655b3314")
@(define-href Dan-Rothenberg "http://danielrothenberg.com")
@(define-href Dave-Viera "https://www.google.com")
@(define-href Mark-Vigeant "https://www.markvigeant.com")
@(define-href Nicole-Roy "https://www.cs.cornell.edu/undergrad/ustaff")
@(define-href Thorsten-Joachims "http://www.cs.cornell.edu/people/tj/")
@(define-href Ramin-Zabih "http://www.cs.cornell.edu/~rdz/")
@(define-href OCaml-3.12 "https://ocaml.org/releases/3.12.1.html")
@(define-href Nate-Foster "https://www.cs.cornell.edu/~jnfoster/")
@(define-href Sam-Park "https://www.csail.mit.edu/person/sam-park")
@(define-href Ben-Carriel "https://botlablp.com")
@(define-href Ross-Tate "http://www.cs.cornell.edu/~ross/")
@(define-href Ehsan-Hoque "http://www.hoques.com")
@(define-href Fabian-Muehlboeck "http://pub.ist.ac.at/~fmuehlbo/")
@(define-href PRL-Lab-at-Northeastern "https://prl.ccs.neu.edu")

@; -----------------------------------------------------------------------------

Thank you Sarah Lee Greenman for the support, financial or otherwise, and
 civilizing guidance.
Without Sarah, I might still be a Ph.D. student (why not?) living the monastic
 lifestyle.
You know the drill: saltine crackers and peanut butter, candlelight,
 frequent nights at the lab, and austere conditions at home.
With Sarah, I have two cats to attend and a baby to look forward to.
It's been fun.

Thank you parents for your support, financial and otherwise.
One stormy high-school afternoon, I set out to test my domestic liberty
 by reading The Communist Manifesto at home.
Instead of outrage, though, this action met calm encouragement.
``That's great.''
I may have finished the book all the same, but what stands out is a lesson
 in patience and respect for primary sources.

Thank you @|Matthias-Felleisen| for teaching me how to be a scientist,
 and that there is much more to programming languages than dependent type
 systems and the lambda calculus.

Thank you committee members: @|Jan-Vitek|, @|Amal-Ahmed|, @|Fritz-Henglein|,
 @|Shriram-Krishnamurthi|, and @|Sam-Tobin-Hochstadt|.
Suffice it to say, they improved this dissertation and taught
 rigorous theory, reproducible artifacts, and critical discussion.

Ten years ago I was happily working as a janitor for @|Pete-Migliorini| and
 attending @|Hudson-Valley-Community-College|, getting ready to transfer to the
 @|ILR-School-at-Cornell| the next year.
One night, between cleanings, I got talking to the security guard (Rusty?)
 about life and college.
``Learn a trade,'' he said, ``something they can't take away from you.''
That was some excellent advice.
Next semester I took an intro programming course from @|Andrew-Hurd|,
 who suggested we students code on paper first instead of throwing ideas at the
 Java compiler.
What an idea.
The following Fall at Cornell, my housemates
 @|Kevin-Dolan|, @|Dan-Rothenberg|, @|Dave-Viera|, and @|Mark-Vigeant|
 told me to minor in computing rather than physics.
``Jobs are good in CS; you can earn as much as the majors.''
@|Nicole-Roy| explained the degree requirements, and things snowballed from there.
@|Thorsten-Joachims| sparked my interest in programming languages by remarking
 that there is no Moore's Law for humans.
@|Ramin-Zabih| taught me a first decent language (@|OCaml-3.12|).
@|Nate-Foster| hired me as a teaching assistant; did he know I was an ILR major?
Classmates @|Ben-Carriel| and @|Sam-Park|, among many others, inspired harder and better work.
@|Ross-Tate| helped me to succeed at a first research project and to
 apply broadly for graduate school.
@|Ehsan-Hoque| insisted I go somewhere new, really anywhere but Cornell.
@|Fabian-Muehlboeck| recommended the @|PRL-Lab-at-Northeastern|.
Thank you all.

@; I tried to fit all but sometimes in life you have to give up.

@; Gee, remembering all this good advice makes me wonder how much I missed.
