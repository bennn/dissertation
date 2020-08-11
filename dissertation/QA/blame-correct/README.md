Is my blame implementation correct?

### from samth-2020-07-23.txt

> 4. Have you replicated the numbers that Vitousek et al got for their
> (smaller) benchmarks? Particularly on the blame side, since the larger
> benchmarks get worse numbers, it seems important to check that.

I know their blame is also slow.

Just now I ran their `chaos` 'go` and `spectralnorm` and see roughly-similar
overhead between Retic fully-typed and Retic with blame (3x, 2x, 1.5x).

But I also converted our TR sieve program to Retic. I had trouble with the
stack limit at first. After that, adding blame took the program up from 40s
to a timeout after 10min (I killed it).

- - -

snake untyped 719
snake 10101111 normal 12779 (17.8x)
snake 10101111 unsafe 10925 (15.2x)


- - -


chaos transient
1.6675470000000001
1.67934
1.6625699999999999
1.6599590000000002
1.6580850000000003
1.6649479999999999

chaos mgd-transient
5.175612
5.195894
5.054855
5.187823
5.075536
5.193404999999999


go transient
10.767317000000002
10.329541
10.320514000000001
10.402410999999999
10.016009999999998
10.104163999999999

go blame
21.230272
21.51616
20.903669
21.364932
22.077095
21.48814

spectralnorm transient
2.284129
2.32398
2.611606
2.400873
2.422697
2.329733

spectralnorm mgd-transient
3.01538
3.038865
3.0241269999999996
3.180621
2.962125
2.975569


### after Matthias, Christos, Lukas meeting

> port the retic benchmarks bottom-up, take 1/2 day, how much slower are
> they with blame?

retic benchmarks, bottom-up

- [X] call_method_slots --- PS, Zeina has data for this benchmark, but we
  removed it for the PLATEAU submission (5df1b50e42d1e031b76de49d7c6f4c753af588b0)
  There is no explanation,
   but probably to make the figures fit on the page (3 x 5?),
   and because it's only 1-line different from call_method,
   and call_method is more normal (not saying much)

- [X] fannkuch
- [ ] nqueens
- [ ] nbody
- [ ] pidigits
- [ ] spectralnorm
- [ ] call_simple
- [ ] float
- [ ] call_method
- [ ] go
- [ ] meteor
- [ ] pystone
- [ ] chaos


