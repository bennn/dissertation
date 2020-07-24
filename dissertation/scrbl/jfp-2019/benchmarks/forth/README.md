forth
===


Note: this benchmark runs extremely quickly fully-typed and fully-untyped.
This is because its difficult to make slower without making the worst-case
 configurations _extremely_ slow


History
---

Original program: <https://github.com/bennn/forth>

Original benchmark: <https://github.com/nuprl/gradual-typing-performance>

Differences from original:

- `main.rkt` parse data outside main computation
- `main.rkt` added loops to increase runtime, but worst-case is still ~200 sec.
