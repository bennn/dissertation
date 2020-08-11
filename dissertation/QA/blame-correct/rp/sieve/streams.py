@fields({'first': int, 'rest': Dyn})
class Stream:
    first = -1
    rest = lambda: Stream()

def make_stream(hd:int, thunk:Function([], Stream))->Stream:
    st = Stream()
    st.first = hd
    st.rest = thunk
    return st

def stream_unfold(st:Stream)->Tuple(int, Stream):
    return (st.first, st.rest())

def stream_get(st:Stream, i:int)->int:
    hd, tl = stream_unfold(st)
    while(i > 0):
        i -= 1
        hd, tl = stream_unfold(tl)
    return hd


