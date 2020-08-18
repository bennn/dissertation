from no_blame_help import Stream, helper

def main(x:Stream)->List(str):
    def thunk()->List(str):
        return x.tl
    return helper(thunk)

print(main(Stream(1))[0])
