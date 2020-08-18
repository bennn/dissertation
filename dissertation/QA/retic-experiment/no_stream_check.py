from no_stream_check_help import Stream, helper

def main()->Stream:
    def thunk(x:Dyn)->Stream:
        return x.tl
    return helper(thunk)

main()
