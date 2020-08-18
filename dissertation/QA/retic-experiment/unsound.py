
def set_first(x):
    x[0] = "hello"
    return

def mylist(xx:List(int))->Dyn:
    return xx

def main():
    cycles = mylist([0, 1, 2])
    set_first(cycles)
    return cycles[0] < 1

main()
