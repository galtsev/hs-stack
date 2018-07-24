import sys

def run():
    sm = {}
    for ln in sys.stdin:
        gr,v = ln.split(":")
        sm[gr] = sm.get(gr,0)+int(v)
    sm = list(sm.items())
    sm.sort()
    for k,v in sm:
        print("{}:{}".format(k,v))

run()