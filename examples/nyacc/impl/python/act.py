# act.py

def aDEF(*rest):
    if len(rest) > 0:
        return rest[0]
    else:
        return []

def a001(S1, *rest):
    return tl2list(S1)

def a011(S1, *rest):
    return S1

def find_act(name):
    d = globals()
    if d.has_key(name):
        return d[name]
    else:
        return aDEF

# --- last line ---
