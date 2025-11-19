#!/usr/bin/env python
#
# Copyright (C) 2025 - Matthew Wette
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.

import pdb

def exec_n(func, narg, stk):
    if narg == 0:
        return func()
    elif narg == 1:
        return func(stk[-1])
    elif narg == 2:
        return func(stk[-2], stk[-1])
    elif narg == 3:
        return func(stk[-3], stk[-2], stk[-1])
    elif narg == 4:
        return func(stk[-4], stk[-3], stk[-2], stk[-1])
    elif narg == 5:
        return func(stk[-5], stk[-4], stk[-3], stk[-2], stk[-1])
    elif narg == 6:
        return func(stk[-6], stk[-5], stk[-4], stk[-3], stk[-2],
                    stk[-1])
    elif narg == 7:
        return func(stk[-7], stk[-6], stk[-5], stk[-4], stk[-3],
                    stk[-2], stk[-1])
    elif narg == 8:
        return func(stk[-8], stk[-7], stk[-6], stk[-5], stk[-4],
                    stk[-3], stk[-2], stk[-1])
    elif narg == 9:
        return func(stk[-9], stk[-8], stk[-7], stk[-6], stk[-5],
                    stk[-4], stk[-3], stk[-2], stk[-1])
    elif narg == 10:
        return func(stk[-10], stk[-9], stk[-8], stk[-7], stk[-6],
                    stk[-5], stk[-4], stk[-3], stk[-2], stk[-1])
    else:
        raise Exception("needs work")

def esc_char(c):
    if c == 'r':
        return '\r'
    elif c == 'n':
        return '\n'
    elif c == 't':
        return '\t'
    elif c == 'b':
        return '\b'
    else:
        return c

DEFRED = 1

class Parser:

    def __init__(self, lexer, tabled, actionv):
        self.lexr = lexer
        self.lenv = tabled['len']
        self.rtov = tabled['rto']
        self.patv = tabled['pat']
        self.actv = actionv
        self.skip_if_unexp = []
        self.user_hook = lambda stal, tval, state: None
        pass

    def runit(self, debug=False):
        state = [0]
        stack = [None]
        nval = None
        lval = None
        while True:
            if (nval is None) and (lval is None):
                lval = self.lexr.gettok()
            laval = nval if nval is not None else lval
            tval = laval[0]; sval = laval[1]
            stal = self.patv[state[-1]]
            stx = self.user_hook(stal, tval, state)
            if stx is not None:
                pass
            elif tval in stal:
                stx = stal[tval]
            elif (stx not in self.skip_if_unexp) and (DEFRED in stal):
                stx = stal[DEFRED]
            else:
                stx = None
            if debug: self.debug(stx, nval, tval, sval)
            if stx is None:
                if tval in self.skip_if_unexp:
                    nval = lval = None
                    continue
                else:
                    raise Exception("parse error")
            elif stx < 0:       # reduce
                gx = abs(stx)
                gl = self.lenv[gx]
                rz = exec_n(self.actv[gx], gl, stack)
                state = state[:-gl]
                stack = stack[:-gl]
                nval = (self.rtov[gx], rz)
            elif stx > 0:       # shift+goto
                state.append(stx)
                stack.append(sval)
                lval = lval if nval is not None else None
                nval = None
            else:
                return stack[-1]
            
    def debug(self, stx, nval, tval, sval):
        if nval is not None:
            print("parse: state %d, token %d" % (stx, tval))
        else:
            print("parse: state %d, token %s" % (stx, sval))
        pass


# --- last line ---
