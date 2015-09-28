#!/usr/bin/env python
#
# lexer
#
#

import string

id_f = string.letters + "$_"
id_r = cf + string.digits

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

# This is not pretty, being a translation of the scheme version, but I don't
# want to get into clean design in all the target languages. -- Matt
class Lexer:

    def __init__(self, mtab):
        self.spaces = " \t\r\n"
        self.id_f = id_f
        self.id_r = id_r
        self.s_st = '"'
        self.s_nd = '"'

    def set_input(self, f0):
        self.f0 = f0

    def read(self):
        self.f0.read(1)

    def unread(self, ch):
        self.f0.seek(-1,1)

    def read_ident(self, ch):
        if ch not in self.id_f: return false
        chl = [ch]
        ch = self.read()
        while ch in self.id_r:
            chl.append(ch)
        f0.
        return ('$ident', chl)

    def read_num(self, ch):
        chl = ""
        tt = '$fixed'
        st = 0
        while True:
            if st == 0:
                if len(ch) == 0:
                    st = 5
                elif isdigit(ch):
                    chl.append(ch)
                    st = 1
                else:
                    return false
            elif st == 1:
                if len(ch) == 0:
                    st = 5
                elif isdigit(ch):
                    chl.append(ch)
                elif ch == '.':
                    chl.append(ch)
                    tt = '$float'
                    st = 2
                else:
                    st = 5
            elif st == 2:
                if len(ch) == 0:
                    st = 5
                elif isdigit(ch):
                    chl.append(ch)
                elif ch in 'eEdD':
                    #if chl[-1] == '.': chl.append('0')
                    chl.append(ch)
                    st = 3
                else:
                    #if chl[-1] == '.': chl.append('0')
                    st = 5
            elif st == 3:
                if len(ch) == 0:
                    st = 5
                elif ch in '+-':
                    chl.append(ch)
                    st = 4
                elif isdigit(ch):
                    chl.append(ch)
                    st = 4
                else:
                    raise Exception, "syntax error"
            elif st == 4:
                if len(ch) == 0:
                    st = 5
                elif isdigit(ch):
                    chl.append(ch)
                else:
                    st = 5
            elif st == 5:
                self.unread(ch)
                return (tt, chl)
            ch = self.read()

    def read_string(self, ch):
        if ch != self.s_st: return false
        chl = [ch]
        while True:
            ch = self.read()
            if ch == '\\':
                chl.append(esc_char(self.read()))
            elif ch == '"':
                break
            else:
                chl.append(ch)
        return ('$string', buf)

    def read_chlit(self, ch):
        if ch != self.c_st: return false
        ch = self.read()
        if ch == '\\': ch = esc_char(self.read())
        self.read()
        return ('$chlit', ch)

    def read_comm(self, ch):    
        if ch != '/': return false
        st = [ch, self.read()]
        if st == "//":
            nd = "\n"
        elif st == "/*":
            nd = "*/"
        else:
            self.unread(st[1])
            self.unread('/')
            return false
        chl = ""
        ix = 0
        while True:
            ch = self.read()
            if ix == len(nd):
                return ('$comm', chl)
            elif ch == nd[ix]:
                ix = ix + 1
                continue
            else:
                ix = 0
            chl.append(ch)
        pass

    def skip_comm(self, ch):
        return self.read_comm(ch)

    def read_chseq(self, ch):
        return false
    
    #ident-like

    def gettok(self, ):
        ch = self.read_char()
        while True:
            if ch == eof:
                sys.exit(0)
            elif ch.isspace():
                ch = self.read_char()
                continue
            p = self.read_comm(ch)
            if p:
                return p
            p = self.skip_comm(ch)
            if p:
                ch = self.read_char()
                continue
            p = self.read_ident(ch)
            if p:
                return p
            p = self.read_num(ch)
            if p:
                return p
            p = self.read_string(ch)
            if p:
                return p
            p = self.read_chlit(ch)
            if p:
                return p
            p = self.read_chseq(ch)
            if p:
                return p
            p = self.assq_ref_chrtab(ch)
            if p:
                return p
            print "*** ERROR"
            sys.exit(0)
            
    
# --- last line ---

