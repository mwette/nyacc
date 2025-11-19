#!/usr/bin/env python
#
# Copyright (C) 2015,2025 - Matthew Wette
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

import os
import pdb

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

def unread(f0, ch):
    f0.seek(f0.tell()-1,0)

def eof(f):
    return f.tell() == os.fstat(f.fileno()).st_size


# This is not pretty, being a translation of the scheme version, but I don't
# want to get into clean design in all the target languages. -- Matt

def read_c_ident(f0, ch):
    cfirst = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
    cafter = cfirst + "0123456789"
    if ch not in cfirst:
        return False
    else:
        ids = ch
        ch = f0.read(1)
        while ch in cafter:
            ids = ids + ch
            ch = f0.read(1)
        unread(f0, ch)
        return ids

def read_num(f0, ch):
    ns = ""
    tt = '$fixed'
    st = 0
    while True:
        if st == 0:
            # todo: .
            if len(ch) == 0:
                st = 5
            elif ch.isdigit():
                ns += ch
                st = 1
            else:
                return (False, None)
        elif st == 1:
            if len(ch) == 0:
                st = 5
            elif ch.isdigit():
                ns += ch
            elif ch == '.':
                ns += ch
                tt = '$float'
                st = 2
            else:
                st = 5
        elif st == 2:
            if len(ch) == 0:
                st = 5
            elif ch.isdigit():
                ns += ch
            elif ch in 'eEdD':
                ns += ch
                st = 3
            else:
                st = 5
        elif st == 3:
            if len(ch) == 0:
                st = 5
            elif ch in '+-':
                ns += ch
                st = 4
            elif ch.isdigit():
                ns += ch
                st = 4
            else:
                raise Exception("syntax error")
        elif st == 4:
            if len(ch) == 0:
                st = 5
            elif ch.isdigit():
                ns += ch
            else:
                st = 5
        if st == 5:
            unread(f0, ch)
            return (tt, ns)
        ch = f0.read(1)

            
class Lexer:

    def __init__(self, symtab, kwdtab, csqtab):
        self.symtab = symtab
        self.kwdtab = kwdtab
        self.csqtab = csqtab
        self.strlit = { '"': '"', }
        self.chrlit = { "'": "'", }

    def set_input(self, f0):
        self.f0 = f0
        return

    def is_eof(self, tok):
        tv,sv = tok
        return tv == self.symtab['$end']

    def read_char(self):
        ch = self.f0.read(1)
        return ch

    def unread_char(self, ch):
        return self.f0.seek(self.f0.tell()-1,0)
        
    def pushback(self, s):
        """ pushback string"""
        for ix in range(len(s)-1,-1,-1):
            self.unread_char(s[ix])
        return

    def read_ident(self, ch):
        ids = read_c_ident(self.f0, ch)
        if ids:
            if ids in self.kwdtab:
                return (self.kwdtab[ids], ids)
            else:
                return (self.symtab['$ident'], ids)
        else:
            return (False, None)

    def read_num(self, ch):
        nty, nst = read_num(self.f0, ch)
        if nty == False:
            return (False, None)
        else:
            return (self.symtab[nty], nst)

    def read_string(self, ch):
        if ch not in self.strlit: return (False, None)
        end_ch = self.strlit[ch]
        stng = ""
        while True:
            ch = self.read_char()
            if ch == '\\':
                stng += '\\' + esc_char(self.read_char())
            elif ch == end_ch:
                break
            else:
                stng += ch
        return (self.symtab['$string'], stng)

    def read_chlit(self, ch):
        if ch not in self.chrlit: return (False, None)
        ch = self.read_char()
        if ch == '\\': ch = '\\' + esc_char(self.read_char())
        self.read_char() # end
        return (self.symtab['$chlit'], ch)

    def read_comm(self, ch):    
        if ch != '/': return (False, None)
        st = [ch, self.read_char()]
        if st == "//":
            nd = "\n"
        elif st == "/*":
            nd = "*/"
        else:
            self.unread_char(st[1])
            self.unread_char('/')
            return (False, None)
        chl = ""
        ix = 0
        while True:
            ch = self.read_char()
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
        val, lxm = self.read_comm(ch)
        return self.read_comm(ch)

    def read_chseq(self, ch):
        chs = ch
        node = self.csqtab
        while True:
            ch = chs[-1]
            if ch in node:
                nd = node[ch];
                if len(nd) == 1:
                    #assert('else' in nd)
                    return (nd['else'], chs)
                else:
                    chs += self.read_char()
                    node = nd
                    continue
            elif node.get('else',False):
                self.unread_char(chs[-1])
                return (node['else'], chs[:-1])
            else:
                self.pushback(chs)
                return (False,None)
    
    def gettok(self):
        ch = self.read_char()
        while True:
            if eof(self.f0):
                return (self.symtab['$end'], "<eof>")
            elif ch.isspace():
                ch = self.read_char()
                continue
            elif ch == '\n':
                ch = self.read_char()
                continue
            v,s = self.skip_comm(ch)
            if v:
                ch = self.read_char()
                continue
            v,s = self.read_ident(ch)
            if v:
                return (v,s)
            v,s = self.read_num(ch)
            if v:
                return (v,s)
            v,s = self.read_string(ch)
            if v:
                return (v,s)
            v,s = self.read_chlit(ch)
            if v:
                return (v,s)
            v,s = self.read_chseq(ch)
            if v:
                return (v,s)
            pdb.set_trace()
            print("*** scanning error, exiting")
            sys.exit(1)
            
    
# --- last line ---
