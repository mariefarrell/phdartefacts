#!/usr/bin/python3

import sys
import os.path

import bumparser

class IdentInfo(object):
    def __init__(self, pos):
        self.count = 0
        self.pos = pos

class Tokeniser(object):
    keywords = ['BOOL', 'FALSE', 'TRUE', 
                'bool', 'card', 'dom', 'finite', 'id', 
                'inter', 'max', 'min', 'mod', 'pred', 
                'prj1', 'prj2', 'ran', 'succ', 'union'
                u'\u2115', u'\u2115\u0031',   # N, N1
                u'\u2119', u'\u2119\u0031',   # P, P1
                u'\u2124'                     # Z
    ]
    punctuation = [ '(', ')', '{', '}', '[', ']', 
                    ',', '.',  u'\u2223'   # u'\u2223' is 'such-that'
    ]
    quantifier = [u'\u2200', u'\u2203', u'\u03BB']  # forall, exists, lambda 
    constant = [u'\u22A4', u'\u22A5', u'\u2205' ]   # true, false, emptyset
    def __init__(self, anonymise=False, keep_whitespace=False):
        self.ident_list = { }
        self.anonymise = anonymise
        self.anonymise_literals = anonymise  # Edit as required
        self.keep_whitespace = keep_whitespace
    def _gobble_while(self, input, wanted):
        i = 0
        while i<len(input) and wanted(input[i]):
            i += 1
        return i
    def _anonymise_name(self, ident) :
         pos = self.ident_list[ident].pos
         return '${}'.format(str(pos))
    def _record_ident(self, ident):
        if not ident in self.ident_list :
            self.ident_list[ident] = IdentInfo(len(self.ident_list))
        self.ident_list[ident].count += 1
    def tokenise(self, input):
        token_list = [ ]
        i = 0
        while i<len(input):
            # Eat up whitespace
            oldi = i
            i += self._gobble_while(input[i:], lambda s: s.isspace())
            if i>oldi and self.keep_whitespace:
                token_list.append(input[oldi:i])
            if i >= len(input) : break
            # Multi-character tokens:
            if input[i].isidentifier():
                oldi = i
                i += self._gobble_while(input[i:], lambda s: s.isidentifier()
                                        or s.isdigit())
                ident = input[oldi:i]
                if ident in ['FALSE', 'TRUE'] and self.anonymise_literals:
                    ident = '$B'
                elif not ident in Tokeniser.keywords and self.anonymise:
                    self._record_ident(ident)
                    ident = self._anonymise_name(ident)
                token_list.append(ident)
            elif input[i].isdigit():
                oldi = i
                i += self._gobble_while(input[i:], lambda s: s.isdigit())
                intlit = input[oldi:i]
                if self.anonymise_literals:
                    intlit = '$I'
                token_list.append(intlit)
            else: # assume a one-token operator at this point
                token_list.append(input[i])
                i += 1
        return token_list
    def tokenise_list(self, inputlist):
        return [self.tokenise(input) for input in inputlist]

    

def anonymise_bsen_list(bsenlist):
    ''' Copy list, but replace BSentence body with an anonymised version.
        The same tokeniser is used for the whole list.
    '''
    t = Tokeniser(True, True)  # Variable counts shared for the whole list.
    res = []
    for bsen in bsenlist:
        newbody = t.tokenise(bsen.get_body())
        bsen = bsen.clone(''.join(newbody))
        res.append(bsen)
    return res

def anonymise_bsen_dict(sen_dict) :
    ''' Anonymise each BSentence-list in a dictionary of them.
        Each entry gets a new tokeniser (so variable-count is reset).
    '''
    return { name : anonymise_bsen_list(bsenlist)
             for name, bsenlist in sen_dict.items() }
    
def tokenise_sen_dict(sen_dict) :
    ''' Tokenise each sentence-list in a dictionary of them.
        Returned dict values are lists of token-lists (not BSentences).
    '''
    return { name : Tokeniser().tokenise_list(senlist)
             for name, senlist in sen_dict.items() }
      

if __name__ == "__main__":
    # Read in the machines:
    pathnames = [ ]
    switches = [ ]
    for opt in sys.argv[1:] :
        if opt.startswith('-') :
            switches.append(opt)
        else:
            pathnames.append(opt)
    # Distill into sets-of-sentences, grouped by machine or events:
    group_on = 'M'
    if '-event' in switches:
        group_on = 'E'
    if '-context' in switches:
        group_on = 'C'
    bsen_dict = bumparser.get_sentences(pathnames, 
                                        group_on,
                                        '-init' in switches, 
                                        '-inv' in switches)
    is_anon = '-anon' in switches
    if is_anon:
        sen_dict = anonymise_bsen_dict(bsen_dict)
    else:
        sen_dict = tokenise_sen_dict(bsen_dict)
    for name, senlist in sen_dict.items():
        print('#####', name)
        for sen in senlist:
            print('\t', sen)
        
