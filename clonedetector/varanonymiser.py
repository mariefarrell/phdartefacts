#!/usr/bin/python3
## -*- coding: utf-8 -*-
import sys
import os.path
import re

from eventb import BSentence
from bumparser import process_path

'''  This is the code that handles the anonymisation of variable names.
     The key function here is AnonymisedSentence.anonymise()
'''

class VarIndex(object) :
    ''' Maps a variable to a unique index; mainly I use this for sorting,
    since longer variable names must be replaced before shorter ones.
    Really this is just a helper class for AnonymisedSentence.
    '''
    # Regexp to recognise identifiers:
    ident_pattern = re.compile('[A-Za-z]\w*', re.UNICODE)
    def __init__(self, index, name) :
        self.name = name
        self.len = 0-len(name)  # For sorting: want longer variable names replaced first
        self.index = index      # Unique index number
    def __str__(self) :
        return '%s->%s' % (self.name, self.index)
    @classmethod
    def collect_variable_names(cls, orig_str) :
        seen_already = { }  # Maps name to a VarIndex instance
        for match in cls.ident_pattern.finditer(orig_str):
            varname = match.group()
            vi = VarIndex(len(seen_already), varname)
            seen_already.setdefault(varname, vi)  # Record if not already seen
        return seen_already.values()

   
class AnonymisedSentence(BSentence) :
    ''' Wraps a single sentence in a machine definition.
    Handles the replacement of actual variables with dummies.
    '''
    def __init__(self, orig_sentence) :
        super().__init__(orig_sentence.event_name, orig_sentence.kind, \
                         orig_sentence.label, orig_sentence.body)
        self.orig_vars = [ ]    # List of variables that have been anonymised
        self.anon_str = None 
        self._anonymise()
    @staticmethod    
    def _make_dummy_name(vi) :
        return '${}'.format(str(vi.index))
    def _anonymise(self) :
        ''' Changes variable names to $1, $2 etc. in order of appearance. 
        Results are stored in fields self.orig_vars and self.anon_str
        '''
        self.anon_str = '%s' % self.body  
        if self.is_event():
            self.anon_str = '[%s] %s' % (os.path.basename(self.event_name), \
                                         self.anon_str)
        # First of all, collect the original variable names from the str:
        replacements = VarIndex.collect_variable_names(self.anon_str)
        # Store a copy of the var names (in their original order):
        replacements = sorted(replacements, key=lambda vi:vi.index)
        self.orig_vars = [vi.name for vi in replacements]
        # Now repeatedly do the replacements:
        replacements.sort(key=lambda vi:vi.len) # Replace longer names first
        for vi in replacements :
            new_name = AnonymisedSentence._make_dummy_name(vi)
            self.anon_str = self.anon_str.replace(vi.name, new_name)
    def get_body(self):
        return self.anon_str
    def __str__(self) :
        str = '%s ' % self.kind.name
        if self.is_event():
            str += '[%s] ' % self.event_name
        str += '%s' % self.body
        str += (' -> ' + self.anon_str)
        str += (' via ' + ','.join(self.orig_vars))
        return str

def anonymise_machine(sen_list) :
    ''' Transform a machine to a list of AnonymisedSentence objects.
    '''
    return [AnonymisedSentence(sen) for sen in sen_list]
        
def anonymise_events(sen_dict) :
    ''' Transform a machine to a dict, mapping events to AnonymisedSentence-lists
    Option to add the machine's variants/invariants to each event.
    '''
    anon_dict = { }
    for ename, esen_list in sen_dict.items():
        anon_dict[ename] =anonymise_machine(esen_list)
    return anon_dict
          

if __name__ == "__main__":
    machines = [ ]
    for path in sys.argv[1:] :
        machines.extend(process_path(path))
    for mac in machines :
        print('###', mac.name)
        print('\n'.join([str(m) for m in anonymise_machine(mac)]))
        
