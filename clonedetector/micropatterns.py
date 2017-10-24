#!/usr/bin/python3

import sys

import bumparser
import eventb
import tokeniser

SEP = ':'
EVENT_SIZE_LOWER_BOUND = 2  # Ignore events with <= 2 sentences
PATTERN_MUST_OCCUR = 1      # Assume a pattern must occur more than twice

class Pattern(object):
    def __init__(self, orig_str, size):
        self.orig_str = orig_str
        self.size = size
        self.occurs_in = [ ]
    def add_occur(self, where_found):
        self.occurs_in.append(where_found)
    def count(self):
        return len(self.occurs_in)
    def __str__(self):
        data = [self.count(), self.size, self.orig_str, self.occurs_in]
        return SEP.join([str(d) for d in data])


class PatternCollection(object):
    def __init__(self):
        self.count = { }  # Map pattern to count
    def add_counts(self, bsen_dict):
        for evtname, bsen_list in bsen_dict.items():
            if eventb.count_actions(bsen_list) <= EVENT_SIZE_LOWER_BOUND:
                continue
            ss_str = '; '.join([line.get_body() for line in bsen_list])
            no_sp_str = ss_str.replace(' ','')
            if not no_sp_str in self.count:
                self.count[no_sp_str] = Pattern(ss_str, len(bsen_list))
            self.count[no_sp_str].add_occur(evtname)
    def __str__(self):
        return '\n'.join([str(c) for c in self.count.values()
                          if c.count() > PATTERN_MUST_OCCUR])

   
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
        bsen_dict = tokeniser.anonymise_bsen_dict(bsen_dict)
    # Collect and count the micro-patterns:
    pc = PatternCollection()
    pc.add_counts(bsen_dict)
    print(pc)

