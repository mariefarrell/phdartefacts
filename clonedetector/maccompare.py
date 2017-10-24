#!/usr/bin/python3

import os
import sys
import time

import varanonymiser 
import eventb
import bumparser

class VariableRenaming(object) :
    ''' Represent a variable-renaming possibility; count the votes for it.
    The renaming is represented as a dictionary of variables.
    '''
    all_votes = { }  # Keep track of all the VariableRenaming objects
    @classmethod
    def clear(cls) :
        cls.all_votes.clear()
    @classmethod
    def record_vote(cls, my_renaming, ur_renaming) :
        ''' Record a single vote for this variable-variable mapping '''
        v = VariableRenaming(my_renaming, ur_renaming)
        v = cls.all_votes.setdefault(v._as_key(), v) # May be there already
        v.vote_count += 1   # One more vote
    @classmethod
    def get_all_votes(cls) :
        return list(cls.all_votes.values())
    def __init__(self, my_sentence, ur_sentence) :
        self.my_sentence = my_sentence
        self.matches = dict(zip(my_sentence.orig_vars,ur_sentence.orig_vars))
        self.vote_count = 0
    def _as_key(self) :
        ''' Return a string to be used as a key in all_votes '''
        str = ['%s->%s' % (myv, self.matches[myv]) for myv in self.my_sentence.orig_vars]
        return ','.join(str)
    def find_first_incompatability(self, other):
        ''' Return the first variable where I'm incompatible with other.
        Return None if there isn't one (i.e. I'm compatible with all)
        '''
        for (v1,v2) in self.matches.items():
            if other.matches.get(v1, v2) != v2 : # NB: OK if other doesn't have v1
                return v1  # Incompatibility on this variable
        return None # No incompatibility
    def is_compatible_with(self, other):
        return self.find_first_incompatability(other) is None
    def is_compatible_with_all(self, otherList) :
        for other in otherList :
            if not self.is_compatible_with(other) :
                return False
        return True
    def __str__(self) :
        return '%d votes: %s: %s' % (self.vote_count, self.my_sentence.name(), self._as_key())


   
class SentenceSet(object) :
    ''' Represents a processed set-of-sentences; provides comparison with other such sets.
    These can be the sentences in a whole machine, or just a single event.
    '''
    def __init__(self, name, lines) :
        self.name = name
        # Sort the lines based on the length of the line, shortest first:
        self.lines = sorted(lines, key=lambda sen: len(sen))
    def __str__(self) :
        mstr = ('### %s\n' % self.name)
        mstr += '\n'.join(map(str, self.lines))
        return mstr
    def _compare_same_length(self, my_line, other, o_pos) :
        ''' Compare my_line with lines of the same length in other[o_pos:] 
        Record a vote for any that match.
        Assumes lines in other are sorted by length.
        '''
        j = o_pos
        while j < len(other.lines) and len(my_line) == len(other.lines[j]) :
            if my_line.matches(other.lines[j]):
                VariableRenaming.record_vote(my_line, other.lines[j])
            j += 1
    def compare_with(self, other) :
        ''' Compare each line in self with each line in other
        - provided they have the same length (so assumes both are sorted).
        Returns a list of VariableRenaming.
        '''
        VariableRenaming.clear()
        s_pos = 0   # Position in self.lines
        o_pos = 0   # Position in other.lines
        while s_pos < len(self.lines) and o_pos < len(other.lines) :
            if len(self.lines[s_pos]) > len(other.lines[o_pos]) :
                o_pos += 1  # ... They should move on
            elif len(self.lines[s_pos]) < len(other.lines[o_pos]) :
                s_pos += 1  # ... I should move on
            else : # Both lines have the same length
                self._compare_same_length(self.lines[s_pos], other, o_pos)
                s_pos += 1  # ... I should move on
        return VariableRenaming.get_all_votes()
    def __len__(self):
        return len(self.lines)



############################    
### Breadth-first Search ###
############################


class SearchTree(object) :
    ''' Represent a single path in the breadth-first search tree.
    To avoid permutations, we keep the order of the original match-set.
    '''
    def __init__(self, doneVotes, todoVotes) :
        self.doneVotes = doneVotes  # List of parent elements in the current path
        self.todoVotes = todoVotes  # Nodes that could be added as children to this path
    def calc_total_votes(self) :
        return sum(v.vote_count for v in self.doneVotes)
    def expand_by_one(self) :
        ''' Try adding each element (in turn) from todoVotes to doneVotes '''
        survivors = [ ]
        for i,todo in enumerate(self.todoVotes) :
            if todo.is_compatible_with_all(self.doneVotes) :
                survivors.append(SearchTree(self.doneVotes+[todo],
                                           self.todoVotes[i+1:])) # Keeps order
        return survivors
    def height(self):
        return len(self.doneVotes)
 
def best_possible_subset(todoList, width_limit=None, noisy=False) :
    ''' Basically a brute-force breadth-first search for the best possible subset of todoList.
    Start with subsets of size 1, then all (compatible) subsets of size 2, etc.
    Can control the search size by restricting the max width of the tree (extras just dumped).
    '''
    limit_was_applied = False
    if noisy: print('BPS: starting with', len(todoList), 'potenital renamings')
    # First collect singleton-sets into a todo list:
    todoMaybes = [ ]
    for i,v in enumerate(todoList) :
        todoMaybes.append(SearchTree([v], todoList[i+1:]))
    bestMaybe = todoMaybes[0]  # Pick any one...
    # Now iteratively process the todo list until it is empty:
    while todoMaybes :
        if noisy: print('BPS: looping with', len(todoMaybes))
        if width_limit and len(todoMaybes) > width_limit:
            if noisy: print('BPS: capped at %d, lost %d' % (width_limit, len(todoMaybes)-width_limit))
            del todoMaybes[width_limit:]
            limit_was_applied = True
        # Try add one element to each set:
        newMaybes = [ ]
        for todo in todoMaybes :
            nextRound = todo.expand_by_one()
            if nextRound :
                newMaybes.extend(nextRound)
            else : # No expansions possible, so this is the best this branch can do
                if todo.calc_total_votes() > bestMaybe.calc_total_votes() :
                    bestMaybe = todo
                # Branch is pruned by not being added to newMaybes.
        # Expanded all match-sets by 1, so go again:
        todoMaybes = newMaybes
    # Finished; all possible subsets are now in doneMaybes
    return (bestMaybe.doneVotes, limit_was_applied)
        



#######################    
### Helper Routines ###
#######################


def compare_all(mi_list, howto_compare, cap=-1, noisy=False) :
    ''' Launch the search between all machines in the list and report the results 
    '''
    start_time = time.time()
    for i,mi in enumerate(mi_list) :
        for mi2 in mi_list[(i+1):] :
            votes = mi.compare_with(mi2)
            if votes :
                if noisy: print('----- Compare', mi.name, 'with', mi2.name)
                #print('\n'.join(map(str, votes)))
                (bestMatch, was_limited) = howto_compare(votes, cap, noisy)
                tot =  sum(v.vote_count for v in bestMatch)
                capped = 'Capped' if was_limited else ' '
                perc1 = '{:.2f}'.format(tot*100.0/len(mi))
                perc2 = '{:.2f}'.format(tot*100.0/len(mi2))
                print(':'.join(str(s) for s in [mi.name,mi2.name,tot,perc1,perc2,capped]))
                if noisy: print('---'+'\n#'.join([str(b) for b in bestMatch]))
    if noisy: print('--- Took %d seconds' % (time.time() - start_time))



def _write_one_compatible(voteList, filename) :
    ''' Write a compatibility matrix comparing all the votes 
    in voteList to each other.  Each written row has the number of votes, 
    then a list of 0 or 1 for compatible.
    '''
    print('Writing to', filename)
    with open(filename, 'w') as fh :
        for v1 in voteList :
            row_str = str(v1.vote_count)
            for v2 in voteList :
                if v1.is_compatible_with(v2) : row_str += ':1'
                else: row_str += ':0'
            fh.write(row_str+'\n')

def write_all_compatible(macList) :
    ''' Mainly for debugging: Compare each machine in macList 
    with all the others.  Write a compatibility matrix 
    to a suitably-named CSV file (one file per machine).
    '''
    for i,mi in enumerate(macList) :
        print(mi)
        for mi2 in mi_list[(i+1):] :
            votes = mi.compare_with(mi2)
            if votes :
                filename = '%s-%s-%s' % (mi.name, mi2.name, len(votes))
                filename = filename.replace('.bum','').replace(os.path.sep,'_')
                filename += '.csv'
                _write_one_compatible(votes, filename)


def read_machines(mac_list, is_anon=True, keep_init=False):
    senset_list = [ ]
    for mac in mac_list:
        msen_list = mac.get_all_sentences(keep_init)
        if is_anon:
            msen_list = varanonymiser.anonymise_machine(msen_list)
        senset_list.append(SentenceSet(mac.name, msen_list))
    return senset_list
                      

def read_events(mac_list, is_anon=True, keep_init=False, keep_invariants=False):
    ''' Returns a list of sentence-sets, one for each event '''
    senset_list = [ ]
    for mac in mac_list:
        if not isinstance(mac,eventb.BMachine): continue  # ignore contexts
        sen_dict = mac.get_event_sentences(keep_init, keep_invariants)
        if is_anon:
            sen_dict = varanonymiser.anonymise_events(sen_dict)
        for e_name, e_sens in sen_dict.items():
            senset_list.append(SentenceSet(e_name, e_sens)) 
    return senset_list


SEARCH_LIMIT = 500

   
if __name__ == "__main__":
    # Read in the machines:
    machines = [ ]
    operations = [ ]
    for opt in sys.argv[1:] :
        if opt.startswith('-') :
            operations.append(opt)
        else:
            machines.extend(bumparser.process_path(opt))
    # Distill into sets-of-sentences, grouped by machine or events:
    keep_init = '-init' in operations
    is_anon = '-anon' in operations
    keep_invariants = '-inv' in operations
    if ('-event' in operations):
        sen_list = read_events(machines, is_anon, keep_init, keep_invariants)
    else:  # Group on machines
        sen_list = read_machines(machines, is_anon, keep_init)
    # Find the clones:
    noisy = '-noisy' in operations
    if noisy:
        print('Read', len(sen_list), 'machines')
        print('Configured for', 'BFS, cut-off=', SEARCH_LIMIT)
    compare_all(sen_list, best_possible_subset, SEARCH_LIMIT, noisy)
    if '-matrix' in operations:
        write_all_compatible(sen_list)
