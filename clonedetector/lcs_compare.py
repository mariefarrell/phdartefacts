#!/usr/bin/python3

import os
import sys
import time

import bumparser
import eventb
import tokeniser 

SEP = ':'  # separator for csv file
LOWEST_RELEVANT_MATCH = 2  # Ignore events and matches <= this size


def lcsubstring_length(s1, s2):
    ''' Return the length of the longest common subsequence of s1 and s2.
    See: https://en.wikipedia.org/wiki/Longest_common_subsequence_problem 
    '''
    m = len(s1)
    n = len(s2)
    max_so_far = 0
    count = [ [0]*(n) for i in range(m) ]
    for i in range(m):
        for j in range(n):
            if s1[i].matches(s2[j]):
                if i==0 or j==0:
                    count[i][j] = 1
                else:
                    count[i][j] = count[i-1][j-1] + 1
            if count[i][j] > max_so_far:
                max_so_far = count[i][j]
    return max_so_far


def lcsubseq_length(s1, s2):
    ''' Return the length of the longest common subsequence of s1 and s2.
    See: https://en.wikipedia.org/wiki/Longest_common_subsequence_problem 
    '''
    m = len(s1)
    n = len(s2)
    # Note that the indices in count are one ahead of those in s1/s2
    count = [ [0]*(n+1) for i in range(m+1) ]
    for i in range(m):
        for j in range(n):
            if s1[i].matches(s2[j]):
                    count[i+1][j+1] = count[i][j] + 1
            else:
                count[i+1][j+1] = max(count[i+1][j], count[i][j+1])
    return count[m][n]


def compare(bsen1, bsen2, threshold=0):
    mkperc = lambda m,t: '{:.2f}'.format(m*100.0/len(t))
    str_match = lcsubstring_length(bsen1, bsen2)
    if str_match < threshold:
        return None
    seq_match = lcsubseq_length(bsen1, bsen2)
    return [str_match, 
            mkperc(str_match, bsen1), 
            mkperc(str_match, bsen2),
            mkperc(2*seq_match, bsen1+bsen2)
            ]


def compare_all(bsen_dict, threshold=0) :
    ''' Compare BSen-list in the dict with its successors in that dict.
        Report results (over the threshold), one line for each comparison.
    '''
    names = sorted(list(bsen_dict.keys()))
    for i, name1 in enumerate(names) :
        bsen1 = bsen_dict[name1]
        if eventb.count_actions(bsen1) <= threshold:
            continue
        anon_bsen1 = tokeniser.anonymise_bsen_list(bsen1)
        for name2 in names[(i+1):] :
            bsen2 = bsen_dict[name2]
            if  eventb.count_actions(bsen2) <= threshold:
                continue
            anon_bsen2 = tokeniser.anonymise_bsen_list(bsen2)
            # Now do the comparisons
            cmp1 = compare(bsen1, bsen2, threshold)
            if not cmp1:
                continue
            cmp2 = compare(anon_bsen1, anon_bsen2)
            sline = [name1, len(bsen1), name2, len(bsen2)] + cmp1 + cmp2
            print(SEP.join(str(s) for s in sline))



   
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
    # Find the clones:
    compare_all(bsen_dict, LOWEST_RELEVANT_MATCH)
