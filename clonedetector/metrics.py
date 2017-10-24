#!/usr/bin/python3

import sys
import os.path
from bumparser import process_path, get_context_sentences, get_machine_sentences, get_event_sentences, get_ps_sentences

from eventb import BContext, BMachine, BProofStatus

''' Some simple metrics '''

SEP=':'

def print_context_metrics(wrapper, ctx):
    line = ['C', ctx.path, ctx.name]
    line += [wrapper.depth, wrapper.refined_by, wrapper.seen_by]
    line += [len(x) for x in [ctx.extends,
                              ctx.sets, ctx.constants, ctx.axioms]]
    line += [len(ctx.sets) + len(ctx.constants) + len(ctx.axioms)]#number of sentences
    print(SEP.join([str(d) for d in line]))

def print_proof_metrics(wrapper, proof):
    line = ['P', proof.path, proof.name]
    line += [proof.ps.count((proof.name,"false"))]
    line += [proof.ps.count((proof.name,"true"))]
    line+= [proof.ps.count((proof.name,'ref'))]
    print(SEP.join([str(d) for d in line]))

def get_event_metrics(evt):
    ''' Return a list of metric values for a single event '''
    line = [evt.convergence.value]
    line += [1 if evt.isExtended else 0]
    line += [len(x) for x in [evt.refines, 
                              evt.parameters, evt.witnesses,
                              evt.guards, evt.actions]]
    line +=[len(evt.parameters) + len(evt.witnesses) + len(evt.guards) + len(evt.actions)]
    return line

def print_event_metrics(mac):
    ''' Prints metrics for all events in the machine, returns overall totals '''
    mtotals = None
    for evt in mac.events.values():
        emetrics = get_event_metrics(evt)
        eline = ['E', mac.path, mac.name, evt.event_name] \
                + [str(m) for m in emetrics]
        print(SEP.join(eline))
        if not mtotals :  # Initialise running totals (now I know # of metrics):
            mtotals = 0  #len(emetrics)
        mtotals = mtotals + emetrics[7]#[t+e for (t,e) in zip(mtotals, emetrics[3])]
    return mtotals

def print_machine_metrics(wrapper, mac):
    mline = ['M', mac.path, mac.name]
    mline += [wrapper.depth, wrapper.refined_by]
    mline += [len(x) for x in [mac.refines,
                               mac.sees, mac.variables,
                               mac.invariants, mac.variants, mac.events]]
    mtotals = print_event_metrics(mac)
    mline += [mtotals] # mline.extend(mtotals)
    mline += [len(mac.variables)+len(mac.variants)+ len(mac.invariants)+mtotals]
    print(SEP.join([str(d) for d in mline]))


def print_metrics(comp):
    if (type(comp.component) == BContext) :
        print_context_metrics(comp, comp.component)
    elif (type(comp.component) == BMachine) :
        print_machine_metrics(comp, comp.component)
    elif (type(comp.component) == BProofStatus) :
        print_proof_metrics(comp, comp.component)

class BComponentWrapper(object):
    ''' Wrapper around a component to count references to it.
        We may see the reference before we see the component, OK either way.
    '''
    def __init__(self):
        self.component = None
        self.seen_by = 0  # Relevant for contexts only
        self.refined_by = 0  # Used for machines and contexts
        self.depth = 0
    def set_component(self, component):
        assert component and not self.component, 'Cannot overwrite component'
        self.component = component
    def get_depth(self, comp_dict):
        parents = self.component.get_parents()
        if self.depth==0 and len(parents)>0:
            # Get my parents' (max) depth, and add 1
            my_dir = os.path.dirname(self.component.fullname())
            for par_name in parents:
                par_path = os.path.join(my_dir, par_name)
                parent = comp_dict[par_path]
                self.depth = max(self.depth, parent.get_depth(comp_dict)+1)
        return self.depth
    def __str__(self):
        return self.component.fullname()
    
class CouplingCounter(object):
    def __init__(self):
        self.components = { }
    def process_component(self, component):
        n = component.fullname()
        if not n in self.components:
            self.components[n] = BComponentWrapper()
        self.components[n].set_component(component)
        if (type(component) == BContext) :
            for ext in component.extends:
                self.add_refines(os.path.dirname(n), ext)
        elif (type(component) == BMachine) :
            for ext in component.refines:
                self.add_refines(os.path.dirname(n), ext)
            for ext in component.sees:
                self.add_sees(os.path.dirname(n), ext)
    def add_refines(self, path, ext):
        n = os.path.join(path, ext)
        if not n in self.components:
            self.components[n] = BComponentWrapper()
        self.components[n].refined_by +=1
    def add_sees(self, path, ext):
        n = os.path.join(path, ext)
        if not n in self.components:
            self.components[n] = BComponentWrapper()
        self.components[n].seen_by +=1
    def __getitem__(self, key):
        return self.components[key]
    def __str__(self):
        cstr = [ ]
        for name, comp in self.components.items():
            data = [name, comp.seen_by, comp.refined_by]
            if not comp.component:
                data.append('***')
            cstr.append(SEP.join([str(d) for d in data]))
        return '\n'.join(cstr)
        
if __name__ == "__main__":
    cc = CouplingCounter()
    for pathname in sys.argv[1:] :
        for mac in process_path(pathname) :
            cc.process_component(mac)
    for component in cc.components.values():
        component.get_depth(cc)
        print_metrics(component)


