from collections import OrderedDict
from enum import Enum
import os

''' Classes to represent the data in a machine or context.  
    All these classes are "plain ol' data" - no interesting functionality here.
'''


def _indent_list(label, a_list, sep1='\n\t', sep2='\n'):
    ''' I use this for pretty-printing the machines, events etc. later '''
    _indent = lambda s: s.replace('\n', '\n\t')
    if a_list:
        return [label + sep1 + _indent(sep2.join([str(s) for s in a_list]))]
    else:
        return []


class BSentence(object):
    ''' A single (possibly labelled) predicate/assignment/expression etc. '''

    Kinds = Enum('Kinds', 'action axiom guard invariant variant witness ')

    def __init__(self, event_name, kind, label, body):
        assert isinstance(kind, BSentence.Kinds), \
            'invalid sentence kind "%s"' % kind.name
        assert (not event_name) or \
            (kind in [BSentence.Kinds.action,
                      BSentence.Kinds.guard, BSentence.Kinds.witness]), \
            'invalid sentence kind %s in event %s' % (kind.name, event_name)
        self.event_name = event_name  # May be None
        self.kind = kind
        self.label = label
        self.body = body.replace('\n',' ')

    def clone(self, newbody):
        return BSentence(self.event_name, self.kind,
                         str(self.label), newbody)

    def is_event(self):
        return self.event_name != None

    def get_body(self):
        return self.body

    def has_same_kind(self, other) :
        return self.kind == other.kind

    def has_same_form(self, other) :
        return self.get_body() == other.get_body()

    def matches(self, other) :
        return self.has_same_kind(other) and self.has_same_form(other)

    def __len__(self):
        return len(self.get_body())

    def __getitem__(self, key):
        return self.get_body()[key]

    def name(self) :
        str = ''
        if self.event_name:
            str += self.event_name + '.'
        str += self.kind.name
        if self.label:
            str += '.' + self.label
        return str

    def __str__(self):
        return ('[%s] ' % self.kind.name) + \
            (self.label+': ' if self.label else '') + self.body


class BEvent(object):
    ''' The elements in an Event definition. '''
    
    class Conv(Enum):
        ordinary=0
        convergent=1
        anticipated=2

    def __init__(self, machine, event_name):
        self.machine = machine
        self.event_name = event_name
        self.convergence = BEvent.Conv.ordinary
        self.isExtended = False
        self.refines = []
        self.parameters = []
        self.witnesses = []
        self.guards = []
        self.actions = []

    def set_convergence(self, convergence):
        assert convergence in ['0','1','2'], \
            'invalid convergence index: %s' % convergence
        self.convergence = BEvent.Conv(int(convergence))

    def set_extended(self, isExtended):
        self.isExtended = (isExtended == 'True')

    def add_parameter(self, param):
        self.parameters.append(param)

    def add_guard(self, label, predicate):
        self.guards.append(BSentence(self.qual_name(),
                                     BSentence.Kinds.guard,
                                     label, predicate))

    def add_action(self, label, assignment):
        self.actions.append(BSentence(self.qual_name(),
                                      BSentence.Kinds.action,
                                      label, assignment))

    def add_refines(self, name):
        self.refines.append(name)

    def add_witness(self, label, predicate):
        self.witnesses.append(BSentence(self.qual_name(),
                                        BSentence.Kinds.witness,
                                        label, predicate))

    def is_init(self):
        return self.event_name == 'INITIALISATION'

    def get_all_sentences(self):
        return self.witnesses + self.guards + self.actions

    def qual_name(self):
        return os.path.join(self.machine, self.event_name)
    
    def __str__(self):
        header = 'Event %s' % self.qual_name()
        if self.isExtended:
            header += ' [extended]'
        header += (' [%s]' % self.convergence.name)
        lines = [header + ' ='] + \
                _indent_list('refines', self.refines) + \
                _indent_list('any', self.parameters, '\n\t', ', ') + \
                _indent_list('when', self.guards) + \
                _indent_list('with', self.witnesses) + \
                _indent_list('then', self.actions) + \
                ['End event %s.' % self.event_name]
        return '\n'.join(lines)


class BComponent(object):
    ''' Abstract base class for contexts and machines '''
    
    def __init__(self, path, name):
        self.path = path
        # Trim path from name (if there):
        if name.startswith(path):
            name = name[len(path):]
        if name.startswith(os.sep):
            name = name[1:]
        # Trim suffix from name (if there):
        if name.endswith(('.buc','.bum', '.bps')):
            name = name[:-4]
        self.name = name
        
    def fullname(self):
        return os.path.join(self.path, self.name)
    
    def get_name_body_pairs(self):
        return []

    def get_parents(self):
        return []


class BContext(BComponent):
    ''' The elements of a context definition.   '''

    def __init__(self, path, name):
        BComponent.__init__(self, path, name)
        self.extends = []
        self.sets = []
        self.constants = []
        self.axioms = []

    def add_extend(self, an_extend):
        self.extends.append(an_extend)

    def add_set(self, a_set):
        self.sets.append(a_set)

    def add_constant(self, a_constant):
        self.constants.append(a_constant)

    def add_axiom(self, label, predicate):
        self.axioms.append(BSentence(None,
                                     BSentence.Kinds.axiom, label, predicate))

    def get_parents(self):
        return self.extends

    def get_all_sentences(self, want_init=False, add_invariants=False):
        return list(self.axioms)

    def get_event_sentences(self,  keep_init=False, add_invariants=False) :
        return []

    def __str__(self):
        lines = ['CONTEXT ' + self.fullname()] + \
                _indent_list('EXTENDS', self.extends, ' ', ', ') + \
                _indent_list('SETS', self.sets) + \
                _indent_list('CONSTANTS', self.constants, '\n\t', ', ') + \
                _indent_list('AXIOMS', self.axioms)
        return '\n'.join(lines)


class BMachine(BComponent):
    ''' The elements of a machine definition. '''

    def __init__(self, path, name):
        BComponent.__init__(self, path, name)
        self.refines = []
        self.sees = []
        self.variables = []
        self.invariants = []
        self.variants = []  # Actually 0..1, but easier as a list.
        self.events = OrderedDict()    # Map event-names to BEvent objects

    def add_variable(self, an_identifier):
        self.variables.append(an_identifier)

    def add_invariant(self, label, a_predicate):
        self.invariants.append(BSentence(None,
                                         BSentence.Kinds.invariant, label,
                                         a_predicate))

    def add_variant(self, event, an_expression):
        self.variants.append(BSentence(None,
                                       BSentence.Kinds.variant, None,
                                       an_expression))

    def add_event(self, eventName):
        return self.events.setdefault(eventName,
                                      BEvent(self.fullname(), eventName))

    def add_sees(self, a_context_name):
        self.sees.append(a_context_name)

    def add_refines(self, a_machine_name):
        self.refines.append(a_machine_name)

    def get_parents(self):
        return self.refines

    def get_all_sentences(self, want_init=False, add_invariants=False):
        ''' Return a list of all the sentences in the machine '''
        all_sens = []
        if add_invariants:
            all_sens = self.invariants + self.variants
        for name, event in self.events.items():
            if event.is_init() and not want_init:
                continue
            all_sens.extend(event.witnesses)
            all_sens.extend(event.guards)
            all_sens.extend(event.actions)
        return all_sens

    def get_event_sentences(self,  want_init=False, add_invariants=False) :
        ''' Transform a machine to a dict, event_name -> sentence-list.
        Option to add the machine's variants/invariants to each event.
        '''
        edict = { }
        for name, event in self.events.items():
            if event.is_init() and not want_init:
                continue
            esens = [ ]
            if add_invariants:
                esens.extend(self.invariants)
                if event.convergence != BEvent.Conv.ordinary:
                    esens.extend(self.variants)
            esens.extend(event.get_all_sentences())
            edict[event.qual_name()] = esens
        return edict

    def __str__(self):
        lines = ['MACHINE ' + self.fullname()] + \
                _indent_list('REFINES', self.refines, ' ', ', ') + \
                _indent_list('SEES', self.sees, ' ', ', ') + \
                _indent_list('VARIABLES', self.variables, '\n\t', ', ') + \
                _indent_list('INVARIANTS', self.invariants) + \
                _indent_list('VARIANTS', self.variants) + \
                ['EVENTS'] + \
                [str(e_def) for e_def in self.events.values()]
        return '\n'.join(lines)


def count_actions(bsen_list):
    return len([s for s in bsen_list 
                if s.kind in [BSentence.Kinds.action, 
                              BSentence.Kinds.axiom]])


class BProofStatus(BComponent):
    ''' The elements of a proof status file.   '''

    def __init__(self, path, name):
        BComponent.__init__(self, path, name)
        self.ps = []
        self.name = name

    def add_ps(self, a_ps):
        self.ps.append(a_ps)

    def get_all_sentences(self):
        return self.ps

    def __str__(self):
        lines = []+ _indent_list('ProofObligation ', self.ps) 
        return '\n'.join(lines)
