#!/usr/bin/python3

## -*- coding: utf-8 -*-

from zipfile import ZipFile
import xml.sax
import fnmatch
import os.path
import sys

from eventb import BContext, BMachine, BProofStatus

''' Process .bum/.buc files using a SAX XML parser.
'''


####################################################################
# The XML processing
####################################################################

class BXMLHandler(xml.sax.ContentHandler) :
    ns = 'org.eventb.core.'
    @classmethod
    def add_ns(cls, tag) :
        if tag == 'name':
          return tag
        else:
          return cls.ns + tag
    @classmethod
    def remove_ns(cls, tag) :
        if not tag.startswith(cls.ns) :
            return None
        return tag[len(cls.ns):]
    def __init__(self, want_warnings=False) :
        self.component = None
        self.want_warnings = want_warnings
    def get_component(self) :
        return self.component
    def get(self, attributes, name) :
        return attributes.get(self.add_ns(name))
    def startElement(self, tag, attributes):
        tag_name = self.remove_ns(tag)
        if not tag_name:
            if self.want_warnings:
                print('Ignoring unknown XML namespace/tag:', tag, file=sys.stderr)
            return
        handler_name = 'do_' + tag_name
        if hasattr(self, handler_name) :
            getattr(self, handler_name)(attributes)
        elif self.want_warnings:
            print('Ignoring unknown XML tag:', tag_name, file=sys.stderr)
    def endElement(self, tag):
        pass

class BpsHandler(BXMLHandler) :
    def __init__(self, root, filename):
        BXMLHandler.__init__(self)
        self.component = BProofStatus(root, filename)
    def do_psFile(self, attributes) :
        pass
    def do_psStatus(self, attributes) :
        self.component.add_ps((self.component.name,self.get(attributes, 'psManual')))
        if 'GRD' in str(self.get(attributes, 'name')):
          self.component.add_ps((self.component.name,'ref'))
        elif 'MRG' in str(self.get(attributes, 'name')):
          self.component.add_ps((self.component.name,'ref'))
        elif 'SIM' in str(self.get(attributes, 'name')):
          self.component.add_ps((self.component.name,'ref'))
        elif 'EQL' in str(self.get(attributes, 'name')):
          self.component.add_ps((self.component.name,'ref'))
        elif 'WWD' in str(self.get(attributes, 'name')):
          self.component.add_ps((self.component.name,'ref'))
        elif 'WFIS' in str(self.get(attributes, 'name')):
          self.component.add_ps((self.component.name,'ref'))
      #  self.component.add_ps((self.component.name,self.get(attributes, 'name')))

class BucHandler(BXMLHandler) :
    def __init__(self, root, filename):
        BXMLHandler.__init__(self)
        self.component = BContext(root, filename)
    def do_contextFile(self, attributes) :
        pass
    def do_extendsContext(self, attributes) :
        self.component.add_extend(self.get(attributes, 'target'))
    def do_carrierSet(self, attributes) :
        self.component.add_set(self.get(attributes, 'identifier'))
    def do_constant(self, attributes) :
        self.component.add_constant(self.get(attributes, 'identifier'))
    def do_axiom(self, attributes) :
        self.component.add_axiom(self.get(attributes, 'label'),
                                 self.get(attributes, 'predicate'))
    def do_theorem(self, attributes):
        pass  ### Not collecting theorems


class BumHandler(BXMLHandler) :
    def __init__(self, root, filename):
        BXMLHandler.__init__(self)
        self.component = BMachine(root, filename)
        self.currentEvent = None
    def endElement(self, tag):
        tag_name = self.remove_ns(tag)
        if tag_name == 'event' :
            self.currentEvent = None
    def do_machineFile(self, attributes) :
        pass
    def do_event(self, attributes) :
        self.currentEvent = self.component.add_event(self.get(attributes, 'label'))
        self.currentEvent.set_convergence(self.get(attributes, 'convergence'))
        self.currentEvent.set_extended(self.get(attributes, 'extended'))
    def do_action(self, attributes) :
        self.currentEvent.add_action(self.get(attributes, 'label'),
                                     self.get(attributes, 'assignment'))
    def do_parameter(self, attributes) :
        self.currentEvent.add_parameter(self.get(attributes, 'identifier'))
    def do_witness(self, attributes) :
        self.currentEvent.add_witness(self.get(attributes, 'label'),
                                    self.get(attributes, 'predicate'))
    def do_guard(self, attributes) :
        self.currentEvent.add_guard(self.get(attributes, 'label'),
                                    self.get(attributes, 'predicate'))
    def do_variable(self, attributes) :
        self.component.add_variable(self.get(attributes, 'identifier'))
    def do_invariant(self, attributes) :
        self.component.add_invariant(self.get(attributes, 'label'),
                           self.get(attributes, 'predicate'))
    def do_variant(self, attributes) :
        self.component.add_variant(self.currentEvent,
                            self.get(attributes, 'expression'))
    def do_refinesMachine(self, attributes) :
        self.component.add_refines(self.get(attributes, 'target'))
    def do_seesContext(self, attributes) :
        self.component.add_sees(self.get(attributes, 'target'))
    def do_refinesEvent(self, attributes) :
        self.currentEvent.add_refines(self.get(attributes, 'target'))
    def do_theorem(self, attributes):
        pass  ### Not collecting theorems



####################################################################
# File I/O, zip files, recurse directories
####################################################################
 

def _process_b_file(root, filename, fh) :
    ''' Parse and process an single opened bum/buc file.
    Return a B-component object with this file's contents.
    '''
    parser = xml.sax.make_parser()
    if filename.endswith('.bum') :
        my_handler = BumHandler(root, filename)
    elif filename.endswith('.buc') :
        my_handler = BucHandler(root, filename)
    elif filename.endswith('.bps') :
        my_handler = BpsHandler(root, filename)
    else:
        assert False, "Expecting a B component file here"
    parser.setContentHandler(my_handler)
    parser.parse(fh)
    return my_handler.get_component()


def _process_file_list(root, namelist, howto_open, machines) :
    for b_filename in (fnmatch.filter(namelist, '*.bu[cm]')+fnmatch.filter(namelist, '*.bps')):
        with howto_open(b_filename) as b_fh :
            mac = _process_b_file(root, b_filename, b_fh)
            machines.append(mac)

def _process_zip_file(root, zip_filename, machines) :
    with ZipFile(os.path.join(root, zip_filename),'r') as zip_fh :
        zip_pathname = os.path.join(root, zip_filename)
        _process_file_list(root, ZipFile.namelist(zip_fh), zip_fh.open, machines)
            
def _process_dir(path, machines):
    for root, dirs, files in os.walk(path):
        for zip_filename in (fnmatch.filter(files, '*.zip')):
            _process_zip_file(root, zip_filename, machines)
        fullnames = map(lambda f:os.path.join(root, f), files)
        _process_file_list(root, fullnames, open, machines) 

     
def process_path(pathname) :
    ''' Read bum/buc files, return a list of machines/contexts 
        Path can be file, directory or .zip file.
    '''
    machines = []
    if pathname.endswith('.zip') :
        _process_zip_file('.', pathname, machines)
    elif os.path.isdir(pathname):
        _process_dir(pathname, machines)
    else :
        _process_file_list('.', [pathname], open, machines) 
    return machines


####################################################################
# Public interface: return a dictionary of sentence-lists
####################################################################

def get_machine_sentences(mac_list, keep_init=False, keep_inv=False):
    ''' Returns a dictionary of sentence-lists, one for each machine '''
    sen_dict = { }
    for mac in mac_list:
        if not isinstance(mac, BMachine): 
            continue  # ignore contexts
        msen_list = mac.get_all_sentences(keep_init, keep_inv)
        sen_dict[mac.name] = msen_list
    return sen_dict

def get_context_sentences(mac_list):
    ''' Returns a dictionary of sentence-lists, one for each context '''
    sen_dict = { }
    for mac in mac_list:
        if not isinstance(mac, BContext): 
            continue  # ignore machines
        msen_list = mac.get_all_sentences()
        sen_dict[mac.name] = msen_list
    return sen_dict
                      

def get_event_sentences(mac_list, keep_init=False, keep_inv=False):
    '''Returns a dictionary of sentence-lists, one for each event '''
    sen_dict = { }
    for mac in mac_list:
        if not isinstance(mac, BMachine): 
            continue  # ignore contexts
        edict = mac.get_event_sentences(keep_init, keep_inv)
        sen_dict.update(edict)
    return sen_dict

def get_sentences(pathnames, group_on, keep_init=False, keep_inv=False):
    ''' Returns a dictionary of sentence-lists; key is machine/event name '''
    assert group_on in ['C', 'E', 'M', 'P']
    sen_dict = { }
    for path_name in pathnames:
        mac_list = process_path(path_name)
        if group_on == 'E':
            mdict = get_event_sentences(mac_list, keep_init, keep_inv)
        elif group_on == 'C':
            mdict = get_context_sentences(mac_list)
        elif group_on == 'M':
            mdict = get_machine_sentences(mac_list, keep_init, keep_inv)
        elif group_on == 'P':
            mdict = get_ps_sentences(mac_list)
        sen_dict.update(mdict)
    return sen_dict

def get_ps_sentences(mac_list):
    ''' Returns a dictionary of sentence-lists, one for each proof obligation'''
    sen_dict = { }
    for mac in mac_list:
        if not isinstance(mac, BProofStatus):
            continue  # ignore machines
        msen_list = mac.get_all_sentences()
        sen_dict[mac.name] = msen_list
    return sen_dict
   

if __name__ == "__main__":
    for pathname in sys.argv[1:] :
        for mac in process_path(pathname) :
            print(mac,'\n')


