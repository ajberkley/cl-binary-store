# Reading a cl-binary-store file into python: read_from_file(filename)
# simple specialized array -> numpy array with specialized dtype
# hash-tables -> dicts
# strings -> strings
# numbers -> numbers
# all floats -> double-floats
# list -> list
# vector -> numpy array dtype=object
# array -> numpy array dtype=object
# NOT SUPPORTED:
#  displaced arrays
#  fill pointers
#  trees / cons structures (except simple lists)
#  circular reference resolution (hash table with itself inside, for example)
# TODO, add a code in the userspace for True / False
# TODO support circular references

import numpy as np
import math
import struct

def read_one_object (stream):
    return read_tag_and_dispatch(stream, []);

def decode_single_float(stream):
    return struct.unpack('<f', stream.read(4))[0];
    
def decode_double_float(stream):
    return struct.unpack('<d', stream.read(8))[0];
    
def read_bignum(stream):
    raise ValueError("Bignum decoding not supported yet")

def read_standard_structure_object(stream, references):
    "Decode a struct.  We map to an extant python class."
    object_info = read_tag_and_dispatch(stream, references);
    slot_names = object_info.slots;
    just_slot_names = [slot_name for (slot_name, package_name) in slot_names]
    slots = [(slot_name, read_tag_and_dispatch(stream, references)) for slot_name in just_slot_names]
    cls = globals()[object_info.object_name](**dict(slots))
    return cls

def read_simple_base_string(stream):
    length = restore_tagged_unsigned_fixnum(stream);
    bytes = stream.read(length);
    string = bytes.decode("ascii");
    return string;

def read_simple_string(stream):
    length = restore_tagged_unsigned_fixnum(stream);
    string = stream.read(length).decode("utf-8");
    return string

def restore_tagged_unsigned_fixnum(stream, interior=True):
    byte = int.from_bytes(stream.read(1), byteorder='little');
    match byte:
        case 0:
            result=int.from_bytes(stream.read(1), byteorder='little');
            if interior:
                result+=252;
        case 1:
            result=int.from_bytes(stream.read(2), signed=False, byteorder='little');
            if interior:
                result+=252;
        case 2:
            result=int.from_bytes(stream.read(4), signed=False, byteorder='little');
            if interior:
                result+=252;
        case 3:
            result=int.from_bytes(stream.read(8), signed=True, byteorder='little');
            if interior:
                result+=252;
        case n if not interior and 208 <= n: result = n - 208;
        case n if interior and 4 <= n: result = n - 4;
    return result;

def read_symbol(stream, references):
    symbol_name = read_tag_and_dispatch(stream, references);
    package_name = read_tag_and_dispatch(stream, references);
    return symbol_name, package_name

class BLARG:
    def __init__(self, A=None, B=None):
        self.A = A
        self.B = B

class ObjectInfo:
    def __init__(self, object_name, package_name, slots):
        self.object_name = object_name;
        self.package_name = package_name;
        self.slots = slots;
        myclass = globals().get(object_name);
        if isinstance(myclass, type):
            self.myclass = myclass
        else:
            raise ValueError(f"{object_name} in {package_name} is not a class object")

def read_object_info(stream, references):
    num_slots = read_tag_and_dispatch(stream, references)
    object_name , package_name = read_tag_and_dispatch(stream, references)
    slots = [read_tag_and_dispatch(stream, references) for _ in range(num_slots)]
    return ObjectInfo(object_name, package_name, slots)

def read_simple_vector(stream, references):
    "Note that this creates a numpy.array"
    length = restore_tagged_unsigned_fixnum(stream, interior=True);
    # to support numpy 2.2 and before we do this weirdness
    aslist = [read_tag_and_dispatch(stream, references) for _ in range(length)]
    array = np.empty(length, dtype=object)
    array[:] = aslist
    return array

def read_simple_specialized_array(stream, references):
    num_dimensions = int.from_bytes(stream.read(1), byteorder='little', signed=False)
    array_dimensions = [restore_tagged_unsigned_fixnum(stream, interior=False) for _ in range(num_dimensions)];
    data_as_vector = read_simple_specialized_vector(stream, references, num_elts=math.prod(array_dimensions));
    data_as_array = np.reshape(data_as_vector, array_dimensions);
    return data_as_array;

def read_simple_specialized_vector(stream, references, num_elts = None):
    if num_elts is None:
        num_elts = restore_tagged_unsigned_fixnum(stream, interior=True);
    byte = int.from_bytes(stream.read(1), byteorder='little')
    match byte:
        case 0: # ("bit", 1)
            result = np.unpackbits(np.frombuffer(stream.read(math.ceil(num_elts / 8)),dtype=np.uint8),bitorder='little');
        case 1: # ("base-char", 8) not used, we encode as simple-base-string
            result = stream.read(num_elts).decode("ascii");
        case 2: # ("character", 32)
            result = stream.read(4*num_elts).decode("utf-16");
        case 3: # ("fixnum", 64)
            result=np.frombuffer(stream.read(num_elts*8), dtype=np.int64);
            result = result >> 1;
        case 4: # ("single-float", 32)
            result = np.frombuffer(stream.read(num_elts*4), dtype=np.float32);
        case 5: # ("double-float", 64)
            result = np.frombuffer(stream.read(num_elts*8), dtype=np.float64);
        case 6: # ("(signed-byte 8)", 8)
            result = np.frombuffer(stream.read(num_elts), dtype=np.int8);
        case 7: # ("(signed-byte 16)", 16)
            result = np.frombuffer(stream.read(num_elts*2), dtype=np.int16);
        case 8: # ("(signed-byte 32)", 32)
            result = np.frombuffer(stream.read(num_elts*4), dtype=np.int32);
        case 9: # ("(signed-byte 64)", 64)
            result = np.frombuffer(stream.read(num_elts*8), dtype=np.int64);
        case 10: # ("(unsigned-byte 2)", 2)
            shifts = np.array([0,2,4,6],dtype=np.uint8)
            result = np.frombuffer(stream.read(math.ceil(num_elts / 4)),dtype=np.uint8)
            result = ((result[:, None] >> shifts) &0b11).ravel()
        case 11: # ("(unsigned-byte 4)", 4)
            shifts = np.array([0,4],dtype=np.uint8)
            result = np.frombuffer(stream.read(math.ceil(num_elts / 2)),dtype=np.uint8)
            result = ((result[:, None] >> shifts) &0b1111).ravel()
        case 12 | 13: # ("(unsigned-byte 7 or 8)", 8)
            result = np.frombuffer(stream.read(math.ceil(num_elts)),dtype=np.uint8);
        case 14 | 15: # ("(unsigned-byte 15 or 16)", 16)
            result = np.frombuffer(stream.read(math.ceil(num_elts*2)),dtype=np.uint16);
        case 16 | 17: # ("(unsigned-byte 31 or 32)", 32)
            result = np.frombuffer(stream.read(math.ceil(num_elts*4)),dtype=np.uint32);
        case 18 | 19: # ("(unsigned-byte 62 or 64)", 64)
            result = np.frombuffer(stream.read(math.ceil(num_elts*8)),dtype=np.uint64);
        case 20: # complex double-float 128
            result = np.frombuffer(stream.read(math.ceil(num_elts*16)),dtype=np.complex128);
        case 21: # complex single-float 64
            result = np.frombuffer(stream.read(math.ceil(num_elts*8)),dtype=np.complex64);
        case _:
            raise ValueError(f"Unknown element type {byte}")
    return result;

def read_finite_length_list(stream, references):
    length = restore_tagged_unsigned_fixnum(stream, interior=True);
    list = [read_tag_and_dispatch(stream,references) for _ in range(length)]
    tail = read_tag_and_dispatch(stream,references)
    if not tail == []:
        raise ValueError("Cannot handle list with non-nil tail in python");
    return list

def build_nd_list(dims, reader):
    if len(dims)==1:
        return [reader() for _ in range(dims[0])];
    else:
        return [build_nd_list(dims[1:], reader) for _ in range(dims[0])]

def read_array(stream, references):
    has_fill_pointer = read_tag_and_dispatch(stream, references);
    if has_fill_pointer: # we could support this
        raise ValueError("fill pointers not supported");
    adjustable = read_tag_and_dispatch(stream, references);
    array_rank = int.from_bytes(stream.read(1), byteorder='little', signed=False)
    dimensions = [restore_tagged_unsigned_fixnum(stream, interior=False) for _ in range(array_rank)]
    displaced = read_tag_and_dispatch(stream, references);
    if displaced: # we could support these with numpy slices, etc
        raise ValueError("Displaced arrays not supported");
    array_total_size = math.prod(dimensions);
    # element_type is ignored
    element_type = read_tag_and_dispatch(stream, references);
    result = build_nd_list(dimensions, lambda: read_tag_and_dispatch(stream, references));
    return np.array(result, dtype=object);

def read_hashtable(stream, references):
    count = restore_tagged_unsigned_fixnum(stream, interior=False);
    size = restore_tagged_unsigned_fixnum(stream, interior=False);
    test = read_tag_and_dispatch(stream, references);
    rehash_threshold = read_tag_and_dispatch(stream, references);
    rehash_size = read_tag_and_dispatch(stream, references);
    synchronized = read_tag_and_dispatch(stream, references);
    weakness = read_tag_and_dispatch(stream, references);
    ht = {}; # cannot hint size easily
    for _ in range(count):
        key = read_tag_and_dispatch(stream,references);
        value = read_tag_and_dispatch(stream,references);
        ht[key] = value;
    return ht;

def read_tag_and_dispatch (stream, references):
    byte = int.from_bytes(stream.read(1), byteorder='little');
    match byte:
        case 0: result=int.from_bytes(stream.read(1), byteorder='little');
        case 1: result=int.from_bytes(stream.read(2), signed=False, byteorder='little');
        case 2: result=int.from_bytes(stream.read(4), signed=False, byteorder='little');
        case 3: result=int.from_bytes(stream.read(8), signed=True, byteorder='little');
        case 4: result=read_cons(stream, references);
        case 5: result=[]; # NIL -> empty list
        case 6: result=-1 * int.from_bytes(stream.read(1), signed=False, byteorder='little');
        case 7: result=-1 * int.from_bytes(stream.read(2), signed=False, byteorder='little');
        case 8: result=-1 * int.from_bytes(stream.read(4), signed=False, byteorder='little');
        case 9: result=read_bignum(stream);
        case 10: result=decode_single_float(stream);
        case 11: result=decode_double_float(stream);
        case 12: result=0.0; # all floats are double floats in python
        case 13: result=read_ratio(stream);
        case 14: result=read_complex(stream);
        case 15: result=read_complex_double_float(stream);
        case 16: result=read_complex_single_float(stream);
        case 17: result=read_symbol(stream, references);
        case 18: result=read_uninterned_symbol(stream);
        case 19: result=read_standard_structure_object(stream, references);
        case 20: result=True;
        case 21: result=read_simple_specialized_vector(stream, references);
        case 22: result=read_simple_vector(stream, references);
        case 23: result=read_simple_specialized_array(stream, references);
        case 24: result=read_array(stream, references);
        case 25: result=read_object_info(stream, references);
        case 26:
            result=None; # unbound marker
        case 27: result=read_pathname(stream);
        case 28: result=read_hashtable(stream, references);
        case 29: result=read_simple_base_string(stream);
        case 30: result=read_simple_string(stream);
        case 31: result=read_and_parse_action_code(stream, references);
        case 32: result=read_finite_length_list(stream, references);
        case 33: result=read_tagged_reference(stream);
        case 34:
            result=read_tag_and_dispatch(stream, references);
            references.append(result);
        case n if 35 <= n and n <= 63: # direct reference code
            result = references[n-35];
        case n if 64 <= n and n <= 127: # one byte reference code
            id = decode_one_byte_reference_id(n, stream.read(1));
            result = references[id];
        case n if 128 <= n and n <= 191:
            id = decode_two_byte_reference_id(n, stream.read(2));
        case n if 192 <= n and n <= 224:
            result = n - 208;
        case n if n > 225:
            raise ValueError("Unimplemented user tag")

    return result;

def read_from_file(filename):
    with open(filename, "rb") as file:
        result = read_one_object(file);
        return result
