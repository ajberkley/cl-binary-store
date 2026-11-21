def read_one_object (stream):
    return read_tag_and_dispatch(stream, []);

def read_bignum(stream):
    raise ValueError("Bignum decoding not supported yet")

def read_standard_structure_object(stream, references):
    "Decode a struct.  We map to an extant python class."
    object_info = read_tag_and_dispatch(stream, references);
    slot_names = object_info.slots;
    just_slot_names = [slot_name for (slot_name, package_name) in slot_names]
    slots = [(slot_name, read_tag_and_dispatch(stream, references)) for slot_name in just_slot_names]
    print(f"Restoring object {object_info.object_name} with slots {slots}");
    cls = globals()[object_info.object_name](**dict(slots))
    print(f"{object_info.object_name} created with slot values {[(slot_name, getattr(cls, slot_name, '<unset>')) for slot_name in just_slot_names]}")
    return cls

def read_simple_base_string(stream):
    length = restore_tagged_unsigned_fixnum(stream);
    bytes = stream.read(length);
    string = bytes.decode("ascii");
    print(f"simple_base_string {length}: {string}")
    return string;

def read_simple_string(stream):
    length = restore_tagged_unsigned_fixnum(stream);
    return stream.read(length).decode("utf-8");

def restore_tagged_unsigned_fixnum(stream, interior=True):
    byte = int.from_bytes(stream.read(1));
    print(f"byte is {byte}, interior is {interior}")
    match byte:
        case 0:
            result=int.from_bytes(stream.read(1));
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
            print(f"{object_name} in {package_name} is not a class object")
            raise ValueError(f"{object_name} in {package_name} is not a class object")

def read_object_info(stream, references):
    num_slots = read_tag_and_dispatch(stream, references)
    print(f"references starts as {references}")
    object_name , package_name = read_tag_and_dispatch(stream, references)
    print(f"references is now {references}")
    slots = [read_tag_and_dispatch(stream, references) for _ in range(num_slots)]
    print(f"{object_name} in {package_name} with {num_slots} slots: {slots}")
    return ObjectInfo(object_name, package_name, slots)

import numpy as np
import math

def read_simple_specialized_array(stream, references):
    num_dimensions = int.from_bytes(stream.read(1))
    array_dimensions = [restore_tagged_unsigned_fixnum(stream, interior=False) for _ in range(num_dimensions)];
    data_as_vector = read_simple_specialized_vector(stream, references, num_elts=math.prod(array_dimensions));
    data_as_array = np.reshape(data_as_vector, array_dimensions);
    return data_as_array;

def read_simple_specialized_vector(stream, references, num_elts = None):
    if num_elts is None:
        num_elts = restore_tagged_unsigned_fixnum(stream, interior=True);
    print(f"Simple specialized vector length {num_elts}")
    byte = int.from_bytes(stream.read(1))
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

def read_tag_and_dispatch (stream, references):
    byte = int.from_bytes(stream.read(1));
    print(f"Byte is {byte}")
    match byte:
        case 0: result=int.from_bytes(stream.read(1));
        case 1: result=int.from_bytes(stream.read(2), signed=False, byteorder='little');
        case 2: result=int.from_bytes(stream.read(4), signed=False, byteorder='little');
        case 3: result=int.from_bytes(stream.read(8), signed=True, byteorder='little');
        case 4: result=read_cons(stream, references);
        case 5: result=False
        case 6: result=-1 * int.from_bytes(stream.read(1), signed=False);
        case 7: result=-1 * int.from_bytes(stream.read(2), signed=False, byteorder='little');
        case 8: result=-1 * int.from_bytes(stream.read(4), signed=False, byteorder='little');
        case 9: result=read_bignum(stream);
        case 10: result=decode_single_float(stream.read(4));
        case 11: result=decode_double_float(stream.read(8));
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
            print("unbound marker")
            result=None; # unbound marker
        case 27: result=read_pathname(stream);
        case 28: result=read_hashtable(stream, references);
        case 29: result=read_simple_base_string(stream);
        case 30: result=read_simple_string(stream);
        case 31: result=read_and_parse_action_code(stream, references);
        case 32: result=read_finite_length_list(stream);
        case 33: result=read_tagged_reference(stream);
        case 34:
            result=read_tag_and_dispatch(stream, references);
            references.append(result);
            print(f"Stored reference to {result}")
        case n if 35 <= n and n <= 63: # direct reference code
            print(f"Reference to {references[n-35]}");
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
        print(result)

import sys

if len(sys.argv) < 2:
    print(f"Usage python {sys.argv[0]} filename")
    sys.exit(1);

read_from_file(sys.argv[1]);

