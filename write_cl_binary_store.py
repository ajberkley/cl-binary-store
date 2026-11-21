# Create cl-binary-store (.cbs) files from python objects
# Main function is:
#  write_to_file(filename, object)
# lists -> lists
# numpy arrays -> simple-arrays, simple-vectors, simple specialized arrays
# dict -> hash-table
# strings -> strings
# No reference handling (currently)
# No circularity handling (currently)
# From python to lisp: False -> NIL, True -> T
# From lisp to python: not determined yet.

import struct
import numpy as np
import math

def send_int(stream, num):
    if(num > 0):
        if(num <= 16):
            stream.write((num+208).to_bytes(1, byteorder='little'))
        elif(num < 256):
            stream.write(b'\x00');
            stream.write(num.to_bytes(1, byteorder='little', signed=False));
        elif(num < 65536):
            stream.write(b'\x01');
            stream.write(num.to_bytes(2, byteorder='little', signed=False))
        elif(num < 2**32):
            stream.write(b'\x02');
            stream.write(num.to_bytes(4, byteorder='little', signed=False))
        elif(num < 2**64):
            stream.write(b'\x03');
            stream.write(num.to_bytes(8, byteorder='little', signed=False))
        else:
            raise ValueError("Bignum writing from python not implemented yet")
    else:
        if(num >= -16):
            stream.write((num+208).to_bytes(1, byteorder='little'))
        elif(num > -256):
            stream.write(b'\x06');
            stream.write((-num).to_bytes(1, byteorder='little', signed=False));
        elif(num > -65536):
            stream.write(b'\x07');
            stream.write((-num).to_bytes(2, byteorder='little', signed=False));
        elif(num > -(2**32)):
            stream.write(b'\x08');
            stream.write((-num).to_bytes(4, byteorder='little', signed=False));
        elif(num > -(2**64)):
            stream.write(b'\x03');
            stream.write(num.to_bytes(8, byteorder='little', signed=True))
        else:
            raise ValueError("Bignum writing from python not implemented yet")

def send_string(stream, string):
    stream.write(b'\x1E') # utf-8 encoded string
    encoded = string.encode('utf-8');
    send_tagged_unsigned_fixnum(stream, len(encoded), interior=True)
    stream.write(encoded)

def send_tagged_unsigned_fixnum(stream, num, interior=True):
    if(interior and (num < 252)):
        stream.write((num+4).to_bytes(1, byteorder='little', signed=False));
    elif((not interior) and num < 16):
        stream.write((num+208).to_bytes(1, byteorder='little', signed=False));
    else:
        if interior:
            num = num - 252;
        if(num < 256):
            stream.write(b'\x00');
            stream.write(num.to_bytes(1, byteorder='little', signed=False));
        elif(num < 65536):
            stream.write(b'\x01');
            stream.write(num.to_bytes(2, byteorder='little', signed=False));
        elif(num < 2**32):
            stream.write(b'\x02')
            stream.write(num.to_bytes(4, byteorder='little', signed=False));
        else:
            stream.write(b'\x03')
            stream.write(num.to_bytes(8, byteorder='little', signed=False));

def send_numpy_array_element_type(stream, dtype):
    match dtype:
        case np.float32: stream.write(b'\x04')
        case np.float64: stream.write(b'\x05')
        case np.uint64 | np.datetime64: stream.write(b'\x13')
        case np.uint32: stream.write(b'\x11')
        case np.uint16: stream.write(b'\x0F')
        case np.uint8: stream.write(b'\x0D')
        case np.int64: stream.write(b'\x09')
        case np.int32: stream.write(b'\x08')
        case np.int16: stream.write(b'\x07')
        case np.int8: stream.write(b'\x06')
        case np.csingle: stream.write(b'\x15')
        case np.cdouble: stream.write(b'\x14')
        case _:
            raise ValueError(f"Do not know how to encode numpy array type {dtype}")

def send_simple_specialized_vector(stream, nparray):
    stream.write(b'\x15');
    send_tagged_unsigned_fixnum(stream, len(nparray), interior=True);
    send_numpy_array_element_type(stream, nparray.dtype);
    stream.write(nparray.tobytes())

def send_simple_vector(stream, vector):
    stream.write(b'\x16');
    send_tagged_unsigned_fixnum(stream, len(vector), interior=True);
    for elt in vector:
        write_one_object(stream, elt)

def send_simple_array(stream, array):
    # 0x18 is array, 0x05 is nil for fill-pointer, nil for adjustable
    stream.write(b'\x18\x05\x05');
    dimensions = array.shape;
    stream.write(len(dimensions).to_bytes(1,signed=False,byteorder='little'));
    for dim in dimensions:
        send_tagged_unsigned_fixnum(stream, dim, interior=False);
    stream.write(b'\x05\x14'); # no displacement, array element-type t
    for elt in np.nditer(array, flags=['refs_ok']):
        # nditer returns 0 dimensional numpy arrays which are bonkers
        write_one_object(stream, elt.item());
        
def send_simple_specialized_array(stream, nparray):
    stream.write(b'\x17');
    stream.write(len(nparray.shape).to_bytes(1, signed=False, byteorder='little'));
    for dim in nparray.shape:
        send_tagged_unsigned_fixnum(stream, dim, interior=False);
    send_numpy_array_element_type(stream, nparray.dtype);
    stream.write(nparray.tobytes());

def send_hashtable(stream, ht):
    stream.write(b'\x1C');
    send_tagged_unsigned_fixnum(stream, len(ht), interior=False); # count
    send_tagged_unsigned_fixnum(stream, len(ht), interior=False); # size
    stream.write(b'\x11'); # interned symbol?
    send_string(stream, "EQL");
    send_string(stream, "COMMON-LISP");
    write_one_object(stream, 1.0)
    write_one_object(stream, 1.5)
    stream.write(b'\x05'); # synchronized is nil
    stream.write(b'\x05'); # weakness is nil
    for key, value in ht.items():
        write_one_object(stream, key)
        write_one_object(stream, value)
    
def write_one_object (stream, obj):
    match obj:
        case bool():
            if obj:
                stream.write(b'\x14');
            else:
                stream.write(b'\x05');
        case int():
            send_int(stream, obj)
        case float():
            if(obj == 0.0):
                stream.write(b'\x0C');
            else:
                stream.write(b'\x0B');
                stream.write(struct.pack('<d', obj));
        case type(): # type -> uninterned symbol
            stream.write(b'\x12');
            send_string(stream, obj.__name__);
        case list() | tuple():
            length = len(obj);
            if length > 0:
                stream.write(b'\x20');
                send_tagged_unsigned_fixnum(stream, len(obj), interior=True)
                for elt in obj:
                    write_one_object(stream, elt);
            stream.write(b'\x05');
        case str():
            send_string(stream, obj);
        case np.ndarray():
            if obj.ndim == 1:
                if obj.dtype == object:
                    send_simple_vector(stream, obj);
                else:
                    send_simple_specialized_vector(stream, obj);
            else:
                if obj.dtype == object:
                    send_simple_array(stream, obj);
                else:
                    send_simple_specialized_array(stream, obj);
        case dict():
            send_hashtable(stream, obj);
        case _:
            raise ValueError(f"Do not know how to encode {obj}")

def write_to_file(filename, object):
    with open(filename, "wb") as stream:
        write_one_object(stream, object);
