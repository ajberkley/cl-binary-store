# Read from a cl-binary-store file, and emit an output file with what we saw.

import read_cl_binary_store
import write_cl_binary_store

import sys

if len(sys.argv) < 3:
    print(f"Usage python {sys.argv[0]} input_filename output_filename")
    sys.exit(1);

data = read_cl_binary_store.read_from_file(sys.argv[1]);
# print(f"From {sys.argv[1]}, read {data}")
print(f"Writing to {sys.argv[2]}")
write_cl_binary_store.write_to_file(sys.argv[2], data);

