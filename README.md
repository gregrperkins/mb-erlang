# Module mb - Erlang module for encoding-decoding between charsets

This readme contains:

	* DataTypes
	* Function Index
	* Frunction Details

## Function index

	Function name/arity | Description
	--- | --- 
	bocu/1 |
	charToInt/1 |
	convertEncoding/2 | Converts an mb string from Encoding to NewEncoding
	format/1 | Formats the mbstring for output
	fwrite/2 | Outputs an mbstring to a file
	getNextCharAsInt/1 | 
	hasProcess/1 |
	hasTable/1 | 
	hexString_to_List/1 | 
	init/0 | Reads the unicode- and encodings-related tables and builds them if absent
	isASCII/1
	len/1
	lenB/1
	new/0 | Creates a new empty mb string, a tuple {encoding_atom, <<"">>}
	new/1 | Creates a new mb string, a tuple {encoding_atom, <<Binary_String>>}
	new/2 | Creates a new mb string, a tuple {encoding_atom, <<Binary_String>>}
	new/3 | Creates a new mb string, a tuple {encoding_atom, <<Binary_String>>}
	print/1 | 
	print/2 | Formats an mbstring and outputs it
	reset/0 | Rebuilds the unicode- and encodings-related tables

## Function details