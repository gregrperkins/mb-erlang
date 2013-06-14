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
	isASCII/1 | 
	len/1 | 
	lenB/1 | 
	new/0 | Creates a new empty mb string, a tuple {encoding_atom, <<"">>}
	new/1 | Creates a new mb string, a tuple {encoding_atom, <<Binary_String>>}
	new/2 | Creates a new mb string, a tuple {encoding_atom, <<Binary_String>>}
	new/3 | Creates a new mb string, a tuple {encoding_atom, <<Binary_String>>}
	print/1 | 
	print/2 | Formats an mbstring and outputs it
	reset/0 | Rebuilds the unicode- and encodings-related tables

## Function details

###	bocu/1

	*bocu(X1) -> any()*

### charToInt/1

	*charToInt(Char) -> any()*

### convertEncoding/2

	*convertEncoding(OriginalString::mbString(), NewEncoding::atom()) -> mbString()*

converts an mb string from Encoding to NewEncoding.

Example:

 	1> HONG2=mb:convertEncoding({utf8, "\345\274\230"}, utf16).
 	{utf16,<<95,24>>}

### format/1

	*format(MBString::mbString()) -> list()*

formats the mbstring for output.

Example: 

	1> HAN=mb:new("\345\257\222"). ==> That's [U+5BD2]/Cold.
	{utf8,<<229,175,146>>}
	2> io:format("~s~n",[mb:format(HAN)]).
	\345\257\222
  
Just a hack really.

### fwrite/2

	*fwrite(MBString::mbString(), FileName::list()) -> atom()*

outputs an mbstring to a file.

Example

	1> HAN=mb:new("\345\257\222"). ==> That's [U+5BD2]/Cold.
	{utf8,<<229,175,146>>}
	2> mb:fwrite(HAN, MyFile).
	ok

### getNextCharAsInt/1

	*getNextCharAsInt(X1) -> any()*

### hasProcess/1

	*hasProcess(X) -> any()*

### hasTable/1

	*hasTable(X) -> any()*

### hexString_to_List/1

	*hexString_to_List(Code) -> any()*

### init/0

	*init() -> atom()*

reads the unicode- and encodings-related tables and builds them if absent

See also: reset/0.

### isASCII/1

	*isASCII(Tring) -> any()*

### len/1

	*len(MyString) -> any()*

### lenB/1

	*lenB(X1) -> any()*

### new/0

	*new() -> mbString()*

creates a new empty mb string, a tuple {encoding_atom, <<"">>}.

Example:

	1> HAN=mb:new("").
	{utf8,<<>>}
  
See also: new/1, new/2, new/3.

### new/1

	*new(String::list()) -> mbString()*

creates a new mb string, a tuple {encoding_atom, <<Binary_String>>}. You can specify the encoding, and even request an on-the-fly conversion.

Example:

	1> HAN=mb:new("\345\257\222"). ==> That's [U+5BD2]/Cold.
	{utf8,<<229,175,146>>}
  
See also: new/0, new/2, new/3.

### new/2

	*new(String::list(), OriginalEncoding::atom()) -> mbString()*

creates a new mb string, a tuple {encoding_atom, <<Binary_String>>}. You can specify the encoding, and even request an on-the-fly conversion.

Example:

	2> HONG=mb:new(<<"\345\274\230">>). ==> That's [U+5F18]/Strong
	{utf8,<<229,188,152>>}
  
See also: new/0, new/1, new/3.

### new/3

	*new(String::list(), OriginalEncoding::atom(), RequestedEncoding::atom()) -> mbString()*

creates a new mb string, a tuple {encoding_atom, <<Binary_String>>}. You can specify the encoding, and even request an on-the-fly conversion.

Example:

	3> HONG2=mb:new("\345\274\230", utf8, utf16).
	{utf16,<<95,24>>}
  
See also: new/0, new/1, new/2.

### print/1

	*print(MBString::mbString()) -> atom()*

See also: format/1, fwrite/2, print/2.

### print/2

	*print(MBString::mbString(), Params::list()) -> atom()*

formats an mbstring and outputs it.

Example:

	1> HAN=mb:new("\345\257\222"). ==> That's [U+5BD2]/Cold.
	{utf8,<<229,175,146>>}
	2> mb:print(HAN, ["\n"]).
	\345\257\222
	ok
  
See also: format/1, fwrite/2, print/1.

# reset/0

	*reset() -> atom()*

rebuilds the unicode- and encodings-related tables. What it really does is delete the dets tables.

See also: init/0.