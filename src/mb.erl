-module(mb).
-export([new/0, new/1, new/2, new/3, split/2, join/2, inStr/2,
isASCII/1, kangxi/1, kangxi/0, convert16/1, convert2/1, utf8_to_utf16/1,
utf16_to_utf8/1, utf16Hex_to_utf8/1, charToInt/1, hasProcess/1, hasTable/1,
surrogate/1, surrogateInt/1, convert10/1,getNextCharAsInt/1,
bocu/1, convertEncoding/2, oneByte_to_utf8/1, utf8_to_oneByte/2,
format/1, print/2, print/1, fwrite/2, test/1, hexString_to_List/1,
init/0, reset/0, latin1L/0, 
loadNorm/0, convert16To1oInt/1, hexString2Int/1, getNFC/1, getNFD/1, getNFKC/1, getNFKD/1,
lowercase/1, uppercase/1]).

-export([getNextChar/1, len/1, lenB/1, left/2, leftB/2, mid/2, mid/3, midB/2, midB/3,
right/2, reverse/1, reverseB/1, filter/2, filterB/2,
strLength/1, replace/3, replaceAll/3]).

-include("encodings.hrl").
-include("manipulations.hrl").
-include("conversions.hrl").
-include("test.hrl").


%% @headerfile "manipulations.hrl"
%% @headerfile "conversions.hrl"
%% @headerfile "test.hrl"
%% @headerfile "encodings.hrl"
%% @type mbString() = {Encoding::atom(), String::binary()}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                              %%
%% Initialisation of UniHan.txt-based functions %%
%%                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec reset() -> atom()
%%
%% @doc rebuilds the unicode- and encodings-related tables. What it <b>really</b> does is delete the dets tables.
%% 
%% @see init/0
%% @end

reset() ->
  file:delete("caseFoldingTable"),
  file:delete("bigFiveTable"),
  file:delete("ccciiTable"),
  file:delete("euckrTable"),
  file:delete("shiftjisTable"),
  file:delete("variantTable"),
  file:delete("normalizationTable"),
  init().

%% @spec init() -> atom()
%%
%% @doc reads the unicode- and encodings-related tables and builds them if absent
%%
%% @see reset/0
%% @end

init() ->
  loadCaseFolding(),
  loadEUR_KR(),
  loadSHIFTJIS(),
  loadBigFive(),
  loadCCCII(),
  loadVariant(),
  loadNorm(),
  done.

loadCaseFolding() ->
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Case Folding. Used for uppercase/lowercase   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  {Result, FileRef}=file:open(caseFoldingTable, read),
  case Result of
%% No file -- we have to create the database
    error ->
      io:format("Creating the caseFoldingTable database~n",[]),
      dets:open_file(caseFoldingTable,[]),
      io:format("Reading file CaseFolding~n",[]),
      Status=file:read_file("../priv/CaseFolding.txt"),
      case Status of
        {ok, Binary} ->
          io:format("Binary/CaseFolding.txt is good to go~n",[]),
          parseCaseFolding(split({utf8, Binary},{utf8, <<10>>})),
          dets:close(caseFoldingTable),
          dets:open_file(caseFoldingTable,[]);
        {What, Now} ->
          io:format("~w ~w~n",[What,Now]);
        _ ->
          io:format("Sigh...~n",[])
      end;
    ok ->
      file:close(FileRef),
      io:format("Reading the case folding database~n",[]),
      dets:open_file(caseFoldingTable,[])
  end.

loadBigFive() ->
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Big Five Encoding                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  {Result2, FileRef2}=file:open(bigFiveTable, read),
  case Result2 of
%% No file -- we have to create the database
    error ->
      io:format("Creating the bigFive database~n",[]),
      dets:open_file(bigFiveTable,[]),
%      dets:new(bigFiveTable,[named_table]),
      Status2=file:read_file("../priv/kBigFive.txt"),
      case Status2 of
        {ok, Binary2} ->
%          Bin2=binary_to_list(Binary2),
          io:format("Binary/BigFive.txt is good to go~n",[]),
          parseBigFive(split({utf8, Binary2},{utf8, <<10>>})),
          dets:close(bigFiveTable),
          dets:open_file(bigFiveTable,[]);
        {What2, Now2} ->
          io:format("~w ~w~n",[What2,Now2]);
        _ ->
          io:format("Sigh...~n",[])
      end;
    ok ->
      file:close(FileRef2),
      io:format("Reading the Big 5 database~n",[]),
      dets:open_file(bigFiveTable,[])
  end.

loadCCCII() ->
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CCCII Encoding                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  {Result2, FileRef2}=file:open(ccciiTable, read),
  case Result2 of
%% No file -- we have to create the database
    error ->
      io:format("Creating the CCCII database~n",[]),
      dets:open_file(ccciiTable, []),
%      dets:new(ccciiTable,[named_table]),
      Status2=file:read_file("../priv/kCCCII.txt"),
      case Status2 of
        {ok, Binary2} ->
%          Bin2=binary_to_list(Binary2),
          io:format("Binary/kCCCII.txt is good to go~n",[]),
          parseCCCII(split({utf8, Binary2},{utf8, <<10>>})),
          dets:close(ccciiTable),
          dets:open_file(ccciiTable,[]);
        {What2, Now2} ->
          io:format("~w ~w~n",[What2,Now2]);
        _ ->
          io:format("Sigh...~n",[])
      end;
    ok ->
      file:close(FileRef2),
      io:format("Reading the CCCII database~n",[]),
      dets:open_file(ccciiTable,[])
  end.

loadEUR_KR() ->
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EUC-KR Encoding [Korean]                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  {Result3, FileRef3}=file:open(euckrTable, read),
  case Result3 of
%% No file -- we have to create the database
    error ->
      io:format("Creating the EUC KR database~n",[]),
      dets:open_file(euckrTable, []),
%      dets:new(euckrTable,[named_table]),
      Status3=file:read_file("../priv/kEUCKR.txt"),
      case Status3 of
        {ok, Binary3} ->
%          Bin3=binary_to_list(Binary3),
%          io:format("Binary/kEUCKR.txt [~p bytes] is good to go~n",[length(Bin3)]),
          parseEUC_KR(split({utf8, Binary3},{utf8, <<10>>})),
          dets:close(euckrTable),
          dets:open_file(euckrTable,[]);
        {What3, Now3} ->
          io:format("~w ~w~n",[What3,Now3]);
        _ ->
          io:format("Sigh...~n",[])
      end;
    ok ->
      file:close(FileRef3),
      io:format("Reading the EUC-KR database~n",[]),
      dets:open_file(euckrTable,[])
  end.

loadSHIFTJIS() ->
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SHIFTJIS Encoding [Japanese]                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  {Result3, FileRef3}=file:open(shiftjisTable, read),
  case Result3 of
%% No file -- we have to create the database
    error ->
      io:format("Creating the SHIFT-JIS database~n",[]),
      dets:open_file(shiftjisTable, []),
%      dets:new(shiftjisTable,[named_table]),
      Status3=file:read_file("../priv/ShifJIS.txt"),
      case Status3 of
        {ok, Binary3} ->
%          Bin3=binary_to_list(Binary3),
%          io:format("Binary/ShifJIS.txt [~p bytes] is good to go~n",[length(Bin3)]),
          parseSHIFTJIS(split({utf8, Binary3},{utf8, <<10>>})),
          dets:close(ccciiTable),
          dets:open_file(ccciiTable,[]);
        {What3, Now3} ->
          io:format("~w ~w~n",[What3,Now3]);
        _ ->
          io:format("Sigh...~n",[])
      end;
    ok ->
      file:close(FileRef3),
      io:format("Reading the SHIFTJIS database~n",[]),
      dets:open_file(shiftjisTable,[])
  end.

loadVariant() ->
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CJK Variants [eg. 內 -> 内]                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  {Result3, FileRef3}=file:open(variantTable, read),
  case Result3 of
%% No file -- we have to create the database
    error ->
      io:format("Creating the Traditional Variants database~n",[]),
      dets:open_file(variantTable, []),
%      dets:new(variantTable,[named_table]),
      Status3=file:read_file("../priv/kTraditionalVariant.txt"),
      case Status3 of
        {ok, Binary3} ->
%          Bin3=binary_to_list(Binary3),
%          io:format("Binary/kTraditionalVariant.txt [~p bytes] is good to go~n",[length(Bin3)]),
          parseVariant(split({utf8, Binary3},{utf8, <<10>>})),
          dets:close(variantTable),
          dets:open_file(variantTable,[]);
        {What3, Now3} ->
          io:format("~w ~w~n",[What3,Now3]);
        _ ->
          io:format("Sigh...~n",[])
      end;
    ok ->
      file:close(FileRef3),
      io:format("Reading the Traditional Variants database~n",[]),
      dets:open_file(shiftjisTable,[])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseCaseFolding()                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseCaseFolding(Lines) ->
  io:format("~p lines to parse~n", [length(Lines)]),
  parseCaseFolding(Lines, 1).
parseCaseFolding([], Count) -> done;
parseCaseFolding([L|Ines], Count) ->
  [{utf8, Code}, {utf8, Status}, {utf8, Mapping}, {utf8, Name}] = split(L, {utf8, <<"; ">>}),
  CodeList=utf16Hex_to_utf8(Code),
  MappingList=utf16Hex_to_utf8(Mapping),
  dets:insert(caseFoldingTable,{Count, CodeList, Status, MappingList, Name}),
  parseCaseFolding(Ines, Count+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseBigFive() Used in encoding conversions  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseBigFive(Lines) ->
  io:format("~p lines to parse~n", [length(Lines)]),
  parseBigFive(Lines, 1).
parseBigFive([], Count) -> done;
parseBigFive([L|Ines], Count) ->
  [{utf8, Code}, {utf8, BigFiveCode}] = split(L, {utf8, <<";">>}),
  CodeList=utf16Hex_to_utf8(Code),
  BigFiveList=hexString_to_List(BigFiveCode),
  dets:insert(bigFiveTable,{CodeList, BigFiveList}),
  parseBigFive(Ines, Count+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseCCCII() Used in encoding conversions    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseCCCII(Lines) ->
  io:format("~p lines to parse~n", [length(Lines)]),
  parseCCCII(Lines, 1).
parseCCCII([], Count) -> done;
parseCCCII([L|Ines], Count) ->
  [{utf8, Code}, {utf8, CCCIICode}] = split(L, {utf8, <<";">>}),
  CodeList=utf16Hex_to_utf8(Code),
  CCCIIList=hexString_to_List(CCCIICode),
  dets:insert(ccciiTable,{CodeList, CCCIIList}),
  parseCCCII(Ines, Count+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseEUC_KR() Used in encoding conversions   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseEUC_KR(Lines) ->
  io:format("~p lines to parse~n", [length(Lines)]),
%  file:write_file("EUCKR.log", integer_to_list(length(Lines))++" lines to parse\n", [write]),
  parseEUC_KR(Lines, 1).
parseEUC_KR([], Count) -> done;
parseEUC_KR([L|Ines], Count) ->
  [{utf8, EUC_KRCode}, {utf8, Code}] = split(L, {utf8, <<";">>}),
  CodeList=utf16Hex_to_utf8(Code),
  EUC_KRList=hexString_to_List(EUC_KRCode),
  dets:insert(euckrTable,{EUC_KRList, CodeList}),
%  file:write_file("EUCKR.log", list_to_binary([EUC_KRList, <<"\t">>, CodeList, <<"\n">>]), [append]),
%  file:write_file("EUCKR.log", list_to_binary([<<"  ">>, EUC_KRCode]), [append]),
%  file:write_file("EUCKR.log", list_to_binary([<<"  ">>, Code, <<"\n">>]), [append]),
  parseEUC_KR(Ines, Count+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseSHIFTJIS() Used in encoding conversions %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseSHIFTJIS(Lines) ->
  io:format("~p lines to parse~n", [length(Lines)]),
  parseSHIFTJIS(Lines, 1).
parseSHIFTJIS([], Count) -> done;
parseSHIFTJIS([L|Ines], Count) ->
  [{utf8, SHIFTJISCode}, {utf8, Code}] = split(L, {utf8, <<";">>}),
  CodeList=utf16Hex_to_utf8(Code),
  SHIFTJISList=hexString_to_List(SHIFTJISCode),
  dets:insert(shiftjisTable,{SHIFTJISList, CodeList}),
  parseSHIFTJIS(Ines, Count+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseVariant() Used in encoding conversions  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseVariant(Lines) ->
  io:format("~p lines to parse~n", [length(Lines)]),
  parseVariant(Lines, 1).
parseVariant([], Count) -> done;
parseVariant([L|Ines], Count) ->
  [{_, Code}, {_, Variant}] = split(L, {utf8, <<";">>}),
  CodeList=utf16Hex_to_utf8(Code),
  VariantList=utf16Hex_to_utf8(Variant),
  dets:insert(variantTable,{CodeList, VariantList}),
%  io:format("dets:insert(variantTable,{~w, ~w}),~n",[CodeList, VariantList]),
  parseVariant(Ines, Count+1).

hexString_to_List(Code) ->
  Code1=convert16To1oInt(Code),
  <<Code1:16>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                              %%
%% MB string creation                           %%
%%                                              %%
%% There are several ways of creating a string: %%
%% * new(Params*)                               %%
%%   Params optional, defaults to utf8          %%
%% * <encoding>_to_<encoding>(MBString)         %%
%% * convertEncoding(MBString, NewEncoding)     %%
%%                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec new() -> mbString()
%%
%% @doc creates a new empty mb string, a tuple {encoding_atom, &lt;&lt;"">>}.
%%
%% <p>Example</p>
%% <pre>
%% 1> HAN=mb:new("").
%% {utf8,&lt;&lt;>>}
%% </pre>
%% @see new/1
%% @see new/2
%% @see new/3
%% @end
new() ->
%% create an empty string with utf-8 by default
  new(<<"">>, utf8).

%% @spec new(String::list()) -> mbString()
%%
%% @doc creates a new mb string, a tuple {encoding_atom, &lt;&lt;Binary_String>>}. You can specify the encoding, and even request an on-the-fly conversion.
%%
%% <p>Example</p>
%% <pre>
%% 1> HAN=mb:new("\345\257\222"). ==> That's [U+5BD2]/Cold.
%% {utf8,&lt;&lt;229,175,146>>}
%% </pre>
%% @see new/0
%% @see new/2
%% @see new/3
%% @end

new(String) when list(String) ->
%% create a string with utf-8 by default
  new(list_to_binary(String), utf8);

new(String) when binary(String) ->
%% create a string with utf-8 by default from a binary object
%% possibly coming from a file or a TCP transfer
  new(String, utf8);

new(Encoding) when atom(Encoding) ->
%% create an empty string with specified encoding
  case Encoding of
    utf8     -> new(<<"">>,Encoding);
    utf16    -> new(<<"">>,Encoding);
    ascii    -> new(<<"">>,Encoding);
    latin1   -> new(<<"">>,Encoding);
    latin2   -> new(<<"">>,Encoding);
    latin3   -> new(<<"">>,Encoding);
    latin4   -> new(<<"">>,Encoding);
    latin5   -> new(<<"">>,Encoding);
    latin6   -> new(<<"">>,Encoding);
    latin9   -> new(<<"">>,Encoding);
    latin10  -> new(<<"">>,Encoding);
    macRoman -> new(<<"">>,Encoding);
    cp1252   -> new(<<"">>,Encoding);
    cp437    -> new(<<"">>,Encoding);
    bigFive  -> new(<<"">>,Encoding);
    cccii    -> new(<<"">>,Encoding);
    euckr    -> new(<<"">>,Encoding);
    shiftjis -> new(<<"">>,Encoding);
    _        -> unknown
  end.

%% @spec new(String::list(), OriginalEncoding::atom()) -> mbString()
%%
%% @doc creates a new mb string, a tuple {encoding_atom, &lt;&lt;Binary_String>>}. You can specify the encoding, and even request an on-the-fly conversion.
%%
%% <p>Example</p>
%% <pre>
%% 2> HONG=mb:new(&lt;&lt;"\345\274\230">>). ==> That's [U+5F18]/Strong
%% {utf8,&lt;&lt;229,188,152>>}
%% </pre>
%% @see new/0
%% @see new/1
%% @see new/3
%% @end

new(String, Encoding) when list(String), atom(Encoding) ->
% String is already [hopefully] encoded with the required Encoding
% String is a [List].
  new(list_to_binary(String), Encoding);
new(String, Encoding) when binary(String), atom(Encoding) ->
% String is already [hopefully] encoded with the required Encoding
% We won't check whether the encoding is proper. Your problem.
% String is a <<Binary>>.
  case Encoding of
    utf8 ->   {Encoding, String};
    utf16 ->  {Encoding, String};
    ascii ->
      case isASCII(String) of
        true -> {Encoding, String};
        false -> this_is_not_an_ASCII_string
      end;
    latin1   -> {Encoding, String};
    latin2   -> {Encoding, String};
    latin3   -> {Encoding, String};
    latin4   -> {Encoding, String};
    latin5   -> {Encoding, String};
    latin6   -> {Encoding, String};
    latin9   -> {Encoding, String};
    latin10  -> {Encoding, String};
    macRoman -> {Encoding, String};
    cp1252   -> {Encoding, String};
    cp437    -> {Encoding, String};
    bigFive  -> {Encoding, String};
    cccii    -> {Encoding, String};
    euckr    -> {Encoding, String};
    shiftjis -> {Encoding, String};
    _        -> unknown
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% String2 is encoded with the standard shell encoding [Encoding2] %%
%% which is possibly utf8 – at least on my Mac :D                  %%
%% or another encoding – source being a file or else...            %%
%% and it needs converting before assignment.                      %%
%% Of course you need to know the encoding of the original string. %%
%% you can do something like                                       %%
%% L1=mb:new("Décembre", utf8, latin1).                            %%
%% --> L1 will contain "Décembre" but encoded in Latin-1           %%
%% Nifty, no?                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec new(String::list(), OriginalEncoding::atom(), RequestedEncoding::atom()) -> mbString()
%%
%% @doc creates a new mb string, a tuple {encoding_atom, &lt;&lt;Binary_String>>}. You can specify the encoding, and even request an on-the-fly conversion.
%%
%% <p>Example</p>
%% <pre>
%% 3> HONG2=mb:new("\345\274\230", utf8, utf16).                                    
%% {utf16,&lt;&lt;95,24>>}
%% </pre>
%% @see new/0
%% @see new/1
%% @see new/2
%% @end

new([], Encoding, Encoding2) when atom(Encoding), atom(Encoding2) ->
%% Easiest case: empty string. Return that.
  {Encoding,<<"">>};
new(<<"">>, Encoding, Encoding2) when atom(Encoding), atom(Encoding2) ->
%% Easiest case: empty string. Return that.
  {Encoding,<<"">>};

new(String2, Encoding, Encoding2) when list(String2), atom(Encoding), atom(Encoding2) ->
%% Here again the string is a binary, possibly from a file or a TCP transfer
  new(list_to_binary(String2), Encoding, Encoding2);

new(String2, Encoding, Encoding2) when binary(String2), atom(Encoding), atom(Encoding2) ->
  {Enc, String}=convertEncoding({Encoding, String2}, Encoding2),
%% Convert the string to the proper
  case Enc of
    utf8 ->   {Enc, String};
    utf16 ->  {Enc, String};
    ascii ->
      case isASCII(String) of
        true -> {Enc, String};
        false -> this_is_not_an_ASCII_string
      end;
    latin1   -> {Enc, String};
    latin2   -> {Enc, String};
    latin3   -> {Enc, String};
    latin4   -> {Enc, String};
    latin5   -> {Enc, String};
    latin6   -> {Enc, String};
    latin9   -> {Enc, String};
    latin10  -> {Enc, String};
    macRoman -> {Enc, String};
    cp1252   -> {Enc, String};
    cp437    -> {Enc, String};
    bigFive  -> {Enc, String};
    cccii    -> {Enc, String};
    euckr    -> {Enc, String};
    shiftjis -> {Enc, String};
    _        -> unknown
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                        %%
%% Encoding Conversions                   %%
%% Another way of creating MB Strings     %%
%%                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                        %%
%% Main API entry                         %%
%%                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec convertEncoding(OriginalString::mbString(), NewEncoding::atom()) -> mbString()
%%
%% @doc converts an mb string from Encoding to NewEncoding.
%%
%% <p>Example</p>
%% <pre>
%% 1> HONG2=mb:convertEncoding({utf8, "\345\274\230"}, utf16).                                    
%% {utf16,&lt;&lt;95,24>>}
%% </pre>
%% @end

convertEncoding({Encoding, String}, NewEncoding) ->
  case Encoding of
    utf8 ->
      case NewEncoding of
        utf8     -> {Encoding, String};
        utf16    -> utf8_to_utf16({utf8, String});
        bigFive  -> utf8_to_BigFive({utf8, String});
        cccii    -> utf8_to_CCCII({utf8, String});
        euckr    -> utf8_to_euckr({utf8, String});
        shiftjis -> utf8_to_shiftjis({utf8, String});
        _        -> utf8_to_oneByte({utf8, String}, NewEncoding)
      end;
    utf16 ->
      case NewEncoding of
        utf16    -> {Encoding, String};
        utf8     -> utf16_to_utf8({utf16, String});
        bigFive  -> utf8_to_BigFive(utf16_to_utf8({utf16, String}));
        cccii    -> utf8_to_CCCII(utf16_to_utf8({utf16, String}));
        euckr    -> utf8_to_euckr(utf16_to_utf8({utf16, String}));
        shiftjis -> utf8_to_shiftjis(utf16_to_utf8({utf16, String}));
        _        -> utf8_to_oneByte(utf16_to_utf8({utf16, String}), NewEncoding)
      end;
    bigFive ->
      case NewEncoding of
        bigFive  -> {Encoding, String};
        utf8     -> bigFive_to_utf8({bigFive, String});
        utf16    -> utf8_to_utf16(bigFive_to_utf8({bigFive, String}));
        cccii    -> utf8_to_CCCII(bigFive_to_utf8({bigFive, String}));
        euckr    -> utf8_to_euckr(bigFive_to_utf8({bigFive, String}));
        shiftjis -> utf8_to_shiftjis(bigFive_to_utf8({bigFive, String}));
        _        -> utf8_to_oneByte(bigFive_to_utf8({bigFive, String}), NewEncoding)
      end;
    cccii ->
      case NewEncoding of
        cccii   -> {Encoding, String};
        utf8    -> cccii_to_utf8({cccii, String});
        utf16   -> utf8_to_utf16(cccii_to_utf8({cccii, String}));
        bigFive -> utf8_to_BigFive(cccii_to_utf8({cccii, String}));
        euckr   -> utf8_to_euckr(cccii_to_utf8({cccii, String}));
        shiftjis -> utf8_to_shiftjis(cccii_to_utf8({cccii, String}));
        _       -> utf8_to_oneByte(cccii_to_utf8({cccii, String}), NewEncoding)
      end;
    euckr ->
      case NewEncoding of
        euckr   -> {Encoding, String};
        utf8    -> euckr_to_utf8({euckr, String});
        utf16   -> utf8_to_utf16(euckr_to_utf8({euckr, String}));
        bigFive -> utf8_to_BigFive(euckr_to_utf8({euckr, String}));
        cccii   -> utf8_to_CCCII(euckr_to_utf8({euckr, String}));
        shiftjis -> utf8_to_shiftjis(euckr_to_utf8({euckr, String}));
        _       -> utf8_to_oneByte(euckr_to_utf8({euckr, String}), NewEncoding)
      end;
    shiftjis ->
      case NewEncoding of
        shiftjis   -> {Encoding, String};
        utf8    -> shiftjis_to_utf8({shiftjis, String});
        utf16   -> utf8_to_utf16(shiftjis_to_utf8({shiftjis, String}));
        bigFive -> utf8_to_BigFive(shiftjis_to_utf8({shiftjis, String}));
        cccii   -> utf8_to_CCCII(shiftjis_to_utf8({shiftjis, String}));
        euckr -> utf8_to_euckr(shiftjis_to_utf8({shiftjis, String}));
        _       -> utf8_to_oneByte(shiftjis_to_utf8({shiftjis, String}), NewEncoding)
      end;
    _    ->
      case NewEncoding of
        utf8    -> oneByte_to_utf8({Encoding, String});
        utf16   -> oneByte_to_utf8(utf16_to_utf8({utf16, String}));
        bigFive -> {NewEncoding, String}; % Not doing anything...
        cccii   -> {NewEncoding, String}; % Not doing anything...
        euckr   -> {NewEncoding, String}; % Not doing anything...
        shiftjis -> {NewEncoding, String}; % Not doing anything...
        _       -> oneByte_to_oneByte({Encoding, String}, NewEncoding)
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                        %%
%% MBString Info                          %%
%%                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
len(MyString) ->  len(MyString,0).
len({Encoding,<<"">>},Acc) -> Acc;
len(MyString,Acc) ->
  {Char,String2} = getNextChar(MyString),
  len(String2,Acc+1).

lenB({utf8, String}) ->  size(String).

isASCII([S|Tring]) -> isAscii(lists:flatten([S,Tring]));
isASCII({utf8, String}) -> isAscii(String).
isAscii(<<"">>) -> true;
isAscii(<<S:8, Tring/binary>>) ->
  if
    S>127 -> false;
    S<128 -> isAscii(Tring)
  end.

charToInt(Char) -> charToInt(lists:reverse(Char),0,1).
charToInt([],Acc,Pow) -> Acc;
charToInt([H|T],Acc,Pow) ->
  charToInt(T,Acc+H*element(Pow,{1, 256, 65536, 16777216}),Pow+1).

hasProcess(X) ->
  hasProcess(X,registered()).
hasProcess(X,[]) -> no;
hasProcess(X,[X|_]) -> yes;
hasProcess(X,[H|T]) -> hasProcess(X,T).

hasTable(X) ->
  hasTable(X,dets:all()).
hasTable(X,[]) -> no;
hasTable(X,[X|_]) -> yes;
hasTable(X,[H|T]) -> hasTable(X,T).

getNextCharAsInt({Encoding, String}) -> getNextCharAsInt(Encoding, String,<<"">>).
getNextCharAsInt(ascii, <<S:8, Tring/binary>>, Acc) ->
  { S, {ascii, Tring}};
getNextCharAsInt(utf8, <<S:8, Tring/binary>>, Acc) ->
  if
    S<128 -> {S, {utf8, Tring}};
    S<224 ->
      <<T:8, Ring/binary>>=Tring,
      {S*256+T,{utf8, Ring}};
    S<240 ->
      <<T:8, Ring/binary>>=Tring,
      <<R:8, Ing/binary>>=Ring,
      {S*65536+T*256+R,{utf8,Ing}};
    S>239 ->
      <<T:8, Ring/binary>>=Tring,
      <<R:8, Ing/binary>>=Ring,
      <<I:8, Ng/binary>>=Ing,
      {S*16777216+T*65536+R*256+I,{utf8,Ng}}
  end.  

bocu({utf8,String}) ->
  Prev=64,
  bocu1({utf8,String}, Prev,term_to_binary([])).

bocu1({_,[]},Prev,Acc) -> Acc;
bocu1(MBString,Prev,Acc) ->
  {C,MBString2}=getNextCharAsInt(MBString),
  if
    C<32 -> bocu1(MBString2, 64,term_to_binary(binary_to_term(Acc) ++ C));
    C==32 -> bocu1(MBString2, Prev,term_to_binary(binary_to_term(Acc) ++ C));
    C>32 ->
      Diff= C - Prev,
      if
        Diff > -65, Diff < 64 ->
         bocu1(MBString2, 64,term_to_binary(binary_to_term(Acc) ++ [Diff+144])); 
        Diff < -64 ->
         bocu1(MBString2, 64,term_to_binary(binary_to_term(Acc) ++ [Diff+144]));
        Diff > 63 ->
             bocu1(MBString2, 64,term_to_binary(binary_to_term(Acc) ++ [Diff+144]))
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Other crap                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A simple formatting utility:           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec format(MBString::mbString()) -> list()
%%
%% @doc formats the mbstring for output.
%%
%% <p>Example</p>
%% <pre>
%% 1> HAN=mb:new("\345\257\222"). ==> That's [U+5BD2]/Cold.
%% {utf8,&lt;&lt;229,175,146>>}
%% 2> io:format("~s~n",[mb:format(HAN)]).
%% \345\257\222
%% </pre> Just a hack really.
%% @see print/1
%% @see print/2
%% @see fwrite/2
%% @end
format({Encoding, String}) ->
  lists:flatten(io_lib:format("~s",[binary_to_list(String)])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A simple console output utility:       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec print(MBString::mbString()) -> atom()
%%
%% @see format/1
%% @see print/2
%% @see fwrite/2
%% @end
print({Encoding, String}) ->
  print({Encoding, String}, "\n").

%% @spec print(MBString::mbString(), Params::list()) -> atom()
%%
%% @doc formats an mbstring and outputs it.
%%
%% <p>Example</p>
%% <pre>
%% 1> HAN=mb:new("\345\257\222"). ==> That's [U+5BD2]/Cold.
%% {utf8,&lt;&lt;229,175,146>>}
%% 2> mb:print(HAN, ["\n"]).
%% \345\257\222
%% ok
%% </pre>
%% @see format/1
%% @see print/1
%% @see fwrite/2
%% @end
print({Encoding, String}, Params) ->
  A=io:format("~s"++Params,[binary_to_list(String)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Output to a file:                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec fwrite(MBString::mbString(), FileName::list()) -> atom()
%%
%% @doc outputs an mbstring to a file.
%%
%% <p>Example</p>
%% <pre>
%% 1> HAN=mb:new("\345\257\222"). ==> That's [U+5BD2]/Cold.
%% {utf8,&lt;&lt;229,175,146>>}
%% 2> mb:fwrite(HAN, MyFile).
%% ok
%% </pre>
%% @see format/1
%% @see print/1
%% @see print/2
%% @end
fwrite({Encoding, String}, FileName) ->
  file:write_file(FileName, String).

