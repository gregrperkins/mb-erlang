%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                        %%
%% Conversion utilities                   %%
%%                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

oneByte_to_oneByte({Encoding, String}, NewEncoding) ->
  utf8_to_oneByte(oneByte_to_utf8({Encoding, String}), NewEncoding).

oneByte_to_utf8({Encoding, String}) ->
  case Encoding of
    latin1    -> oneByte_to_utf8(String, <<"">>, latin1());
    latin2    -> oneByte_to_utf8(String, <<"">>, latin2());
    latin3    -> oneByte_to_utf8(String, <<"">>, latin3());
    latin4    -> oneByte_to_utf8(String, <<"">>, latin4());
    latin5    -> oneByte_to_utf8(String, <<"">>, latin5());
    latin6    -> oneByte_to_utf8(String, <<"">>, latin6());
    latin9    -> oneByte_to_utf8(String, <<"">>, latin9());
    latin10   -> oneByte_to_utf8(String, <<"">>, latin10());
    cp1252    -> oneByte_to_utf8(String, <<"">>, cp1252());
    cp437     -> oneByte_to_utf8(String, <<"">>, cp437());
    macRoman  -> oneByte_to_utf8(String, <<"">>, macRoman());
    _         -> unknown_encoding
  end.
oneByte_to_utf8(<<"">>,    Acc, EncTable) -> {utf8, Acc};
oneByte_to_utf8(<<S:8, Tring/binary>>, Acc, EncTable) ->
  if
    S<128 ->
      oneByte_to_utf8(Tring, list_to_binary([Acc,S]), EncTable);
    S>127 ->
      oneByte_to_utf8(Tring, list_to_binary([Acc,element(S-127,EncTable)]), EncTable)
  end.

utf8_to_oneByte({utf8, String}, Encoding) ->
  case Encoding of
    latin1    -> utf8_to_oneByte(String, <<"">>, latin1L(),    Encoding);
    latin2    -> utf8_to_oneByte(String, <<"">>, latin2L(),    Encoding);
    latin3    -> utf8_to_oneByte(String, <<"">>, latin3L(),    Encoding);
    latin4    -> utf8_to_oneByte(String, <<"">>, latin4L(),    Encoding);
    latin5    -> utf8_to_oneByte(String, <<"">>, latin5L(),    Encoding);
    latin6    -> utf8_to_oneByte(String, <<"">>, latin6L(),    Encoding);
    latin9    -> utf8_to_oneByte(String, <<"">>, latin9L(),    Encoding);
    latin10   -> utf8_to_oneByte(String, <<"">>, latin10L(),   Encoding);
    cp1252    -> utf8_to_oneByte(String, <<"">>, cp1252L(),    Encoding);
    cp437     -> utf8_to_oneByte(String, <<"">>, cp437L(),     Encoding);
    macRoman  -> utf8_to_oneByte(String, <<"">>, macRomanL(),  Encoding);
    _         -> unknown_encoding
  end.

utf8_to_oneByte(<<"">>, Acc, EncTable, Encoding) -> {Encoding,list_to_binary([Acc])};
utf8_to_oneByte(String, Acc, EncTable, Encoding) ->
  {{_,S},{_,Tring}}=getNextChar({utf8,String}),
  case size(S) of
    1 ->
      utf8_to_oneByte(Tring, list_to_binary([Acc,S]), EncTable, Encoding);
    _ ->
      H2=lookup(S, EncTable),
      utf8_to_oneByte(Tring, list_to_binary([Acc,H2]), EncTable, Encoding)
  end.

lookup(X, EncTable) ->
%%  XX=binary_to_list(X),
  lookup(X, EncTable, 0).
lookup(X, [], Acc) -> "ǂ";
lookup(X, [X|T], Acc) ->
  list_to_binary([Acc+128]);
lookup(X, [H|T], Acc) ->
  lookup(X, T, Acc+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTF-16 to UTF-8 conversion             %%
%% Not yet complete                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
utf16_to_utf8({utf16, String}) ->
  utf16_to_utf8(String, <<"">>).
utf16_to_utf8(<<"">>, Acc) -> {utf8, Acc};
utf16_to_utf8(<<D:16, Rest/binary>>, Acc) ->
  utf16_to_utf8(Rest, list_to_binary([Acc,utf16Char_to_utf8([D])])).
utf16Char_to_utf8([D]) ->
  B=io_lib:fwrite("~.2B",[D]),
  utf162utf8(B, D).

utf16Hex_to_utf8(X) ->
  [D,O,B]=convert16(X),
  utf162utf8(B, list_to_integer(D)).

utf162utf8(AA, D) when D <128 ->
  <<D>>;
utf162utf8(AA, D) when D < 2048 ->
  {utf8, A}=right({utf8, list_to_binary([<<"000000000000000000000">>, list_to_binary(AA)])},11),
  A1=binary_to_list(A),
  B=lists:flatten(io_lib:fwrite("110~s10~s",[string:substr(A1, 1, 5),string:substr(A1, 6)])),
  [D2,H,O2]=convert2(B),
  D3=list_to_integer(D2),
  <<D3:16>>;
utf162utf8(AA, D) when D < 65536 ->
  {utf8, A}=right({utf8, list_to_binary([<<"000000000000000000000">> , list_to_binary(AA)])},16),
  A1=binary_to_list(A),
  B=lists:flatten(io_lib:fwrite("1110~s10~s10~s",[string:substr(A1,1,4), string:substr(A1,5,6), string:substr(A1,11)])),
  [D2,H,O2]=convert2(B),
  DD=list_to_integer(D2),
  <<DD:24>>;
utf162utf8(AA,D) when D > 65535 ->
  {utf8,A}=right({utf8, list_to_binary([<<"000000000000000000000">>, list_to_binary(AA)])},21),
  A1=binary_to_list(A),
  B=lists:flatten(io_lib:fwrite("11110~s10~s10~s10~s",[string:substr(A1,1,3), string:substr(A1,4,6),
    string:substr(A1,10,6), string:substr(A1,16)])),
  [D2,H,O2]=convert2(B),
  DD=list_to_integer(D2),
  <<DD:32>>.

utf8_to_utf16({utf8,String}) ->
  utf8_to_utf16(String, <<"">>).
utf8_to_utf16(<<"">>, Acc) -> {utf16, Acc};
utf8_to_utf16(String, Acc) ->
  {{_, Char}, {_,Rest}}=getNextChar({utf8,String}),
  case size(Char) of
    1 -> 
      H2=utf8Char_to_utf16(list_to_binary([<<0,0>>, Char]));
    2 -> 
      H2=utf8Char_to_utf16(list_to_binary([<<0,0>>, Char]));
    3 -> 
      H2=utf8Char_to_utf16(Char);
    4 -> 
      H2=utf8Char_to_utf16(Char)
  end,
  utf8_to_utf16(Rest, list_to_binary([Acc,H2])).

utf8Char_to_utf16(H2) ->
  S=size(H2)*8,
  <<D:S>> = H2,
  [B|_]=io_lib:fwrite("~.2B",[D]),
  utf82utf16(B, length(B), D).

utf8Hex_to_utf16(X) ->
  [D,O,A]=convert16(X),
  utf82utf16(A,length(A),D).

utf82utf16(A, L, D) when L < 9 ->
  list_to_binary([<<0>>,D]);

utf82utf16(A,L,D) when L < 17 ->
  N=lists:flatten(io_lib:fwrite("~s~s",[string:substr(A,5,4),string:substr(A,11,6)])),
  N0=length(N),
  [N1|_]=convert2(string:substr(N,1,N0-8)),
  [N2|_]=convert2(string:substr(N,N0-8)),
  [list_to_integer(N1), list_to_integer(N2)];

utf82utf16(A,L,D) when L < 25 ->
  N=lists:flatten(io_lib:fwrite("~s~s~s",[string:substr(A,5,4), string:substr(A,11,6), string:substr(A,19,6)])),
  [N1|_]=convert2(string:substr(N,1,length(N)-8)),
  [N2|_]=convert2(string:substr(N,9)),
  [list_to_integer(N1), list_to_integer(N2)];

utf82utf16(A,L,D) when L < 33 ->
  N=lists:flatten(io_lib:fwrite("~s~s~s~s",[string:substr(A,5,4), string:substr(A,11,6),
    string:substr(A,19,6), string:substr(A,27,6)])),
  [N1|_]=convert2(string:substr(N,1,length(N)-16)),
  [N2|_]=convert2(string:substr(N,length(N)-15,8)),
  [N3|_]=convert2(string:substr(N,length(N)-7,8)),
  NN1=list_to_integer(N1),
  NN2=list_to_integer(N2),
  NN3=list_to_integer(N3),
  [NN1, NN2, NN3].

%% Fix This Code!
%% Doesn't seem to produce the expected result?!?
%% Maybe make a table from NormalizationText.txt
%% Should be easier...
surrogate(CP) ->
  [CP1|_]=convert16(CP),
  surrogateInt(list_to_integer(CP1)).
surrogateInt(Codepoint) ->
  LEAD_OFFSET = 55296 - (65536 bsr 10),
  SURROGATE_OFFSET = 65536 - (55296 bsl 10) - 56320,
  Lead = LEAD_OFFSET + (Codepoint bsr 10),
  Trail = 56320 + (Codepoint band 1023),
  Lead1 = Lead div 256,
  Lead2 = Lead - (Lead1 * 256),
  Trail1 = Trail div 256,
  Trail2 = Trail - (Trail1 * 256),
  list_to_binary([Lead1, Lead2, Trail1, Trail2]).

utf8_to_BigFive({utf8, String}) -> utf8_to_BigFive(String, <<"">>).
utf8_to_BigFive(<<>>, Acc) -> {bigFive, list_to_binary([Acc])};
utf8_to_BigFive(String, Acc) ->
  {{_, Char}, {_, Rest}} = getNextChar({utf8, String}),
  H=lists:flatten(dets:match(bigFiveTable,{Char,'$1'})),
  case H of
    [] -> %% No match
      utf8_to_BigFive(Rest, [Acc, <<"??">>]);
    _ -> %% Match
      [H2|_]=H,
      utf8_to_BigFive(Rest, [Acc, H])
  end.

bigFive_to_utf8({bigFive, String}) -> bigFive_to_utf8(String, []).
bigFive_to_utf8(<<"">>, Acc) -> {utf8, list_to_binary([Acc])};
bigFive_to_utf8(String, Acc) ->
  {{_, Char}, {_, Rest}} = getNextChar({bigFive, String}),
  H=lists:flatten(dets:match(bigFiveTable,{'$1',Char})),
  case H of
    [] -> %% No match
      bigFive_to_utf8(Rest, [Acc, <<"??">>]);
    _ -> %% Match
      bigFive_to_utf8(Rest, [Acc, <<H>>])
  end.

utf8_to_CCCII({utf8, String}) -> utf8_to_CCCII(String, []).
utf8_to_CCCII(<<"">>, Acc) -> {cccii, list_to_binary([Acc])};
utf8_to_CCCII(String, Acc) ->
  {{_, Char}, {_, Rest}} = getNextChar({utf8, String}),
  H=lists:flatten(dets:match(ccciiTable,{Char,'$1'})),
  case H of
    [] -> %% No match
      case size(Char) of
        1 -> utf8_to_CCCII(Rest, [Acc, <<0,0,Char>>]);
        _ -> utf8_to_CCCII(Rest, [Acc, <<"??">>])
      end;
    _ -> %% Match
      utf8_to_CCCII(Rest, [Acc, <<H>>])
  end.

cccii_to_utf8({cccii, String}) -> cccii_to_utf8(String, []).
cccii_to_utf8(<<"">>, Acc) -> {utf8, list_to_binary([Acc])};
cccii_to_utf8(String, Acc) ->
  {{_, Char}, {_, Rest}} = getNextChar({cccii, String}),
  H=lists:flatten(dets:match(ccciiTable,{'$1', Char})),
  case H of
    [] -> %% No match
      cccii_to_utf8(Rest, [Acc, <<"??">>]);
    _ -> %% Match
      cccii_to_utf8(Rest, [Acc, <<H>>])
  end.

utf8_to_euckr({utf8, String}) -> utf8_to_euckr(String, <<"">>).
utf8_to_euckr(<<"">>, Acc) -> {euckr, list_to_binary([Acc])};
utf8_to_euckr(String, Acc) ->
  {{_, Char}, {_, Rest}} = getNextChar({utf8, String}),
  H=lists:flatten(dets:match(euckrTable,{'$1',Char})),
  case H of
    [] -> %% No match
      case size(Char) of
        1 -> utf8_to_euckr(Rest, [Acc, <<0,0,Char>>]);
        _ -> utf8_to_euckr(Rest, [Acc, <<"??">>])
      end;
    _ -> %% Match
      [H2|_]=H,
      utf8_to_euckr(Rest, [Acc, H2])
  end.

euckr_to_utf8({euckr, String}) -> euckr_to_utf8(String, []).
euckr_to_utf8(<<"">>, Acc) -> {utf8, list_to_binary([Acc])};
euckr_to_utf8(String, Acc) ->
  {{_, Char}, {_, Rest}} = getNextChar({euckr, String}),
  H=lists:flatten(dets:match(euckrTable,{Char,'$1'})),
  case H of
    [] -> %% No match
      euckr_to_utf8(Rest, [Acc, <<"??">>]);
    _ -> %% Match
      euckr_to_utf8(Rest, [Acc, <<H>>])
  end.

utf8_to_shiftjis({utf8, String}) -> utf8_to_shiftjis(String, <<"">>).
utf8_to_shiftjis(<<"">>, Acc) -> {shiftjis, list_to_binary([Acc])};
utf8_to_shiftjis(String, Acc) ->
  {{_, Char}, {_, Rest}} = getNextChar({utf8, String}),
  H=lists:flatten(dets:match(shiftjisTable,{'$1',Char})),
  case H of
    [] ->
      Variant=hasSimplified(Char),
      case Variant of
        [] -> %% No match
          case size(Char) of
            1 -> utf8_to_shiftjis(Rest, [Acc, Char]);
            _ -> utf8_to_shiftjis(Rest, [Acc, <<"??">>])
          end;
        _  -> %% Match
          H2=lists:flatten(dets:match(shiftjisTable, {'$1',Variant})),
          case H2 of
            [] -> % No match
              utf8_to_shiftjis(Rest, [Acc, <<"??">>]);
            _  ->
              utf8_to_shiftjis(Rest, [Acc, <<H2>>])
          end
      end;
    _  ->
      utf8_to_shiftjis(Rest, [Acc, list_to_binary(H)])
  end.

shiftjis_to_utf8({shiftjis, String}) -> shiftjis_to_utf8(String, []).
shiftjis_to_utf8(<<"">>, Acc) -> {utf8, list_to_binary([Acc])};
shiftjis_to_utf8(String, Acc) ->
  {{_, Char}, {_, Rest}} = getNextChar({shiftjis, String}),
  H=lists:flatten(dets:match(shiftjisTable,{Char,'$1'})),
  case H of
    [] -> %% No match
      Variant=hasTraditional(Char),
      case Variant of
        [] -> %% No match
          case size(Char) of
            1 -> shiftjis_to_utf8(Rest, [Acc, <<Char>>]);
            _ -> shiftjis_to_utf8(Rest, [Acc, <<"@">>])
          end;
        _  -> %% Match
          H2=lists:flatten(dets:match(shiftjisTable,{Variant, '$1'})),
          case H2 of
            [] -> % No match
              shiftjis_to_utf8(Rest, [Acc, <<"@">>]);
            _  ->
              shiftjis_to_utf8(Rest, [Acc, <<H2>>])
          end
      end;
    _ -> %% Match
      shiftjis_to_utf8(Rest, [Acc, <<H>>])
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                        %%
%% LowerCase / Uppercase                  %%
%%                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lowercase({utf8, String}) -> lowercase({utf8, String}, []);
lowercase({Encoding, String}) ->
  String2=convertEncoding({Encoding, String}, utf8),
  {Encoding2, String3}=lowercase(String2, []),
  convertEncoding({utf8, String3}, Encoding2).
  

lowercase({utf8, <<"">>}, Acc) -> {utf8, Acc};
lowercase({Encoding, String}, Acc) ->
  {{Enc1, Char}, {Enc2, Rest}} = getNextChar({Encoding, String}),
  H=lists:flatten(dets:match(caseFoldingTable,{'_', Char, <<"C">>, '$1', '_'})),
  case H of
    [] -> %% No match
      lowercase({Enc2, Rest}, list_to_binary([Acc,Char]));
    _ -> %% Match
      lowercase({Enc2, Rest}, list_to_binary([Acc, H]))
  end.

uppercase({utf8, String}) -> uppercase({utf8, String}, []);
uppercase({Encoding, String}) ->
  String2=convertEncoding({Encoding, String}, utf8),
  {Encoding2, String3}=uppercase(String2, <<"">>),
  convertEncoding({Encoding2, String3}, Encoding).

uppercase({Encoding, <<"">>}, Acc) -> {Encoding, Acc};
uppercase({Encoding, String}, Acc) ->
  {{Enc1, Char}, {Enc2, Rest}} = getNextChar({Encoding, String}),
  H=lists:flatten(dets:match(caseFoldingTable,{'_', '$1', <<"C">>, Char, '_'})),
  case H of
    [] -> %% No match
      uppercase({Enc2, Rest}, list_to_binary([Acc,Char]));
    _ -> %% Match
      uppercase({Enc2, Rest}, list_to_binary([Acc, H]))
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Other crap                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert16To1oInt(Str) when binary(Str) ->
  convert16To1oInt(binary_to_list(Str));
convert16To1oInt(Str) ->
    {ok, [Num], _ } = io_lib:fread("~16u", Str),
    Num.
convert16(Str) when binary(Str) ->
  convert16(binary_to_list(Str));
convert16(Str) ->
    {ok, [Num], _ } = io_lib:fread("~16u", Str),
    io_lib:fwrite("~B~.8B~.2B", [Num, Num, Num]).
convert10(Num) ->
    io_lib:fwrite("~.16B~.8B~.2B", [Num, Num, Num]).
convert10_to_16([H,M,L]) ->
    io_lib:fwrite("~.16B", [H*65536+M*256+L]).
convert2(Str) ->
    {ok, [Num], _ } = io_lib:fread("~2u", Str),
    io_lib:fwrite("~B~.16B~.8B",
        [Num, Num, Num]).

