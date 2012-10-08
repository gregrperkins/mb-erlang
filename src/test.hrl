%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                        %%
%% Test Suite                             %%
%%                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(all) ->
  io:format("Encoding tables~n",[]),
  test(encodingTable),
  io:format("Hanzi Encodings~n",[]),
  test(hanzi),
  io:format("Testing Normalization.~n",[]),
  test(normalization),
  io:format("Testing strLength.~n",[]),
  test(strLength),
  io:format("Done.~n",[]),
  done;

test(encodingTable) ->
  file:delete("tests/Test_EncodingTable.html"),
  file:write_file("tests/Test_EncodingTable.html",
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<HTML>\n<HEAD>\n  <TITLE>Table of one-byte Encodings the mb module groks</TITLE>\n  <meta http-equiv=\"content-type\" content=\"text/html;charset=utf-8\">\n</HEAD>\n<BODY style='font: 300 11px Times;'>\n<!-- Produced automatically by the mb module's test suite -->\n<table border='1'>\n<tr>\n  <td width='50'>Code</td>\n  <td width='50'>UTF-16</td>\n  <td width='50'>Latin-1</td>\n  <td width='50'>Latin-2</td>\n  <td width='50'>Latin-3</td>\n  <td width='50'>Latin-4</td>\n  <td width='50'>Latin-5</td>\n  <td width='50'>Latin-6</td>\n  <td width='50'>Latin-9</td>\n  <td width='50'>Latin-10</td>\n  <td width='50'>Mac Roman</td>\n  <td width='50'>cp437</td>\n  <td width='50'>cp1252</td>\n</tr>\n"),
  testEncodingTable(128);
test(oneTwentyEight) -> testOneTwentyEight(128);
test(normalization) ->
  file:delete("tests/NormTest.html"),
  file:write_file("tests/NormTest.html",
    "<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'><HTML>\n<HEAD>\n  <TITLE>Normalization Test</TITLE>\n
    <meta http-equiv='content-type' content='text/html;charset=utf-16'>\n
    </HEAD><BODY style='font: 300 14px Times;'>\n
    <!-- Produced automatically by mb's test suite... -->\n
    <table border='1'>\n<tr>\n
    <td width='50'>Char NFC</td>\n
    <td width='100'>Char NFD</td></tr>\n"),
  testNormalization(new("éàüîç"));

test(strLength) ->
  file:delete("tests/strLength.html"),
  file:write_file("tests/strLength.html",
    "<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'><HTML>\n<HEAD>\n  <TITLE>strLength Test</TITLE>\n
    <meta http-equiv='content-type' content='text/html;charset=utf-8'>\n
    </HEAD><BODY style='font: 300 14px Times;'>\n
    <!-- Produced automatically by mb's test suite... -->\n\n"),
  teststrLength("Deceased"),
  teststrLength("Décision"),
  A = <<"Décision">>,
  B = <<9, 1, 2>>,
  C = <<"à prendre">>,
  D = << A/binary, B/binary, C/binary >>,
  teststrLength(D),
  teststrLength("¦廣東話	好好"),
  E = new(A),
  io:format("E = ~w~n", [E]),
  F = convertEncoding(E, latin1),
  io:format("F = ~w~n", [F]),
  teststrLength(F);
  
test(hanzi) ->
  file:delete("tests/KanjiTest.html"),
  file:write_file("tests/KanjiTest.html",
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"><HTML>\n<HEAD>\n  <TITLE>Different Encodings for Sinograms</TITLE>\n  <meta http-equiv=\"content-type\" content=\"text/html;charset=utf-8\">\n</HEAD><BODY style='font: 300 14px Times;'>\n<!-- Produced automatically by the mb module's test suite -->\n<table border='1'>\n<tr>\n  <td width='50'>Char</td>\n  <td width='100'>UTF-8</td>\n  <td width='100'>UTF-16</td>\n  <td width='100'>SHIFT-JIS</td>\n  <td width='100'>EUC KR</td>\n  <td width='100'>Big Five</td>\n</tr>\n"),
  file:write_file("tests/KanjiTestUTF8.html",
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"><HTML>\n<HEAD>\n  <TITLE>Different Encodings for Sinograms</TITLE>\n  <meta http-equiv=\"content-type\" content=\"text/html;charset=utf-8\">\n</HEAD><BODY style='font: 300 14px Times;'>\n<!-- Produced automatically by the mb module's test suite -->\n<table border='1'>\n<tr>\n  <td width='50'>Char</td>\n  <td width='100'>UTF-8</td>\n</tr>\n"),
    {_,I16} = convertEncoding({utf8, <<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"><HTML>\n<HEAD>\n  <TITLE>Different Encodings for Sinograms</TITLE>\n  <meta http-equiv=\"content-type\" content=\"text/html;charset=utf-16\">\n</HEAD><BODY style='font: 300 14px Times;'>\n<!-- Produced automatically by the mb module's test suite -->\n<table border='1'>\n<tr>\n  <td width='50'>Char</td>\n  <td width='100'>UTF-16</td>\n</tr>\n">>}, utf16),
  file:write_file("tests/KanjiTestUTF16.html", I16),
  file:write_file("tests/KanjiTestSJ.html",
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"><HTML>\n<HEAD>\n  <TITLE>Different Encodings for Sinograms</TITLE>\n  <meta http-equiv=\"content-type\" content=\"text/html;charset=shift-jis\">\n</HEAD><BODY style='font: 300 14px Times;'>\n<!-- Produced automatically by the mb module's test suite -->\n<table border='1'>\n<tr>\n  <td width='50'>Char</td>\n  <td width='100'>Shift JIS</td>\n</tr>\n"),
  testHanzi(new("寒一龍覽法學晩堂古內守央字次卓巴")).
  
testEncodingTable(256) ->
    file:write_file("tests/Test_EncodingTable.html", "\n</table></body>", [append]);
testEncodingTable(Index) ->
  [H, O, B]=convert10(Index),
  {_,H2}=right({utf8,list_to_binary([<<"0000">>, list_to_binary(H)])},4),
  TR=list_to_binary([<<"<tr>\n  <td>&nbsp;">>, H2, <<"&nbsp;</td>">>]),
  file:write_file("tests/Test_EncodingTable.html", TR, [append]),
  if
    Index < 160 ->
      file:write_file("tests/Test_EncodingTable.html", "\n  <td>&nbsp;&nbsp;</td>", [append]);
    Index > 159 ->
      U=utf16Char_to_utf8([Index]),
      file:write_file("tests/Test_EncodingTable.html", 
        list_to_binary([<<"\n  <td>&nbsp;">>, U, <<"&nbsp;</td>">>]), [append])
  end,
  file:write_file("tests/Test_EncodingTable.html",
    list_to_binary([<<"\n  <td>&nbsp;">>, element(Index-127, latin1()), <<"&nbsp;</td>">>]), [append]),
  file:write_file("tests/Test_EncodingTable.html",
    list_to_binary([<<"\n  <td>&nbsp;">>, element(Index-127, latin2()), <<"&nbsp;</td>">>]), [append]),
  file:write_file("tests/Test_EncodingTable.html",
    list_to_binary([<<"\n  <td>&nbsp;">>, element(Index-127, latin3()), <<"&nbsp;</td>">>]), [append]),
  file:write_file("tests/Test_EncodingTable.html",
    list_to_binary([<<"\n  <td>&nbsp;">>, element(Index-127, latin4()), <<"&nbsp;</td>">>]), [append]),
  file:write_file("tests/Test_EncodingTable.html",
    list_to_binary([<<"\n  <td>&nbsp;">>, element(Index-127, latin5()), <<"&nbsp;</td>">>]), [append]),
  file:write_file("tests/Test_EncodingTable.html",
    list_to_binary([<<"\n  <td>&nbsp;">>, element(Index-127, latin6()), <<"&nbsp;</td>">>]), [append]),
  file:write_file("tests/Test_EncodingTable.html",
    list_to_binary([<<"\n  <td>&nbsp;">>, element(Index-127, latin9()), <<"&nbsp;</td>">>]), [append]),
  file:write_file("tests/Test_EncodingTable.html",
    list_to_binary([<<"\n  <td>&nbsp;">>, element(Index-127, latin10()), <<"&nbsp;</td>">>]), [append]),
  file:write_file("tests/Test_EncodingTable.html",
    list_to_binary([<<"\n  <td>&nbsp;">>, element(Index-127, macRoman()), <<"&nbsp;</td>">>]), [append]),
  file:write_file("tests/Test_EncodingTable.html",
    list_to_binary([<<"\n  <td>&nbsp;">>, element(Index-127, cp437()), <<"&nbsp;</td>">>]), [append]),
  file:write_file("tests/Test_EncodingTable.html",
    list_to_binary([<<"\n  <td>&nbsp;">>, element(Index-127, cp1252()), <<"&nbsp;</td>">>]), [append]),
  file:write_file("tests/Test_EncodingTable.html", "\n</tr>\n", [append]),
  testEncodingTable(Index+1).

testOneTwentyEight(256) -> done;
testOneTwentyEight(Index) ->
  [H, O, B]=convert10(Index),
  {_,H2}=right({utf8, list_to_binary([<<"0000">>, list_to_binary([H])])},4),
  file:write_file("tests/oneTwentyEight.txt", list_to_binary([H2, <<"  ",Index,"\n">>]),[append]),
  testOneTwentyEight(Index+1).

testHanzi({utf8,<<>>}) ->
    io:format("end of testHanzi~n"),
    io:format("Closing KanjiTest.html~n"),
    file:write_file("tests/KanjiTest.html", "\n</table></body>\n", [append]),
    io:format("Closing KanjiTestUTF8.html~n"),
    file:write_file("tests/KanjiTestUTF8.html", "\n</table></body>\n", [append]),
    {_,U16}=convertEncoding({utf8,<<"\n</table></body>\n">>}, utf16),
    file:write_file("tests/KanjiTestUTF16.html", U16, [append]),
    io:format("Closing KanjiTestUTF16.html~n");

testHanzi(HanziList) ->
  {{_, Hanzi}, List} = getNextChar(HanziList),
  io:format("Hanzi to convert: ~p~n", [Hanzi]),
  <<HanziDec:24>> = Hanzi,
  H1=lists:flatten("000"++[io_lib:fwrite("~.16B",[HanziDec])]),
  {_,H2}=right({utf8, list_to_binary(H1)},6),
  file:write_file("tests/KanjiTest.html",
    list_to_binary([<<"<tr>\n  <td><center>">>, Hanzi, <<"</center></td>">>]), [append]),
  file:write_file("tests/KanjiTest.html",
    list_to_binary([<<"\n  <td>0x">>, H2, <<"</td>">>]), [append]),
  file:write_file("tests/KanjiTestUTF8.html",
    list_to_binary([<<"<tr>\n  <td><center>">>, Hanzi, <<"</center></td>">>]), [append]),
  file:write_file("tests/KanjiTestUTF8.html",
    list_to_binary([<<"\n  <td>0x">>, H2, <<"</td></tr>\n">>]), [append]),
  
  {_,U16}=convertEncoding({utf8, Hanzi}, utf16),
  <<D:16>> = U16,
  H3=lists:flatten(io_lib:fwrite("~.16B",[D])),
  io:format("HanziDec: ~p~n", [D]),
  {_,I16_1}=convertEncoding({utf8, list_to_binary([<<"\n  <td>0x">>, H3, <<"</td>">>])}, utf16),
  file:write_file("tests/KanjiTest.html", list_to_binary([<<"\n  <td>0x">>, H3, <<"</td>">>]), [append]),
  {_,I16_2}=convertEncoding({utf8, <<"<tr>\n  <td><center>">>}, utf16),
  {_,I16_3}=convertEncoding({utf8, <<"</center></td>">>}, utf16),
  file:write_file("tests/KanjiTestUTF16.html",  list_to_binary([I16_2, U16, I16_3]), [append]),
  {_,I16_4}=convertEncoding({utf8,<<"</tr>\n">>}, utf16),
  file:write_file("tests/KanjiTestUTF16.html",  list_to_binary([I16_1, I16_4]), [append]),

  {_,SJ}=convertEncoding({utf8, Hanzi}, shiftjis),
  io:format("Hanzi in Shift JIS: ~p~n", [SJ]),
  <<D2:16>> = SJ,
  io:format("HanziDec: ~p~n", [D2]),
  H5=lists:flatten(io_lib:fwrite("~.16B",[D2])),
  Concat=list_to_binary([<<"\n  <td>0x">>, H5, <<"</td>">>]),
  file:write_file("tests/KanjiTest.html",  Concat, [append]),
  {_,SJ_1}=convertEncoding({utf8, Concat}, shiftjis),
  file:write_file("tests/KanjiTestSJ.html", list_to_binary([<<"<tr>\n  <td><center>">>, SJ, <<"</center></td>">>]), [append]),
  file:write_file("tests/KanjiTestSJ.html", list_to_binary([SJ_1, <<"</tr>\n">>]), [append]),

  {_,EU}=convertEncoding({utf8, Hanzi}, euckr),
  io:format("Hanzi in EUC-KR: ~p~n", [EU]),
  <<D3:16>>= EU,
  H7=lists:flatten(io_lib:fwrite("~.16B",[D3])),
  file:write_file("tests/KanjiTest.html", list_to_binary([<<"\n  <td><center>0x">>, H7, <<"</center></td>">>]), [append]),

  {_,B5}=convertEncoding({utf8, Hanzi}, bigFive),
  io:format("Hanzi in Big 5: ~p~n", [B5]),
  <<D4:16>> = B5,
  H9=lists:flatten(io_lib:fwrite("~.16B",[D4])),
  file:write_file("tests/KanjiTest.html", list_to_binary([<<"\n  <td><center>0x">>, H9, <<"</center></td>">>]), [append]),

  file:write_file("tests/KanjiTest.html", "\n</tr>\n", [append]),
  testHanzi(List).

testNormalization({utf8, <<>>}) ->
  io:format("end of testNormalization~n"),
  io:format("Closing NormTest.html~n"),
  file:write_file("tests/NormTest.html", "\n</table></body>\n", [append]);
testNormalization({utf8, List}) ->
  {A, Ist}=getNextChar({utf8, List}),
  {utf16, AA}=convertEncoding(A, utf16),
  {utf16,B}=getNFD({utf16, AA}),
  file:write_file("tests/NormTest.html", list_to_binary([<<"<tr>\n  <td><center>">> , AA, <<"</center></td>">>]), [append]),
  file:write_file("tests/NormTest.html", list_to_binary([<<"\n  <td><center>">> , B, <<"</center></td>\n</tr>">>]), [append]),
  testNormalization(Ist).

teststrLength(X) when list(X) ->
  TestString=new(list_to_binary(X), utf8),
  L=strLength(TestString),
  file:write_file("tests/strLength.html", "<p>strLenght(", [append]),
  file:write_file("tests/strLength.html", X, [append]),
  file:write_file("tests/strLength.html", ")=", [append]),
  file:write_file("tests/strLength.html", list_to_binary(integer_to_list(L)), [append]),
  file:write_file("tests/strLength.html", "</p>\n", [append]),
  io:format("strLength(~s) = ~.10B.~n", [X, L]);

teststrLength(X) when binary(X) ->
  TestString=new(X, utf8),
  L=strLength(TestString),
  file:write_file("tests/strLength.html", "<p>strLenght(", [append]),
  file:write_file("tests/strLength.html", X, [append]),
  file:write_file("tests/strLength.html", ")=", [append]),
  file:write_file("tests/strLength.html", list_to_binary(integer_to_list(L)), [append]),
  file:write_file("tests/strLength.html", "</p>\n", [append]),
  io:format("strLength(~s) = ~.10B.~n", [X, strLength(TestString)]);

teststrLength({Encoding, TestString}) when binary(TestString) ->
  L=strLength({Encoding, TestString}),
  file:write_file("tests/strLength.html", "<p>strLenght(", [append]),
  {utf8, TestStringutf8}=convertEncoding({Encoding, TestString}, utf8),
  file:write_file("tests/strLength.html", TestStringutf8, [append]),
  file:write_file("tests/strLength.html", ")[", [append]),
  file:write_file("tests/strLength.html", atom_to_list(Encoding), [append]),
  file:write_file("tests/strLength.html", "]=", [append]),
  file:write_file("tests/strLength.html", list_to_binary(integer_to_list(L)), [append]),
  file:write_file("tests/strLength.html", "</p>\n", [append]),
  io:format("strLength(~s) = ~.10B.~n", [TestStringutf8, L]).















%%%%%%%%% Normalisation

loadNorm() ->
  file:delete("normalizationTable"),
  {Result, FileRef}=file:open(normalizationTable, read),
  case Result of
%% No file -- we have to create the database
    error ->
      io:format("Creating the caseFoldingTable database~n",[]),
      dets:open_file(normalizationTable,[]),
%      dets:new(normalizationTable,[named_table]),
      Status=file:read_file("./txt/NormalizationTest.txt"),
      case Status of
        {ok, Binary} ->
          io:format("Binary/NormalizationTest.txt is good to go~n",[]),
          parseNormalization(split({utf8, Binary},{utf8, <<10>>})),
          dets:close(normalizationTable),
          dets:open_file(normalizationTable,[]);
        {What, Now} ->
          io:format("~w ~w~n",[What,Now]);
        _ ->
          io:format("Sigh...~n",[])
      end;
    ok ->
      file:close(FileRef),
      io:format("Reading the normalization database~n",[]),
      dets:open_file(normalizationTable,[])
  end.

parseNormalization(Lines) ->
  io:format("~p lines to parse~n", [length(Lines)]),
  parseNormalization(Lines, 1).
parseNormalization([], Count) -> done;
parseNormalization(_, Count) when Count == 1500 -> done;
parseNormalization([L|Ines], Count) ->
%  io:format("L = ~p~n", [L]),
  {utf8, LL} = L,
%  io:format("LL = ~p~n", [LL]),
  <<L2:8, LLL/binary>> = LL,
%  io:format("L2 = ~p~n", [L2]),
  case L2 of
    35 -> parseNormalization(Ines, Count+1);
    64 -> parseNormalization(Ines, Count+1);
    _   ->
      [{utf8, C1}, {utf8, C2}, {utf8, C3}, {utf8, C4}, {utf8, C5}|_] = split(L, {utf8, <<";">>}),
      dets:insert(normalizationTable,{Count, hexString2Int(C1), hexString2Int(C2), hexString2Int(C3), hexString2Int(C4), hexString2Int(C5)}),
%      io:format("dets:insert(normalizationTable,{~p, ~p, ~p, ~p, ~p, ~p}~n", [Count, hexString2Int(C1), hexString2Int(C2), hexString2Int(C3), hexString2Int(C4), hexString2Int(C5)]),
      parseNormalization(Ines, Count+1)
   end.


%% Will be ued by surrogate functions
hexString2Int(A) ->
  {utf8, B}=filter({utf8, A}, {utf8, <<" ">>}),
  hexString2Int(binary_to_list(B), []).
hexString2Int([], Acc) -> Acc;
hexString2Int([B1,B2|B3], Acc) ->
  {ok, [Num], _ } = io_lib:fread("~16u", [B1,B2]),
  hexString2Int(B3, list_to_binary([Acc, Num])).



getNFC({utf8, CP}) ->
  % Assuming a single char
  {CP1,CPX} = getNextChar({utf8, CP}),
  CP2=convertEncoding(CP1, utf16),
  convertEncoding(getNFC(CP2), utf8);
getNFC({utf16, CP}) ->
  {{utf16, CP1},CPX} = getNextChar({utf16, CP}),
  [[Count, NFC, NFD, NFKC, NFKD]] = dets:match(normalizationTable,{'$1', CP1, '$2', '$3', '$4', '$5'}),
  % returns NFC
  {utf16, NFC}.
  
getNFD({utf8, CP}) ->
  % Assuming a single char
  {CP1,CPX} = getNextChar({utf8, CP}),
  CP2=convertEncoding(CP1, utf16),
  convertEncoding(getNFD(CP2), utf8);
getNFD({utf16, CP}) ->
  {{utf16, CP1},CPX} = getNextChar({utf16, CP}),
  [[Count, NFC, NFD, NFKC, NFKD]] = dets:match(normalizationTable,{'$1', CP1, '$2', '$3', '$4', '$5'}),
  % returns NFD
  {utf16, NFD}.
  
getNFKC({utf8, CP}) ->
  % Assuming a single char
  {CP1,CPX} = getNextChar({utf8, CP}),
  CP2=convertEncoding(CP1, utf16),
  convertEncoding(getNFKC(CP2), utf8);
getNFKC({utf16, CP}) ->
  {{utf16, CP1},CPX} = getNextChar({utf16, CP}),
  [[Count, NFC, NFD, NFKC, NFKD]] = dets:match(normalizationTable,{'$1', CP1, '$2', '$3', '$4', '$5'}),
  % returns NFKC
  {utf16, NFKC}.
  
getNFKD({utf8, CP}) ->
  % Assuming a single char
  {CP1,CPX} = getNextChar({utf8, CP}),
  CP2=convertEncoding(CP1, utf16),
  convertEncoding(getNFKD(CP2), utf8);
getNFKD({utf16, CP}) ->
  {{utf16, CP1},CPX} = getNextChar({utf16, CP}),
  [[Count, NFC, NFD, NFKC, NFKD]] = dets:match(normalizationTable,{'$1', CP1, '$2', '$3', '$4', '$5'}),
  % returns NFKD
  {utf16, NFKD}.
  

















