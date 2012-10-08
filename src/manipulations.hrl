
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                        %%
%% String Manipulations                   %%
%%                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getNextChar: tries its best to send    %%
%% back the first char + the remainder of %%
%% the string.                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec getNextChar(MBString::mbString()) -> Result::{OneChar::mbString(), RestOfString::mbString()}
%% @doc returns a tuple() with the following elements as mbString(): the next character, the rest of the mbString()
%%
%% <p>Example</p>
%% <pre>
%% 1> HANGUL=mb:new("\355\225\234\352\270\200").
%% {utf8,<<237,149,156,234,184,128>>}
%% 2> {HAN, GUL}=mb:getNextChar(HANGUL).
%% {{utf8,<<237,149,156>>},{utf8,<<234,184,128>>}}
%% </pre>
%% @end

getNextChar({Encoding, <<"">>}) ->
  {{Encoding, <<"">>}, {Encoding, <<"">>}};
getNextChar({utf8, String}) ->
%% UTF-8: decide whether to send 1, 2 or three bytes
  <<S:8, Tring/binary>>=String,
  if
    S<128 ->
      {S1, Tring1} = split_binary(String, 1);
    S<224 ->
      {S1, Tring1} = split_binary(String, 2);
    S<240 ->
      {S1, Tring1} = split_binary(String, 3);
    S>239 ->
      {S1, Tring1} = split_binary(String, 4)
  end,
  {{utf8, S1} ,{utf8, Tring1}};

getNextChar({utf16, String}) ->
%% UTF-16: send back two bytes
%% TODO: surrogates... ie two separate UTF-16 chars that combine visually as one
%% é [&eacute;] can be encoded as 00E9 --> é or 0065 0301 --> e◌́ [e + acute accent over the previous char]
  {S1, Tring1} = split_binary(String, 2),
  {{utf16, S1}, {utf16, Tring1}};
getNextChar({bigFive, String}) ->
%% Big Five: send back two bytes
  {S1, Tring1} = split_binary(String, 2),
  {{bigFive, S1}, {bigFive, Tring1}};
getNextChar({cccii, String}) ->
%% CCCII: send back three bytes
  {S1, Tring1} = split_binary(String, 3),
  {{cccii, S1}, {cccii, Tring1}};
getNextChar({euckr, String}) ->
%% EUC KR: send back two bytes
  {S1, Tring1} = split_binary(String, 2),
  {{euckr, S1}, {euckr, Tring1}};
getNextChar({shiftjis, String}) ->
%% SHIFT-JIS: send back two bytes
  {S1, Tring1} = split_binary(String, 2),
  {{shiftjis, S1}, {shiftjis, Tring1}};

getNextChar({Encoding, String}) ->
%% Assuming here one-byte encodings
  {S1, Tring1} = split_binary(String, 1),
  {{Encoding, S1}, {Encoding, Tring1}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% strLength                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec strLength(MBString::mbString()) -> Result::LengthOfString
%%
%% @doc returns the length of mbString() - in characters, as opposed to bytes - as an integer.

strLength({Encoding, String}) -> strLength({Encoding, String}, 0).
strLength({Encoding, <<"">>}, L) -> L;
strLength({Encoding, String}, L) ->
  {_, {Encoding, Tring1}} = getNextChar({Encoding, String}),
  strLength({Encoding, Tring1}, L+1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% split, shortSplit, longSplit and join  %%
%% split is the main API entry, where it  %%
%% will be decided which function to call %%
%% depending on the size of the delimitor %%
%% join does the reverse, assemble a list %%
%% of MBStrings [possibly with different  %%
%% encodings] into one MBString.          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
split({Encoding, String},{Encoding, Delim}) ->
%% Accept Delim and String as mb string
  split(Encoding, String, Delim, []).
split(Encoding, <<"">>, Delim, Acc) -> Acc;
split(Encoding, String, Delim, Acc) ->
  L=size(Delim),
%% if Delim is a single byte, go for the short version
%% else go for the long one
  case L of
    1 ->
      <<Del:8>> = Delim,
      shortSplit(Encoding, String, Del, Acc, []);
    _ -> longSplit(Encoding, String, Delim, Acc, [], Delim, <<"">>)
  end.

shortSplit(Encoding,<<"">>,Delim,Acc,Temp) -> lists:flatten([Acc,{Encoding,Temp}]);
shortSplit(Encoding, <<S:8, Tring/binary>>,Delim,Acc,Temp) ->
  case S of
    Delim ->
      shortSplit(Encoding, Tring, Delim, [Acc,{Encoding,Temp}],[]);
    _     ->
      shortSplit(Encoding, Tring, Delim, Acc, list_to_binary([Temp,S]))
  end.

longSplit(Encoding, String, <<"">>, Acc, Temp, Delim2, DelimAcc) ->
  longSplit(Encoding, String, Delim2, [Acc,{Encoding,Temp}], <<"">>, Delim2, <<"">>);
longSplit(Encoding, <<"">>, Delim, Acc, Temp, Delim2, DelimAcc) ->
  lists:flatten([Acc,{Encoding,Temp}]);
longSplit(Encoding, <<S:8, Tring/binary>>, <<D:8, Elim/binary>>, Acc, Temp, Delim2, DelimAcc) ->
  case S of
    D ->
      longSplit(Encoding, Tring, Elim, Acc, Temp, Delim2, list_to_binary([DelimAcc, S]));
    _ ->
      case DelimAcc of
        <<"">> ->
          longSplit(Encoding, Tring, Delim2, Acc, list_to_binary([Temp, S]), Delim2, <<"">>);
        _ ->
          longSplit(Encoding,Tring,Delim2,Acc,list_to_binary([Temp, DelimAcc, S]), Delim2, <<"">>)
      end
  end.



join(MBStringList, {Encoding, String2}) ->
  [H|T]=MBStringList,
  {MBEnc, MBChar} = H,
  case MBEnc of
    Encoding -> % Same encoding, zoowie
      join(T, String2, MBChar, Encoding);
    _        -> % Er... let's unify here...
     {Enc3, Str3} = convertEncoding({Encoding, String2}, MBEnc),
     %% Convert the delimiter to the main
     join(T, Str3, MBChar, Encoding)
  end.
  %% Acc is set up with the first string
join([], Delim, Acc, Encoding) -> {Encoding, Acc};
join([H|T], Delim, Acc, Encoding) ->
  {Enc, Str} = H,
  case Enc of
    Encoding -> % same encoding as what we started with: good
      join(T, Delim, list_to_binary([Acc,Delim,Str]), Encoding);
    _        -> % different encodings... Grrr!
      {Enc2, Str2} = convertEncoding({Enc, lists:flatten(Str)}, Encoding),
      join(T, Delim, list_to_binary([Acc,Delim,Str2]), Encoding)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% reverse, right, left, mid, in char and %%
%% byte-level [~B] versions.              %%
%% Also included, inStr(HayStack, Needle) %%
%% which returns the position of Needle   %%
%% HayStack, or -1 if Needle is not found.%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reverseB({utf8,String}) -> lists:reverse(String).

reverse({Encoding, String}) ->
  reverse(String,[], Encoding).
reverse(<<"">>, Acc, Encoding) -> {Encoding, Acc};
reverse(String, Acc, Encoding) ->
  {{Encoding, Char},{Encoding, String2}} = getNextChar({Encoding, String}),
  reverse(String2, list_to_binary([Char,Acc]), Encoding).

right({Encoding, MyString}, Len) -> reverse(left(reverse({Encoding, MyString}),Len,[])).

left({Encoding, MyString}, Len) -> left({Encoding, MyString}, Len, <<"">>).
left({Encoding, String}, 0, Acc) -> {Encoding,Acc};
left({Encoding, MyString}, Len, Acc) ->
  {{Enc,Char}, String2} = getNextChar({Encoding, MyString}),
  left(String2, Len-1, list_to_binary([Acc,Char])).

leftB({Encoding, MyString}, Len) ->
  {A, _} = split_binary(MyString, Len),
  A.

inStr({Encoding, String}, {Encoding, String2}) -> 
%% Same encoding, zoowie
  inStr(String, String2, Encoding, 0);
inStr({Encoding, String}, {Encoding2, String2}) -> 
%% different encodings, let's unify here...
  {Enc3, Str3} = convertEncoding({Encoding2, String2}, Encoding),
  inStr({Encoding, String}, {Encoding, Str3}).

%% Same principle as longSplit
inStr(X, <<"">>, Encoding, Pos) -> Pos+1;
inStr(<<"">>, _, Encoding, Pos) -> -1;
inStr(HayStack, Needle, Encoding, Pos) ->
  %% Take the size(Needle) first chars and compare
  %% with needle. See if they match :D
  L=size(Needle),
  M=size(HayStack),
  if
    M < L -> -1;
    %% Remainder is shorter than L. Quit
    L=<M ->
    {Temp1, Temp2} = split_binary(HayStack, L),
    case Temp1 of
      Needle -> Pos+1;
      %% Match, return
      _ ->
        {{_, H}, {_, AyStack}} = getNextChar({Encoding, HayStack}),
        inStr(AyStack, Needle, Encoding, Pos+1)
    end
  end.

mid({Encoding, String}, Start) when integer(Start) ->
  L=len({Encoding, String})-Start,
  mid({Encoding, String}, Start, L).

mid({Encoding, String}, Start, Length) when integer(Start), integer(Length) ->
  {_,Str2}=skip({Encoding, String}, Start-1),
  left({Encoding ,Str2}, Length).

skip({Encoding, String}, 0) -> {Encoding, String};
skip({Encoding, String}, Start) ->
  {_, MBString} = getNextChar({Encoding, String}),
  skip(MBString, Start-1).

midB({Encoding, String}, Start) when integer(Start) ->
  L=size(String)-Start+1,
  midB({Encoding, String}, Start, L).

midB({Encoding, String}, Start, Length) when integer(Start), integer(Length) ->
  Str2=skipB(String, Start-1),
  leftB({Encoding, Str2}, Length).

skipB(String, Start) ->
  X=Start*8,
  <<S:X, Tring/binary>>=String,
  Tring.


filter({Encoding, String}, {Encoding, String2}) ->
%% filters out characters in String2 from String
  filter(String, String2, Encoding);
filter({Encoding, String}, {Encoding2, String2}) ->
%% Not the same encoding here... Let's unify
  {Enc3,Str3} = convertEncoding({Encoding2, String2}, Encoding),
  filter(String, Str3, Encoding).

filter(String, <<"">>, Encoding) -> {Encoding, String};
filter(String, Chars, Encoding) ->
  {{_,Char}, {_, Chars2}} = getNextChar({Encoding, Chars}),
  String2=filterOut(String, Char, Encoding, <<"">>),
  filter(String2, Chars2, Encoding).

filterOut(<<"">>, Char, Encoding, Acc) -> Acc;
filterOut(String, Char, Encoding, Acc) ->
  {{_,FirstChar}, {_, Chars}} = getNextChar({Encoding, String}),
  case FirstChar of
    Char -> filterOut(Chars, Char, Encoding, Acc);
    _    -> filterOut(Chars, Char, Encoding, list_to_binary([Acc, FirstChar]))
  end.  

filterB({Encoding, String}, String2) ->
%% filters out bytes in String2 from String
filterB(String, String2, Encoding).

filterB(String, <<"">>, Encoding) -> String;
%% returns a plain binary because we can't know for sure we have a valid MBString
filterB(String, <<C:8, Hars/binary>>, Encoding) ->
  String2=filterBout(String, C, <<"">>),
  filterB(String2, Hars, Encoding).

filterBout(<<"">>, Char, Acc) -> Acc;
filterBout(<<S:8, Tring/binary>>, <<C:8, Har/binary>>, Acc) ->
  case S of
    C ->
      filterBout(Tring, Har, Acc);
    _ ->
      filterBout(Tring, Har, list_to_binary([Acc, S]))
  end.  

replace({Encoding, Source}, {Encoding, What}, {Encoding, To}) ->
  Pos=inStr({Encoding, Source}, {Encoding, What}),
  case Pos of
    -1 -> {Encoding, lists:flatten(Source)};
    1 ->
      S1 = mid({Encoding, Source},len({Encoding, What})),
      {Encoding, lists:flatten([To]++S1)};
    _ ->
      Ln=len({Encoding, What}),
      {_,S1}=left({Encoding, Source},Pos-1),
      {_,S2}=mid({Encoding, Source},Pos+Ln),
      {Encoding, S1++To++S2}
  end.

replaceAll({Encoding, Source}, {Encoding, What}, {Encoding, To}) ->
%  io:format("Source =~s, What=~s~n",[Source, What]),
  Pos=inStr({Encoding, Source}, {Encoding, What}),
%  io:format("Source =~s, What=~s, Pos = ~p~n",[Source, What, Pos]),
  case Pos of
    -1 -> {Encoding, lists:flatten(Source)};
    1 ->
      {_,S1} = mid({Encoding, Source},len({Encoding, What})+1),
      replaceAll({Encoding, [To]++S1}, {Encoding, What}, {Encoding, To});
    _ ->
      Ln=len({Encoding, What}),
      {_,S1}=left({Encoding, Source},Pos-1),
      {_,S2}=mid({Encoding, Source},Pos+Ln),
%  io:format("Modified Source =~s~n",[S1++[To]++S2]),
      replaceAll({Encoding, S1++[To]++S2}, {Encoding, What}, {Encoding, To})
  end.

hasSimplified(Char) ->
  lists:flatten(dets:match(variantTable,{Char,'$1'})).
hasTraditional(Char) ->
  lists:flatten(dets:match(variantTable,{'$1', Char})).

































