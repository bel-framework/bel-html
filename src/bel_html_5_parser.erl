%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc HTML5 parser.
%%%
%%% Copyright 2024 William Fank Thomé
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @end
%%%---------------------------------------------------------------------
-module(bel_html_5_parser).
-compile(inline_list_funcs).

% API
-export([ parse/1 ]).

-record(state, { comments }).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=====================================================================
%%% API
%%%=====================================================================

parse(Tokens) ->
    parse(Tokens, #{}).

parse(Tokens, Opts) when is_list(Tokens), is_map(Opts) ->
    do_parse(Tokens, #state{
        comments = maps:get(comments, Opts, false)
    }, []).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

do_parse([{void, _Anno, {TagName, Attrs}} | T], State, [{N, A, C} | Acc]) ->
    do_parse(T, State, [{N, A, C ++ [{TagName, normalize_attrs(Attrs), []}]} | Acc]);
do_parse([{void, _Anno, {TagName, Attrs}} | T], State, Acc) ->
    do_parse(T, State, [{TagName, normalize_attrs(Attrs), []} | Acc]);
do_parse([{open, _Anno, {TagName, Attrs}} | T], State, Acc) ->
    do_parse(T, State, [{TagName, normalize_attrs(Attrs), []} | Acc]);
do_parse([{close, _Anno, TagName} | T], State, [{TagName, _, _} = Token, {N, A, C} | Acc]) ->
    do_parse(T, State, [{N, A, C ++ [Token]} | Acc]);
do_parse([{close, _Anno, TagName} | T], State, [{TagName, Attrs, ChildrenNodes} | Acc]) ->
    do_parse(T, State, [terminate, {TagName, Attrs, ChildrenNodes} | Acc]);
do_parse([{text, _Anno, Text} | T], State, [{N, A, C} | Acc]) ->
    do_parse(T, State, [{N, A, C ++ [Text]} | Acc]);
do_parse([{text, _Anno, Text} | T], State, [terminate | Acc]) ->
    do_parse(T, State, [terminate, Text | Acc]);
do_parse([{text, _Anno, Text}| T], State, Acc) ->
    do_parse(T, State, [Text | Acc]);
do_parse([{comment, _Anno, _Comment} | T], #state{comments = false} = State, Acc) ->
    do_parse(T, State, Acc);
do_parse([{comment, _Anno, _Comment} = Token | T], State, [{N, A, C} | Acc]) ->
    do_parse(T, State, [{N, A, C ++ [Token]} | Acc]);
do_parse([{comment, _Anno, _Comment} = Token | T], State, [terminate | Acc]) ->
    do_parse(T, State, [terminate, Token | Acc]);
do_parse([{comment, _Anno, _Comment} = Token | T], State, Acc) ->
    do_parse(T, State, [Token | Acc]);
do_parse([], _State, [terminate | Acc]) ->
    lists:reverse(Acc);
do_parse([], _State, Acc) ->
    lists:reverse(Acc).

normalize_attrs(Attrs) ->
    lists:foldl(fun({K, {V, _Anno}}, Acc) ->
        Acc#{K => V}
    end, #{}, Attrs).

%%%=====================================================================
%%% Test
%%%=====================================================================

-ifdef(TEST).

parse_test() ->
    [
        parse_test_1(),
        parse_test_2()
    ].

parse_test_1() ->
    Tokens = [
        {open,{{2,5},undefined,undefined},
            {<<"!DOCTYPE">>,[{<<"html">>,{true,{2,5}}}]}},
        {open,{{3,5},undefined,undefined},
            {<<"html">>,[{<<"lang">>,{<<"\"en\"">>,{3,5}}}]}},
        {comment,{{4,5},undefined,undefined},<<" Comment ">>},
        {open,{{5,5},undefined,undefined},{<<"head">>,[]}},
        {open,{{6,9},undefined,undefined},{<<"title">>,[]}},
        {text,{{6,16},undefined,undefined},
            <<"<b>content inside <title> must be treated as plaintext</b>">>},
        {close,{{6,74},undefined,undefined},<<"title">>},
        {open,{{7,9},undefined,undefined},
            {<<"script">>,
            [{<<"src">>,{<<"\"assets/foo.js\"">>,{7,9}}}]}},
        {close,{{7,37},undefined,undefined},<<"script">>},
        {open,{{8,9},undefined,undefined},{<<"style">>,[]}},
        {text,{{8,16},undefined,undefined},
            <<":root {\n                --foo: 0;\n            }">>},
        {close,{{12,9},undefined,undefined},<<"style">>},
        {close,{{13,5},undefined,undefined},<<"head">>},
        {open,{{14,5},undefined,undefined},{<<"body">>,[]}},
        {open,{{15,9},undefined,undefined},{<<"h1">>,[]}},
        {text,{{15,13},undefined,undefined},<<"Form">>},
        {close,{{15,17},undefined,undefined},<<"h1">>},
        {void,{{16,9},undefined,undefined},{<<"br">>,[]}},
        {void,{{17,9},undefined,undefined},{<<"br">>,[]}},
        {open,{{18,9},undefined,undefined},{<<"form">>,[]}},
        {open,{{19,13},undefined,undefined},{<<"div">>,[]}},
        {text,{{19,18},undefined,undefined},<<"Foo Form">>},
        {close,{{19,26},undefined,undefined},<<"div">>},
        {void,{{20,13},undefined,undefined},
            {<<"input">>,
            [{<<"id">>,{<<"\"foo\"">>,{20,13}}},
            {<<"name">>,{<<"'foo'">>,{20,22}}},
            {<<"value">>,{<<"'\"bar\"'">>,{20,33}}}]}},
        {void,{{21,13},undefined,undefined},
            {<<"input">>,
            [{<<"type">>,{<<"\"number\"">>,{21,13}}},
            {<<"value">>,{<<"10">>,{21,27}}}]}},
        {close,{{22,9},undefined,undefined},<<"form">>},
        {close,{{23,5},undefined,undefined},<<"body">>},
        {close,{{24,5},undefined,undefined},<<"html">>}
    ],
    ?assertMatch([
        {<<"!DOCTYPE">>,#{<<"html">> := true},[
            {<<"html">>,#{<<"lang">> := <<"\"en\"">>},[
                {<<"head">>,#{},[
                    {<<"title">>,#{},[
                        <<"<b>content inside <title> must be treated as plaintext</b>">>
                    ]},
                    {<<"script">>,#{<<"src">> := <<"\"assets/foo.js\"">>},[]},
                    {<<"style">>,#{},[
                        <<":root {\n                --foo: 0;\n            }">>
                    ]}
                ]},
                {<<"body">>,#{},[
                    {<<"h1">>,#{},[<<"Form">>]},
                    {<<"br">>,#{},[]},
                    {<<"br">>,#{},[]},
                    {<<"form">>,#{},[
                        {<<"div">>,#{},[<<"Foo Form">>]},
                        {<<"input">>,#{
                            <<"id">> := <<"\"foo\"">>,
                            <<"name">> := <<"'foo'">>,
                            <<"value">> := <<"'\"bar\"'">>
                        },[]},
                        {<<"input">>,#{
                            <<"type">> := <<"\"number\"">>,
                            <<"value">> := <<"10">>
                        },[]}
                    ]}
                ]}
            ]}
        ]}
    ], parse(Tokens)).

parse_test_2() ->
    Html = <<"<div><span id='foo'>bar</span></div>">>,
    Tokens = bel_html_5_scan:string(Html),
    ?assertMatch([
        {<<"div">>,#{},[
            {<<"span">>,#{<<"id">> := <<"'foo'">>},[<<"bar">>]}
        ]}
    ], bel_html_5:parse(Tokens)).

-endif.
