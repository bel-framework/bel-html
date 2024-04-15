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

do_parse([{void, {TagName, Attrs}} | T], State, [{N, A, C} | Acc]) ->
    do_parse(T, State, [{N, A, C ++ [{TagName, Attrs, []}]} | Acc]);
do_parse([{void, {TagName, Attrs}} | T], State, Acc) ->
    do_parse(T, State, [{TagName, Attrs, []} | Acc]);
do_parse([{open, {TagName, Attrs}} | T], State, Acc) ->
    do_parse(T, State, [{TagName, Attrs, []} | Acc]);
do_parse([{close, TagName} | T], State, [{TagName, _, _} = Token, {N, A, C} | Acc]) ->
    do_parse(T, State, [{N, A, C ++ [Token]} | Acc]);
do_parse([{close, TagName} | T], State, [{TagName, Attrs, ChildrenNodes} | Acc]) ->
    do_parse(T, State, [terminate, {TagName, Attrs, ChildrenNodes} | Acc]);
do_parse([{text, Text} | T], State, [{N, A, C} | Acc]) ->
    do_parse(T, State, [{N, A, C ++ [Text]} | Acc]);
do_parse([{text, Text} | T], State, [terminate | Acc]) ->
    do_parse(T, State, [terminate, Text | Acc]);
do_parse([{text, Text}| T], State, Acc) ->
    do_parse(T, State, [Text | Acc]);
do_parse([{comment, _Comment} | T], #state{comments = false} = State, Acc) ->
    do_parse(T, State, Acc);
do_parse([{comment, _Comment} = Token | T], State, [{N, A, C} | Acc]) ->
    do_parse(T, State, [{N, A, C ++ [Token]} | Acc]);
do_parse([{comment, _Comment} = Token | T], State, [terminate | Acc]) ->
    do_parse(T, State, [terminate, Token | Acc]);
do_parse([{comment, _Comment} = Token | T], State, Acc) ->
    do_parse(T, State, [Token | Acc]);
do_parse([], _State, [terminate | Acc]) ->
    lists:reverse(Acc);
do_parse([], _State, Acc) ->
    lists:reverse(Acc).

%%%=====================================================================
%%% Test
%%%=====================================================================

-ifdef(TEST).

parse_test() ->
    Expect = [
        {<<"!DOCTYPE">>,[<<"html">>],[
            {<<"html">>,[{<<"lang">>,<<"\"en\"">>}],[
                {<<"head">>,[],[
                    {<<"title">>,[],[<<"<b>content inside <title> must be treated as plaintext</b>">>]},
                    {<<"script">>,[{<<"src">>,<<"\"assets/foo.js\"">>}],[]},
                    {<<"style">>,[],[<<"\n            :root {\n                --foo: 0;\n            }\n        ">>]}
                ]},
                {<<"body">>,[],[
                    {<<"h1">>,[],[<<"Form">>]},
                    {<<"br">>,[],[]},
                    {<<"br">>,[],[]},
                    {<<"form">>,[],[
                        {<<"div">>,[],[<<"Foo Form">>]},
                        {<<"input">>,[{<<"id">>,<<"\"foo\"">>},{<<"name">>,<<"'foo'">>},{<<"value">>,<<"'\"bar\"'">>},<<>>],[]},
                        {<<"input">>,[{<<"type">>,<<"\"number\"">>},{<<"value">>,<<"10">>}],[]}
                    ]}
                ]}
            ]}
        ]}
    ],
    Tokens = [
        {open,{<<"!DOCTYPE">>,[<<"html">>]}},
        {open,{<<"html">>,[{<<"lang">>,<<"\"en\"">>}]}},
        {comment,<<" Comment ">>},
        {open,{<<"head">>,[]}},
        {open,{<<"title">>,[]}},
        {text,<<"<b>content inside <title> must be treated as plaintext</b>">>},
        {close,<<"title">>},
        {open,{<<"script">>,[{<<"src">>,<<"\"assets/foo.js\"">>}]}},
        {close,<<"script">>},
        {open,{<<"style">>,[]}},
        {text,<<"\n            :root {\n                --foo: 0;\n            }\n        ">>},
        {close,<<"style">>},
        {close,<<"head">>},
        {open,{<<"body">>,[]}},
        {open,{<<"h1">>,[]}},
        {text,<<"Form">>},
        {close,<<"h1">>},
        {void,{<<"br">>,[]}},
        {void,{<<"br">>,[]}},
        {open,{<<"form">>,[]}},
        {open,{<<"div">>,[]}},
        {text,<<"Foo Form">>},
        {close,<<"div">>},
        {void,{<<"input">>,
            [{<<"id">>,<<"\"foo\"">>},
                {<<"name">>,<<"'foo'">>},
                {<<"value">>,<<"'\"bar\"'">>},
                <<>>]}},
        {void,{<<"input">>,
            [{<<"type">>,<<"\"number\"">>},
                {<<"value">>,<<"10">>}]}},
        {close,<<"form">>},
        {close,<<"body">>},
        {close,<<"html">>}
    ],
    Expr = parse(Tokens),
    ?assertEqual(Expect, Expr).

-endif.
