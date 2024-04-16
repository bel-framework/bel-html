%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc HTML5 scanner.
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
-module(bel_html_5_scan).
-compile(inline_list_funcs).
-behaviour(bel_scan).

% API
-export([ string/1, string/2 ]).

% bel_scan callbacks
-export([ init/1, handle_char/3, handle_tokens/2 ]).

% Default module callbacks
-export([ handle_attrs/2 ]).

-callback handle_attrs(binary(), location()) -> [attribute()].

-import(bel_scan, [ continue/2
                  , fold/2
                  , snapshot/1
                  , new_ln/1
                  , skip_new_lns/2
                  , incr_col/1
                  , incr_col/2
                  , update_pos/1
                  , pos_text/1
                  , get_tokens/1
                  , anno/1
                  , get_snap_loc/1
                  ]).

% Libs

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Types

-record(state, { handler }).

-type location()  :: bel_scan:location().
-type attribute() :: {Key :: binary(), {Value :: binary(), location()}}.

%%%=====================================================================
%%% API
%%%=====================================================================

% Fixes no local return warning from bel_scan/new/1.
-dialyzer({nowarn_function, [string/1, string/2]}).

string(String) ->
    string(String, #{}).

string(String, Opts) when is_binary(String), is_map(Opts) ->
    bel_scan:string(Opts, bel_scan:new(#{
        input => String,
        handler => ?MODULE
    })).

%%%=====================================================================
%%% bel_scan callbacks
%%%=====================================================================

init(Opts) ->
    {ok, #state{handler = maps:get(handler, Opts, ?MODULE)}}.

handle_char($<, <<"!--", Rest/bitstring>>, Scan) ->
    parse_comment(Rest, fold(Scan, [
        fun(S) -> push_token(text_token(S), S) end,
        fun(S) -> snapshot(S) end,
        fun(S) -> incr_col(4, S) end,
        fun(S) -> update_pos(S) end
    ]));
handle_char($<, <<$/, Rest/bitstring>>, Scan) ->
    TxtToken = text_token(Scan),
    parse_closing_tag(TxtToken, Rest, incr_col(2, snapshot(Scan)));
handle_char($<, <<Rest/bitstring>>, Scan) ->
    TxtToken = text_token(Scan),
    parse_tag(TxtToken, Rest, incr_col(snapshot(Scan)));
handle_char(_Char, <<>>, Scan) ->
    continue(<<>>, fold(Scan, [
        fun(S) -> incr_col(S) end,
        fun(S) -> push_token(text_token(S), S) end
    ]));
handle_char(_Char, <<Rest/bitstring>>, Scan) ->
    continue(Rest, incr_col(Scan)).

handle_tokens(Tokens, _Scan) ->
    lists:reverse(Tokens).

%%%=====================================================================
%%% Default callbacks
%%%=====================================================================

handle_attrs(Text0, InitialLoc) when is_binary(Text0) ->
    {Text, Loc} = skip_spaces(Text0, InitialLoc),
    do_handle_attrs(Text, Loc, []).

do_handle_attrs(<<>>, _Loc, Acc) ->
    lists:reverse(Acc);
do_handle_attrs(Text, Loc0, Acc) ->
    case get_attr_key(Text, Loc0) of
        {without_value, Pos, Len, KRest, KLoc} ->
            Key = binary_part(Text, Pos, Len),
            {Rest, Loc} = skip_spaces(KRest, KLoc),
            do_handle_attrs(Rest, Loc, push_attribute(Key, true, Loc0, Acc));
        {with_value, KPos, KLen, Rest0, KLoc} ->
            Key = binary_part(Text, KPos, KLen),
            {VPos, VLen, VRest, VLoc} = get_attr_value(Rest0, KLoc),
            Value = binary_part(Rest0, VPos, VLen),
            {Rest, Loc} = skip_spaces(VRest, VLoc),
            do_handle_attrs(Rest, Loc, push_attribute(Key, Value, Loc0, Acc));
        none ->
            lists:reverse(Acc)
    end.

skip_spaces(<<$\r, $\n, Rest/bitstring>>, {Ln, _Col}) ->
    skip_spaces(Rest, {Ln+1, 1});
skip_spaces(<<$\r, Rest/bitstring>>, {Ln, _Col}) ->
    skip_spaces(Rest, {Ln+1, 1});
skip_spaces(<<$\n, Rest/bitstring>>, {Ln, _Col}) ->
    skip_spaces(Rest, {Ln+1, 1});
skip_spaces(<<$\f, Rest/bitstring>>, {Ln, _Col}) ->
    skip_spaces(Rest, {Ln+1, 1});
skip_spaces(<<$\s, Rest/bitstring>>, {Ln, Col}) ->
    skip_spaces(Rest, {Ln, Col+1});
skip_spaces(Rest, Loc) ->
    {Rest, Loc}.

push_attribute(K, V, L, Attrs) ->
    case attribute_defined(K, Attrs) of
        true ->
            ?LOG_WARNING(#{
                message => <<"Attribute ", K/binary, " already defined. Skipping.">>,
                reason => attribute_defined
            }),
            Attrs;
        false ->
            [{K, {V, L}} | Attrs]
    end.

attribute_defined(Key, Attrs) ->
    lists:keymember(Key, 1, Attrs).

get_attr_key(Text, Loc) ->
    do_get_attr_key(Text, 0, Loc).

do_get_attr_key(<<$\r, $\n, Rest/bitstring>>, Pos, {Ln, _Col}) ->
    do_get_attr_key(Rest, Pos+2, {Ln+1, 1});
do_get_attr_key(<<$\r, Rest/bitstring>>, Pos, {Ln, _Col}) ->
    do_get_attr_key(Rest, Pos+1, {Ln+1, 1});
do_get_attr_key(<<$\n, Rest/bitstring>>, Pos, {Ln, _Col}) ->
    do_get_attr_key(Rest, Pos+1, {Ln+1, 1});
do_get_attr_key(<<$\f, Rest/bitstring>>, Pos, {Ln, _Col}) ->
    do_get_attr_key(Rest, Pos+1, {Ln+1, 1});
do_get_attr_key(<<$\s, Rest/bitstring>>, Pos, {Ln, Col}) ->
    do_get_attr_key(Rest, Pos+1, {Ln, Col+1});
do_get_attr_key(<<Rest/bitstring>>, Pos, Loc) ->
    do_get_attr_key_1(Rest, Pos, 0, Loc);
do_get_attr_key(<<>>, _Pos, _Loc) ->
    none.

do_get_attr_key_1(<<$\r, $\n, Rest/bitstring>>, Pos, Len, {Ln, _Col}) ->
    {without_value, Pos, Len, Rest, {Ln+1, 1}};
do_get_attr_key_1(<<$\r, Rest/bitstring>>, Pos, Len, {Ln, _Col}) ->
    {without_value, Pos, Len, Rest, {Ln+1, 1}};
do_get_attr_key_1(<<$\n, Rest/bitstring>>, Pos, Len, {Ln, _Col}) ->
    {without_value, Pos, Len, Rest, {Ln+1, 1}};
do_get_attr_key_1(<<$\f, Rest/bitstring>>, Pos, Len, {Ln, _Col}) ->
    {without_value, Pos, Len, Rest, {Ln+1, 1}};
do_get_attr_key_1(<<$\s, Rest/bitstring>>, Pos, Len, {Ln, Col}) ->
    {without_value, Pos, Len, Rest, {Ln, Col+1}};
do_get_attr_key_1(<<$=, Rest/bitstring>>, Pos, Len, {Ln, Col}) ->
    {with_value, Pos, Len, Rest, {Ln, Col+1}};
do_get_attr_key_1(<<_, Rest/bitstring>>, Pos, Len, {Ln, Col}) ->
    do_get_attr_key_1(Rest, Pos, Len+1, {Ln, Col+1});
do_get_attr_key_1(<<>>, Pos, Len, Loc) ->
    {without_value, Pos, Len, <<>>, Loc}.

get_attr_value(<<$", Rest/bitstring>>, {Ln, Col}) ->
    get_str_value(Rest, 1, 0, $", {Ln, Col+1});
get_attr_value(<<$', Rest/bitstring>>, {Ln, Col}) ->
    get_str_value(Rest, 1, 0, $', {Ln, Col+1});
get_attr_value(<<Rest/bitstring>>, Loc)->
    get_number_value(Rest, 0, 0, Loc).

get_str_value(<<$\\, Q, Rest/bitstring>>, Pos, Len, Q, {Ln, Col}) ->
    get_str_value(Rest, Pos, Len+2, Q, {Ln, Col+2});
get_str_value(<<Q, Rest/bitstring>>, Pos, Len, Q, {Ln, Col}) ->
    {Pos, Len, Rest, {Ln, Col+1}};
get_str_value(<<_, Rest/bitstring>>, Pos, Len, Q, {Ln, Col}) ->
    get_str_value(Rest, Pos, Len+1, Q, {Ln, Col+1}).

get_number_value(<<$\r, $\n, Rest/bitstring>>, Pos, Len, {Ln, _Col}) ->
    {Pos, Len, Rest, {Ln+1, 1}};
get_number_value(<<$\r, Rest/bitstring>>, Pos, Len, {Ln, _Col}) ->
    {Pos, Len, Rest, {Ln+1, 1}};
get_number_value(<<$\n, Rest/bitstring>>, Pos, Len, {Ln, _Col}) ->
    {Pos, Len, Rest, {Ln+1, 1}};
get_number_value(<<$\f, Rest/bitstring>>, Pos, Len, {Ln, _Col}) ->
    {Pos, Len, Rest, {Ln+1, 1}};
get_number_value(<<$\s, Rest/bitstring>>, Pos, Len, {Ln, Col}) ->
    {Pos, Len, Rest, {Ln, Col+1}};
get_number_value(<<$/, Rest/bitstring>>, Pos, Len, {Ln, Col}) ->
    {Pos, Len, Rest, {Ln, Col+1}};
get_number_value(<<$>, Rest/bitstring>>, Pos, Len, {Ln, Col}) ->
    {Pos, Len, Rest, {Ln, Col+1}};
get_number_value(<<_, Rest/bitstring>>, Pos, Len, {Ln, Col}) ->
    get_number_value(Rest, Pos, Len+1, {Ln, Col+1});
get_number_value(<<>>, Pos, Len, Loc) ->
    {Pos, Len, <<>>, Loc}.

comment_token(Scan) ->
    token(comment, pos_text(Scan), Scan).

text_token(Scan) ->
    token(text, pos_text(Scan), Scan).

close_token(TagName, Scan) ->
    token(close, TagName, Scan).

void_token(TagName, Attrs, Scan) ->
    token(void, {TagName, Attrs}, Scan).

open_token(TagName, Attrs, Scan) ->
    token(open, {TagName, Attrs}, Scan).

token(Tag, Metadata, Scan) ->
    bel_scan:token(Tag, bel_scan:anno(Scan), Metadata).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

get_state(Scan) ->
    bel_scan:get_handler_state(Scan).

get_handler(#state{handler = Handler}) ->
    Handler;
get_handler(Scan) ->
    get_handler(get_state(Scan)).

% TODO: Make trim optional.
%       Maybe have an option to trim and another to skip empty texts.
push_token({text, Anno, Text0}, Scan) ->
    case string:trim(Text0) of
        <<>> ->
            Scan;
        Text ->
            bel_scan:push_token({text, Anno, Text}, Scan)
    end;
push_token(Token, Scan) ->
    bel_scan:push_token(Token, Scan).

push_tokens(Tokens, Scan) ->
    lists:foldl(fun push_token/2, Scan, Tokens).

parse_comment(<<"-->", Rest/bitstring>>, Scan) ->
    continue(Rest, fold(Scan, [
        fun(S) -> push_token(comment_token(S), S) end,
        fun(S) -> incr_col(3, S) end,
        fun(S) -> update_pos(S) end,
        fun(S) -> snapshot(S) end
    ]));
parse_comment(<<Rest0/bitstring>>, Scan0) ->
    {ok, {_Char, Rest, Scan}} = skip_new_lns(Rest0, Scan0),
    parse_comment(Rest, incr_col(Scan)).

parse_plain_text(TagName, Rest, Scan) ->
    TagSize = byte_size(TagName),
    continue_plain_text(Rest, TagName, TagSize, snapshot(update_pos(Scan))).

continue_plain_text(Rest0, TagName, TagSize, Scan0) ->
    case Rest0 of
        <<"</", TagName:TagSize/binary, $>, Rest/bitstring>> ->
            continue(Rest, fold(Scan0, [
                fun(S) -> update_pos(S) end,
                fun(S) -> snapshot(S) end,
                fun(S) -> push_tokens([text_token(Scan0), close_token(TagName, S)], S) end,
                fun(S) -> incr_col(2 + TagSize + 1, S) end,
                fun(S) -> update_pos(S) end,
                fun(S) -> snapshot(S) end
            ]));
        <<Rest0/bitstring>> ->
            {ok, {_Char, Rest, Scan}} = skip_new_lns(Rest0, Scan0),
            continue_plain_text(Rest, TagName, TagSize, incr_col(Scan))
    end.

parse_tag(TxtToken, Rest0, Scan0) ->
    case parse_tag_name(Rest0, Scan0) of
        {plain_text_without_attrs, TagName, Rest, Scan} ->
            Token = open_token(TagName, [], Scan),
            parse_plain_text(TagName, Rest, push_token(Token, Scan));
        {plain_text_with_attrs, TagName, Rest1, Scan1} ->
            case parse_attrs(Rest1, Scan1) of
                {opening, Attrs, Rest, Scan} ->
                    Token = open_token(TagName, Attrs, Scan),
                    parse_plain_text(TagName, Rest, push_token(Token, Scan))
            end;
        {void, TagName, Rest, Scan} ->
            continue(Rest, fold(Scan, [
                fun(S) -> push_tokens([TxtToken, void_token(TagName, [], S)], S) end,
                fun(S) -> update_pos(S) end,
                fun(S) -> snapshot(S) end
            ]));
        {without_attrs, TagName, Rest, Scan} ->
            continue(Rest, fold(Scan, [
                fun(S) -> push_tokens([TxtToken, open_token(TagName, [], S)], S) end,
                fun(S) -> update_pos(S) end,
                fun(S) -> snapshot(S) end
            ]));
        {with_attrs, TagName, Rest1, Scan1} ->
            case parse_attrs(Rest1, Scan1) of
                {void, Attrs, Rest, Scan} ->
                    continue(Rest, fold(Scan, [
                        fun(S) -> push_token(void_token(TagName, Attrs, S), S) end,
                        fun(S) -> update_pos(S) end,
                        fun(S) -> snapshot(S) end
                    ]));
                {opening, Attrs, Rest, Scan} ->
                    continue(Rest, fold(Scan, [
                        fun(S) -> push_token(open_token(TagName, Attrs, S), S) end,
                        fun(S) -> update_pos(S) end,
                        fun(S) -> snapshot(S) end
                    ]))
            end
    end.

parse_closing_tag(TxtToken, Rest0, Scan0) ->
    {_, TagName, Rest, Scan} = parse_tag_name(Rest0, Scan0),
    case hd(get_tokens(Scan0)) of
        {Tag, _, _} when Tag =:= void; Tag =:= open; Tag =:= comment ->
            continue(Rest, fold(Scan, [
                fun(S) -> push_tokens([TxtToken, close_token(TagName, S)], S) end
            ]));
        {close, _, CloseTagName} ->
            case is_plain_text(CloseTagName) of
                true ->
                    continue(Rest, fold(Scan, [
                        fun(S) -> push_token(close_token(TagName, S), S) end
                    ]));
                false ->
                    continue(Rest, fold(Scan, [
                        fun(S) -> push_tokens([close_token(TagName, S), TxtToken], S) end
                    ]))
            end;
        {text, _, _} ->
            continue(Rest, fold(Scan, [
                fun(S) -> push_tokens([close_token(TagName, S), TxtToken], S) end
            ]))
    end.

is_plain_text(<<"title">>) ->
    true;
is_plain_text(<<"script">>) ->
    true;
is_plain_text(<<"style">>) ->
    true;
is_plain_text(<<_TagName/bitstring>>) ->
    false.

parse_tag_name(Text, Scan) ->
    continue_tag_name(Text, update_pos(Scan)).

continue_tag_name(<<$/, $>, Rest/bitstring>>, Scan) ->
    TagName = pos_text(Scan),
    {void, TagName, Rest, fold(Scan, [
        fun(S) -> incr_col(2, S) end,
        fun(S) -> update_pos(S) end
    ])};
continue_tag_name(<<$>, Rest/bitstring>>, Scan) ->
    TagName = pos_text(Scan),
    Kind = case is_void(TagName) of
        true -> void;
        false ->
            case is_plain_text(TagName) of
                true ->
                    plain_text_without_attrs;
                false ->
                    without_attrs
            end
    end,
    {Kind, TagName, Rest, fold(Scan, [
        fun(S) -> incr_col(S) end,
        fun(S) -> update_pos(S) end
    ])};
continue_tag_name(<<$\s, Rest/bitstring>>, Scan) ->
    TagName = pos_text(Scan),
    Kind =
        case is_plain_text(TagName) of
            true ->
                plain_text_with_attrs;
            false ->
                with_attrs
        end,
    {Kind, TagName, Rest, fold(Scan, [
        fun(S) -> incr_col(S) end,
        fun(S) -> update_pos(S) end
    ])};
continue_tag_name(<<Rest0/bitstring>>, Scan0) ->
    {ok, {_Char, Rest, Scan}} = skip_new_lns(Rest0, Scan0),
    continue_tag_name(Rest, incr_col(Scan)).

is_void(<<"area">>) ->
    true;
is_void(<<"base">>) ->
    true;
is_void(<<"br">>) ->
    true;
is_void(<<"col">>) ->
    true;
is_void(<<"embed">>) ->
    true;
is_void(<<"hr">>) ->
    true;
is_void(<<"img">>) ->
    true;
is_void(<<"input">>) ->
    true;
is_void(<<"link">>) ->
    true;
is_void(<<"meta">>) ->
    true;
is_void(<<"param">>) ->
    true;
is_void(<<"source">>) ->
    true;
is_void(<<"track">>) ->
    true;
is_void(<<"wbr">>) ->
    true;
is_void(<<_TagName/bitstring>>) ->
    false.

parse_attrs(Text, Scan) ->
    continue_attrs(Text, Scan).

continue_attrs(<<$/, $>, Rest/bitstring>>, Scan) ->
    Handler = get_handler(Scan),
    Attrs = Handler:handle_attrs(pos_text(Scan), get_snap_loc(Scan)),
    {void, Attrs, Rest, fold(Scan, [
        fun(S) -> incr_col(2, S) end,
        fun(S) -> update_pos(S) end
    ])};
continue_attrs(<<$>, Rest/bitstring>>, Scan) ->
    Handler = get_handler(Scan),
    Attrs = Handler:handle_attrs(pos_text(Scan), get_snap_loc(Scan)),
    Kind = case Attrs =:= [] of
        true -> void;
        false -> opening
    end,
    {Kind, Attrs, Rest, fold(Scan, [
        fun(S) -> incr_col(S) end,
        fun(S) -> update_pos(S) end
    ])};
continue_attrs(<<Rest0/bitstring>>, Scan0) ->
    {ok, {_Char, Rest, Scan}} = skip_new_lns(Rest0, Scan0),
    continue_attrs(Rest, incr_col(Scan)).

%%%=====================================================================
%%% Test
%%%=====================================================================

-ifdef(TEST).

scan_test() ->
    Expect = [
        {open,{{2,5},undefined,undefined},
            {<<"!DOCTYPE">>,[{<<"html">>,{true,{2,5}}}]}},
        {open,{{3,5},undefined,undefined},
            {<<"html">>,[{<<"lang">>,{<<"en">>,{3,5}}}]}},
        {comment,{{4,5},undefined,undefined},<<" Comment ">>},
        {open,{{5,5},undefined,undefined},{<<"head">>,[]}},
        {open,{{6,9},undefined,undefined},{<<"title">>,[]}},
        {text,{{6,16},undefined,undefined},
            <<"<b>content inside <title> must be treated as plaintext</b>">>},
        {close,{{6,74},undefined,undefined},<<"title">>},
        {open,{{7,9},undefined,undefined},
            {<<"script">>,
            [{<<"src">>,{<<"assets/foo.js">>,{7,9}}}]}},
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
            [{<<"id">>,{<<"foo">>,{20,13}}},
            {<<"name">>,{<<"foo">>,{20,22}}},
            {<<"value">>,{<<"\"bar\"">>,{20,33}}}]}},
        {void,{{21,13},undefined,undefined},
            {<<"input">>,
            [{<<"type">>,{<<"number">>,{21,13}}},
            {<<"value">>,{<<"10">>,{21,27}}}]}},
        {close,{{22,9},undefined,undefined},<<"form">>},
        {close,{{23,5},undefined,undefined},<<"body">>},
        {close,{{24,5},undefined,undefined},<<"html">>}
    ],
    String = <<"
    <!DOCTYPE html>
    <html lang=\"en\">
    <!-- Comment -->
    <head>
        <title><b>content inside <title> must be treated as plaintext</b></title>
        <script src=\"assets/foo.js\"></script>
        <style>
            :root {
                --foo: 0;
            }
        </style>
    </head>
    <body>
        <h1>Form</h1>
        <br>
        <br/>
        <form>
            <div>Foo Form</div>
            <input id=\"foo\" name='foo' value='\"b\ar\"' />
            <input type=\"number\" value=10 />
        </form>
    </body>
    </html>
    ">>,
    Expr = string(String),
    ?assertEqual(Expect, Expr).

handle_attrs_test() ->
    Expect = [
        {<<"id">>,{<<"foo">>,{2,5}}},
        {<<"name">>,{<<"foo">>,{2,14}}},
        {<<"value">>,{<<"\"bar\"">>,{2,25}}},
        {<<"maxlength">>,{<<"10">>,{2,39}}},
        {<<"required">>,{true,{3,5}}},
        {<<"disabled">>,{true,{3,14}}}
    ],
    Text = <<"
    id=\"foo\" name='foo' value='\"b\ar\"' maxlength=10
    required disabled disabled
    ">>,
    Expr = handle_attrs(Text, {1, 1}),
    ?assertEqual(Expect, Expr).

-endif.
