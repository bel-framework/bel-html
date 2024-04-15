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
-export([ handle_attrs/1 ]).

-callback handle_attrs(binary(), scan()) -> attributes().

-import(bel_scan, [ continue/2
                  , new_ln/1
                  , incr_col/1
                  , incr_col/2
                  , update_pos/1
                  , pos_text/1
                  , get_tokens/1
                  ]).

-record(state, { handler }).

-type scan()       :: bel_scan:t().
-type attributes() :: [{binary(), binary()}]
                    | #{binary() => binary()}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
    Text = pos_text(Scan),
    TxtToken = text_token(Text),
    parse_comment(Rest, update_pos(incr_col(4, push_token(TxtToken, Scan))));
handle_char($<, <<$/, Rest/bitstring>>, Scan) ->
    Text = pos_text(Scan),
    parse_closing_tag(Text, Rest, incr_col(2, Scan));
handle_char($<, Rest, Scan) ->
    Text = pos_text(Scan),
    parse_tag(Text, Rest, incr_col(Scan));
handle_char(_Char, Rest, Scan) ->
    continue(Rest, incr_col(Scan)).

handle_tokens(_Tokens, Scan0) ->
    Text = pos_text(Scan0),
    TxtToken = text_token(Text),
    Scan = push_token(TxtToken, Scan0),
    Tokens = get_tokens(Scan),
    lists:reverse(Tokens).

%%%=====================================================================
%%% Default callbacks
%%%=====================================================================

handle_attrs(Text) ->
    do_handle_attrs(Text, []).

do_handle_attrs(<<>>, Acc) ->
    lists:reverse(Acc);
do_handle_attrs(Text, Acc) ->
    case get_attr_key(Text) of
        {without_value, Pos, Len, Rest} ->
            Key = binary_part(Text, Pos, Len),
            do_handle_attrs(Rest, [Key | Acc]);
        {with_value, KPos, KLen, Rest0} ->
            Key = binary_part(Text, KPos, KLen),
            {VPos, VLen, Rest} = get_attr_value(Rest0),
            Value = binary_part(Rest0, VPos, VLen),
            do_handle_attrs(Rest, [{Key, Value} | Acc]);
        none ->
            lists:reverse(Acc)
    end.

get_attr_key(Text) ->
    get_attr_key(Text, 0).

get_attr_key(<<$\r, $\n, Rest/binary>>, Pos) ->
    get_attr_key(Rest, Pos+1);
get_attr_key(<<$\r, Rest/binary>>, Pos) ->
    get_attr_key(Rest, Pos+1);
get_attr_key(<<$\n, Rest/binary>>, Pos) ->
    get_attr_key(Rest, Pos+1);
get_attr_key(<<$\s, Rest/binary>>, Pos) ->
    get_attr_key(Rest, Pos+1);
get_attr_key(<<Rest/binary>>, Pos) ->
    do_get_attr_key(Rest, Pos, 0);
get_attr_key(<<>>, _Pos) ->
    none.

do_get_attr_key(<<$\r, $\n, Rest/binary>>, Pos, Len) ->
    {without_value, Pos, Len, Rest};
do_get_attr_key(<<$\r, Rest/binary>>, Pos, Len) ->
    {without_value, Pos, Len, Rest};
do_get_attr_key(<<$\n, Rest/binary>>, Pos, Len) ->
    {without_value, Pos, Len, Rest};
do_get_attr_key(<<$\s, Rest/binary>>, Pos, Len) ->
    {without_value, Pos, Len, Rest};
do_get_attr_key(<<$=, Rest/binary>>, Pos, Len) ->
    {with_value, Pos, Len, Rest};
do_get_attr_key(<<_, Rest/binary>>, Pos, Len) ->
    do_get_attr_key(Rest, Pos, Len+1);
do_get_attr_key(<<>>, Pos, Len) ->
    {without_value, Pos, Len, <<>>}.

get_attr_value(<<$", Rest/binary>>) ->
    do_get_attr_value(Rest, 0, 1, $");
get_attr_value(<<$', Rest/binary>>) ->
    do_get_attr_value(Rest, 0, 1, $');
get_attr_value(<<Rest/binary>>) ->
    do_get_attr_value(Rest, 0, -1, $\s).

do_get_attr_value(<<$\\, _, Rest/binary>>, Pos, Len, Q) ->
    do_get_attr_value(Rest, Pos, Len+2, Q);
do_get_attr_value(<<Q, Rest/binary>>, Pos, Len, Q) ->
    {Pos, Len+1, Rest};
do_get_attr_value(<<_, Rest/binary>>, Pos, Len, Q) ->
    do_get_attr_value(Rest, Pos, Len+1, Q).

comment_token(Comment) ->
    {comment, Comment}.

text_token(Text) ->
    {text, Text}.

tag_token(close, TagName) ->
    {close, TagName}.

tag_token(void, TagName, Attrs) ->
    {void, {TagName, Attrs}};
tag_token(open, TagName, Attrs) ->
    {open, {TagName, Attrs}}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

get_state(Scan) ->
    bel_scan:get_metadata(Scan).

get_handler(#state{handler = Handler}) ->
    Handler;
get_handler(Scan) ->
    get_handler(get_state(Scan)).

push_token({text, Text} = Token, Scan) ->
    case string:trim(Text) =:= <<>> of
        true ->
            Scan;
        false ->
            bel_scan:push_token(Token, Scan)
    end;
push_token(Token, Scan) ->
    bel_scan:push_token(Token, Scan).

push_tokens(Tokens, Scan) ->
    lists:foldl(fun push_token/2, Scan, Tokens).

parse_comment(<<"-->", Rest/bitstring>>, Scan) ->
    Text = pos_text(Scan),
    Token = comment_token(Text),
    continue(Rest, update_pos(incr_col(3, push_token(Token, Scan))));
parse_comment(<<$\r, $\n, Rest/bitstring>>, Scan) ->
    parse_comment(Rest, new_ln(Scan));
parse_comment(<<$\r, Rest/bitstring>>, Scan) ->
    parse_comment(Rest, new_ln(Scan));
parse_comment(<<$\n, Rest/bitstring>>, Scan) ->
    parse_comment(Rest, new_ln(Scan));
parse_comment(<<_, Rest/bitstring>>, Scan) ->
    parse_comment(Rest, incr_col(Scan)).

parse_plain_text(TagName, Rest, Scan) ->
    TagSize = byte_size(TagName),
    continue_plain_text(Rest, TagName, TagSize, update_pos(Scan)).

continue_plain_text(Rest0, TagName, TagSize, Scan) ->
    case Rest0 of
        <<"</", TagName:TagSize/binary, $>, Rest/bitstring>> ->
            TxtToken = text_token(pos_text(Scan)),
            Token = tag_token(close, TagName),
            continue(Rest, update_pos(incr_col(2+TagSize+1, push_tokens([TxtToken, Token], Scan))));
        <<_, Rest/bitstring>> ->
            continue_plain_text(Rest, TagName, TagSize, incr_col(Scan))
    end.

parse_tag(Text, Rest0, Scan0) ->
    case parse_tag_name(Rest0, Scan0) of
        {plain_text_without_attrs, TagName, Rest, Scan} ->
            Token = tag_token(open, TagName, []),
            parse_plain_text(TagName, Rest, push_token(Token, Scan));
        {plain_text_with_attrs, TagName, Rest1, Scan1} ->
            case parse_attrs(Rest1, Scan1) of
                {opening, Attrs, Rest, Scan} ->
                    Token = tag_token(open, TagName, Attrs),
                    parse_plain_text(TagName, Rest, push_token(Token, Scan))
            end;
        {void, TagName, Rest, Scan} ->
            TxtToken = text_token(Text),
            Token = tag_token(void, TagName, []),
            continue(Rest, update_pos(push_tokens([TxtToken, Token], Scan)));
        {without_attrs, TagName, Rest, Scan} ->
            TxtToken = text_token(Text),
            Token = tag_token(open, TagName, []),
            continue(Rest, update_pos(push_tokens([TxtToken, Token], Scan)));
        {with_attrs, TagName, Rest1, Scan1} ->
            case parse_attrs(Rest1, Scan1) of
                {void, Attrs, Rest, Scan} ->
                    Token = tag_token(void, TagName, Attrs),
                    continue(Rest, push_token(Token, Scan));
                {opening, Attrs, Rest, Scan} ->
                    Token = tag_token(open, TagName, Attrs),
                    continue(Rest, push_token(Token, Scan))
            end
    end.

parse_closing_tag(Text, Rest0, Scan0) ->
    {_, TagName, Rest, Scan} = parse_tag_name(Rest0, Scan0),
    case hd(get_tokens(Scan0)) of
        {void, _} ->
            TxtToken = text_token(Text),
            Token = tag_token(close, TagName),
            continue(Rest, update_pos(push_tokens([TxtToken, Token], Scan)));
        {open, _} ->
            TxtToken = text_token(Text),
            Token = tag_token(close, TagName),
            continue(Rest, update_pos(push_tokens([TxtToken, Token], Scan)));
        {close, CloseTagName} ->
            case is_plain_text(CloseTagName) of
                true ->
                    Token = tag_token(close, TagName),
                    continue(Rest, update_pos(push_token(Token, Scan)));
                false ->
                    TxtToken = text_token(Text),
                    Token = tag_token(close, TagName),
                    continue(Rest, update_pos(push_tokens([Token, TxtToken], Scan)))
            end;
        {text, _} ->
            TxtToken = text_token(Text),
            Token = tag_token(close, TagName),
            continue(Rest, update_pos(push_tokens([Token, TxtToken], Scan)))
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
    {void, TagName, Rest, update_pos(incr_col(2, Scan))};
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
    {Kind, TagName, Rest, update_pos(incr_col(Scan))};
continue_tag_name(<<$\s, Rest/bitstring>>, Scan) ->
    TagName = pos_text(Scan),
    Kind =
        case is_plain_text(TagName) of
            true ->
                plain_text_with_attrs;
            false ->
                with_attrs
        end,
    {Kind, TagName, Rest, update_pos(incr_col(Scan))};
continue_tag_name(<<$\r, $\n, Rest/bitstring>>, Scan) ->
    continue_tag_name(Rest, new_ln(Scan));
continue_tag_name(<<$\r, Rest/bitstring>>, Scan) ->
    continue_tag_name(Rest, new_ln(Scan));
continue_tag_name(<<$\n, Rest/bitstring>>, Scan) ->
    continue_tag_name(Rest, new_ln(Scan));
continue_tag_name(<<_, Rest/bitstring>>, Scan) ->
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

continue_attrs(<<$/, $>, Rest/binary>>, Scan) ->
    Handler = get_handler(Scan),
    Attrs = Handler:handle_attrs(pos_text(Scan)),
    {void, Attrs, Rest, update_pos(incr_col(2, Scan))};
continue_attrs(<<$>, Rest/binary>>, Scan) ->
    Handler = get_handler(Scan),
    Attrs = Handler:handle_attrs(pos_text(Scan)),
    Kind = case Attrs =:= [] of
        true -> void;
        false -> opening
    end,
    {Kind, Attrs, Rest, update_pos(incr_col(Scan))};
continue_attrs(<<$\r, $\n, Rest/bitstring>>, Scan) ->
    continue_attrs(Rest, new_ln(Scan));
continue_attrs(<<$\r, Rest/bitstring>>, Scan) ->
    continue_attrs(Rest, new_ln(Scan));
continue_attrs(<<$\n, Rest/bitstring>>, Scan) ->
    continue_attrs(Rest, new_ln(Scan));
continue_attrs(<<_, Rest/binary>>, Scan) ->
    continue_attrs(Rest, incr_col(Scan)).

%%%=====================================================================
%%% Test
%%%=====================================================================

-ifdef(TEST).

parse_test() ->
    Expect = [
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
        {<<"id">>, <<"\"foo\"">>},
        {<<"name">>, <<"'foo'">>},
        {<<"value">>, <<"'\"bar\"'">>},
        {<<"maxlength">>, <<"10">>},
        <<"required">>,
        <<"disabled">>
    ],
    Text = <<"id=\"foo\" name='foo' value='\"b\ar\"' maxlength=10 required disabled">>,
    Expr = handle_attrs(Text),
    ?assertEqual(Expect, Expr).

-endif.
