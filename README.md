# bel-framework/bel-html

HTML utilities for Erlang.

## Scanner and Parser

### Example

```erlang
1> String = <<"
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
           <input id=\"foo\" class=' foo   bar   ' name='foo' value='\"b\ar\"' />
           <input type=\"number\" value=10 />
       </form>
   </body>
   </html>
   ">>.

2> Tokens = bel_html:scan_html5(String).
[{open,{{2,4},undefined,undefined},
       {<<"!DOCTYPE">>,[{<<"html">>,{true,{2,4}}}]}},
 {open,{{3,4},undefined,undefined},
       {<<"html">>,[{<<"lang">>,{<<"en">>,{3,4}}}]}},
 {comment,{{4,4},undefined,undefined},<<" Comment ">>},
 {open,{{5,4},undefined,undefined},{<<"head">>,[]}},
 {open,{{6,8},undefined,undefined},{<<"title">>,[]}},
 {text,{{6,15},undefined,undefined},
       <<"<b>content inside <title> must be treated as plaintext</b>">>},
 {close,{{6,73},undefined,undefined},<<"title">>},
 {open,{{7,8},undefined,undefined},
       {<<"script">>,[{<<"src">>,{<<"assets/foo.js">>,{7,8}}}]}},
 {close,{{7,36},undefined,undefined},<<"script">>},
 {open,{{8,8},undefined,undefined},{<<"style">>,[]}},
 {text,{{8,15},undefined,undefined},
       <<":root {\n               --foo: 0;\n           }">>},
 {close,{{12,8},undefined,undefined},<<"style">>},
 {close,{{13,4},undefined,undefined},<<"head">>},
 {open,{{14,4},undefined,undefined},{<<"body">>,[]}},
 {open,{{15,8},undefined,undefined},{<<"h1">>,[]}},
 {text,{{15,12},undefined,undefined},<<"Form">>},
 {close,{{15,16},undefined,undefined},<<"h1">>},
 {void,{{16,8},undefined,undefined},{<<"br">>,[]}},
 {void,{{17,8},undefined,undefined},{<<"br">>,[]}},
 {open,{{18,8},undefined,undefined},{<<"form">>,[]}},
 {open,{{19,12},undefined,undefined},{<<"div">>,[]}},
 {text,{{19,17},undefined,undefined},<<"Foo Form">>},
 {close,{{19,25},undefined,undefined},<<"div">>},
 {void,{{20,12},undefined,undefined},
       {<<"input">>,
        [{<<"id">>,{<<"foo">>,{20,12}}},
         {<<"class">>,{[<<"foo">>,<<"bar">>],{20,21}}},
         {<<"name">>,{<<"foo">>,{20,43}}},
         {<<"value">>,{<<"\"bar\"">>,{20,54}}}]}},
 {void,{{21,12},undefined,undefined},
       {<<"input">>,
        [{<<"type">>,{<<"number">>,{21,12}}},
         {<<"value">>,{<<"10">>,{21,26}}}]}},
 {close,{{22,8},undefined,undefined},<<"form">>},
 {close,{{23,4},undefined,undefined},<<"body">>},
 {close,{{24,4},undefined,undefined},<<"html">>}]

3> bel_html:parse_html5(Tokens).
[{<<"!DOCTYPE">>,
  #{<<"html">> => true},
  [{<<"html">>,
    #{<<"lang">> => <<"en">>},
    [{<<"head">>,#{},
      [{<<"title">>,#{},
        [<<"<b>content inside <title> must be treated as plaintext</b>">>]},
       {<<"script">>,#{<<"src">> => <<"assets/foo.js">>},[]},
       {<<"style">>,#{},
        [<<":root {\n               --foo: 0;\n           }">>]}]},
     {<<"body">>,#{},
      [{<<"h1">>,#{},[<<"Form">>]},
       {<<"br">>,#{},[]},
       {<<"br">>,#{},[]},
       {<<"form">>,#{},
        [{<<"div">>,#{},[<<"Foo Form">>]},
         {<<"input">>,
          #{<<"class">> => [<<"foo">>,<<"bar">>],
            <<"id">> => <<"foo">>,<<"name">> => <<"foo">>,
            <<"value">> => <<"\"bar\"">>},
          []},
         {<<"input">>,
          #{<<"type">> => <<"number">>,<<"value">> => <<"10">>},
          []}]}]}]}]}]
```

> [!NOTE]
> The structure of the parsed tokens is `{Tag, Attributes, Nodes}`.

## Build

```shell
$ rebar3 compile
```
