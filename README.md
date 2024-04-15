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
           <input id=\"foo\" name='foo' value='\"b\ar\"' />
           <input type=\"number\" value=10 />
       </form>
   </body>
   </html>
   ">>.

2> Tokens = bel_html:scan_html5(String).
[{open,{<<"!DOCTYPE">>,[<<"html">>]}},
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
        [{<<"type">>,<<"\"number\"">>},{<<"value">>,<<"10">>}]}},
 {close,<<"form">>},
 {close,<<"body">>},
 {close,<<"html">>}]

3> bel_html:parse_html5(Tokens).
[{<<"!DOCTYPE">>,
  [<<"html">>],
  [{<<"html">>,
    [{<<"lang">>,<<"\"en\"">>}],
    [{<<"head">>,[],
      [{<<"title">>,[],
        [<<"<b>content inside <title> must be treated as plaintext</b>">>]},
       {<<"script">>,[{<<"src">>,<<"\"assets/foo.js\"">>}],[]},
       {<<"style">>,[],
        [<<"\n            :root {\n                --foo: 0;\n            }\n        ">>]}]},
     {<<"body">>,[],
      [{<<"h1">>,[],[<<"Form">>]},
       {<<"br">>,[],[]},
       {<<"br">>,[],[]},
       {<<"form">>,[],
        [{<<"div">>,[],[<<"Foo Form">>]},
         {<<"input">>,
          [{<<"id">>,<<"\"foo\"">>},
           {<<"name">>,<<"'foo'">>},
           {<<"value">>,<<"'\"bar\"'">>},
           <<>>],
          []},
         {<<"input">>,
          [{<<"type">>,<<"\"number\"">>},{<<"value">>,<<"10">>}],
          []}]}]}]}]}]
```

> [!NOTE]
> The structure of the parsed tokens is `{TagName, Attributes, ChildrenNodes}`.

## Build

```shell
$ rebar3 compile
```
