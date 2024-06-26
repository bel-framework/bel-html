%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc HTML module.
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
-module(bel_html).

% HTML5
-export([ scan_html5/1, parse_html5/1 ]).

%%%=====================================================================
%%% HTML5
%%%=====================================================================

% Fixes no local return warning from bel_scan/new/1.
-dialyzer({nowarn_function, [scan_html5/1]}).

scan_html5(String) ->
    bel_html_5_scan:string(String).

parse_html5(Tokens) ->
    bel_html_5_parser:parse(Tokens).
