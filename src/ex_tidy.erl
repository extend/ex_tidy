%% Copyright (c) 2011, Dev:Extend
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ex_tidy).
-author('Anthony Ramine <nox@dev-extend.eu>').

-export([string/1, string/2]).

%% @spec string(binary()) -> binary()
%% @doc Tidy a given string with the default options.
string(Markup) ->
  string(Markup, []).

%% @spec string(binary(), list()) -> binary()
%% @doc Tidy a given string with some options.
%% @todo Document `Options'.
string(Markup, Options) ->
  Ref = ex_tidy_drv:new(),
  ex_tidy_drv:options(Ref, Options),
  ex_tidy_drv:parse(Ref, Markup),
  ex_tidy_drv:clean_repair(Ref),
  ex_tidy_drv:dump(Ref).
