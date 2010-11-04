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

-module(ex_tidy_drv).
-author('Anthony Ramine <nox@dev-extend.eu>').

-export([release_date/0,
       new/0,
       new/1,
       status/1,
       type/1,
       errors_count/1,
       warnings_count/1,
       access_warnings_count/1,
       config_errors_count/1,
       encoding/2,
       input_encoding/1,
       input_encoding/2,
       output_encoding/1,
       output_encoding/2,
       option/2,
       option/3,
       option_is_readonly/2,
       option_doc/2,
       options/1,
       options/2,
       parse/2,
       clean_repair/1,
       dump/1,
       diagnostics/1]).

-on_load(init/0).

init() ->
  SoName = filename:join(code:priv_dir(ex_tidy), ex_tidy_drv),
  erlang:load_nif(SoName, 0).

release_date() ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

new() ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

new(_Ref) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

status(_Ref) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

type(_Ref) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

errors_count(_Ref) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

warnings_count(_Ref) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

access_warnings_count(_Ref) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

config_errors_count(_Ref) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

encoding(_Ref, _Enc) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

input_encoding(_Ref) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

input_encoding(_Ref, _Enc) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

output_encoding(_Ref) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

output_encoding(_Ref, _Enc) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

option(_Ref, _Name) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

option(_Ref, _Name, _Value) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

option_is_readonly(_Ref, _Name) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

option_doc(_Ref, _Name) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

options(_Ref) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

options(_Ref, _Options) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

parse(_Ref, _Data) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

clean_repair(_Ref) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

dump(_Ref) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).

diagnostics(_Ref) ->
  erlang:nif_error({nif_not_loaded, ?MODULE, ?LINE}).
