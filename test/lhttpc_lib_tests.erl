%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of Erlang Training and Consulting Ltd. nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY Erlang Training and Consulting Ltd. ''AS IS''
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Erlang Training and Consulting Ltd. BE
%%% LIABLE SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% ----------------------------------------------------------------------------

%%% @author Oscar Hellström <oscar@hellstrom.st>
-module(lhttpc_lib_tests).

-include_lib("eunit/include/eunit.hrl").

parse_url_test_() ->
    [
        ?_assertEqual({"host", 80, "/", false},
            lhttpc_lib:parse_url("http://host")),
        ?_assertEqual({"host", 80, "/", false},
            lhttpc_lib:parse_url("http://host/")),
        ?_assertEqual({"host", 443, "/", true},
            lhttpc_lib:parse_url("https://host")),
        ?_assertEqual({"host", 443, "/", true},
            lhttpc_lib:parse_url("https://host/")),
        ?_assertEqual({"host", 180, "/", false},
            lhttpc_lib:parse_url("http://host:180")),
        ?_assertEqual({"host", 180, "/", false},
            lhttpc_lib:parse_url("http://host:180/")),
        ?_assertEqual({"host", 180, "/foo", false},
            lhttpc_lib:parse_url("http://host:180/foo")),
        ?_assertEqual({"host", 180, "/foo/bar", false},
            lhttpc_lib:parse_url("http://host:180/foo/bar")),
        ?_assertEqual({"host", 80, "?query", false},
            lhttpc_lib:parse_url("http://host?query")),
        ?_assertEqual({"host", 443, "?query", true},
            lhttpc_lib:parse_url("https://host?query")),
        ?_assertEqual({"host", 180, "?query", false},
            lhttpc_lib:parse_url("http://host:180?query"))
    ].
