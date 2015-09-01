%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(lvc_mgmt_dispatcher).

-behaviour(rabbit_mgmt_extension).
-export([dispatcher/0, web_ui/0]).

web_ui() -> [{javascript, <<"lvc.js">>}].

dispatcher() ->
    [
     {["lvc", vhost, exchange], lvc_mgmt_handler, []},
     {["lvc", vhost, exchange, key], lvc_mgmt_handler, []}
    ].

% end of file
