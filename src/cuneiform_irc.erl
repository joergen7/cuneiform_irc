%% -*- erlang -*-
%%
%% A Cuneiform IRC chatterbot.
%%
%% Copyright 2018 Jörgen Brandt
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module( cuneiform_irc ).
-behaviour( gen_ircclient ).

%%====================================================================
%% Includes
%%====================================================================

-include_lib( "cuneiform/include/cuneiform_shell.hrl" ).

%%====================================================================
%% Record definitions
%%====================================================================

-record( bot_state, { nick_name,
                      cf_client,
                      public_shell = #shell_state{},
                      mode_map     = #{},
                      shell_map    = #{} } ).

%%====================================================================
%% Exports
%%====================================================================

-export( [main/1] ).
-export( [init/1, handle_privmsg/4, handle_join/2, handle_part/2] ).

%%====================================================================
%% API functions
%%====================================================================

main( _Args ) ->

  Server   = "irc.freenode.net",
  Port     = 6667,
  NickName = "cuneiform",
  UserName = "cuneiform-lang",
  RealName = "Cuneiform: A functional workflow language for large-scale data analysis",

  ConnInfo = {conn_info, Server, Port, NickName, UserName, RealName},
  Channel = "#botwar",
  UsrMod  = ?MODULE,

  gen_ircclient:start_link( ConnInfo, Channel, UsrMod, {NickName} ),

  receive
    X -> X
  end.
  







%%====================================================================
%% gen_ircclient callback functions
%%====================================================================


init( {NickName} ) ->

  % start CRE
  ok = cre:start(),

  % attach workers
  ok = cf_worker:start(),

  % attach client service
  ok = cf_client:start(),

  #bot_state{ nick_name = NickName, cf_client = cf_client }.


handle_privmsg( public, User, Content, BotState ) ->

  #bot_state{ nick_name = NickName,
              public_shell = PublicShell,
              mode_map  = ModeMap } = BotState,

  case string:prefix( Content, NickName ) of

    [$:|Ctl] ->

      case string:tokens( Ctl, " \t" ) of

        ["mode", "comment"] ->
          ModeMap1 = ModeMap#{ User => comment },
          BotState1 = BotState#bot_state{ mode_map = ModeMap1 },
          {reply, User++": mode set to comment.", BotState1};

        ["mode", "code"] ->
          ModeMap1 = ModeMap#{ User => code },
          BotState1 = BotState#bot_state{ mode_map = ModeMap1 },
          {reply, User++": mode set to code.", BotState1};

        ["mode", X] ->
          {reply, User++": Error: mode not recognized: "++X, BotState};

        ["mode"|_] ->
          {reply, User++": Error: mode expects exactly one argument.", BotState};

        [Cmd|_] ->
          {reply, User++": Command not recognized: "++Cmd, BotState}

      end;

    [$>|Code] ->

      {ReplyLst, PublicShell1} = cuneiform_shell:shell_eval( Code++"\n", PublicShell ),
      Reply = lists:flatten( string:join( [io_lib:format( "~s: ~p", [User, X] ) || X <- ReplyLst], "\n" ) ),
      {reply, Reply, BotState#bot_state{ public_shell = PublicShell1 }};

    _ ->

      #{ User := Mode } = ModeMap,
      case Mode of

        code ->

          {ReplyLst, PublicShell1} = cuneiform_shell:shell_eval( Content++"\n", PublicShell ),
          Reply = lists:flatten( string:join( [io_lib:format( "~s: ~p", [User, X] ) || X <- ReplyLst], "\n" ) ),
          {reply, Reply, BotState#bot_state{ public_shell = PublicShell1 }};

        comment ->
          {noreply, BotState}

      end

  end.







handle_join( User, BotState ) ->

  #bot_state{ mode_map  = ModeMap,
              shell_map = ShellMap } = BotState,

  ModeMap1  = ModeMap#{ User => spectate },
  ShellMap1 = ShellMap#{ User => #shell_state{} },

  BotState#bot_state{ mode_map = ModeMap1, shell_map = ShellMap1 }.


handle_part( User, BotState ) ->

  #bot_state{ mode_map  = ModeMap,
              shell_map = ShellMap } = BotState,

  ModeMap1 = maps:remove( User, ModeMap ),
  ShellMap1 = maps:remove( User, ShellMap ),

  BotState#bot_state{ mode_map = ModeMap1, shell_map = ShellMap1 }.






%%====================================================================
%% Internal functions
%%====================================================================
