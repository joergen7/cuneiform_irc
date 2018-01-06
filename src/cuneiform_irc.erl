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
                      public_shell = #shell_state{},
                      mode_map     = #{} } ).

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

  % attach client service
  ok = cf_client:start(),

  #bot_state{ nick_name = NickName }.


handle_privmsg( public, User, Content, BotState ) ->

  #bot_state{ nick_name    = NickName,
              public_shell = PublicShell,
              mode_map     = ModeMap } = BotState,

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

        ["reset"] ->
          {reply, User++": Shell reinitialized.", BotState#bot_state{ public_shell = #shell_state{} }};

        ["hist"] ->

          G =
            fun( {assign, _, R, E} ) ->
              SR = string:pad( cuneiform_shell:format_pattern( R ), 16, trailing ),
              SE = cuneiform_shell:format_expr( E ),
              io_lib:format( "let ~s = ~s;~n", [SR, SE] )
            end,

          #shell_state{ def_lst = DefLst } = PublicShell,

          Reply = User++": "++lists:flatten( string:join( [G( Def ) || Def <- DefLst], "\n" ) ),

          {reply, Reply, BotState};

        [Cmd|_] ->
          {reply, User++": Command not recognized: "++Cmd, BotState}

      end;

    [$>|Code] ->
      {F, PublicShell1} = process_code( User, Code, PublicShell ),
      {spawn, F, BotState#bot_state{ public_shell = PublicShell1 }};

    _ ->

      #{ User := Mode } = ModeMap,
      case Mode of

        code ->
          {F, PublicShell1} = process_code( User, Content, PublicShell ),
          {spawn, F, BotState#bot_state{ public_shell = PublicShell1 }};


        comment ->
          {noreply, BotState}

      end

  end;

handle_privmsg( private, _, _, BotState ) ->
  {noreply, BotState}.







handle_join( User, BotState ) ->

  #bot_state{ mode_map  = ModeMap } = BotState,

  ModeMap1  = ModeMap#{ User => comment },

  BotState#bot_state{ mode_map = ModeMap1 }.


handle_part( User, BotState ) ->

  #bot_state{ mode_map  = ModeMap } = BotState,

  ModeMap1 = maps:remove( User, ModeMap ),

  BotState#bot_state{ mode_map = ModeMap1 }.






%%====================================================================
%% Internal functions
%%====================================================================

process_code( User, Code, ShellState ) ->

  {ReplyLst, ShellState1} = cuneiform_shell:shell_eval( Code++"\n", ShellState ),

  F =
    fun

      ( {query, E} ) ->
        V = cre_client:eval( cf_client, E ),
        {ok, T} = cuneiform_type:type( V ),
        SV = cuneiform_shell:format_expr( V ),
        ST = cuneiform_shell:format_type( T ),
        io_lib:format( "~s : ~s", [SV, ST] );

      ( {parrot, E, T} ) ->
        SE = cuneiform_shell:format_expr( E ),
        ST = cuneiform_shell:format_type( T ),
        io_lib:format( "~s : ~s", [SE, ST] );

      ( Reply = {error, _Stage, _Reason} ) ->
        S = cuneiform_shell:format_error( Reply ),
        io_lib:format( "~s", [S] )

    end,

  G =
    fun() ->
      lists:flatten( User++": "++string:join( [F( X ) || X <- ReplyLst], "\n" ) )
    end,

  {G, ShellState1}.