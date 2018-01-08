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
                      mode_map     = #{},
                      white_lst    = [] } ).

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
  Channel  = "#cuneiform-lang",
  UsrMod   = ?MODULE,
  WhiteLst = ["joergen7"],

  gen_ircclient:start_link( ConnInfo, Channel, UsrMod, {NickName, WhiteLst} ),

  receive
    X -> X
  end.
  







%%====================================================================
%% gen_ircclient callback functions
%%====================================================================


init( {NickName, WhiteLst} ) ->

  % attach client service
  ok = cf_client:start(),

  #bot_state{ nick_name = NickName, white_lst = WhiteLst }.


handle_privmsg( public, User, Content, BotState ) ->

  #bot_state{ nick_name    = NickName,
              mode_map     = ModeMap,
              white_lst    = WhiteLst } = BotState,

  % get the mode for the user
  %
  % while we should have received a join event for a user before we can ever get
  % a private message, we set an unseen user to spectate, just in case
  Mode = maps:get( User, ModeMap, spectate ),

  try

    case WhiteLst of

      % an empty white list means everybody can edit
      [] ->
        ok;
      
      % a non-empty white list means we need to check whether the user is in the
      % white list
      [_|_] ->
        case lists:member( User, WhiteLst ) of
          true  -> ok;
          false -> throw( ignore )
        end

    end,


    case Content of

      % should the message start with a hashtag interpret it as a control
      % message
      [$&|Ctl] ->
        case string:tokens( Ctl, " " ) of
          ["mode", "cuneiform"]  -> throw( {mode, cuneiform} );
          ["mode", _]            -> throw( {mode, spectate} );
          ["reset", "cuneiform"] -> throw( reset );
          ["hist"]               -> throw( hist );
          _                      -> throw( ignore )
        end;

      % no control message means: go on
      _ ->
        ok

    end,



    case string:prefix( Content, NickName++":" ) of

      % a generic message is interpreted only if we are in cuneiform mode
      nomatch ->
        case Mode of
          cuneiform -> throw( {process, Content} );
          spectate  -> throw( ignore )
        end;

      % a message that addresses the bot is interpreted as code
      Code ->
        throw( {process, Code} )


    end,

    % dead end; some exception should have been thrown
    error( end_of_control )

  catch
    throw:ignore       -> {noreply, BotState};
    throw:{process, C} -> process_code( C, BotState );
    throw:{mode, M}    -> set_mode( M, User, BotState );
    throw:reset        -> reset( BotState );
    throw:hist         -> hist( BotState )
  end;

handle_privmsg( private, _, _, BotState ) ->
  {noreply, BotState}.


handle_join( User, BotState ) ->

  #bot_state{ mode_map  = ModeMap } = BotState,

  ModeMap1  = ModeMap#{ User => spectate },

  BotState#bot_state{ mode_map = ModeMap1 }.


handle_part( User, BotState ) ->

  #bot_state{ mode_map  = ModeMap } = BotState,

  ModeMap1 = maps:remove( User, ModeMap ),

  BotState#bot_state{ mode_map = ModeMap1 }.






%%====================================================================
%% Internal functions
%%====================================================================

process_code( Code, BotState ) ->

  #bot_state{ public_shell = ShellState } = BotState,

  {ReplyLst, ShellState1} = cuneiform_shell:shell_eval( Code++"\n", ShellState ),

  F =
    fun

      ( {query, E} ) ->
        V = cre_client:eval( cf_client, E ),
        case V of

          {err, _, _} ->
            cuneiform_shell:format_error( {error, eval,V} );

          _ ->
            {ok, T} = cuneiform_type:type( V ),
            SV = cuneiform_shell:format_expr( V ),
            ST = cuneiform_shell:format_type( T ),
            io_lib:format( "~s : ~s", [SV, ST] )

        end;

      ( {parrot, E, T} ) ->
        SE = cuneiform_shell:format_expr( E ),
        ST = cuneiform_shell:format_type( T ),
        io_lib:format( "~s : ~s", [SE, ST] );

      ( Reply = {error, _Stage, _Reason} ) ->
        cuneiform_shell:format_error( Reply )

    end,

  G =
    fun() ->
      lists:flatten( string:join( [F( X ) || X <- ReplyLst], "\n" ) )
    end,

  BotState1 = BotState#bot_state{ public_shell = ShellState1 },

  case ReplyLst of
    [] ->    {noreply, BotState1};
    [_|_] -> {spawn, G, BotState1}
  end.


set_mode( Mode, User, BotState )
when Mode =:= spectate orelse Mode =:= cuneiform ->

  #bot_state{ mode_map = ModeMap } = BotState,

  ModeMap1 = ModeMap#{ User => Mode },
  BotState1 = BotState#bot_state{ mode_map = ModeMap1 },

  {reply, User++": mode set to "++atom_to_list( Mode ), BotState1}.


reset( BotState ) ->
  {reply, "shell state reinitialized",
          BotState#bot_state{ public_shell = #shell_state{} }}.


hist( BotState ) ->

  G =
    fun( {assign, _, R, E} ) ->
      SR = string:pad( cuneiform_shell:format_pattern( R ), 16, trailing ),
      SE = cuneiform_shell:format_expr( E ),
      io_lib:format( "let ~s = ~s;~n", [SR, SE] )
    end,

  #bot_state{ public_shell = PublicShell } = BotState,
  #shell_state{ def_lst = DefLst } = PublicShell,

  Reply = lists:flatten( string:join( [G( Def ) || Def <- DefLst], "\n" ) ),

  {reply, Reply, BotState}.
