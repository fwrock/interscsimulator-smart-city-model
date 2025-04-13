%Class that represents a Metro Graph
-module(class_Metro).

% Determines what are the mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CityName , MetroFile ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("sim_diasca_for_actors.hrl").

% Creates a new metro graph actor
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , sensor_type() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

        case ets:info(options) of
	    undefined -> ets:new(options, [public, set, named_table]);
            _ -> ok
        end,
        ets:insert(options, {metro_pid, self() }),

	ActorState = class_Actor:construct( State, ActorSettings, CityName ),

	MetroGraph = metro_parser:show( MetroFile , false ),

	setAttributes( ActorState, [
		{ graph, MetroGraph } ] ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

% The City is a passive actor. Never start spontanely an action
-spec actSpontaneous( wooper:state() ) -> const_oneway_return().
actSpontaneous( State ) ->

	wooper:const_return().

% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> const_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	wooper:const_return().

