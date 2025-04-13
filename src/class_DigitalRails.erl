%Class that represents a simple City Map
-module(class_DigitalRails).

% Determines what are the mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings , DigitalRails ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("sim_diasca_for_actors.hrl").

-spec construct( wooper:state(), class_Actor:actor_settings(), parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->
	create_digital_rails(DigitalRails),
	class_Actor:construct(State, ActorSettings, "DigitalRails").

create_digital_rails([]) -> ok;

create_digital_rails([{rail, [{name, Name}, {cycle, CycleStr}, {bandwidth, BandwidthStr}], [{links, Links}]} | Rails]) ->
	create_digital_rails_links(Name, list_to_integer(CycleStr), list_to_integer(BandwidthStr), Links),
	create_digital_rails(Rails);

create_digital_rails([_ | Rails]) ->
	create_digital_rails(Rails).

create_digital_rails_links(_, _, _, []) -> ok;
create_digital_rails_links(Name, Cycle, Bandwidth, [{link, [{origin, Origin}, {destination, Destination}], _} | Links]) ->
	Edge = list_to_atom(lists:concat([Origin, Destination])),
	ets:update_element(list_streets, Edge, {8 , {Name, 1, Cycle, Bandwidth, false, 0}}),
	create_digital_rails_links(Name, Cycle, Bandwidth, Links);

create_digital_rails_links(Name, Cycle, Bandwidth, [{link, [{origin, Origin}, {destination, Destination}, {signalized, Signalized}, {offset, Offset}], _} | Links]) ->
	Edge = list_to_atom(lists:concat([Origin, Destination])),
	ets:update_element(list_streets, Edge, {8 , {Name, 1, Cycle, Bandwidth, Signalized, Offset}}),
	create_digital_rails_links(Name, Cycle, Bandwidth, Links);

create_digital_rails_links(Name, Cycle, Bandwidth, [_ | Links]) ->
	create_digital_rails_links(Name, Cycle, Bandwidth, Links).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) -> State.

-spec actSpontaneous( wooper:state() ) -> const_oneway_return().
actSpontaneous( State ) -> wooper:const_return().

-spec onFirstDiasca( wooper:state(), pid() ) -> const_oneway_return().
onFirstDiasca( State, _SendingActorPid ) -> wooper:const_return().
