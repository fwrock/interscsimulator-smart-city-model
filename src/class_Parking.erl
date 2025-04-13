%Class that manage the parking spots in the city
-module(class_Parking).

% Determines what are the mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings , SpotName , ListOfSpots ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("sim_diasca_for_actors.hrl").

% Creates a list with the parking spots in the city
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

    case ets:info(options) of
	undefined -> ets:new(options, [public, set, named_table]);
        _ -> ok
    end,
    ets:insert(options, {parking_pid, self() }),

    AvailableParkingSpots = dict:from_list( ListOfSpots ),
    UnavailableParkingSpots = dict:new(),

    %print( ListOfSpots ),

    ActorState = class_Actor:construct( State, ActorSettings , SpotName ),

    setAttributes( ActorState, [
                                { availableSpots , AvailableParkingSpots },
                                { unavailableSpots , UnavailableParkingSpots } ] ).


%print( [] ) ->
%    ok;

%print( [ Obj | List ] ) ->

%    Uuid = element( 1 , Obj ),
%    IdNo = element( 2 , Obj ),
%    io:format("spot: ~s ~s" , [ Uuid , IdNo ] ),
%    print( List ).

% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

    State.

% The City is a passive actor. Never start spontanely an action
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

    AvailableParkingSpots = getAttribute( State, availableSpots ),
    UnavailableParkingSpots = getAttribute( State, unavailableSpots ),

    CurrentTick = class_Actor:get_current_tick_offset( State ),

    { NewAPS , NewUPS } = update_spots( dict:to_list( UnavailableParkingSpots ) , { AvailableParkingSpots , UnavailableParkingSpots } , CurrentTick ),

    NewState = setAttribute( State, unavailableSpots , NewUPS ),
    FinalState = setAttribute( NewState, availableSpots , NewAPS ),

    wooper:return_state( executeOneway( FinalState , addSpontaneousTick, CurrentTick + 600 ) ).



update_spots( [] , { AvailableParkingSpots , UnavailableParkingSpots } , _CurrentTick ) ->
   
    { AvailableParkingSpots , UnavailableParkingSpots };

update_spots( [ Spot | List ] , { AvailableParkingSpots , UnavailableParkingSpots } , CurrentTick ) ->

    Elemento = list_utils:get_element_at( element( 2 , Spot ) , 1 ),
    { NewAPS , NewUPS } = case ( element( 3 , Elemento ) - CurrentTick ) < 1200 of
        true -> { AvailableParkingSpots , UnavailableParkingSpots };
        false -> { dict:append( element( 1 , Spot ) , { element( 2 , Spot ) , element( 3 , Spot) } , AvailableParkingSpots ),
		   dict:erase( element( 1 , Spot ) , UnavailableParkingSpots ) }
    end,
    update_spots( List , { NewAPS , NewUPS } , CurrentTick ).

% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + 100 ),

	wooper:return_state( ScheduledState ).

