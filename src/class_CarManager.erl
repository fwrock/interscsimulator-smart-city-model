%Class that represents a person that can moves around the city graph on foot or by car
-module(class_CarManager).

% Determines what are the mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName , CarList ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("sim_diasca_for_actors.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->
	ActorState = class_Actor:construct( State, ActorSettings, CarName ),
        DictCars = create_dict( dict:new() , CarList ),
	setAttributes( ActorState, [ { car_list, DictCars }] ).

create_dict( Dict , [] ) -> Dict;
create_dict( Dict , [ Car | CarList ] ) ->
	{ Key , Value } = Car,
	Element = dict:find( Key , Dict ),
	
	NewDict = case Element of 
		error -> dict:store( Key , Value , Dict );
		{ ok , ListCar } -> dict:store( Key , ListCar ++ Value , Dict )
	end,
	create_dict( NewDict , CarList ).


-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentTick = class_Actor:get_current_tick_offset( State ),

	DictCars = getAttribute( State , car_list ),
	Cars = dict:find( CurrentTick , DictCars ),

	NewState = case Cars of
		error -> State;
		{ ok , List } -> init_cars( List ,  State )
	end,

	wooper:return_state (executeOneway( NewState , addSpontaneousTick , CurrentTick + 1 ) ).
	
init_cars( [] , State ) -> State;
init_cars( [ Car | Cars ] , State ) ->

	{ CarName , ListTripsFinal , Type, Park , Mode , Count, DigitalRailsCapable } = Car,

	NewState = case Mode of
		car ->
			create_person_car( Count , State , CarName , ListTripsFinal , Type , Park , Mode, DigitalRailsCapable );
		walk ->	
			create_person_car( Count , State , CarName , ListTripsFinal , Type , Park , Mode, DigitalRailsCapable );
		platoon ->
			create_person_car( Count , State , CarName , ListTripsFinal , Type , Park , Mode, DigitalRailsCapable );
		bike ->
			create_person_bike( Count , State , CarName , ListTripsFinal , Type , Park , Mode, DigitalRailsCapable );
		_ ->
			create_person_public( Count , State , CarName , ListTripsFinal , Type , Mode, DigitalRailsCapable )
	end,
	init_cars( Cars , NewState ).


create_person_car( 0 , State , _CarName , _ListTripsFinal , _Type , _Park , _Mode, _DigitalRailsCapable ) -> State;
create_person_car( Count , State , CarName , ListTripsFinal , Type , Park , Mode, DigitalRailsCapable ) ->
	CarFinalName = io_lib:format( "~s_~B", [ CarName , Count ] ),
	% StartTime = class_RandomManager:get_uniform_value( 1200 ),
	% TODO: Should be > 0. Why?
	StartTime = 1,

	NewState = class_Actor:create_actor( class_Car,
		[ CarFinalName , ListTripsFinal , StartTime , Type , Park , Mode, DigitalRailsCapable ] , State ),

	create_person_car( Count - 1 , NewState , CarName , ListTripsFinal , Type , Park , Mode, DigitalRailsCapable ).


create_person_public( _Count = 0 , State , _CarName , _ListTripsFinal , _Type , _Mode, _DigitalRailsCapable ) -> State;
create_person_public( Count , State , CarName , ListTripsFinal , Type , Mode, DigitalRailsCapable ) ->
	CarFinalName = io_lib:format( "~s_~B", [ CarName , Count ] ),
	% StartTime = class_RandomManager:get_uniform_value( 1200 ),
	% TODO: Should be > 0. Why?
	StartTime = 1,

	NewState = class_Actor:create_actor( class_Person,
		[ CarFinalName , ListTripsFinal , StartTime , Type , Mode, DigitalRailsCapable ]  , State ),

	create_person_public( Count - 1 , NewState , CarName , ListTripsFinal , Type , Mode, DigitalRailsCapable ).


create_person_bike( 0 , State , _CarName , _ListTripsFinal , _Type , _Park , _Mode, _DigitalRailsCapable ) -> State;
create_person_bike( Count , State , CarName , ListTripsFinal , Type , Park , Mode, DigitalRailsCapable ) ->
	CarFinalName = io_lib:format( "~s_~B", [ CarName , Count ] ),
	StartTime = class_RandomManager:get_uniform_value( 1200 ),

	NewState = class_Actor:create_actor( class_Bike,
		[ CarFinalName , ListTripsFinal , StartTime , Type , Park , Mode, DigitalRailsCapable ] , State ),

	create_person_bike( Count - 1 , NewState , CarName , ListTripsFinal , Type , Park , Mode, DigitalRailsCapable ).


-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->
	FirstActionTime = class_Actor:get_current_tick_offset( State ) + 1,
	wooper:return_state (executeOneway( State , addSpontaneousTick , FirstActionTime ) ).
