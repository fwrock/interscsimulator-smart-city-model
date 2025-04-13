%Class that represents a person that can moves around the city graph on foot or by car
-module(class_Person).

% Determines what are the mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName , ListTripsFinal , StartTime , Type , Mode, TrafficModel ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("sim_diasca_for_actors.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter(), parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, CarName ),

	setAttributes( ActorState, [
		{ car_name, CarName },
		{ trips , ListTripsFinal },
		{ type, Type },
		{ distance , 0 },
		{ car_position, -1 },
		{ start_time , StartTime },
		{ path , ok },
		{ mode , Mode },
		{ pt_status , start }, %public transport -> bus or metro
		{ traffic_model, TrafficModel}
						] ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->
	
	Trips = getAttribute( State , trips ), 
	
	case length( Trips ) > 0 of

		false ->

			Path = getAttribute( State , path ), 

			case Path of 

				finish -> 
					
					wooper:return_state( executeOneway( State , declareTermination ) );

				_ ->

					NewState = setAttribute( State , path , finish ),

					Type = getAttribute( NewState , type ),
						
					TotalLength = getAttribute( NewState , distance ),

					StartTime = getAttribute( NewState , start_time ),

					CarId = getAttribute( NewState , car_name ),	

					CurrentTickOffset = class_Actor:get_current_tick_offset( NewState ), 

					LastPosition = getAttribute( NewState , car_position ),

					Mode = getAttribute( NewState , mode ), 

					print:write_final_message( Type , TotalLength , StartTime , CarId , CurrentTickOffset , LastPosition , Mode , csv ),

					wooper:return_state( executeOneway( NewState, scheduleNextSpontaneousTick ) )

				end;


		true ->

			CurrentTrip = list_utils:get_element_at( Trips , 1 ),

			Mode = element( 1 , CurrentTrip ),			

			case Mode of 

				"walk" ->
	
					NewState = request_position( State , CurrentTrip  ),
					wooper:return_state( NewState );

				"bus" ->


					PtStatus = getAttribute( State , pt_status ), 

					case PtStatus of 
	
	
						finish ->

							NewTrips = list_utils:remove_element_at( Trips , 1 ),

							NewState = setAttributes( State , [ {trips , NewTrips } , {  pt_status , start } ] ),					

							wooper:return_state( executeOneway( NewState , scheduleNextSpontaneousTick ) );

						start ->								
							
							{ _ , Origin , Destination , Line , _ , _ } = CurrentTrip,
							ets:insert(waiting_bus, { list_to_atom( Origin ) , Line , Destination , self() } ),

							wooper:const_return()

					end;


				"metro" ->
			
					PtStatus = getAttribute( State , pt_status ), 

					case PtStatus of 
	
	
						finish ->
							
							NewTrips = list_utils:remove_element_at( Trips , 1 ),

							NewState = setAttributes( State , [ {trips , NewTrips } , {  pt_status , start } ] ),					
							wooper:return_state( executeOneway( NewState , scheduleNextSpontaneousTick ) );

						start ->
	
							NewState = request_position_metro( State , CurrentTrip ),

							wooper:return_state( NewState )

					end

			end


	end.
			
-spec request_position_metro( wooper:state() , parameter() ) -> wooper:state().
request_position_metro( State , Trip ) -> 

	{ _ , Origin , _ , Destination , _ } = Trip,

	class_Actor:send_actor_message( ets:lookup_element(options, metro_pid, 2 ) ,
		{ getTravelTime, { Origin , Destination } }, State ).

-spec request_position( wooper:state() , parameter() ) -> wooper:state().
request_position( State , Trip ) ->
	
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 	

	PathTest = getAttribute( State , path ),

	PathState = case PathTest of

		ok -> 
			
			PathTrip = element( 5 , Trip ),
			setAttribute( State, path, PathTrip );

		_ ->

			State

	end,

			
	Path = getAttribute( PathState , path ),

	case Path of 

		finish ->

			Trips = getAttribute( State , trips ), 
			
			NewTrips = list_utils:remove_element_at( Trips , 1 ),

			NewState = setAttributes( State , [ { trips , NewTrips } , { path, ok } ] ),

			executeOneway( NewState , addSpontaneousTick , CurrentTickOffset + 1 );	
	
		false ->

			State;

		_ ->

			case length( Path ) > 1 of

				true ->	

					% get the current and the next vertex in the path	
					{ InitialVertice , FinalVertice } = { lists:nth( 1 , Path ), lists:nth( 2 , Path ) },
					
					Vertices = list_to_atom( lists:concat( [ InitialVertice , FinalVertice ] )),

					FinalState = setAttribute( PathState , path, list_utils:remove_element_at( Path , 1 ) ), % remove the current element of the path

					
					Data = lists:nth( 1, ets:lookup( list_streets , Vertices ) ),
					StreetData = traffic_models:get_speed_walk( Data, getAttribute( PathState, traffic_model) ),
                        		go( FinalState , StreetData );

				false ->							

					LastPosition = getAttribute( PathState , car_position ),

					case LastPosition == -1 of

						true ->
							
							executeOneway( PathState , declareTermination );	

						false ->	

						
							FinalState = setAttribute( PathState, path, finish ),

							executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + 1 )
						
					end

			end

	end.

-spec go( wooper:state(), car_position() ) -> class_Actor:actor_oneway_return().
go( State, PositionTime ) ->

	TotalTime = class_Actor:get_current_tick_offset( State ) + element( 2 , PositionTime ), % CurrentTime + Time to pass the link

	% Calculate the total distance that the person moved until now.
	TotalLength = getAttribute( State , distance ) + element( 3 , PositionTime),
	LengthState = setAttributes( State,  [ { distance , TotalLength } , { car_position , element( 1 , PositionTime ) } ] ),
		
	LastPosition = getAttribute( State , car_position ),
	NewPosition = getAttribute( LengthState , car_position ),

	Trips = getAttribute( LengthState , trips ), 

	CurrentTrip = list_utils:get_element_at( Trips , 1 ),

	CurrentTickOffset = class_Actor:get_current_tick_offset( LengthState ), 	
	CarId = getAttribute( LengthState , car_name ),
  	Type = getAttribute( LengthState , type ),

	case LastPosition == -1 of

		false ->
				
			print:write_movement_car_message( CarId , LastPosition , Type , CurrentTickOffset , NewPosition , csv  );
 

		true -> 
				
			LinkOrigin = element( 3 , CurrentTrip ), 

			print:write_initial_message( CarId , Type , CurrentTickOffset , LinkOrigin , LastPosition , csv )

	end,

	executeOneway( LengthState , addSpontaneousTick , TotalTime ).

% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->
	StartTime = getAttribute( State , start_time ),
    	FirstActionTime = class_Actor:get_current_tick_offset( State ) + StartTime,   	
	NewState = setAttribute( State , start_time , FirstActionTime ),
	wooper:return_state( executeOneway( NewState , addSpontaneousTick , FirstActionTime ) ).
