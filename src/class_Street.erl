%Class that represents a simple City Map
-module(class_Street).

% Determines what are the mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings , StreetName , ListEdges , LogName , Paths ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("sim_diasca_for_actors.hrl").


% Creates a new city graph
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , sensor_type() , parameter() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	case ets:info(list_streets) of
		undefined -> ets:new(list_streets, [public, set, named_table]);
                _ -> ok
		end,
		
	case ets:info(list_streets_dr) of
		undefined -> ets:new(list_streets_dr, [public, set, named_table]);
                _ -> ok
		end,
		
	case ets:info(drs_streets) of
		undefined -> ets:new(drs_streets, [public, set, named_table]);
                _ -> ok
        end,

	case ets:info(waiting_bus) of
		undefined -> ets:new(waiting_bus, [public, bag, named_table]);
                _ -> ok
        end,

	iterate_list( ListEdges ),

	create_option_table( LogName , Paths ),

%	InitFile = ets:lookup_element(options, log_file, 2 ),
%	file_utils:write( InitFile, "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<events version=\"1.0\">\n" ),

	class_Actor:construct( State, ActorSettings, StreetName ).

create_option_table( LogName , Paths ) ->

	filelib:ensure_dir( LogName ),
	InitFile = file_utils:open( LogName , _Opts=[ write , delayed_write ] ),

	case ets:info(options) of
		undefined -> ets:new(options, [public, set, named_table]);
                _ -> ok
        end,

	ets:insert(options, {log_file, InitFile }),

        code:add_pathsa( Paths ).

%	{ ok, Connection } = amqp_connection:start( #amqp_params_network{} ),
%	{ ok, Channel } = amqp_connection:open_channel( Connection ),

%	Exchange = #'exchange.declare'{ exchange = <<"simulator_exchange">>,
  %                                  type = <<"topic">> },
%	#'exchange.declare_ok'{} = amqp_channel:call( Channel, Exchange ),

%	Publish = #'basic.publish'{ exchange = <<"simulator_exchange">>,
 %                               routing_key = <<"log_output">> },

%	amqp_channel:cast( Channel,
%					   Publish,
%					   #amqp_msg{ payload = <<"<events version=\"1.0\">\n">> }),


%	ets:insert(options, {rabbitmq_channel, Channel }),
%	ets:insert(options, {rabbitmq_connection, Connection }),
%	ets:insert(options, {rabbitmq_publish, Publish }),
%	ets:insert(options, {rabbitmq_exchange, Exchange }).


iterate_list([]) -> ok;
iterate_list([ Element | List ]) ->
	
	Vertices = element( 1, Element),
	{ Id , Length , _ , Freespeed , Count, Lanes, {}, IsCycleway, IsCyclelane, Inclination } = element(2, Element),

	% CellSize = 7.5, % Cell size of 7.5m according to MATSim user guide
	CellSize = 5 * 7.5, % Previously capacity was given in "cars",
                      % so we used CellSzie = 7.5.
									    % Now the unit is a bike (1/5 of a car),
											% so the capacity will tell "how many bikes fit in the link".
										  % Obs: 7.5 is the car cell size.
	CellSizeDR = 4.0,
 
	ets:insert(drs_streets, { Vertices , 0 }),
	
	CountOnlyBikes = 0,
	case Lanes == 1 of
		true ->
			StorageCapacity = math:ceil((Lanes) * Length / CellSize),
			LinkData = {Vertices,  Id , Length , StorageCapacity , Freespeed , Count, Lanes, {}, IsCycleway, IsCyclelane, Inclination, CountOnlyBikes },
			ets:insert(list_streets, LinkData),
	
			StorageCapacityDR = math:ceil(1 * Length / CellSizeDR ),
			ets:insert(list_streets_dr, {Vertices,  Id , Length , StorageCapacityDR , Freespeed , Count, Lanes, {}, IsCycleway, IsCyclelane, Inclination, CountOnlyBikes });
		false ->
			StorageCapacity = math:ceil((Lanes - 1) * Length / CellSize),
			LinkData = {Vertices,  Id , Length , StorageCapacity , Freespeed , Count, Lanes, {}, IsCycleway, IsCyclelane, Inclination, CountOnlyBikes },
			ets:insert(list_streets, LinkData),

			StorageCapacityDR = math:ceil(1 * Length / CellSizeDR ),
			ets:insert(list_streets_dr, {Vertices,  Id , Length , StorageCapacityDR , Freespeed , Count, Lanes, {}, IsCycleway, IsCyclelane, Inclination, CountOnlyBikes })
	end,

	iterate_list( List ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%Connection = ?getAttr(connection),
	%Channel = ?getAttr(channel),
	%Publish = ?getAttr(publish),
	%Exchange = ?getAttr(exchange),

	%#'exchange.declare_ok'{} = amqp_channel:call( Channel, Exchange ),

	%amqp_channel:cast( Channel,
         %              Publish,
          %             #amqp_msg{ payload = <<"</events>">> } ),

	%ok = amqp_channel:close(Channel),
	%ok = amqp_connection:close(Connection),

%	InitFile = ets:lookup_element(options, log_file, 2 ),
%	file_utils:write( InitFile, "</events>" ),
%	file_utils:close( InitFile ),

	State.

-spec actSpontaneous( wooper:state() ) -> const_oneway_return().
actSpontaneous( State ) ->

	wooper:const_return().

-spec onFirstDiasca( wooper:state(), pid() ) -> const_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	wooper:const_return().
