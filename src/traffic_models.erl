-module(traffic_models).

-export([get_speed_car/2, get_speed_walk/2, get_speed_bike/8, get_personal_bike_speed/0]).

% Occupation: the count of vehicles in the link; one unit corresponds to one car; 
%             one bike counts 1/5 of occupation for mixed traffic and
%                             1/2.5 for cicleways and cyclelanes.
%             It is the "Count" of the link.
% There is DR in link and car can use it:
get_speed_car({_, Id, Length, RawCapacity, Freespeed, Occupation, Lanes, {_DRName, _DigitalRailsLanes, _Cycle, _Bandwidth, _Signalized, _Offset}, IsCycleway, IsCyclelane, _Inclination, OccupationOnlyBikes}, true) ->
	link_density_speed(Id, Length, RawCapacity, Occupation, OccupationOnlyBikes, Freespeed, Lanes, IsCycleway, IsCyclelane);

% There is DR but not effective:
get_speed_car({Whatever, Id, Length, RawCapacity, Freespeed, Occupation, Lanes, {_DRName, _DigitalRailsLanes, _Cycle, _Bandwidth, _Signalized, _Offset}, _IsCycleway, _IsCyclelane, _Inclination, OccupationOnlyBikes}, noeffect) ->
	get_speed_car({Whatever, Id, Length, RawCapacity, Freespeed, Occupation, OccupationOnlyBikes, Lanes, {}}, noeffect);

% There is DR but car cannot use it:
get_speed_car({_, Id, Length, RawCapacity, Freespeed, Occupation, Lanes, {_DRName, _DigitalRailsLanes, _Cycle, _Bandwidth, _Signalized, _Offset}, IsCycleway, IsCyclelane, _Inclination, OccupationOnlyBikes}, false) ->
	link_density_speed(Id, Length, RawCapacity, Occupation, OccupationOnlyBikes, Freespeed, Lanes, IsCycleway, IsCyclelane);

% There is no DR:
get_speed_car({_, Id, Length, RawCapacity, Freespeed, Occupation, Lanes, {}, IsCycleway, IsCyclelane, _Inclination, OccupationOnlyBikes}, _) ->
	link_density_speed(Id, Length, RawCapacity, Occupation, OccupationOnlyBikes, Freespeed, Lanes, IsCycleway, IsCyclelane).

link_density_speed(Id, Length, Capacity, Occupation, OccupationOnlyBikes, Freespeed, _Lanes, IsCycleway, IsCyclelane) ->

    ConsideredOccupation =
        if 
            IsCycleway or IsCyclelane ->
                Occupation - OccupationOnlyBikes;
            true ->
                Occupation
        end,

	Alpha = 1,
	Beta = 1,
	Speed = case ConsideredOccupation >= Capacity of
		true -> 1.0;
		false -> Freespeed * math:pow(1 - math:pow((ConsideredOccupation / Capacity), Beta), Alpha)
	end,

	Time = (Length / Speed) + 1,
	{Id, round(Time), round(Length)}.




get_speed_walk(LinkData, _) ->
	{_, Id, Length, _, _, _} = LinkData,	
	Time = ( Length / 2 ),

	{Id, ceil(Time), round(Length)}.



%%%%%%%%%%%%%%% BIKES %%%%%%%%%%%%%%%%%%%%%%%

get_personal_bike_speed() ->
    % The sampled speed is retrieved from a previously generated list following a distribution observed in the OD (Pesquisa OrigemDestino) data.
    SampledSpeed = get_next_value_from_speeds_distribution(),
    % What matters in this speeds list is the distribution (a generalized gama, as told us by the fit test done in R).
    % But the speeds in our OD dataset are lower than actual speeds, because we used as distance the euclidean distance from origin to destination - a straight line linking origin and destination. 
    % The mean speed for the OD dataset is 7.5km/h.
    % We considered the Bike Sampa dataset to me more accurate. In this Bike Sampa dataset the mean speed mixed traffic is 10km/h.
    % Therefore, we apply a factor over the sampled speed to correct the offset of the speed distribution.
    FactorOdToBikeSampa = 10/7.44,
    SampledSpeed * FactorOdToBikeSampa.

get_next_value_from_speeds_distribution() ->

    % First, we open the file if it's the first time this function is invoked
    case lists:member(table_personal_speeds, ets:all()) of
        false ->
            ets:new(table_personal_speeds, [named_table, protected, set, {keypos, 1}]),
            Filename = "/home/coccafukuda/research/Sim-Diasca-2.3.11/mock-simulators/smart_city_model/src/personal_speed_distribution_for_bikes.csv",
            {Ok, File} = file:open(Filename, [read]),
            if
                Ok == error -> % esse caso é pra funcionar para os testes de unidade
                    Filename2 = "personal_speed_distribution_for_bikes.csv",
                    {_Ok, File2} = file:open(Filename2, [read]),
                    ets:insert(table_personal_speeds, {file, File2});
                true -> % esse caso é pra funcionar no Docker
                    ets:insert(table_personal_speeds, {file, File})
                   % we keep the file in the EST, so we keep implicitly together the pointer to the last read line.
            end;
        _ -> nothing_to_do
    end,

    [{_, SpeedsFile}] = ets:lookup(table_personal_speeds, file),
    Line = io:get_line(SpeedsFile, ''), % read next line of the file
    % Obs: if we got at the end of file, the system will crash! But we hope this to not happen!
    {SpeedKmh, _} = string:to_float(Line),
    SpeedKmh.

% PersonalSpeed: different people have different speeds; each agent must hold a personal speed generated once for the actor. The personal speed must be generated using the function get_personal_bike_speed.
% Length: length of the link (in meters)
% Capacity: how many vehicles the link supports (one bike occupies one unit)
% Occupation: the count of vehicles in the link; one unit corresponds to one car; 
%             one bike counts 1/5 of occupation for mixed traffic and
%                             1/2.5 for cicleways and cyclelanes.
% OccupationOnlyBikes: the number of bikes in the link
% IsCycleway: boolean
% IsCyclelane: boolean
% Inclination: (altitude_to - altitude_from) / length 
get_speed_bike(PersonalSpeed, Length, Capacity, Occupation, OccupationOnlyBikes, IsCycleway, IsCyclelane, Inclination) ->
    Freespeed = get_free_speed_for_bike(PersonalSpeed, IsCycleway, IsCyclelane, Inclination),
    Speed = speed_for_bike_considering_traffic(Freespeed, Length, Capacity, Occupation, OccupationOnlyBikes, IsCycleway, IsCyclelane),
    Speed.



get_free_speed_for_bike(PersonalSpeed, IsCycleway, IsCyclelane, Inclination) ->

    Climb = Inclination > 0.02,
    Descent = Inclination < -0.02,
    IsMixedTraffic = (not IsCycleway) and (not IsCyclelane),
    Plane = (not Climb) and (not Descent),

    % The factors were defined based on analysis of real data from Bike Sampa
    if 
        Plane and IsCycleway ->
            PersonalSpeed * 1.4651;
        Plane and IsCyclelane ->
            PersonalSpeed * 0.5524;
        Plane and IsMixedTraffic ->
            PersonalSpeed * 1.5;
        Climb and (IsCycleway or IsCyclelane) ->
            PersonalSpeed * 0.3827;
        Climb and IsMixedTraffic ->
            PersonalSpeed * 0.8027;
        Descent and (IsCycleway or IsCyclelane) ->
            PersonalSpeed * 1.4532;
        Descent and IsMixedTraffic ->
            PersonalSpeed * 1.1217;
        true ->
            erlang:error("It should never happen, unexpected combination of arguments when calculating bike speed: IsCycleway=" ++ IsCycleway ++ ", IsCyclelane=" ++ IsCyclelane ++ ", Inclination=" ++ Inclination)
    end.
   







speed_for_bike_considering_traffic(BaseSpeed, Length, Capacity, Occupation, OccupationOnlyBikes, IsCycleway, IsCyclelane) ->
    BaseSpeed,
    SaturatedLink = if 
        IsCycleway or IsCyclelane -> 
            BikeLength = 1.72, % according to Google
            SafetySpace = 2 * 1/4 * BikeLength, % front and back
            CicleLinkCapacity = Length / (BikeLength + SafetySpace),
            OccupationOnlyBikes >= CicleLinkCapacity;
        true ->
            Occupation >= Capacity
    end,

	Alpha = 1,
	Beta = 1,
    if
        SaturatedLink ->
             5.0;
        true -> % (não saturado) ou (saturado em tráfego misto)
            BaseSpeed * math:pow(1 - math:pow((Occupation / Capacity), Beta), Alpha)
	end.

