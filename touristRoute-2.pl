% tourist_route(+Start, ?Destination, ?Route)

tourist_route(Start, Destination, Route):-
    initStates(Start, Destination, Goals),  %init the goals.
    bfs_a(Goals, R),
    reverse(R, Route).

bfs_a([state(D,P,_,D)|_],P).

bfs_a([state(S,P1,VS,D)|Ss],P) :-
    findall(state(NS,[go(S,NS,L)|P1],[S|VS],D),
        (canGo(S,NS,go(S,NS,L)), \+ member(NS,VS), \+ sameLine(L,P1)),
        NSs),
    append(Ss,NSs,CS),
    bfs_a(CS,P).

initStates(X,Y,Gs) :-
    findall(state(S,[],[],D),(near(X,S),near(Y,D)),Gs).

%avoid unclosed list.
sameLine(L,[go(_,_,L)|_]).

%list all the stations can be directly arrived in station X.
canArrive(X,L,A) :-
    line(L,Ls), findall(S,(member(X,Ls),member(S,Ls),X \= S),Ss), sort(Ss,A).

canGo(X,Y,go(X,Y,L)) :-
    canArrive(X,L,A), member(Y,A).






%subway stations.
%bakerloo
line(bakerloo, [edgware_road,marylebone,baker_street,regents_park,oxford_circus,piccadilly_circus,charing_cross,waterloo]).

%central
line(central, [notting_hill_gate,queensway,lancaster_gate,marble_arch,bond_street,oxford_circus,tottenham_court_road,holborn,chancery_lane,st_pauls,bank,liverpool_street]).
%circle
line(circle, [edgware_road,bayswater,notting_hill_gate,high_street_kensington,gloucester_road,south_kensington,sloane_square,victoria,st_james_park,westminster,embankment,temple,blackfriars,mansion_house,cannon_street,monument,tower_hill,aldgate,liverpool_street,moorgate,barbican,farringdon,kings_cross,euston_square,great_portland_street,baker_street,edgware_road]).

%district
line(district, [edgware_road,bayswater,notting_hill_gate,high_street_kensington,earls_court,gloucester_road,south_kensington,sloane_square,victoria,st_james_park,westminster,embankment,temple,blackfriars,mansion_house,cannon_street,monument,tower_hill,aldgate_east]).

%hammersmith_and_city
line(hammersmith_and_city, [edgware_road,baker_street,great_portland_street,euston_square,kings_cross,farringdon,barbican,moorgate,liverpool_street,aldgate_east]).

%jubilee
line(jubilee, [baker_street,bond_street,green_park,west_kensington,waterloo]).

%metropolitan
line(metropolitan, [baker_street,great_portland_street,euston_square,kings_cross,farringdon,barbican,moorgate,liverpool_street,aldgate]).

%northern
line(northern, [waterloo,charing_cross,leicester_square,tottenham_court_road,goodge_street,warren_street,euston,mornington_crescent,kings_cross,angel,old_street,moorgate,bank,monument,london_bridge,borough,elephant_and_castle]).

%piccadilly
line(piccadilly, [earls_court,south_kensington,knightsbridge,hyde_park_corner,green_park,piccadilly_circus,leicester_square,covent_garden,holborn,russell_square,kings_cross]).

%victoria
line(victoria, [victoria,green_park,oxford_circus,warren_street,kings_cross]).

%walk
line(walk,[bank,monument]).


%nearBy fact.
near(british_museum,tottenham_court_road).
near(british_museum,holborn).
near(british_museum,goodge_street).
near(british_museum,russell_square).

near(national_gallery,leicester_square).
near(national_gallery,charing_cross).
near(national_gallery,piccadilly_circus).

near(natural_history_museum,gloucester_road).
near(natural_history_museum,south_kensington).
near(natural_history_museum,high_street_kensington).
near(natural_history_museum,knightsbridge).

near(tate_modern,blackfriars).
near(tate_modern,mansion_house).
near(tate_modern,london_bridge).
near(tate_modern,southwark).

near(london_eye,westminster).
near(london_eye,embankment).
near(london_eye,waterloo).

near(tower,tower_hill).
near(tower,monument).
near(tower,london_bridge).

near(sherlock_holmes,baker_street).
near(sherlock_holmes,regents_park).
near(sherlock_holmes,marylebone).

near(buckingham_palace,st_james_park).
near(buckingham_palace,green_park).

near(westminster_abbey,westminster).
near(westminster_abbey,st_james_park).

near(st_pauls_cathedral,st_pauls).
near(st_pauls_cathedral,mansion_house).

near(parliament,westminster).
near(parliament,st_james_park).
near(parliament,embankment).

near(british_library,euston_square).
near(british_library,warren_street).


%test case.
test_route(1, british_museum, westminster_abbey, [go(tottenham_court_road, notting_hill_gate, central), go(notting_hill_gate, westminster, circle)]).
test_route(2, tate_modern, buckingham_palace, [go(blackfriars, st_james_park, circle)]).
test_route(3, national_gallery, tower, [go(leicester_square, kings_cross, northern), go(kings_cross, tower_hill, circle)]).
test_route(4, natural_history_museum, british_library, [go(gloucester_road, kings_cross, circle), go(kings_cross, warren_street, northern)]).