dist({X1, Y1}, {X2, Y2}) ->
    Dx = abs(X1-X2),
    Rx = case Dx rem 2 of
	     0 ->
		 case Y2 =< Y1 of
		     true ->
			 Y1-Y2-Dx div 2;
		     false ->
			 Y2-Y1-(Dx+1) div 2
		 end;
	     1 ->
		 case Y2 >= Y1 of
		     true -> 
			 Y2-Y1-Dx div 2;
		     false ->
			 Y1-Y2-(Dx+1) div 2
		 end
	 end,
    Dx + (case Rx > 0 of true -> Rx; false -> 0 end). 

%% if( !( x1 & 1 ) )
%%         {
%%             if( y2 <= y1 )
%%             {
%%                 int rx = y1 - y2 - dx / 2;
%%                 return dx + ( rx > 0 ? rx : 0 );
%%             }
%%             else
%%             {
%%                 int rx = y2 - y1 - ( dx + 1 ) / 2;
%%                 return dx + ( rx > 0 ? rx : 0 );
%%             }
%%         }
%%         else
%%         {
%%             if( y2 >= y1 )
%%             {
%%                 int rx = y2 - y1 - dx / 2;
%%                 return dx + ( rx > 0 ? rx : 0 );
%%             }
%%             else
%%             {
%%                 int rx = y1 - y2 - ( dx + 1 ) / 2;
%%                 return dx + ( rx > 0 ? rx : 0 );
%%             }
%%         }

