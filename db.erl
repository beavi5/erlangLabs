
-module(db).
-export([new/0,destroy/1, write/3, delete/2, read/2, match/2]).

new()-> [].



destroy(Db)->ok.

read(Key,[{Key,Value}|T])-> Value;
read(Key,[H|T])->read(Key,T).


write(Key,Value,[])-> [{Key,Value}];

write(Key,Value,[{K,V}|T])-> 
case K of 
	Key->[{Key,Value}]++T;
	_-> [{K,V}]++write(Key,Value,T)

end.

delete(Key,[])->[];

delete(Key,[{K,V}|T])-> 
case K of
	Key  -> []++delete(Key,T);
	_->[{K,V}]++delete(Key,T)
		
end.

match(Value,[])->[];

match(Value, [{K,V}|T])->
case V of
	Value -> [K]++match(Value,T);
	_-> match(Value,T)
end.







% функция создания бд db:new()
% уничтожения db:destroy(Db)->ok
% записывает в бд db:write(Key, Value, Db)
% db:delete(key, Db)
% db:read(key,Db)
% db:match (value, Db)


