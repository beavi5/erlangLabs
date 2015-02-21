
-module(matt).
-export([fac/1,fib/1,per/1,area/1,memberof/2]).
fac(0)->1;
fac(N)->N*fac(N-1).
fib(0)->0;
fib(1)->1;
fib(N)->fib(N-1)+fib(N-2).





per(Fig)-> case  Fig of  

	{rect, A, B}  -> 2*(A+B);
	{square, A}  -> A*4;
	{circle, A}  -> 2*A*3.14
end.






area(Fig)-> case  Fig of  

	{rect, A, B}  -> A*B;
	{square, A}  -> A*A;
	{circle, A}  -> 3.14*A*A
end.

memberof(O,[])->false;

memberof(O,[H|T]) -> 


case H of
	[] -> false;

	O->true;
	_-> case T of
		[]-> false;
		_->memberof(O,T)
		end
end.
