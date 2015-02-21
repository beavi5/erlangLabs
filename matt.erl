
-module(matt).
-export([fac/1,fib/1,per/1,area/1,memberof/2, price/1,summa/1]).
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



memberof(O,[])->fals;

memberof(O,[H|T]) -> 

case H of
	[] -> false;

	O->true;
	_-> case T of
		[]-> false;
		_->memberof(O,T)
		end
end.



price('pivo') ->50;
price('riba') ->15;
price('myaso') ->100;
price('hleb') ->20.

summa([])->0;

summa([{X,Y}|T]) -> 

case [{X,Y}] of
	[] -> 0;
	_-> price(X)*Y+ summa(T)
		
end.