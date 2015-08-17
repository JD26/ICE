#include <iostream>
#include <armadillo>

using namespace std;
using namespace arma;



class Poly {
	private:		
		mat A;
		int n;		

	public:
		//Contructor
		Poly(mat a) {						 
			A = a;
			n = A.n_rows;			
		}
		
		//Numerical Methods
		vec FaddeevLeverrier();
		vec CHACM();
		vec Budde();
			
};

vec Poly::CHACM() {
	vec c(n);
	return c;
}

vec Poly::Budde() {
	vec c(n);
	return c;
}

vec Poly::FaddeevLeverrier() {
	mat I = eye<mat>(n,n);
	mat C = A;
	vec c(n);
	for(int i = 1; i <= (n-1); i++) {
		c(i-1) = -trace(C)/i;
	 	C = A*(C + c(i-1)*I);
	}
	c(n-1) =  -trace(C)/n;
	return c;
}