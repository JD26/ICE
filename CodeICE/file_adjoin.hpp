#include <iostream>
#include <armadillo>

using namespace std;
using namespace arma;

//http://stackoverflow.com/questions/16757100/get-adjoint-matrix-in-r


class Adjoin {
		private:
			mat A;
			int n;
		
		public:
			Adjoin(mat a) {						 
				A = a;
				n = A.n_rows;			
			}
			mat AdjoinOfMatrix();
			float Cofactor(int,int);
			float Minor(int,int);					
};

float Adjoin::Minor(int i, int j) {
	mat a = A;
	a.shed_rows(i, i);
	a.shed_cols(j, j);	
	return det(a);
}

float Adjoin::Cofactor(int i, int j) {
	return pow(-1,i+j) * Minor(i,j);
}

mat Adjoin::AdjoinOfMatrix() {
	mat Ad(n,n);
	for (int i = 0; i < n; ++i) {
		for (int j = 0; j <n; ++j) {
			Ad(j,i) = Cofactor(i, j);
		}
	}
	return Ad;
}
