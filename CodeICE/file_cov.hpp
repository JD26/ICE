#include <iostream>
#include <armadillo>

using namespace std;
using namespace arma;

class Cov {
	private:		
		mat A;
		int n;		

	public:
		//Contructor
		Cov(mat a) {						 
			A = a;
			n = A.n_rows;			
		}
		
		//Numerical Methods
		float CovarianceOfMatrix();
		vec RowMeans();//E[a(.,i)]
		vec ColMeans();//E[a(i,.)]
		float MeanByRow(int);
		float MeanByCol(int);
		float ProductCroos(vec,vec);
		float MeanOfProductOfMeans();
		float MeanOfRowMeans();
		float MeanOfColMeans();
		float CovOfMeans();
		float MeanOfCov();
		vec VecOfCov();
		float CovOfRowsAndCols(int);
		float MeanOfRowsByCols(int);


			
};

float Cov::MeanByRow(int i) {
	float s = 0;
	for (int j = 0; j < n; ++j) {
		if(i != j) {
			s = s + A(i,j);
		}
	}
	return s/(n-1);
}

float Cov::MeanByCol(int i) {
	float s = 0;
	for (int j = 0; j < n; ++j) {
		if(i != j) {
			s = s + A(j,i);
		}
	}
	return s/(n-1);
}

vec Cov::RowMeans() {
	vec c(n);
	for (int i = 0; i < n; ++i) {		
		c(i) = MeanByRow(i);
	}
	return c;
}

vec Cov::ColMeans() {
	float s;
	vec c(n);
	for (int i = 0; i < n; ++i) {		
		c(i) = MeanByCol(i);
	}
	return c;
}


float Cov::ProductCroos(vec v, vec w) {
	float s = 0;
	for (int i = 0; i < n; ++i)
	{
		s = s + v(i)*w(i);
	}
	return s;
}

float Cov::MeanOfProductOfMeans() {
	vec v = RowMeans();
	vec w = ColMeans();
	return ProductCroos(v,w)/n;
}

float Cov::MeanOfRowMeans() {
	return mean(RowMeans());
}

float Cov::MeanOfColMeans() {
	return	mean(ColMeans());
}

float Cov::CovOfMeans() {
	return MeanOfProductOfMeans() - MeanOfRowMeans()*MeanOfColMeans();
}


float Cov::MeanOfRowsByCols(int i) {
	float s = 0;
	for (int j = 0; j < n; ++j)
	{
		s = s + A[i,j]*A[j,i];
	}
	return s/(n-1);
}

float Cov::CovOfRowsAndCols(int i) {	
	return MeanOfRowsByCols(i) - MeanByRow(i)*MeanByCol(i);
}

vec Cov::VecOfCov() {
	vec v(n);
	for (int i = 0; i < n; ++i) {
		v[i] = CovOfRowsAndCols(i);
	}
	return v;
}

float Cov::MeanOfCov() {
	return mean(VecOfCov());
}


float Cov::CovarianceOfMatrix() {
	return MeanOfCov() + CovOfMeans();
}