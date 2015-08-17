#include <iostream>
#include <armadillo>

using namespace std;
using namespace arma;

class VarAuto {
        private:		
		mat A;
		int n;

	public:
		//Contructor
		VarAuto(mat a) {						 
			A = a;
			n = A.n_rows;			
		}		               
		//Numerical Methods
		float VarOfDiag();
        float SeudoMean();                
        mat Permutations();        
        float VarOfSubmatrix(int, int);        
        //Principal Methods
        //Variance of matrix eigenvalues
        float VarianceOfAuto();
        //Variance of each submatrix eigenvalues
        mat VarOfSubmatrixMat();
        //Mean of variances
        float MeanOfVar();

};

float VarAuto::MeanOfVar() {
	mat PerVar = Permutations();
	vec variances(PerVar.n_rows);
	for (int i = 0; i < PerVar.n_rows; ++i)
	{
		variances(i) = VarOfSubmatrix(PerVar(i, 0),PerVar(i, 1)); 
	}
	return mean(variances);	
}

float VarAuto::VarianceOfAuto() {
        return VarOfDiag() + 2*SeudoMean()/n;
}

float VarAuto::VarOfDiag() {
        return var(diagvec(A),1);
}

float VarAuto::SeudoMean() {
        float s = 0;
        for(int i = 0; i < (n-1); ++i) {
                for(int j = i + 1; j < n; ++j) {                        
                        s = s + A(i,j)*A(j,i);                    
                }
        }
        return s;
}

mat VarAuto::Permutations() {
		int ix = -1;
		mat P(n*(n-1)/2,3);		
        for (int i = 0; i < n-1; ++i)
        {
        	for (int j = i + 1; j < n; ++j)
        	{
        		ix ++;        		
        		P(ix, 0) = i;
        		P(ix, 1) = j;
        	}
        }
        return P;
}

mat VarAuto::VarOfSubmatrixMat() {
	mat PerVar = Permutations();
	for (int i = 0; i < PerVar.n_rows; ++i)
	{
		PerVar(i, 2) = VarOfSubmatrix(PerVar(i, 0),PerVar(i, 1)); 
	}
	return PerVar;
}

float VarAuto::VarOfSubmatrix(int i, int j) {	
	return  (pow(A(i,i),2) + pow(A(j,j),2))/2 -pow(A(i,i) + A(j,j),2)/4 + A(i,j)*A(j,i);	
}

/*
int main(int argc, char const *argv[])
{
	mat data = "5 -2 3;-2 2 2.44949;-1  2.44949 1";
	VarAuto varauto(data);
	cout<<varauto.VarianceOfAuto()<<endl;
	cout<<varauto.VarOfSubmatrixMat()<<endl;
	cout<<varauto.MeanOfVar()<<endl;
	//2000-jorgensen paper
	return 0;
}
*/
