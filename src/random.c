#define IM1 2147483563
#define IM2 2147483399
#define AM (1.0/IM1)
#define IMM1 (IM1-1)
#define IA1 40014
#define IA2 40692
#define IQ1 53668
#define IQ2 52774
#define IR1 12211
#define IR2 3791
#define NTAB 32
#define NDIV (1+IMM1/NTAB)
#define EPS 1.2e-7
#define RNMX (1.0-EPS)

#include "stdio.h"
#include "stdlib.h"
#include "math.h"
#include "mt.h"
/*Long period(>2x10^18) random number generator of L'Ecuyer with Bayes_Durham
Shuffle and added safeguards. Retruns a uniform random deviate between 0.0 
and 1.0(exclusive of the endpoint values). Call with idum a negative integer 
to initialize; hereafter, do not alter idum between successive deviates in 
sequence. RNMX should approximate the larges floating value that is less 
than 1.*/
/*copy from the book of numerical recipes in C pp282(1992), 2nd edition*/
static long dum=-100;
static float ran2(long *idum)
{
  int j;
  long k;
  static long idum2=123456789;
  static long iy=0;
  static long iv[NTAB];
  float temp;
  
  if(*idum<=0){/*initialize*/
    if(-(*idum)<1)  *idum=1;/*be sure to prevent idum=0*/
    else *idum=-(*idum);
    idum2=(*idum);
    for(j=NTAB+7;j>=0;j--){/*Load the shuffle table(after 8 warm-ups)*/
      k=(*idum)/IQ1;
      *idum=IA1*(*idum-k*IQ1)-k*IR1;
      if(*idum<0)*idum+=IM1;
      if(j<NTAB) iv[j]=*idum;
    }
    iy=iv[0];
  }
  k=(*idum)/IQ1;
  *idum=IA1*(*idum-k*IQ1)-k*IR1;
  if(*idum<0) *idum+=IM1;
  k=idum2/IQ2;
  idum2=IA2*(idum2-k*IQ2)-k*IR2;
  if(idum2<0) *idum+=IM2;
  j=iy/NDIV;
  iy=iv[j]-idum2;
  iv[j]=*idum;
  if(iy<1) iy+=IMM1;
  if((temp=AM*iy)>RNMX) return RNMX;
  else return temp;
}
void set_seed(long int seed){
  dum=-abs(seed); /*using the negative values*/
  ran2(&dum);/*initilization*/
}
float get_rand(){
  return ran2(&dum);
}
/*get the n samples from the n-dim vector V. the results are stored 
in the first m member of vector V*/
void sample(int *V, int n, int m)
{
  int i,j,temp;
  float f;
  for(i=0;i<m;i++){
    /* no need to worry yet    if(i==(n-1)) continue;/*no need to swap with the last elements*/
    j=n;
    while (j==n){/*skip the border, we only want random
		    numbers from i,i+1,i+2,...,n-1*/
      f=get_rand()*(n-i);
      j=i+floor(f);
    }
    /*swap the nubmer V[i] and V[j] whther if i==j*/
    temp=V[j];
    V[j]=V[i];
    V[i]=temp;
  }   
}
  

/*void main(int argc, char* argv[])
{
  int n;
  long seed;
  int i;
  float temp;
#define DIM 20
  int V[DIM];
  n=atoi(argv[1]);
  seed=atol(argv[2]);
  set_seed(seed);
  for(i=0;i<n;i++){
    temp=get_rand();
    fprintf(stderr,"%f ",temp);
  }
  fprintf(stderr,"\n");
  fprintf(stderr,"The sampling of %d\n",DIM);
  for(i=0;i<DIM;i++)
    V[i]=i+1;
  sample(V,DIM,DIM);
  for(i=0;i<DIM;i++)
    fprintf(stderr,"%d ",V[i]);
}
*/  
  
  
