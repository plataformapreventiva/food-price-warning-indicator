/* No changes from Version 0.0 */

#include <math.h>
#define SIGN(a,b) ((b)<0.0 ? -fabs(a) : fabs(a))
#include <malloc.h>
#include <stdio.h>

/* Some useful subroutines - linear algebra, dynamic memory allocation, etc. */

/*****************************************************************/
void nrerror(error_text)
char error_text[];
{
  fprintf(stderr,"Numerical Recipes run-time error...\n");
  fprintf(stderr,"%s\n",error_text);
  fprintf(stderr,"...now exiting to system...\n");
  exit(1);
}
/*************************************************************/
double *vector(nh)
int nh;
{
  double *v;

  v=(double *)calloc(nh,sizeof(double));
  if (!v) nrerror("allocation failure in dvector()");
  return v;
}
/*************************************************************/
int *ivector(nh)
int nh;
{
  int *v;

  v=(int *)calloc(nh,sizeof(int));
  if (!v) nrerror("allocation failure in ivector()");
  return v;
}
/**********************************************************/
double **matrix(nrh,nch)
int nrh,nch;
{
  int i;
  double **m;

  m=(double **) calloc(nrh,sizeof(double*));
  if (!m) nrerror("allocation failure 1 in matrix()");
  for(i=0;i<nrh;i++) {
    m[i]=(double *)calloc(nch,sizeof(double));
    if (!m[i]) nrerror("allocation failure 2 in matrix()");
    }
  return m;
}
/******************************************************/
void f_mat(m,nrh)
double **m;
int nrh;
{
  int i=0;

  for(i=nrh-1;i>=0;i--) free((char*) (m[i]));
  free((char*) (m));
}
/**********************************************************/
double **dmatrix(nrh,nch)
int nrh,nch;
{
  int i;
  double **m;

  m=(double **) calloc(nch,sizeof(double*));
  if (!m) nrerror("allocation failure 1 in dmatrix()");
  for(i=0;i<nch;i++) {
    m[i]=(double *)calloc(nrh,sizeof(double));
    if (!m[i]) nrerror("allocation failure 2 in dmatrix()");
    }
  return m;
}

/*****************************************************************/
void ord_mat(nvr,eig,mat)
int nvr;
double *eig,**mat;
{
  int i=0,j=0,k=0;
  double tmp=0.0;

  for (i=0;i<nvr;i++)
    for (j=i;j<nvr;j++) 
      if (eig[j]<eig[i]) {
        tmp=eig[j];
        eig[j]=eig[i];
        eig[i]=tmp;
        for (k=0;k<nvr;k++) {
          tmp=mat[k][j];
          mat[k][j]=mat[k][i];
          mat[k][i]=tmp;
          }
        }
}
/*****************************************************************/
void tred2(a,n,d,e)
double **a,d[],e[];
int n;
{
  int l=0,k=0,j=0,i=0;
  double scale=0.0,hh=0.0,h=0.0,g=0.0,f=0.0;

  for (i=n-1;i>=1;i--) {
    l=i-1;
    h=scale=0.0;
    if (l > 0) {
      for (k=0;k<=l;k++)
        scale += fabs(a[i][k]);
      if (scale == 0.0)
        e[i]=a[i][l];
      else {
        for (k=0;k<=l;k++) {
          a[i][k] /= scale;
          h +=a[i][k]*a[i][k];
          }
        f=a[i][l];
        g = f>0.0 ? -sqrt(h) : sqrt(h);
        e[i]=scale*g;
        h -= f*g;
        a[i][l]=f-g;
        f=0.0;
        for (j=0;j<=l;j++) {
          a[j][i]=a[i][j]/h;
          g=0.0;
          for (k=0;k<=j;k++)
            g +=a[j][k]*a[i][k];
          for (k=j+1;k<=l;k++)
            g +=a[k][j]*a[i][k];
          e[j]=g/h;
          f += e[j]*a[i][j];
          }
        hh=f/(h+h);
        for (j=0;j<=l;j++) {
          f=a[i][j];
          e[j]=g=e[j]-hh*f;
          for (k=0;k<=j;k++)
          a[j][k] -= (f*e[k]+g*a[i][k]);
          }
        }
      } else
      e[i]=a[i][l];
    d[i]=h;
    }
  d[0]=0.0;
  e[0]=0.0;
  for (i=0;i<n;i++) {
    l=i-1;
    if (d[i]) {
      for (j=0;j<=l;j++) {
        g=0.0;
        for (k=0;k<=l;k++)
          g +=a[i][k]*a[k][j];
        for (k=0;k<=l;k++)
          a[k][j] -= g*a[k][i];
        }
      }
    d[i]=a[i][i];
    a[i][i]=1.0;
    for (j=0;j<=l;j++) 
      a[i][j]=a[j][i]=0.0;
  }
}
/******************************************************************/
void tqli(d,e,n,z)
double d[],e[],**z;
int n;
{
  int m=0,l=0,iter=0,i=0,k=0;
  double s=0.0,r=0.0,p=0.0,g=0.0,f=0.0,dd=0.0,c=0.0,b=0.0;
  void nrerror();

  for (i=1;i<n;i++) 
    e[i-1]=e[i];
  e[n-1]=0.0;
  for (l=iter=0;l<n;l++) 
    do {
      for (m=l;m<n-1;m++) {
        dd=fabs(d[m])+fabs(d[m+1]);
        if (fabs(e[m])+dd == dd) break;
        }
      if (m != l) {
        if ((iter++) == 1000)
          nrerror("Too many iterations in TQLI");
        g=(d[l+1]-d[l])/(2.0*e[l]);
        r=sqrt((g*g)+1.0);
        g=d[m]-d[l]+e[l]/(g+SIGN(r,g));
        s=c=1.0;
        p=0.0;
          for (i=m-1;i>=l;i--) {
            f=s*e[i];
            b=c*e[i];
            if (fabs(f) >= fabs(g)) {
              c=g/f;
              r=sqrt((c*c)+1.0);
              e[i+1]=f*r;
              c *= (s=1.0/r);
              } else {
              s=f/g;
              r=sqrt((s*s)+1.0);
              e[i+1]=g*r;
              s *= (c=1.0/r);
              }
            g=d[i+1]-p;
            r=(d[i]-g)*s+2.0*c*b;
            p=s*r;
            d[i+1]=g+p;
            g=c*r-b;
            for (k=0;k<n;k++) {
              f=z[k][i+1];
              z[k][i+1]=s*z[k][i]+c*f;
              z[k][i]=c*z[k][i]-s*f;
              }
            }
          d[l]=d[l]-p;
          e[l]=g;
          e[m]=0.0;
          }
        } while (m != l);
}

/*****************************************************************/

double det(h,m)
double **h;
int m;
{
  double *dv,*er,tmp,**temp;
  double *vector(),**matrix();
  void tqli(),tred2(),f_mat();
  int i,j;

  temp=matrix(m,m);
  for (i=0;i<m;++i)
    for (j=0;j<m;++j)
      temp[i][j]=h[i][j];
  er=vector(m); dv=vector(m);
  tred2(temp,m,dv,er);
  tqli(dv,er,m,temp);
  for (i=0,tmp=1.0;i<m;++i)
    tmp*=dv[i];
  free((char*)(dv)); free((char*)(er));
  f_mat(temp,m);
  return tmp;
}

/***********************************************************************/

void invert_matrix(h,m)
double **h;
int m;
{
  double **hs,*dv,*er,**lam;
  double *vector(),**matrix();
  void tqli(),tred2(),f_mat(),m_mat(),nrerror();
  int i,j;

  er=vector(m); lam=matrix(m,m); hs=matrix(m,m); dv=vector(m);
  tred2(h,m,dv,er);
  tqli(dv,er,m,h);
  for (i=0;i<m;++i) 
    if (dv[i]!=0.0)
      lam[i][i]=1.0/dv[i];
    else nrerror("trying to invert a singular matrix");
  m_mat(hs,h,lam,m,m,m);
  for (i=0;i<m;++i)
    for (j=0;j<m;++j)
      lam[i][j]=h[j][i];
  m_mat(h,hs,lam,m,m,m);
  f_mat(hs,m); f_mat(lam,m);
  free((char*)(dv)); free((char*)(er));
}
/********************************************************************/

void m_mat(m0,m1,m2,n,p,m)
double **m0,**m1,**m2;
int n,p,m;
{
  int i,j,k;

  for (i=0;i<n;++i) 
    for (j=0;j<m;++j)
      for (k=0,m0[i][j]=0.0;k<p;++k)
	m0[i][j]+=m1[i][k]*m2[k][j];
}
/********************************************************************/

void s_mat(m0,m1,m2,n,m,k)
double **m0,**m1,**m2;
int n,m,k;
{
  int i,j;

  if (k==0)
    for (i=0;i<n;++i)
      for (j=0;j<m;++j)
        m0[i][j]=m1[i][j]-m2[i][j];
  else
    for (i=0;i<n;++i)
      for (j=0;j<m;++j)
        m0[i][j]=m1[i][j]+m2[i][j];
}

/* NOTE: The following subroutine has a copyright notice contained within */

/*twiddle.c - generate all combinations of M elements drawn without replacement
  from a set of N elements.  This routine may be used in two ways:
  (0) To generate all combinations of M out of N objects, let a[0..N-1]
      contain the objects, and let c[0..M-1] initially be the combination
      a[N-M..N-1].  While twiddle(&x, &y, &z, p) is false, set c[z] = a[x] to
      produce a new combination.
  (1) To generate all sequences of 0's and 1's containing M 1's, let
      b[0..N-M-1] = 0 and b[N-M..N-1] = 1.  While twiddle(&x, &y, &z, p) is
      false, set b[x] = 1 and b[y] = 0 to produce a new sequence.
  In either of these cases, the array p[0..N+1] should be initialised as
  follows:
    p[0] = N+1
    p[1..N-M] = 0
    p[N-M+1..N] = 1..M
    p[N+1] = -2
    if M=0 then p[1] = 1
  In this implementation, this initialisation is accomplished by calling
  inittwiddle(M, N, p), where p points to an array of N+2 ints.

  Coded by Matthew Belmonte <mkb4@Cornell.edu>, 23 March 1996.  This
  implementation Copyright (c) 1996 by Matthew Belmonte.  Permission for use and
  distribution is hereby granted, subject to the restrictions that this
  copyright notice and reference list be included in its entirety, and that any
  and all changes made to the program be clearly noted in the program text.

  This software is provided 'as is', with no warranty, express or implied,
  including but not limited to warranties of merchantability or fitness for a
  particular purpose.  The user of this software assumes liability for any and
  all damages, whether direct or consequential, arising from its use.  The
  author of this implementation will not be liable for any such damages.

  Reference:

  Phillip J Chase, `Algorithm 382: Combinations of M out of N Objects [G6]',
  Communications of the Association for Computing Machinery 13:6:368 (1970).

  The returned indices x, y, and z in this implementation are decremented by 1,
  in order to conform to the C language array reference convention.  Also, the
  parameter 'done' has been replaced with a Boolean return value.
*/

int twiddle(x, y, z, p)
int *x, *y, *z, *p;
  {
  register int i, j, k;
  j = 1;
  while(p[j] <= 0)
    j++;
  if(p[j-1] == 0)
    {
    for(i = j-1; i != 1; i--)
      p[i] = -1;
    p[j] = 0;
    *x = *z = 0;
    p[1] = 1;
    *y = j-1;
    }
  else
    {
    if(j > 1)
      p[j-1] = 0;
    do
      j++;
    while(p[j] > 0);
    k = j-1;
    i = j;
    while(p[i] == 0)
      p[i++] = -1;
    if(p[i] == -1)
      {
      p[i] = p[k];
      *z = p[k]-1;
      *x = i-1;
      *y = k-1;
      p[k] = -1;
      }
    else
      {
      if(i == p[0])
	return(1);
      else
	{
	p[j] = p[i];
	*z = p[i]-1;
	p[i] = 0;
	*x = j-1;
	*y = i-1;
	}
      }
    }
  return(0);
  }

void inittwiddle(m, n, p)
int m, n, *p;
  {
  int i;
  p[0] = n+1;
  for(i = 1; i != n-m+1; i++)
    p[i] = 0;
  while(i != n+1)
    {
    p[i] = i+m-n;
    i++;
    }
  p[n+1] = -2;
  if(m == 0)
    p[1] = 1;
  }



/* this generates all combinations of size = nsel from the set {1, .., nsite}
   and then stores them sequentially in comb, using the twiddle routine above.
*/

void combgen(nsite, nsel, comb, p, b)
 int *nsite, *nsel, *comb, *p, *b;
  {
  int i, x, y, z, j, M,N, k;
  M = nsel[0];
  N = nsite[0];
  inittwiddle(M, N, p);
  for(i = 0; i != N-M; i++)
    b[i] = 0;
  while(i != N)    b[i++] = 1;
   k = 0;
  for (i = 0; i < N; i++) if (b[i] == 1) {comb[k] = i+1;
                                         k = k +1;
                                        }
   j = 1;
  while(!twiddle(&x, &y, &z, p))
    {
    b[x] = 1;
    b[y] = 0;    
    k = 0;  
    for (i = 0; i < N; i++) if (b[i] == 1) {comb[k+M*j] = i+1;
                                            k = k +1;
                                        }
       j = j + 1;
    }
  }


/*  
 This evaluates log |determinant| of all nsel x nsel submatrices of sigma
*/

void eval_ent(double *sigv, int *nsite, int *nsel, int *comb, int *ncomb, double *ldet)
   {
  int i, j, k, M, N, i1,i2, Q;
  double **aa;
  double det(), **matrix();

  M = nsel[0];
  N = ncomb[0];
  Q = nsite[0];
  aa = matrix(M,M); 
 for (i=0; i < N; i++) {
    for (j=0; j < M; j++) 
     {  i1 = comb[M*i+j]-1;
        for (k=j; k < M; k++) {i2 = comb[M*i+k]-1;
                              aa[j][k] = sigv[Q*i1 + i2]; 
                              }
      }
    for (j=1; j < M; j++)
      for (k=0; k < j; k++)  aa[j][k] = aa[k][j];
     ldet[i] = log(fabs(det(aa,M)));
    }
}

/*  
 This evaluates log |determinant| of one nsel x nsel submatrix of sigma
*/


void eval_ent1c(sigv, M, N, comb, tmp1,aa)
  int *comb, M, N;
  double *sigv, *tmp1, **aa;
   {
  int i, j, k, i1,i2;
  double det();

     for (j=0; j < M; j++) 
     {  i1 = comb[j]-1;
        for (k=j; k < M; k++) {i2 = comb[k]-1;
                              aa[j][k] = sigv[N*i1 + i2]; 
                              }
      }
    for (j=1; j < M; j++)
      for (k=0; k < j; k++)  aa[j][k] = aa[k][j];
     tmp1[0] = log(fabs(det(aa,M)));
   }


/* This goes through all submatrices of size = nsel from the sig matrix (nsite x nsite)
   to evaluate their log|det| and then returns one combination with highest value -
   current version not dealing with ties yet!
*/

void search_ent(sigv, nsite, nsel, comb, ncomb, newcomb, p, b,ldet)
 int *nsite, *nsel, *comb, *p, *b, *newcomb, *ncomb ;
 double *sigv, *ldet;
  {
  int i, x, y, z, j, M,N, k;
  double tmp, tmp1, **aa;
  double **matrix();
  void eval_ent1c();

  tmp = - 1000000.0;
  M = nsel[0];
  N = nsite[0];
  aa = matrix(M,M);
  inittwiddle(M, N, p);
  for(i = 0; i != N-M; i++)
    b[i] = 0;
  while(i != N)    b[i++] = 1;
   k = 0;
  for (i = 0; i < N; i++) if (b[i] == 1) {comb[k] = i+1;
                                         k = k +1;
                                        }
  eval_ent1c(sigv,M,N,comb,ldet,aa);
  tmp1 = ldet[0];
  if (tmp1>tmp) { tmp = tmp1;
                  for (k=0;k<M;k++) newcomb[k] = comb[k];
                }
                  
   j = 1;
  while(!twiddle(&x, &y, &z, p))
    {
    b[x] = 1;
    b[y] = 0;    
    k = 0;  
    for (i = 0; i < N; i++) if (b[i] == 1) {comb[k] = i+1;
                                            k = k +1;
                                        }
    eval_ent1c(sigv,M,N,comb,ldet,aa);
    tmp1 = ldet[0];
    if (tmp1>tmp) { tmp = tmp1;
                  for (k=0;k<M;k++) newcomb[k] = comb[k];
                }
       j = j + 1;
    }
   ncomb[0] = j;
   ldet[0] = tmp;
  }

