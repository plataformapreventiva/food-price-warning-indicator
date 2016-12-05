data {
  int<lower=0> N;
  real x1[N];
  real<lower=0> y[N];
}

parameters {
  real trend[N];
  real season[N];
  real s_trend;
  real s_q;
  real s_season;
  real a;
  real d;
}

model {
	real q[N];
	real cum_trend[N];
	for (i in 13:N) {
		season[i]~normal(-season[i-1]-season[i-2]-season[i-3]-season[i-4]-season[i-5]-season[i-6]-season[i-7]-season[i-8]-season[i-9]-season[i-10]-season[i-11]-season[i-12],s_season);
	}

	for (i in 3:N)
		trend[i]~normal(2*trend[i-1]-trend[i-2],s_trend);

	cum_trend[1]=trend[1];
	for (i in 2:N)
		cum_trend[i]=cum_trend[i-1]+trend[i];

	for (i in 1:N)
		q[i]=y[i]-cum_trend[i]-season[i];

	for (i in 1:N)
		q[i]~normal(a*x1[i] + d,s_q);
}
