#include <config.h>
#include "MNormal.h"

#include <matrix.h>
#include <DMNorm.h>
#include <lapack.h>

#include <graph/StochasticNode.h>
#include <sampler/SingletonGraphView.h>
#include <rng/RNG.h>

#include <cmath>
#include <algorithm>

#define N_REFRESH 100

using std::vector;
using std::copy;
using std::exp;
using std::sqrt;
using std::min;
using std::string;

namespace jags {
namespace bugs {

static vector<double> initValue(SingletonGraphView const *gv, 
				unsigned int chain)
{
    double const *x = gv->node()->value(chain);
    unsigned int N = gv->node()->length();
    vector<double> ivalue(N);
    for (unsigned int i = 0; i < N; ++i) {
	ivalue[i] = x[i];
    }
    return ivalue;
}

MNormMetropolis::MNormMetropolis(SingletonGraphView const *gv, 
				 unsigned int chain)
    : Metropolis(initValue(gv, chain)),
      _gv(gv), _chain(chain), 
      _mean(0), _var(0), _prec(0), 
      _n(0), _n_isotonic(0), _sump(0), _meanp(0), _lstep(0), _nstep(10), 
      _p_over_target(true)
{
    gv->checkFinite(chain); //Check validity of initial values
    
    unsigned int N = gv->length();

    _mean = new double[N];
    _var = new double[N * N];
    _prec = new double[N * N];
    
    // Give initial mean zero and identity variance matrix 
    for (unsigned int i = 0; i < N; ++i) {
	_mean[i] = 0;
	for (unsigned int j = 0; j < N; ++j) {
	    _prec[i + N * j] = _var[i + N * j] = (i == j) ? 1 : 0;
	}
    }
}

MNormMetropolis::~MNormMetropolis()
{
    delete [] _mean;
    delete [] _var;
    delete [] _prec;
}

void MNormMetropolis::update(RNG *rng)
{
    printf("===========================================================\n");
    //printf("ENTERING UPDATE FUNCTION\n");

    double logdensity = -_gv->logFullConditional(_chain);

    printf("entering, _lstep is: %.4f\n", _lstep);

    double step = exp(_lstep);
    

    double const *xold = _gv->node()->value(_chain);
    unsigned int N = _gv->length();

    printf("Using covariance _var matrix: [%02.3f, %02.3f]\n", _var[0], _var[1]);
    printf("Using covariance _var matrix: [%02.3f, %02.3f]\n", _var[2], _var[3]);

    printf("multivariate-Normal proposal scaled by step = exp(_lstep) = %5f\n", step);
    

    double *eps = new double[N];
    DMNorm::randomsample(eps, 0, _var, false, N, rng);
    vector<double> xnew(N);
    for (unsigned int i = 0; i < N; ++i) {
	xnew[i] = xold[i] + eps[i] * step;
    }
    delete [] eps;

    setValue(xnew);
    logdensity += _gv->logFullConditional(_chain);
    accept(rng, exp(logdensity));

    //printf("LEAVING UPDATE FUNCTION\n");
    //printf("===========================================================\n");
}

void MNormMetropolis::rescale(double p)
{
    //printf("ENTERING -rescale- FUNCTION\n");

    ++_n;
    printf("added +1 to _n, it's now: %d\n", _n);
    
    printf("p entering loop is: %.4f\n", p);
    p = min(p, 1.0);
    //printf("p after p=min(p,1) command is: %.4f\n", p);

    _sump += p;
    printf("cumulative sum of _sump is now: %.4f\n", _sump);

    //printf("the value of N_REFRESH is: %d\n", N_REFRESH);  // = 100

    if (_n % N_REFRESH == 0) {
        printf("--- NOW IN: if(_n pcnt N_REFRESH == 0) statement\n");
	//Calculate the running mean acceptance rate 
	_meanp = _sump / N_REFRESH;    
	printf("--- NOW IN: calculated _meanp as: %.4f <-------\n", _meanp);
	_sump = 0;
	printf("--- NOW IN: _sump was just reset to 0\n");
    }

    printf("the value of _n_isotonic is: %d\n", _n_isotonic);
    printf("the value of _nstep is: %d\n", _nstep);  // initially =10

    if (_n_isotonic == 0) {
        printf("NOW IN if(_n_isotonic == 0) statement\n");

	//Adjust scale of proposal distribution to get optimal acceptance
	//rate using a noisy gradient algorithm
	printf("updating _lstep by noisy gradiet, _lstep = _lstep + (p-0.234)/_nstep\n");
	_lstep += (p - 0.234) / _nstep;
	printf("_lstep was just updated to be: %3.4f\n", _lstep);

	printf("value of _p_over_target is currently: %d\n", _p_over_target);
	
	if ((p > 0.234) != _p_over_target) {
	    printf("NOW IN if((p > 0.234) != _p_over_target) statement\n");
	    _p_over_target = !_p_over_target;
	    printf("inverted _p_over_target, now it's: %d\n", _p_over_target);
	    ++_nstep;
	    printf("added +1 to _nstep, it's now: %d\n", _nstep);
	}
	/* 
	   Isotonic random walk. Use the identity matrix (scaled by
	   the _lstep parameter) as the precision of the proposal
	   distribution until the acceptance rate lies in an interval
	   around the optimum.
	*/
	if (_n % N_REFRESH == 0 && _meanp >= 0.15 && _meanp <= 0.35) {
	    printf("-- NOW IN: if(_n pcnt N_REFRESH == 0 && _meanp >= 0.15 && _meanp <= 0.35) statement\n");
	    _n_isotonic = _n;
	    printf("-- NOW IN: set _n_isotonic to _n, so it's now: %d\n", _n_isotonic);
	    _nstep = 100; //reset the step size as we adapt proposal
	    printf("-- NOW IN: set _nstep = 100");
	}
    }
    else {
        printf("************* IN the --else-- clause, that's _n_isotonic != 0\n");
	
        //This give better adaptation in the orange tree example
	printf("updating _lstep by *modified* noisy gradiet, _lstep += (p-0.234)/sqrt(_nstep)\n");
	_lstep += (p - 0.234) / sqrt(static_cast<double>(_nstep));
	printf("_lstep was just updated **** now using sqrt(_nstep) **** to be: %3.4f\n", _lstep);
	
        _nstep++;
	printf("added +1 to _nstep, it's now: %d\n", _nstep);
	/* 
	   Adaptive random walk: The variance of the proposal
	   distribution is adapted to the empirical variance of the
	   posterior distribution.
	   
	   We use weighted moment estimators for the mean and variance
	   so that more recent iterations get greter weight. This is
	   important because the chain has not converged: the effect
	   of an initial transient must be minimized.

	   The weights are proportional to (_n - _n_isotonic) for the
	   mean, and the iterative formula is exact.  For the
	   variance, the weights are proportional to _n, and the
	   formula is asymptotically correct.

	   For small values of (_n - _n_isotonic), the variance
	   estimator is shrunk towards the prior (identity matrix) in
	   order to ensure a smooth transition from the isotonic
	   random walk.
	*/

	unsigned int N = _gv->length();
	double const *x = _gv->node()->value(_chain);

        printf("******************* ABOUT TO ADAPT _mean and _var *************\n");
	printf("node value x is: [%.3f, %.3f]\n", x[0], x[1]);
	printf("_mean value is: [%.3f, %.3f]\n", _mean[0], _mean[1]);
	printf("_n is: %d\n", _n);
	printf("_n_isotonic is: %d\n", _n_isotonic);
	printf("_var matrix is: [%02.3f, %02.3f]\n", _var[0], _var[1]);
	printf("_var matrix is: [%02.3f, %02.3f]\n", _var[2], _var[3]);
	
	//printf("about to update _mean and _var\n");
	for (unsigned int i = 0; i < N; ++i) {
	    _mean[i] += 2 * (x[i] - _mean[i]) / (_n - _n_isotonic + 1);
	}

	printf("_mean just got updated to be: [%.3f, %.3f]\n", _mean[0], _mean[1]);

	
	//printf("about to update _var matrix\n");
	for (unsigned int i = 0; i < N; ++i) {
	    for (unsigned int j = 0; j < N; ++j) {
		_var[i + N * j] += 2 * ((x[i] - _mean[i]) * 
					    (x[j] - _mean[j]) -
					    _var[i + N * j]) / _n;
	    }
	}

	printf("_var matrix just got updated to: [%02.3f, %02.3f]\n", _var[0], _var[1]);
	printf("_var matrix just got updated to: [%02.3f, %02.3f]\n", _var[2], _var[3]);

    }
    //printf("LEAVING -rescale- FUNCTION\n");
}

bool MNormMetropolis::checkAdaptation() const
{
    printf("************** checkAdapatation() just got called **********\n");
    return (_n_isotonic > 0) && (_meanp >= 0.15) && (_meanp <= 0.35);
}

void MNormMetropolis::getValue(vector<double> &value) const
{
    double const *v = _gv->node()->value(_chain);
    copy(v, v + _gv->length(), value.begin());
}

void MNormMetropolis::setValue(vector<double> const &value)
{
    _gv->setValue(value, _chain);
}

}}
