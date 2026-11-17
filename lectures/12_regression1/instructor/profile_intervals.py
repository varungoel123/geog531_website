"""Numerical profile-likelihood reference intervals for broom GLM teaching tables."""
from pathlib import Path
import json
import numpy as np
import pandas as pd
from scipy.special import expit
from scipy.stats import chi2
from scipy.optimize import brentq

root=Path(__file__).resolve().parents[1]
d=pd.read_csv(root/'data/urban_heat.csv')
reference=json.loads((root/'instructor/verified_results.json').read_text())
X=np.column_stack([np.ones(len(d)),d.canopy10,d.deprivation,d.cool_roof])
output={}
for name,response,family in [('logistic','heat_illness','binomial'),('poisson','heat_episodes','poisson')]:
    y=d[response].to_numpy()
    off=np.zeros(len(d)) if family=='binomial' else np.log(d.person_weeks.to_numpy())
    mle=np.array(reference[name]['estimate'])
    se=np.array(reference[name]['se'])
    def nll(b):
        eta=X@b+off
        return np.sum(np.logaddexp(0,eta)-y*eta) if family=='binomial' else np.sum(np.exp(eta)-y*eta)
    minimum=nll(mle)
    intervals=[]
    for j in range(X.shape[1]):
        free=np.arange(X.shape[1])!=j
        Z=X[:,free]
        def profile_root(value):
            b=mle.copy();b[j]=value
            for iteration in range(100):
                eta=X@b+off
                mu=expit(eta) if family=='binomial' else np.exp(eta)
                w=mu*(1-mu) if family=='binomial' else mu
                delta=np.linalg.solve(Z.T@(Z*w[:,None]),Z.T@(y-mu))
                b[free]+=delta
                if np.max(np.abs(delta))<1e-11:break
            else:raise AssertionError('Nuisance optimization did not converge')
            return 2*(nll(b)-minimum)-chi2.ppf(.95,1)
        bounds=[]
        for sign in [-1,1]:
            edge=mle[j]+sign*2*se[j]
            while profile_root(edge)<0:edge=mle[j]+(edge-mle[j])*1.5
            bound=brentq(profile_root,min(edge,mle[j]),max(edge,mle[j]),xtol=1e-11)
            assert abs(profile_root(bound))<1e-7
            bounds.append(float(bound))
        intervals.append(bounds)
    output[name]={'log_scale':intervals,'exponentiated':np.exp(intervals).tolist()}
(root/'instructor/profile_intervals.json').write_text(json.dumps(output,indent=2))
print(json.dumps(output,indent=2))
