"""Independent reference for the categorical/interaction and GLM extensions.

Matches the Quarto generators, but does not claim native R execution.
"""
from pathlib import Path
import json, importlib.util
import numpy as np
import pandas as pd
from scipy import stats, linalg, optimize, special

ROOT=Path(__file__).resolve().parents[1]
spec=importlib.util.spec_from_file_location('base_sc',ROOT/'instructor/validate_simulation.py')
base=importlib.util.module_from_spec(spec);spec.loader.exec_module(base)

def modifiers(d):
    d=d.copy();rng=base.PM(531917);n=len(d)
    u=rng.u(n)
    cooling=np.where(u<.40,'Limited',np.where(u<.75,'Partial','Reliable'))
    housing=np.where(rng.u(n)<special.expit(-.5+.035*(d.poverty_pct.to_numpy()-20)),'Vulnerable','Standard')
    program=np.where(rng.u(n)<.5,'Yes','No')
    h=(d.heat_days.to_numpy()-15)/5;p=(d.poverty_pct.to_numpy()-20)/10
    partial=(cooling=='Partial').astype(float);reliable=(cooling=='Reliable').astype(float)
    vuln=(housing=='Vulnerable').astype(float);yes=(program=='Yes').astype(float)
    add=-.40*partial-.80*reliable+.45*vuln-.25*yes+.25*h*p-.20*h*partial-.45*h*reliable-.65*yes*vuln
    d['heat5']=h;d['poverty10']=p;d['impervious_c10_sq']=d.impervious_c10**2
    d['cooling_access']=pd.Categorical(cooling,categories=['Limited','Partial','Reliable'])
    d['housing_profile']=pd.Categorical(housing,categories=['Standard','Vulnerable'])
    d['warning_program']=pd.Categorical(program,categories=['No','Yes'])
    d['poor_health_days_modified']=d.poor_health_days+add
    return d,add

def glm_data(d):
    d=d.copy();n=len(d);r=base.PM(5311125)
    part=(d.cooling_access=='Partial').to_numpy().astype(float)
    rel=(d.cooling_access=='Reliable').to_numpy().astype(float)
    vul=(d.housing_profile=='Vulnerable').to_numpy().astype(float)
    yes=(d.warning_program=='Yes').to_numpy().astype(float)
    h=d.heat5.to_numpy();p=d.poverty10.to_numpy();i=d.impervious_c10.to_numpy()
    eta=-1.25+.40*h+.35*p+.18*i+.10*i**2-.35*part-.75*rel+.40*vul-.25*yes-.10*h*part-.22*h*rel
    prob=special.expit(eta);illness=(r.u(n)>1-prob).astype(int)
    pop=np.floor(600+3400*r.u(n)).astype(int)
    lograte=-6+.28*h+.30*p+.15*i+.08*i**2-.20*part-.45*rel+.35*vul-.25*yes-.08*h*part-.16*h*rel
    mu=pop*np.exp(lograte)
    visits=stats.poisson.ppf(r.u(n),mu).astype(int)
    theta=1.5
    extra=stats.nbinom.ppf(r.u(n),theta,theta/(theta+mu)).astype(int)
    d['resident_heat_illness']=illness;d['population_2025']=pop;d['person_years']=pop
    d['heat_visits']=visits;d['heat_visits_extra']=extra
    return d,dict(probability=prob,mu_count=mu,theta=theta)

GLM_TERMS=['(Intercept)','heat5','poverty10','impervious_c10','impervious_c10_sq',
           'cooling_accessPartial','cooling_accessReliable','housing_profileVulnerable',
           'warning_programYes','heat5:cooling_accessPartial','heat5:cooling_accessReliable']
def glm_design(d):
    h=d.heat5.to_numpy();p=d.poverty10.to_numpy();i=d.impervious_c10.to_numpy()
    pa=(d.cooling_access=='Partial').to_numpy().astype(float)
    re=(d.cooling_access=='Reliable').to_numpy().astype(float)
    vu=(d.housing_profile=='Vulnerable').to_numpy().astype(float)
    ye=(d.warning_program=='Yes').to_numpy().astype(float)
    return np.column_stack([np.ones(len(d)),h,p,i,i**2,pa,re,vu,ye,h*pa,h*re])

def fit_glm(y,X,family,offset=None):
    off=np.zeros(len(y)) if offset is None else offset
    b=np.zeros(X.shape[1]);b[0]=-1.25 if family=='binomial' else -6
    def nll(v):
        eta=X@v+off
        return np.sum(np.logaddexp(0,eta)-y*eta) if family=='binomial' else np.sum(np.exp(eta)-y*eta)
    for _ in range(100):
        eta=X@b+off;mu=special.expit(eta) if family=='binomial' else np.exp(eta)
        w=mu*(1-mu) if family=='binomial' else mu
        step=linalg.solve(X.T@(X*w[:,None]),X.T@(y-mu),assume_a='sym')
        scale=1
        while nll(b+scale*step)>nll(b)+1e-9:scale*=.5
        b+=scale*step
        if np.max(np.abs(scale*step))<1e-10:break
    else:raise AssertionError('GLM fit did not converge')
    eta=X@b+off;mu=special.expit(eta) if family=='binomial' else np.exp(eta)
    w=mu*(1-mu) if family=='binomial' else mu
    v=linalg.inv(X.T@(X*w[:,None]))
    assert np.max(np.abs(X.T@(y-mu)))<1e-5
    return dict(b=b,se=np.sqrt(np.diag(v)),mu=mu,v=v,nll=nll(b))

def fit_nb(y,X,off,b0):
    def loglike(b,lt):
        theta=np.exp(lt);eta=X@b+off;logsum=np.logaddexp(lt,eta)
        return np.sum(special.gammaln(y+theta)-special.gammaln(theta)-special.gammaln(y+1)+theta*lt+y*eta-(theta+y)*logsum)
    def obj(v):return -loglike(v[:-1],v[-1])
    def jac(v):
        b=v[:-1];theta=np.exp(v[-1]);eta=X@b+off;mu=np.exp(eta)
        gb=X.T@((y-mu)/(1+mu/theta))
        gt=theta*np.sum(special.digamma(y+theta)-special.digamma(theta)+np.log(theta)+1-np.log(theta+mu)-(theta+y)/(theta+mu))
        return -np.r_[gb,gt]
    result=optimize.minimize(obj,np.r_[b0,np.log(1.5)],jac=jac,method='BFGS',options={'gtol':1e-7,'maxiter':500})
    assert np.max(np.abs(jac(result.x)))<1e-3,(result.message,jac(result.x))
    b=result.x[:-1];theta=np.exp(result.x[-1]);mu=np.exp(X@b+off)
    return dict(b=b,theta=float(theta),mu=mu,loglike=float(-result.fun))

def run():
    d,_,_,W,t=base.simulate();d,add=modifiers(d);d,gt=glm_data(d);n=len(d)
    h=d.heat5.to_numpy();p=d.poverty10.to_numpy();i=d.impervious_c10.to_numpy()
    pa=(d.cooling_access=='Partial').to_numpy().astype(float);re=(d.cooling_access=='Reliable').to_numpy().astype(float)
    vu=(d.housing_profile=='Vulnerable').to_numpy().astype(float);ye=(d.warning_program=='Yes').to_numpy().astype(float)
    county=pd.get_dummies(d.county_id,drop_first=True,dtype=float).to_numpy()
    ot=['(Intercept)','heat5','poverty10','impervious_c10','impervious_c10_sq','cooling_accessPartial','cooling_accessReliable',
        'warning_programYes','housing_profileVulnerable']+[f'county_{k}' for k in range(2,47)]+[
        'heat5:poverty10','heat5:cooling_accessPartial','heat5:cooling_accessReliable','warning_programYes:housing_profileVulnerable']
    Xo=np.column_stack([np.ones(n),h,p,i,i**2,pa,re,ye,vu,county,h*p,h*pa,h*re,ye*vu])
    ols=base.fit(d.poor_health_days_modified.to_numpy(),Xo)
    bo=linalg.lstsq(Xo,t['mu_total']+add)[0]
    expected=dict(zip(ot,bo));ob=dict(zip(ot,ols['b']))
    for name,value in [('heat5',.9),('heat5:poverty10',.25),('heat5:cooling_accessPartial',-.2),('heat5:cooling_accessReliable',-.45),
                       ('warning_programYes:housing_profileVulnerable',-.65)]:
        assert abs(expected[name]-value)<1e-9,(name,expected[name])
    X=glm_design(d);yl=d.resident_heat_illness.to_numpy();yp=d.heat_visits.to_numpy();yevent=d.heat_visits_extra.to_numpy();off=np.log(d.person_years.to_numpy())
    lg=fit_glm(yl,X,'binomial');pg=fit_glm(yp,X,'poisson',off);ex=fit_glm(yevent,X,'poisson',off);nb=fit_nb(yevent,X,off,ex['b'])
    np.testing.assert_allclose(np.exp(X@pg['b']+np.log(2*d.person_years.to_numpy())),2*pg['mu'],rtol=1e-12)
    assert np.all((lg['mu']>0)&(lg['mu']<1))
    df=n-X.shape[1]
    report={'ols_modified':{'observed':ob,'expected':expected,'heat5_hc3_se':float(ols['HC3'][1])},
      'factor_counts':{c:{str(k):int(v) for k,v in d[c].value_counts().items()} for c in ['cooling_access','housing_profile','warning_program']},
      'resident_events':int(yl.sum()),'n':n,
      'logistic':{'estimate':dict(zip(GLM_TERMS,lg['b'])),'se':dict(zip(GLM_TERMS,lg['se'])),'Brier':float(np.mean((yl-lg['mu'])**2)),
                  'probability_range':[float(lg['mu'].min()),float(lg['mu'].max())]},
      'poisson':{'estimate':dict(zip(GLM_TERMS,pg['b'])),'se':dict(zip(GLM_TERMS,pg['se'])),
                 'dispersion':float(np.sum((yp-pg['mu'])**2/pg['mu'])/df),
                 'observed_zero_fraction':float(np.mean(yp==0)),'expected_zero_fraction':float(np.mean(np.exp(-pg['mu'])))},
      'extra_poisson':{'estimate':dict(zip(GLM_TERMS,ex['b'])),
           'dispersion':float(np.sum((yevent-ex['mu'])**2/ex['mu'])/df),
           'AIC':float(2*(X.shape[1]+ex['nll']+np.sum(special.gammaln(yevent+1))))},
      'negative_binomial':{'estimate':dict(zip(GLM_TERMS,nb['b'])),'theta':nb['theta'],
           'dispersion':float(np.sum((yevent-nb['mu'])**2/(nb['mu']+nb['mu']**2/nb['theta']))/df),
           'AIC':float(-2*nb['loglike']+2*(X.shape[1]+1))},
      'truth':{'OLS_heat5_base':.9,'OLS_heat5_by_poverty10':.25,'OLS_heat5_by_partial':-.2,'OLS_heat5_by_reliable':-.45,
               'OLS_program_by_vulnerable':-.65,'NB_theta':1.5,'GLM_outcomes_conditionally_independent':True}}
    assert report['extra_poisson']['dispersion']>2
    assert report['negative_binomial']['AIC']<report['extra_poisson']['AIC']
    assert .7<report['poisson']['dispersion']<1.3
    assert 0<yl.sum()<n and np.all((yp>=0)&(yp%1==0))
    assert d.groupby(['cooling_access','housing_profile','warning_program'],observed=True).size().min()>10
    assert d.poor_health_days_modified.between(0,30).all()
    d.to_csv(ROOT/'data/sc_heat_health_2025_extended.csv',index=False,float_format='%.12f')
    (ROOT/'instructor/interactions_glm_reference.json').write_text(json.dumps(report,indent=2))
    print(json.dumps({k:v for k,v in report.items() if k not in ['ols_modified','logistic','poisson','extra_poisson','negative_binomial']},indent=2))
    print('OLS focal coefficients:',{k:round(ob[k],4) for k in ['heat5','heat5:poverty10','heat5:cooling_accessPartial','heat5:cooling_accessReliable','warning_programYes:housing_profileVulnerable']})
    print('Pearson ratios:',report['poisson']['dispersion'],report['extra_poisson']['dispersion'],report['negative_binomial']['dispersion'])
    print('NB theta:',nb['theta'],'AIC:',report['extra_poisson']['AIC'],report['negative_binomial']['AIC'])
    print('PASS: interaction oracle, support, GLM score equations, counts/exposure, overdispersion and NB likelihood checks.')
    return d,report,(ols,lg,pg,ex,nb)

if __name__=='__main__':run()
