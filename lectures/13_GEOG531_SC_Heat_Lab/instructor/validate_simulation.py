"""Independent Python numerical reference for the self-contained R simulation.

This implements the same small Park–Miller generator, qnorm/qpois transforms,
data order and structural equations as the Quarto source. No R execution is
claimed. Requires numpy, pandas, scipy and matplotlib.
"""
from pathlib import Path
import json
import numpy as np
import pandas as pd
from scipy import stats, linalg

ROOT=Path(__file__).resolve().parents[1]
class PM:
    def __init__(self,seed=5312025):self.state=int(seed)
    def u(self,n):
        out=np.empty(n)
        for k in range(n):
            self.state=(16807*self.state)%2147483647
            out[k]=self.state/2147483647
        return out
    def z(self,n):return stats.norm.ppf(self.u(n))

def simulate(seed=5312025):
    rng=PM(seed)
    counties=pd.DataFrame([(cy,cx) for cy in range(1,7) for cx in range(1,9)
                          if not(cy==6 and cx in [1,8])],columns=['county_y','county_x'])
    counties['county_number']=np.arange(1,47)
    counties['county_id']=[f'SCsim_{i:02d}' for i in range(1,47)]
    county_context=rng.z(46)
    counties['hidden_context']=county_context
    d=pd.DataFrame([(c,ly,lx) for c in range(1,47) for ly in range(4) for lx in range(4)],
                   columns=['county_number','local_y','local_x']).merge(counties,on='county_number',sort=False)
    n=len(d)
    x=(d.county_x.to_numpy()-1)*4+d.local_x.to_numpy()+1
    y=(d.county_y.to_numpy()-1)*4+d.local_y.to_numpy()+1
    east=(x-x.mean())/x.std(ddof=1)
    c=d.hidden_context.to_numpy()
    poverty=np.clip(18+2*c+7*rng.z(n)+3*east,4,40)
    impervious=np.clip(40+4*c+14*rng.z(n)+8*east,5,90)
    imp=(impervious-40)/10
    rate=np.exp(2.1+.018*poverty+.13*imp+.28*c+.12*east)
    heat=stats.poisson.ppf(rng.u(n),rate).astype(int)
    hardship=2*poverty+.4*rng.z(n)
    A=((np.abs(x[:,None]-x[None,:])+np.abs(y[:,None]-y[None,:]))==1).astype(float)
    W=A/A.sum(axis=1)[:,None]
    rho=.70
    B=np.eye(n)-rho*W
    sigma=.25+.035*heat+.015*poverty
    z_m=rng.z(n);z_y=rng.z(n)
    components=linalg.solve(B,np.column_stack([.35*z_m,sigma*z_y]))
    mediator_mean=.20*heat+.02*poverty+.15*imp
    mediator=mediator_mean+components[:,0]
    mu=3+.18*heat+.048*poverty+.31*imp+.20*imp**2+.65*c
    health=3+.10*heat+.4*mediator+.04*poverty+.25*imp+.20*imp**2+.65*c+components[:,1]
    d=pd.DataFrame({'tract_id':[f'T{i:04d}' for i in range(1,n+1)],'county_id':d.county_id,
        'year':2025,'x':x,'y':y,'heat_days':heat,'poor_health_days':health,
        'poverty_pct':poverty,'hardship_index':hardship,'impervious_pct':impervious,
        'impervious_c10':imp,'disruption_index':mediator})
    raw=d.copy();raw.loc[370,'poor_health_days']+=35
    audit=pd.DataFrame({'tract_id':[d.loc[370,'tract_id']],
                        'verified_poor_health_days':[health[370]],
                        'audit_note':['Source entry check: added 35 days during data entry.']})
    return d,raw,audit,W,dict(mu_total=mu,rho=rho,sigma=sigma,total_effect=.18,
                              direct_effect=.10,context=c,B=B)

def fit(y,X):
    b=linalg.lstsq(X,y)[0];res=y-X@b
    inv=linalg.inv(X.T@X);df=len(y)-X.shape[1]
    mse=res@res/df;V=inv*mse
    hat=np.einsum('ij,jk,ik->i',X,inv,X)
    score=X*(res/(1-hat))[:,None]
    hc3=inv@(score.T@score)@inv
    return dict(b=b,res=res,fit=X@b,df=df,SE=np.sqrt(np.diag(V)),
                HC3=np.sqrt(np.diag(hc3)),hat=hat,sigma=np.sqrt(mse),X=X,
                r2=1-(res@res)/np.sum((y-y.mean())**2))
def moran(v,W):
    v=v-v.mean()
    return float(len(v)/W.sum()*(v@W@v)/(v@v))
def design(d):
    n=len(d);h=d.heat_days.to_numpy();p=d.poverty_pct.to_numpy();i=d.impervious_c10.to_numpy()
    county=pd.get_dummies(d.county_id,drop_first=True,dtype=float).to_numpy()
    return {'m0':np.column_stack([np.ones(n),h]),
            'm1':np.column_stack([np.ones(n),h,p,i]),
            'm_collinear':np.column_stack([np.ones(n),h,p,i,d.hardship_index.to_numpy()]),
            'm_curve':np.column_stack([np.ones(n),h,p,i,i**2]),
            'm_fe':np.column_stack([np.ones(n),h,p,i,i**2,county]),
            'm_direct':np.column_stack([np.ones(n),h,p,i,i**2,county,d.disruption_index.to_numpy()])}
def vif(X):
    out=[]
    for j in range(1,X.shape[1]):
        cols=[k for k in range(X.shape[1]) if k!=j]
        r=fit(X[:,j],X[:,cols]);out.append(1/(1-r['r2']))
    return out

if __name__=='__main__':
    ROOT.joinpath('data').mkdir(exist_ok=True)
    d,raw,audit,W,t=simulate();XX=design(d);y=d.poor_health_days.to_numpy()
    assert len(d)==736 and d.county_id.nunique()==46
    assert np.all(W.sum(axis=1)>0) and np.allclose(W.sum(axis=1),1)
    assert d.poor_health_days.between(0,30).all()
    assert (~raw.poor_health_days.between(0,30)).sum()==1
    assert np.all(d.groupby('county_id').heat_days.std()>0)
    models={name:fit(y,X) for name,X in XX.items()}
    expected={name:float(linalg.lstsq(X,t['mu_total'])[0][1]) for name,X in XX.items() if name!='m_direct'}
    assert abs(expected['m_fe']-.18)<1e-10
    assert abs(expected['m0']-.18)>.03
    assert max(vif(XX['m_collinear']))>100
    assert max(vif(XX['m1']))<10
    report={}
    for name,m in models.items():
        q=stats.t.ppf(.975,m['df']);e=m['res']
        # Breusch–Pagan-style auxiliary screen; p-value is provisional under spatial dependence.
        aux=fit(e**2,m['X'])
        report[name]={'heat_estimate':float(m['b'][1]),'ordinary_se':float(m['SE'][1]),
          'HC3_se':float(m['HC3'][1]),'ordinary_interval':[float(m['b'][1]-q*m['SE'][1]),float(m['b'][1]+q*m['SE'][1])],
          'r_squared':float(m['r2']),'moran_I':moran(e,W),
          'expected_heat_slope':expected.get(name),
          'BP_screen_LM':float(len(y)*aux['r2'])}
    report['vif_collinear']=vif(XX['m_collinear']);report['vif_repaired']=vif(XX['m1'])
    report['heat_range']=[int(d.heat_days.min()),int(d.heat_days.max())]
    report['health_range']=[float(y.min()),float(y.max())]
    report['truth']={'total_effect':.18,'direct_effect':.10,'rho':.70,
      'indirect_effect':.08,'effect_of_five_more_heat_days':.90}
    # Oracle GLS is a numerical validation benchmark, not a claimed sphet fit.
    SD=np.sqrt(t['sigma']**2+(.4*.35)**2)
    Xo=(t['B']@XX['m_fe'])/SD[:,None];yo=(t['B']@y)/SD
    oracle=fit(yo,Xo)
    report['oracle_GLS_known_covariance']={'heat_estimate':float(oracle['b'][1]),
      'heat_se':float(oracle['SE'][1]),'filtered_residual_I':moran(oracle['res'],W)}
    for name,data in [('sc_heat_health_2025_clean.csv',d),('sc_heat_health_2025_raw.csv',raw),('sc_heat_health_2025_audit.csv',audit)]:
        data.to_csv(ROOT/'data'/name,index=False,float_format='%.12f')
    (ROOT/'instructor/numerical_reference.json').write_text(json.dumps(report,indent=2))
    print(json.dumps(report,indent=2))
    print('PASS: data integrity, oracle mean, known effect, variance, multicollinearity and adjacency checks.')
