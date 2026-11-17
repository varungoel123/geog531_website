from pathlib import Path
import numpy as np
import pandas as pd
from scipy.special import expit
ROOT = Path(__file__).resolve().parents[1]
# Fixed-seed teaching simulation. Overwrites data/urban_heat.csv.
rng = np.random.default_rng(5312026)
n = 400
deprivation = rng.binomial(1, .5, n)
canopy = np.clip(rng.normal(48 - 18*deprivation, 12, n), 5, 78)
roof = rng.binomial(1, .4, n)
x = canopy/10
shade = .7*x + rng.normal(0, .45, n)
temp = 35 - .25*x - .5*shade + 2.3*deprivation - 1.2*roof + rng.normal(0, 1.15, n)
illness_p = expit(-.9 - .38*(x-3) + .9*deprivation - .6*roof)
illness = rng.binomial(1, illness_p)
people = rng.integers(1, 6, n)
weeks = rng.integers(8, 17, n)
pw = people*weeks
rate = np.exp(-3.9 - .28*(x-3) + .6*deprivation - .45*roof)
episodes = rng.poisson(pw*rate)
episodes_extra = rng.poisson(pw*rate*rng.gamma(1/1.5, 1.5, n))
df = pd.DataFrame(dict(household_id=[f'H{i+1:03d}' for i in range(n)], canopy_pct=canopy,
    canopy10=x, deprivation=deprivation, cool_roof=roof, shade_index=shade,
    indoor_temp_c=temp, heat_illness=illness, household_members=people,
    weeks_observed=weeks, person_weeks=pw, heat_episodes=episodes,
    episodes_extra=episodes_extra))
df.to_csv(ROOT/'data/urban_heat.csv', index=False, float_format='%.10f')

print("Regenerated data/urban_heat.csv with seed 5312026.")
