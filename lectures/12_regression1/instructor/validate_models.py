"""Independent refits of the saved teaching CSV. Instructor QA, not student setup."""
from pathlib import Path
import json
import numpy as np
import pandas as pd
from scipy.special import expit

root = Path(__file__).resolve().parents[1]
d = pd.read_csv(root / "data/urban_heat.csv")
ref = json.loads((root / "instructor/verified_results.json").read_text())
assert d.shape == (400, 13)
assert d.household_id.is_unique and not d.isna().any().any()
np.testing.assert_allclose(d.canopy10, d.canopy_pct / 10, atol=1e-9)
np.testing.assert_array_equal(d.person_weeks.to_numpy(), (d.household_members * d.weeks_observed).to_numpy())
assert set(d.heat_illness.unique()) == {0, 1}
assert (d.heat_episodes >= 0).all() and (d.heat_episodes % 1 == 0).all()

X = np.column_stack([np.ones(len(d)), d.canopy10, d.deprivation, d.cool_roof])
y = d.indoor_temp_c.to_numpy()
b = np.linalg.solve(X.T @ X, X.T @ y)
residual = y - X @ b
V = np.linalg.inv(X.T @ X) * (residual @ residual) / (len(d) - 4)
np.testing.assert_allclose(b, ref["precision"]["estimate"], atol=1e-8)
np.testing.assert_allclose(np.sqrt(np.diag(V)), ref["precision"]["se"], atol=1e-8)

def irls(y, X, family, offset=None):
    """Newton/IRLS fit, independent of the optimizer used to make previews."""
    offset = np.zeros(len(y)) if offset is None else offset
    b = np.zeros(X.shape[1])
    b[0] = -1 if family == "binomial" else -3
    for iteration in range(100):
        eta = X @ b + offset
        mu = expit(eta) if family == "binomial" else np.exp(eta)
        w = mu * (1 - mu) if family == "binomial" else mu
        information = X.T @ (X * w[:, None])
        delta = np.linalg.solve(information, X.T @ (y - mu))
        b += delta
        if np.max(np.abs(delta)) < 1e-11:
            break
    else:
        raise AssertionError("IRLS did not converge")
    eta = X @ b + offset
    mu = expit(eta) if family == "binomial" else np.exp(eta)
    w = mu * (1 - mu) if family == "binomial" else mu
    return b, np.sqrt(np.diag(np.linalg.inv(X.T @ (X * w[:, None])))), mu

lb, lse, lp = irls(d.heat_illness.to_numpy(), X, "binomial")
pb, pse, pm = irls(d.heat_episodes.to_numpy(), X, "poisson", np.log(d.person_weeks.to_numpy()))
eb, ese, em = irls(d.episodes_extra.to_numpy(), X, "poisson", np.log(d.person_weeks.to_numpy()))
for name, estimate, se in [("logistic", lb, lse), ("poisson", pb, pse), ("poisson_extra", eb, ese)]:
    np.testing.assert_allclose(estimate, ref[name]["estimate"], atol=1e-6)
    np.testing.assert_allclose(se, ref[name]["se"], atol=1e-7)
profiles = np.array([[1, 2, 0, 0], [1, 4, 0, 0]])
np.testing.assert_allclose(expit(profiles @ lb), ref["probabilities"], atol=1e-7)
np.testing.assert_allclose(np.exp(profiles @ pb) * 100, ref["rates100"], atol=1e-7)
np.testing.assert_allclose(np.sum((d.heat_episodes.to_numpy()-pm)**2/pm)/(len(d)-4),
                           ref["poisson_dispersion"], atol=1e-7)
np.testing.assert_allclose(np.sum((d.episodes_extra.to_numpy()-em)**2/em)/(len(d)-4),
                           ref["extra_dispersion"], atol=1e-7)
roof_mean_difference = d.loc[d.cool_roof == 1, "indoor_temp_c"].mean() - d.loc[d.cool_roof == 0, "indoor_temp_c"].mean()
np.testing.assert_allclose(roof_mean_difference, ref["binary"]["estimate"][1], atol=1e-9)
bread = np.linalg.inv(X.T @ X)
h = np.einsum("ij,jk,ik->i", X, bread, X)
scores = X * (residual / (1-h))[:, None]
hc3 = bread @ (scores.T @ scores) @ bread
np.testing.assert_allclose(np.sqrt(np.diag(hc3)), ref["hc3_se"], atol=1e-9)
print("PASS: CSV integrity; independent OLS/IRLS estimates and uncertainty; predictions; offsets; dispersion; binary contrast; HC3.")
