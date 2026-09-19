"""Additional imbalance comparators on the SAME Simple-B verified records.

PSO-XGB is an explicitly adapted search inspired by Ding et al. (2025), not
an exact reproduction of their unavailable training protocol/data.
Hyperparameters are selected once using ONLY verified D1 records, with
stratified 3-fold CV; SMOTE is fitted separately within each fold.
"""
import numpy as np
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import average_precision_score
from imblearn.ensemble import BalancedRandomForestClassifier
from xgboost import XGBClassifier
from .Modeling import balance_training

def pso_parameters(position):
    return dict(
        n_estimators=int(np.round(60 + 100 * position[0])),
        max_depth=int(np.round(2 + 4 * position[1])),
        learning_rate=float(10 ** (-2 + 1.3 * position[2])),
        subsample=float(.6 + .4 * position[3]),
        colsample_bytree=float(.6 + .4 * position[4]),
    )

def tune_pso(x, y, seed, particles=4, iterations=3):
    rng = np.random.default_rng(seed)
    folds = min(3, int(np.bincount(y, minlength=2).min()))
    if folds < 2:
        raise ValueError("Too few verified minority records for PSO CV.")
    cv = list(StratifiedKFold(folds, shuffle=True, random_state=seed).split(x, y))
    positions = rng.uniform(0, 1, (particles, 5))
    velocities = np.zeros_like(positions)
    personal = positions.copy(); personal_scores = np.full(particles, -np.inf)
    best = positions[0].copy(); best_score = -np.inf
    trace = []; cache = {}
    for step in range(iterations):
        for particle in range(particles):
            params = pso_parameters(positions[particle])
            key = tuple(params.values())
            if key not in cache:
                scores = []
                for fold, (tr, va) in enumerate(cv):
                    xb, yb = balance_training(x[tr], y[tr], seed + fold)
                    model = XGBClassifier(**params, eval_metric="logloss",
                        tree_method="hist", n_jobs=1, random_state=seed + fold)
                    model.fit(xb, yb)
                    scores.append(average_precision_score(y[va], model.predict_proba(x[va])[:, 1]))
                cache[key] = float(np.mean(scores))
            score = cache[key]
            trace.append(dict(iteration=step, particle=particle, cv_ap=score, **params))
            if score > personal_scores[particle]:
                personal_scores[particle] = score
                personal[particle] = positions[particle].copy()
            if score > best_score:
                best_score = score; best = positions[particle].copy()
        velocities = (.7 * velocities + 1.4 * rng.random(positions.shape) * (personal - positions)
                      + 1.4 * rng.random(positions.shape) * (best - positions))
        positions = np.clip(positions + velocities, 0, 1)
    return pso_parameters(best), trace

def fit_comparators(x, y, seed, pso_params):
    # Balanced RF uses within-tree undersampling, so receives no prior SMOTE.
    brf = BalancedRandomForestClassifier(n_estimators=200, min_samples_leaf=2,
        sampling_strategy="all", replacement=True, bootstrap=False,
        random_state=seed, n_jobs=1).fit(x, y)
    xb, yb = balance_training(x, y, seed)
    pso = XGBClassifier(**pso_params, eval_metric="logloss", tree_method="hist",
        random_state=seed, n_jobs=1).fit(xb, yb)
    return {"BalancedRF": brf, "PSO-XGB-adapted": pso}
