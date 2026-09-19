from .common import *


def balance_training(x: np.ndarray, y: np.ndarray, seed: int):
    counts = np.bincount(y, minlength=2)
    if counts.min() == 0:
        raise RuntimeError("The verified training set contains only one class")
    if counts.min() < 2:
        sampler = RandomOverSampler(random_state=seed)
    else:
        sampler = SMOTE(random_state=seed, k_neighbors=min(5, counts.min() - 1))
    return sampler.fit_resample(x, y)


def model_templates(seed: int):
    return {
        "LR": LogisticRegression(max_iter=1000, solver="liblinear", random_state=seed),
        "SVM": SVC(kernel="linear", C=1.0, probability=True, random_state=seed),
        "XGB": XGBClassifier(
            n_estimators=120,
            max_depth=4,
            learning_rate=0.05,
            subsample=0.8,
            colsample_bytree=0.8,
            eval_metric="logloss",
            random_state=seed,
            n_jobs=2,
        ),
        "NB": __import__("sklearn.naive_bayes", fromlist=["GaussianNB"]).GaussianNB(),
        "RF": RandomForestClassifier(
            n_estimators=200, max_depth=None, min_samples_leaf=2, random_state=seed, n_jobs=2
        ),
        "KNN": KNeighborsClassifier(n_neighbors=5, weights="distance"),
        "AdaBoost": AdaBoostClassifier(n_estimators=100, learning_rate=0.5, random_state=seed),
        "ANN": MLPClassifier(
            hidden_layer_sizes=(32,), alpha=1e-4, max_iter=200, early_stopping=True,
            validation_fraction=0.15, random_state=seed,
        ),
    }


def fit_models(x: np.ndarray, y: np.ndarray, seed: int):
    xb, yb = balance_training(x, y, seed)
    fitted = {}
    for name, model in model_templates(seed).items():
        fitted[name] = clone(model).fit(xb, yb)
    return fitted


def predict_ensemble(models, x: np.ndarray, threshold: int):
    hard, proba = [], []
    for model in models.values():
        p = model.predict_proba(x)[:, 1]
        proba.append(p)
        hard.append((p >= 0.5).astype(int))
    hard = np.column_stack(hard)
    proba = np.column_stack(proba)
    return (hard.sum(axis=1) >= threshold).astype(int), proba.mean(axis=1), hard, proba

