from .common import *


def entropy_gain_scores(x: pd.DataFrame, numeric: list[str], categorical: list[str]) -> np.ndarray:
    r"""Mean H(D)-H(D\{x_i}) across discretized predictors, computed in O(np)."""
    coded = pd.DataFrame(index=x.index)
    for col in numeric:
        s = pd.to_numeric(x[col], errors="coerce")
        try:
            coded[col] = pd.qcut(s, q=min(10, max(2, s.nunique())), duplicates="drop").astype(str)
        except ValueError:
            coded[col] = s.fillna(s.median()).astype(str)
    for col in categorical:
        coded[col] = x[col].fillna("__MISSING__").astype(str)
    n = len(coded)
    if n <= 2 or coded.shape[1] == 0:
        return np.zeros(n)
    gains = np.zeros(n, dtype=float)
    for col in coded:
        s = coded[col].fillna("__MISSING__")
        counts = s.value_counts()
        c = s.map(counts).to_numpy(dtype=float)
        sum_clogc = float(np.sum(counts.to_numpy() * np.log2(counts.to_numpy())))
        h_full = math.log2(n) - sum_clogc / n
        cm1 = c - 1.0
        changed = np.where(cm1 > 0, cm1 * np.log2(np.maximum(cm1, 1)), 0.0)
        h_minus = math.log2(n - 1) - (sum_clogc - c * np.log2(c) + changed) / (n - 1)
        gains += h_full - h_minus
    return gains / coded.shape[1]

