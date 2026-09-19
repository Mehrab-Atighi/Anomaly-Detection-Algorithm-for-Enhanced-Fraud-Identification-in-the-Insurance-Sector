from .common import *


def to_binary_target(s: pd.Series) -> np.ndarray:
    values = s.astype(str).str.strip().str.lower()
    positive = {"1", "yes", "y", "true", "fraud", "fraudulent"}
    negative = {"0", "no", "n", "false", "non-fraud", "nonfraud"}
    unknown = set(values.unique()) - positive - negative
    if unknown:
        raise ValueError(f"Unrecognized target values: {sorted(unknown)}")
    return values.isin(positive).astype(int).to_numpy()


def engineer_dates(x: pd.DataFrame) -> pd.DataFrame:
    """Convert date-named columns to year/month/weekday without using labels."""
    out = x.copy()
    for col in list(out.columns):
        if col not in {"policy_bind_date", "incident_date"}:
            continue
        parsed = pd.to_datetime(out[col], format="%m/%d/%Y", errors="coerce")
        out[f"{col}__year"] = parsed.dt.year.astype("float")
        out[f"{col}__month"] = parsed.dt.month.astype("float")
        out[f"{col}__weekday"] = parsed.dt.dayofweek.astype("float")
        out = out.drop(columns=[col])
    return out


def stratified_partitions(y: np.ndarray, seed: int, fractions=(0.5, 0.3, 0.1, 0.1)):
    rng = np.random.default_rng(seed)
    pieces = [[] for _ in fractions]
    for cls in (0, 1):
        idx = np.flatnonzero(y == cls)
        rng.shuffle(idx)
        raw = np.asarray(fractions) * len(idx)
        counts = np.floor(raw).astype(int)
        for j in np.argsort(raw - counts)[::-1][: len(idx) - counts.sum()]:
            counts[j] += 1
        start = 0
        for j, count in enumerate(counts):
            pieces[j].extend(idx[start : start + count].tolist())
            start += count
    for p in pieces:
        rng.shuffle(p)
    return [np.asarray(p, dtype=int) for p in pieces]


class FeaturePlan:
    """Feature screening learned from the unlabeled first partition only."""

    def __init__(self, cfg: SADConfig):
        self.cfg = cfg
        self.keep_: list[str] = []
        self.numeric_: list[str] = []
        self.categorical_: list[str] = []
        self.dropped_: dict[str, str] = {}

    @staticmethod
    def _gini(s: pd.Series) -> float:
        p = s.fillna("__MISSING__").astype(str).value_counts(normalize=True)
        return float(1.0 - np.square(p).sum())

    @staticmethod
    def _near_zero_variance(s: pd.Series) -> bool:
        counts = s.value_counts(dropna=False)
        if len(counts) <= 1:
            return True
        freq_ratio = counts.iloc[0] / max(counts.iloc[1], 1)
        pct_unique = 100.0 * len(counts) / len(s)
        return bool(freq_ratio > 19.0 and pct_unique <= 10.0)

    def fit(self, x_first: pd.DataFrame):
        x_first = engineer_dates(x_first).replace("?", np.nan)
        for col in x_first.columns:
            s = x_first[col]
            unique_ratio = s.nunique(dropna=True) / max(len(s), 1)
            name = col.lower()
            identifier_name = name in {"policy_number", "policynumber", "insured_zip", "incident_location"}
            if identifier_name or (not pd.api.types.is_numeric_dtype(s) and unique_ratio > 0.98):
                self.dropped_[col] = "identifier/high-cardinality"
                continue
            if s.isna().all():
                self.dropped_[col] = "all missing in D1"
                continue
            if pd.api.types.is_numeric_dtype(s):
                if self._near_zero_variance(s):
                    self.dropped_[col] = "near-zero variance"
                else:
                    self.numeric_.append(col)
            else:
                gini = self._gini(s)
                if self.cfg.gini_lower < gini < self.cfg.gini_upper:
                    self.categorical_.append(col)
                else:
                    self.dropped_[col] = f"categorical Gini={gini:.4f}"
        self.keep_ = self.numeric_ + self.categorical_
        if not self.keep_:
            raise ValueError("Feature screening removed every predictor")
        return self

    def transform(self, x: pd.DataFrame) -> pd.DataFrame:
        out = engineer_dates(x).replace("?", np.nan)
        for col in self.keep_:
            if col not in out:
                out[col] = np.nan
        return out[self.keep_].copy()


def make_encoder(plan: FeaturePlan) -> ColumnTransformer:
    num = Pipeline(
        [("imputer", SimpleImputer(strategy="median")), ("scale", StandardScaler())]
    )
    cat = Pipeline(
        [
            ("imputer", SimpleImputer(strategy="most_frequent")),
            ("onehot", OneHotEncoder(handle_unknown="ignore", sparse_output=False)),
        ]
    )
    return ColumnTransformer(
        [("num", num, plan.numeric_), ("cat", cat, plan.categorical_)],
        remainder="drop",
        sparse_threshold=0,
    )


def numeric_matrix(x: pd.DataFrame, cols: list[str]) -> np.ndarray:
    if not cols:
        return np.empty((len(x), 0))
    z = x[cols].apply(pd.to_numeric, errors="coerce")
    z = z.fillna(z.median()).fillna(0.0)
    return z.to_numpy(dtype=float)


def categorical_matrix(x: pd.DataFrame, cols: list[str]) -> np.ndarray:
    if not cols:
        return np.empty((len(x), 0), dtype=str)
    return x[cols].fillna("__MISSING__").astype(str).to_numpy()
