from .common import *


def metrics_row(y: np.ndarray, pred: np.ndarray, score: np.ndarray, **meta):
    tn, fp, fn, tp = confusion_matrix(y, pred, labels=[0, 1]).ravel()
    row = dict(meta)
    row.update(
        n=len(y), fraud_n=int(y.sum()), alerted_n=int(pred.sum()),
        TP=int(tp), FP=int(fp), TN=int(tn), FN=int(fn),
        accuracy=accuracy_score(y, pred),
        precision=precision_score(y, pred, zero_division=0),
        recall=recall_score(y, pred, zero_division=0),
        f1=f1_score(y, pred, zero_division=0),
        specificity=tn / (tn + fp) if (tn + fp) else np.nan,
        balanced_accuracy=balanced_accuracy_score(y, pred),
        roc_auc=roc_auc_score(y, score) if len(np.unique(y)) == 2 else np.nan,
        pr_auc=average_precision_score(y, score) if len(np.unique(y)) == 2 else np.nan,
        alert_rate=float(pred.mean()),
    )
    return row


def aggregate(raw: pd.DataFrame) -> pd.DataFrame:
    metrics = [
        "accuracy", "precision", "recall", "f1", "specificity",
        "balanced_accuracy", "roc_auc", "pr_auc", "alert_rate", "review_rate",
    ]
    out = raw.groupby(["dataset", "architecture", "partition"])[metrics].agg(["mean", "std"])
    out.columns = [f"{a}_{b}" for a, b in out.columns]
    return out.reset_index()
