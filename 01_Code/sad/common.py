from __future__ import annotations

import argparse
import json
import math
import os
import time
import warnings
from dataclasses import asdict, dataclass
from pathlib import Path

import numpy as np
import pandas as pd
from imblearn.over_sampling import RandomOverSampler, SMOTE
from kmodes.kmodes import KModes
from scipy.stats import t
from sklearn.base import clone
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import AdaBoostClassifier, IsolationForest, RandomForestClassifier
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    accuracy_score,
    average_precision_score,
    balanced_accuracy_score,
    confusion_matrix,
    f1_score,
    precision_score,
    recall_score,
    roc_auc_score,
)
from sklearn.neighbors import KNeighborsClassifier, LocalOutlierFactor, NearestNeighbors
from sklearn.neural_network import MLPClassifier
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler
from sklearn.svm import SVC
from xgboost import XGBClassifier


@dataclass(frozen=True)
class SADConfig:
    partition_fractions: tuple[float, float, float, float] = (0.50, 0.30, 0.10, 0.10)
    tad: int = 3
    tml: int = 4
    normal_audit_fraction: float = 0.10
    entropy_tail_fraction: float = 0.20
    lof_fraction: float = 0.15
    knn_fraction: float = 0.20
    iforest_fraction: float = 0.10
    gini_lower: float = 0.10
    gini_upper: float = 0.90
    z_threshold: float = 3.0
    random_state: int = 1234

DATASETS = {"Carclaims":{"filename":"carclaims.csv","target":"FraudFound"}, "InsuranceClaims":{"filename":"insurance_claims.csv","target":"fraud_reported"}}
