"""Meaningful safeguards for entropy, positive class, splits and review policy."""
import unittest
import numpy as np
import pandas as pd
from sad.common import SADConfig
from sad.PreProcessing import FeaturePlan, make_encoder, stratified_partitions
from sad.EntropyMethod import entropy_gain_scores
from sad.Labeling import reviewed_indices
from sad.Evaluation import metrics_row
from sad.FinalModeling import initial

class Invariants(unittest.TestCase):
    def test_entropy_against_exact_leave_one_out(self):
        x = pd.DataFrame({"category": list("aaaabbcd")})
        def h(z):
            p = pd.Series(z).value_counts(normalize=True)
            return float(-(p*np.log2(p)).sum())
        expected = np.array([h(x.category)-h(x.category.drop(i)) for i in x.index])
        np.testing.assert_allclose(entropy_gain_scores(x, [], ["category"]), expected, atol=1e-12)

    def test_cascade_row_mapping_and_candidate_entropy(self):
        x = pd.DataFrame({"c": ["a","a","a","b","a","c","d","d"]})
        votes = pd.DataFrame(np.zeros((8,9),int))
        candidates = [1,3,5,7]; votes.iloc[candidates,:3] = 1
        plan = type("Plan", (), {"numeric_":[], "categorical_":["c"]})()
        prep = dict(votes=votes, xp=[x], plan=plan)
        selected, _ = initial(prep, "multilayer", SADConfig(tad=3))
        self.assertEqual(selected.sum(), 1)
        self.assertTrue(set(np.flatnonzero(selected)).issubset(candidates))

    def test_class_one_metrics(self):
        row = metrics_row(np.array([0,0,0,1,1]), np.array([0,0,1,1,0]),
                          np.array([.1,.2,.7,.9,.3]))
        self.assertEqual((row["TP"],row["FP"],row["TN"],row["FN"]),(1,1,2,1))
        self.assertEqual(row["recall"], .5)
        self.assertAlmostEqual(row["specificity"], 2/3)

    def test_splits_partition_every_row_once(self):
        y = np.r_[np.zeros(923,int), np.ones(77,int)]
        parts = stratified_partitions(y,1234)
        joined = np.concatenate(parts)
        self.assertEqual(sorted(joined.tolist()), list(range(1000)))
        self.assertTrue(all(len(np.unique(y[p])) == 2 for p in parts))

    def test_review_contains_alerts_and_fixed_unlabeled_audit(self):
        hit = np.zeros(100,bool); hit[[2,7,31]] = True
        one = reviewed_indices(hit,.1,np.random.default_rng(10))
        two = reviewed_indices(hit,.1,np.random.default_rng(10))
        np.testing.assert_array_equal(one,two)
        self.assertEqual(len(one),3+10)
        self.assertTrue(set([2,7,31]).issubset(one))

    def test_preprocessor_not_refitted_to_future(self):
        x = pd.DataFrame({"age":np.arange(100)%30,"group":["a","b"]*50,
                          "PolicyNumber":np.arange(100)})
        plan = FeaturePlan(SADConfig()).fit(x)
        self.assertNotIn("PolicyNumber",plan.keep_)
        encoder = make_encoder(plan).fit(plan.transform(x))
        center = encoder.named_transformers_["num"].named_steps["scale"].mean_.copy()
        future = x.copy(); future["age"] = 1e10; future["group"] = "new"
        z = encoder.transform(plan.transform(future))
        self.assertTrue(np.isfinite(z).all())
        np.testing.assert_array_equal(center,encoder.named_transformers_["num"].named_steps["scale"].mean_)

if __name__ == "__main__": unittest.main(verbosity=2)
