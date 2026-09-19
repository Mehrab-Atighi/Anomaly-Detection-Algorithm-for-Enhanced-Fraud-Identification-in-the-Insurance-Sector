# Reference checks and intended use

## Current restoration: 40 cited references

The final article contains the 16 previously curated references plus 24 additions, all cited in the text. Twenty-three additions were matched to DOI registration metadata from Crossref on 17 September 2026; the cache is `added_reference_metadata.json`. The additional scikit-learn reference was verified at the [JMLR publisher page](https://jmlr.org/papers/v12/pedregosa11a.html). The bibliography is rebuilt offline from this cache by `01_Code/build_bibliography.py`; it does not invent entries or require live lookup at runtime.

The additions support the retained method descriptions: LOF, distance outliers, K-Modes, caret preprocessing, random forest, SVM, XGBoost, naive Bayes, robust estimation, Grubbs, entropy, KNN classification, AdaBoost, neural networks, statistical learning, minority-class evaluation, imbalance, anomaly detection and Isolation Forest. Bolton–Hand, Ngai et al., West–Bhattacharya and Debener et al. provide additional fraud/insurance context. These are method and literature references, not extra claimed experiments.

Checks corrected an initially suspected Ngai DOI: the intended paper is in *Decision Support Systems* 50(3), 559–569, DOI `10.1016/j.dss.2010.08.006`, not an Expert Systems with Applications paper. Author details are taken from the DOI record and the [author institution page](https://research.polyu.edu.hk/en/publications/the-application-of-data-mining-techniques-in-financial-fraud-dete/). The incorrect candidate was excluded.

Selected primary publication pages also checked: [K-Modes](https://link.springer.com/article/10.1023/A:1009769707641), [caret](https://www.jstatsoft.org/article/view/v028i05), [Random Forests](https://link.springer.com/article/10.1023/A:1010933404324), [SVM](https://link.springer.com/article/10.1007/BF00994018), [naive Bayes](https://link.springer.com/article/10.1023/A:1007413511361), [statistical learning](https://link.springer.com/book/10.1007/978-0-387-84858-7), [XGBoost author preprint](https://arxiv.org/abs/1603.02754), and [precision–recall evaluation](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0118432). Some publisher pages blocked full-text retrieval; DOI metadata validation is distinguished from full-text access. The bibliography does not imply that every publication's full text was accessible.

## Previously checked insurance and Annals references

The three requested journal references were checked against Springer Nature publisher pages. They are cited in the relevant literature paragraphs rather than appended as unrelated bibliography padding.

| Key | Verified publication | Role |
|---|---|---|
| Kamalov2023 | Kamalov, Thabtah & Leung, *Feature Selection in Imbalanced Data*, Annals of Data Science 10, 1527–1541 (2023). [Publisher](https://link.springer.com/article/10.1007/s40745-021-00366-5) | Minority-sensitive feature selection context; not claimed as the Gini method implemented here |
| XuTian2015 | Xu & Tian, *A Comprehensive Survey of Clustering Algorithms*, Annals of Data Science 2, 165–193 (2015). [Publisher](https://link.springer.com/article/10.1007/s40745-015-0040-1) | Representation and clustering context |
| Hassani2020 | Hassani, Huang, Silva & Ghodsi, *Deep Learning and Implementations in Banking*, Annals of Data Science 7, 433–446 (2020). [Publisher](https://link.springer.com/article/10.1007/s40745-020-00300-1) | Broader financial-domain context, not an insurance-fraud empirical comparator |

The first paper appeared online in 2022 but is cited using its 2023 volume year. The other references retained from the prior revision include actual insurance-specific studies, distinguished from what this package empirically implements.

Additional checks relevant to revised claims:

- Ding et al. (2025), PSO-XGBoost, volume 120, pp. 51–60: [publisher volume](https://www.sciencedirect.com/journal/insurance-mathematics-and-economics/vol/120/suppl/C), DOI `10.1016/j.insmatheco.2024.11.006`. The package runs an **adapted**, explicitly bounded PSO search, not an exact reproduction of the paper.
- Banulescu-Radu and Yankol-Schalck (2024), household insurance guideline: [publisher](https://onlinelibrary.wiley.com/doi/abs/10.1111/jori.12452).
- Khalil et al. (2024), missing values and imbalance, DOI `10.1109/ACCESS.2024.3468993`: [author institution record](https://findanexpert.unimelb.edu.au/scholarlywork/1971360-machine-learning-based-method-for-insurance-fraud-detection-on-class-imbalance-datasets-with-missing-values). This check corrected two inherited given names to **Zaiming Liu** and **Ahmad Salah**. The IEEE page was discoverable but direct PDF retrieval was unavailable.
- Nijhuis and van Lelyveld (2023), costly verification and reinforcement learning: [publisher](https://www.mdpi.com/1099-4300/25/6/842), DOI `10.3390/e25060842`. SAD is explicitly distinguished from a reinforcement-learning policy.
- Vorobyev (2024), claims graphs: [publisher](https://www.sciencedirect.com/science/article/abs/pii/S0957417424009758), DOI `10.1016/j.eswa.2024.124109`. Included as literature context, not an executed graph comparator.

Dataset references point to benchmark-hosting context. The executed CSV bytes come from the supplied thesis archive, not a fresh authenticated publisher data release. No claim is made that a hosting page proves independent insurer provenance. The undated Insurance Claims listing is cited as n.d. rather than with an invented collection year.

The final editorial pass records the researcher-confirmed second-dataset source as mexwell / Insurance Claims on Kaggle and includes its complete URL in the visible bibliography, accessed 19 September 2026. The supplied local CSV bytes remain unchanged.
