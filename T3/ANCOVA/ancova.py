from typing import Optional, List, Union, Dict
import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
from statsmodels.multivariate.manova import MANOVA
from statsmodels.stats.multicomp import MultiComparison
from statsmodels.stats.anova import anova_lm
from scipy.stats import shapiro, bartlett, chi2
from numpy.linalg import inv
import matplotlib.pyplot as plt
import seaborn as sns


def mardia_test(data: pd.DataFrame) -> Dict[str, Union[float, bool]]:
    """
    Perform Mardia's multivariate normality test.

    Parameters
    ----------
    data : pd.DataFrame
        DataFrame with only the response variables.

    Returns
    -------
    Dict[str, Union[float, bool]]
        Dictionary with skewness, kurtosis statistics and test results.
    """
    n, p = data.shape
    centered = data - data.mean()
    X = centered.to_numpy()
    sigma_inv = inv(np.cov(X.T))

    # Skewness
    b1p = np.sum([((x @ sigma_inv @ x.T) ** 3) for x in X]) / n
    chi_skew = n * b1p / 6
    df_skew = p * (p + 1) * (p + 2) / 6
    p_skew = 1 - chi2.cdf(chi_skew, df=df_skew)

    # Kurtosis
    d_squared = np.diag(X @ sigma_inv @ X.T)
    b2p = np.mean(d_squared ** 2)
    expected_kurt = p * (p + 2)
    z_kurt = (b2p - expected_kurt) / np.sqrt(8 * expected_kurt / n)

    return {
        "Skewness stat": b1p,
        "p-value skewness": p_skew,
        "Kurtosis stat": b2p,
        "z kurtosis": z_kurt,
        "Multivariate normality": (p_skew > 0.05 and abs(z_kurt) < 1.96)
    }


class ANCOVA:
    """
    Perform ANCOVA or MANCOVA analysis, including diagnostics and assumptions.

    Parameters
    ----------
    data : pd.DataFrame
        Dataset including all variables.
    response : Union[str, List[str]]
        Response variable name(s).
    factor : str
        Main categorical factor.
    covariates : List[str]
        List of continuous covariate names.
    block : Optional[str]
        Blocking variable name, if applicable.
    subject : Optional[str]
        Nested or subject variable name, if applicable.
    """

    def __init__(
        self,
        data: pd.DataFrame,
        response: Union[str, List[str]],
        factor: str,
        covariates: List[str],
        block: Optional[str] = None,
        subject: Optional[str] = None
    ) -> None:
        self.data: pd.DataFrame = data.copy()
        self.response: List[str] = [response] if isinstance(response, str) else response
        self.factor: str = factor
        self.covariates: List[str] = covariates
        self.block: Optional[str] = block
        self.subject: Optional[str] = subject
        self.model: Union[sm.regression.linear_model.RegressionResultsWrapper, MANOVA, None] = None
        self.result: Union[pd.DataFrame, str, None] = None
        self.manova: bool = False

    def fit(self) -> None:
        """
        Fit the ANCOVA or MANCOVA model.
        """
        rhs = f"C({self.factor}) + {' + '.join(self.covariates)}"
        if self.block:
            rhs += f" + C({self.block})"
        if self.subject:
            rhs += f" + C({self.subject})"

        if len(self.response) == 1:
            formula = f"{self.response[0]} ~ {rhs}"
            self.model = smf.ols(formula=formula, data=self.data).fit()
            self.result = anova_lm(self.model, typ=2)
        else:
            lhs = " + ".join(self.response)
            formula = f"{lhs} ~ {rhs}"
            self.model = MANOVA.from_formula(formula, data=self.data)
            self.result = self.model.mv_test()
            self.manova = True

    def summary(self) -> Union[pd.DataFrame, str]:
        """
        Return ANOVA or MANOVA summary results.

        Returns
        -------
        Union[pd.DataFrame, str]
            ANOVA table or MANOVA test summary.
        """
        if self.result is None:
            raise RuntimeError("Model must be fitted first.")
        return self.result

    def coefficients(self) -> pd.DataFrame:
        """
        Return estimated model coefficients (only for ANCOVA).

        Returns
        -------
        pd.DataFrame
            Coefficients and standard errors.
        """
        if self.manova:
            raise NotImplementedError("Coefficients are not available for MANOVA.")
        return self.model.summary2().tables[1]

    def posthoc(self, method: str = "bonferroni") -> pd.DataFrame:
        """
        Perform post-hoc comparisons using pairwise t-tests (only for ANCOVA).

        Parameters
        ----------
        method : str, optional
            Correction method (default is 'bonferroni').

        Returns
        -------
        pd.DataFrame
            Pairwise comparisons table.
        """
        if self.manova:
            raise NotImplementedError("Post-hoc tests are only available for ANCOVA.")
        mc = MultiComparison(self.model.model.endog, self.data[self.factor])
        result = mc.allpairtest(sm.stats.ttest_ind, method=method)
        return result[0]

    def test_normality(self) -> Dict[str, Union[float, bool]]:
        """
        Perform Shapiro-Wilk test on residuals (ANCOVA only).

        Returns
        -------
        dict
            Test statistic, p-value and decision.
        """
        if self.manova:
            raise NotImplementedError("Use test_multivariate_normality for MANCOVA.")
        stat, p_value = shapiro(self.model.resid)
        return {"W": stat, "p-value": p_value, "Normality": p_value > 0.05}

    def test_homoscedasticity(self) -> Dict[str, Union[float, bool]]:
        """
        Perform Bartlett's test across factor levels (ANCOVA only).

        Returns
        -------
        dict
            Bartlett statistic, p-value and decision.
        """
        if self.manova:
            raise NotImplementedError("Homoscedasticity test not available for MANCOVA.")
        groups = self.data[self.factor].unique()
        group_resids = [self.model.resid[self.data[self.factor] == g] for g in groups]
        stat, p_value = bartlett(*group_resids)
        return {"Bartlett's stat": stat, "p-value": p_value, "Equal variances": p_value > 0.05}

    def test_multivariate_normality(self) -> Dict[str, Union[float, bool]]:
        """
        Perform Mardia's test for multivariate normality (MANCOVA only).

        Returns
        -------
        dict
            Skewness, kurtosis stats and decision.
        """
        if not self.manova:
            raise NotImplementedError("This test is only valid for MANCOVA.")
        return mardia_test(self.data[self.response])

    def plot_diagnostics(self) -> None:
        """
        Plot residual diagnostics: QQ-plot and Residuals vs Fitted (ANCOVA only).
        """
        if self.manova:
            raise NotImplementedError("Diagnostics plot only available for ANCOVA.")

        residuals = self.model.resid
        fitted = self.model.fittedvalues

        fig, axes = plt.subplots(1, 2, figsize=(12, 5))

        sm.qqplot(residuals, line='s', ax=axes[0])
        axes[0].set_title("QQ-plot of Residuals")

        sns.scatterplot(x=fitted, y=residuals, ax=axes[1])
        axes[1].axhline(0, color='gray', linestyle='--')
        axes[1].set_xlabel("Fitted values")
        axes[1].set_ylabel("Residuals")
        axes[1].set_title("Residuals vs Fitted")

        plt.tight_layout()
        plt.show()
