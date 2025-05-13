import numpy as np
import pandas as pd
import scipy.stats as stats
from numpy.linalg import inv, det

class MANOVA:
    def __init__(self, data: pd.DataFrame, group_col: str, response_cols: list[str]):
        """
        Initializes the MANOVA analysis.
        
        :param data: DataFrame containing the data.
        :param group_col: Column name indicating the grouping variable.
        :param response_cols: List of column names representing the dependent variables.
        """
        self.data = data
        self.group_col = group_col
        self.response_cols = response_cols
        self.groups = self.data[group_col].unique()
        self.k = len(self.groups)  # Number of groups
        self.p = len(response_cols)  # Number of dependent variables
        self.n = len(data)  # Total sample size
        self.group_sizes = {g: len(data[data[group_col] == g]) for g in self.groups}

    def compute_means(self):
        """Computes the mean vector for each group and overall mean vector."""
        self.group_means = {g: self.data[self.data[self.group_col] == g][self.response_cols].mean().values for g in self.groups}
        self.overall_mean = self.data[self.response_cols].mean().values

    def compute_SSB_SSW(self):
        """Computes the between-group (SSB) and within-group (SSW) scatter matrices."""
        self.SSB = np.zeros((self.p, self.p))
        self.SSW = np.zeros((self.p, self.p))

        for g in self.groups:
            ng = self.group_sizes[g]
            mean_diff = (self.group_means[g] - self.overall_mean).reshape(-1, 1)
            self.SSB += ng * (mean_diff @ mean_diff.T)

            group_data = self.data[self.data[self.group_col] == g][self.response_cols].values
            for row in group_data:
                row_diff = (row - self.group_means[g]).reshape(-1, 1)
                self.SSW += row_diff @ row_diff.T

    def compute_lambda_wilks(self):
        """Computes Wilks' Lambda statistic."""
        det_SSW = det(self.SSW)
        det_SSB_SSW = det(self.SSB + self.SSW)
        self.lambda_wilks = det_SSW / det_SSB_SSW

    def compute_f_statistic(self):
        """Computes the transformed F-statistic from Wilks' Lambda."""
        d1 = self.p * (self.k - 1)
        d2 = self.n - self.k - self.p + 1
        term = (1 - self.lambda_wilks) / self.lambda_wilks
        self.f_stat = (d2 / d1) * term
        self.p_value = 1 - stats.f.cdf(self.f_stat, d1, d2)

    def fit(self):
        """Runs the MANOVA procedure and returns results."""
        self.compute_means()
        self.compute_SSB_SSW()
        self.compute_lambda_wilks()
        self.compute_f_statistic()

        return {
            "Wilks' Lambda": self.lambda_wilks,
            "F-Statistic": self.f_stat,
            "p-Value": self.p_value
        }
