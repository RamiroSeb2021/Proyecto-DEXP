"""
Experimental Design Implementations in Python

This module contains classes that implement different experimental design scenarios:
1. Completely Randomized Block Design (CRBD).
2. CRBD with one missing observation and its estimation.
3. Balanced Incomplete Block Design (BIBD).
4. Blocked Design with Subsampling (BDWS).

The implementation is aligned with PEP8, mypy, pylint, and flake8 standards. Each class includes documentation and methods for performing the appropriate ANOVA or data handling.
"""

import pandas as pd
import numpy as np
import statsmodels.api as sm
import statsmodels.formula.api as smf
from typing import Optional


class CompletelyRandomizedBlockDesign:
    """
    Class to handle analysis of variance for a Completely Randomized Block Design (CRBD).
    """
    def __init__(self, data: pd.DataFrame) -> None:
        self.data = data

    def perform_anova(self) -> pd.DataFrame:
        model = smf.ols("Produccion ~ C(Suelo) + C(Tratamiento)", data=self.data).fit()
        return sm.stats.anova_lm(model, typ=2)


class CRBDWithMissingValue:
    """
    Class to handle CRBD with one missing value, including estimation.
    """
    def __init__(self, data: pd.DataFrame) -> None:
        self.data = data.copy()

    def estimate_missing_value(self) -> float:
        pivot_table = self.data.pivot(index="Bloque", columns="Tratamiento", values="Respuesta")
        row_means = pivot_table.mean(axis=1)
        col_means = pivot_table.mean(axis=0)
        grand_mean = self.data["Respuesta"].mean(skipna=True)

        missing = self.data[self.data["Respuesta"].isnull()].iloc[0]
        row = missing["Bloque"]
        col = missing["Tratamiento"]

        return row_means[row] + col_means[col] - grand_mean

    def fill_and_analyze(self) -> pd.DataFrame:
        estimated_value = self.estimate_missing_value()
        self.data.loc[self.data["Respuesta"].isnull(), "Respuesta"] = estimated_value
        model = smf.ols("Respuesta ~ C(Bloque) + C(Tratamiento)", data=self.data).fit()
        return sm.stats.anova_lm(model, typ=2)


class BalancedIncompleteBlockDesign:
    """
    Class to analyze a Balanced Incomplete Block Design (BIBD) with missing data.
    """
    def __init__(self, data: pd.DataFrame) -> None:
        self.data = data.copy()

    def clean_data(self) -> pd.DataFrame:
        return self.data.dropna()

    def perform_anova(self) -> pd.DataFrame:
        clean_data = self.clean_data()
        model = smf.ols("Tiempo ~ C(Catalizador) + C(Lote)", data=clean_data).fit()
        return sm.stats.anova_lm(model, typ=2)


class BlockedDesignWithSubsampling:
    """
    Class to analyze a Blocked Design with Subsampling.
    Each block-treatment combination has multiple subobservations.
    Allows specifying custom column names for flexibility.
    """
    def __init__(
        self,
        data: pd.DataFrame,
        block_col: str = "Bloque",
        treatment_col: str = "Tratamiento",
        response_col: str = "Respuesta"
    ) -> None:
        self.data = data.copy()
        self.block_col = block_col
        self.treatment_col = treatment_col
        self.response_col = response_col

    def perform_nested_anova(self) -> pd.DataFrame:
        """
        Perform nested ANOVA to account for subsampling.

        Returns:
            pd.DataFrame: ANOVA table with nesting.
        """
        formula = f"{self.response_col} ~ C({self.block_col}) + C({self.treatment_col}) + C({self.treatment_col}):C({self.block_col})"
        model = smf.ols(formula, data=self.data).fit()
        return sm.stats.anova_lm(model, typ=2)


# --- Contextual Usage ---

# crbd = CompletelyRandomizedBlockDesign(pd.read_csv("ejemplo_dbca.csv"))
# print(crbd.perform_anova())

# crbd_mv = CRBDWithMissingValue(pd.read_csv("ejemplo_dbca_faltante.csv"))
# print("Estimated missing value:", crbd_mv.estimate_missing_value())
# print(crbd_mv.fill_and_analyze())

# bibd = BalancedIncompleteBlockDesign(pd.read_csv("ejemplo_dbib.csv"))
# print(bibd.perform_anova())

# bdws = BlockedDesignWithSubsampling(
#     pd.read_csv("ejemplo_submuestreo.csv"),
#     block_col="TIEMPO",
#     treatment_col="NITROGENO",
#     response_col="CANTIDAD"
# )
# print(bdws.perform_nested_anova())

# --- Conclusions ---
# - CRBD evaluates treatment effects with block control.
# - Missing values can be estimated with balanced means.
# - BIBD allows inference under incomplete structure.
# - Subsampling designs separate observational and experimental variability.
