"""Create datasets"""
from datetime import date, datetime, timedelta

import numpy as np
from dateutil.relativedelta import relativedelta

from utils import (
    datetime_to_int,
    get_all_zip_event,
    get_census_features,
    get_columns,
    get_price_ts,
)


class SingleEventDataset:
    def __init__(
        self,
        hurricane_event: int,
        preceding_months: int = 12,
        ahead_month: int = 6,
        window=3,
    ):
        self.hurricane_event = hurricane_event
        self.zip_codes, self.vals, self.start_time, self.end_time = get_all_zip_event(
            hurricane_event
        )
        self.start_date = datetime.strptime(str(self.start_time), "%Y%m%d").date()
        self.end_date = datetime.strptime(str(self.end_time), "%Y%m%d").date()

        self.start_date_precede = self.start_date - relativedelta(
            months=preceding_months
        )
        self.start_date_precede_int = datetime_to_int(self.start_date_precede)
        self.price_ts, self.zip_codes = get_price_ts(
            self.start_date_precede_int, self.end_time, self.zip_codes
        )

        self.forecast_start_date = datetime_to_int(
            self.end_date + relativedelta(months=ahead_month - window)
        )
        self.forecast_end_date = datetime_to_int(
            self.end_date + relativedelta(months=ahead_month + window)
        )
        self.forecast_columns = get_columns(
            self.forecast_start_date, self.forecast_end_date
        )
        self.forecast_price_ts, self.zip_codes = get_price_ts(
            self.forecast_start_date, self.forecast_end_date, self.zip_codes
        )
        self.census_features = get_census_features(self.zip_codes)
        self.columns = get_columns(self.start_date_precede_int, self.end_time)

        self.features = np.concatenate((self.price_ts, self.census_features), axis=1)

        self.labels = self.forecast_price_ts.mean(axis=1)
        nan_idxs = np.argwhere(np.isnan(self.labels))
        self.features = np.delete(self.features, nan_idxs, axis=0)
        self.labels = np.delete(self.labels, nan_idxs, axis=0)
        self.zip_codes = np.delete(self.zip_codes, nan_idxs, axis=0)

    def __len__(self):
        self.features.shape[0]

    def __getitem__(self, idx):
        return self.features[idx], self.labels[idx]

    @property
    def num_features(self) -> int:
        return self.features.shape[1]

    @property
    def census_feature_size(self) -> int:
        return self.census_features.shape[1]

    @property
    def price_feature_size(self) -> int:
        return self.num_features - self.census_feature_size

    @property
    def time_series(self) -> np.ndarray:
        return self.features[:-self.census_feature_size]

def convert_to_classify_class(dataset: SingleEventDataset, threshold: float = 0.1) -> SingleEventDataset:
    means = dataset.time_series.mean(axis=1)
    labels = []
    for mean, lb in zip(means, dataset.labels):
        frac = (lb - mean) / mean
        if frac > 0.3:
            labels.append(0)
        elif frac > 0.1:
            labels.append(1)
        elif frac > -0.1:
            labels.append(2)
        elif frac > -0.3:
            labels.append(3)
    dataset.labels = np.array(labels)
    return dataset