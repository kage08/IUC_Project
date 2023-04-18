from datetime import date, datetime
from typing import List

import pandas as pd

from consts import CENSUS_FILE, DATA_FILE, STORM_FILE, STORM_TIME_FILE

df_storm_time = pd.read_csv(STORM_TIME_FILE)
df_storm = pd.read_csv(STORM_FILE)
df_data = pd.read_csv(DATA_FILE)
df_census = pd.read_csv(CENSUS_FILE)

census_cols = df_census.columns[1:]


def get_all_zip_event(hurricane_event: int):
    """
    Check if the hurricane event is valid
    """
    if hurricane_event not in df_storm_time["Event"].unique():
        raise ValueError("Invalid hurricane event")
    start_time = (
        df_storm_time[df_storm_time["Event"] == hurricane_event]["Start"].values[0]
        // 100
    )
    end_time = (
        df_storm_time[df_storm_time["Event"] == hurricane_event]["End"].values[0] // 100
    )

    event_str = (
        str(hurricane_event)
        if len(str(hurricane_event)) == 6
        else "0" + str(hurricane_event)
    )

    # Get the zip codes that are affected by the hurricane event
    zip_df = df_storm[["ZIP", event_str]]
    zip_df = zip_df[zip_df[event_str].notna()]
    zip_df = zip_df[zip_df[event_str] != 0]
    zips = zip_df["ZIP"].values
    vals = zip_df[event_str].values

    return list(zips), vals, start_time, end_time


def get_census_features(zip_codes: List[int]):
    """
    Get the census features for the zip codes
    """
    census_df = (
        df_census.set_index("zip_code_tabulation_area").loc[zip_codes].reset_index()
    )
    census_df = census_df.drop(columns=["zip_code_tabulation_area"])
    return census_df.values


def get_columns(start_time: int, end_time: int):
    """
    Get the columns for the price time series
    """
    all_columns = df_data.columns[9:]
    all_columns_ints = [int(x[1:].replace(".", "")) for x in all_columns]
    # Get all columns within the time range
    columns = filter(lambda x: x >= start_time and x <= end_time, all_columns_ints)
    columns = list(columns)
    if len(columns) == 0:
        columns = filter(
            lambda x: x >= start_time and x <= end_time + 31, all_columns_ints
        )
        columns = list(columns)
    columns.sort()
    columns = ["X" + str(x) for x in columns]
    columns = [(x[:5] + "." + x[5:7] + "." + x[7:]) for x in columns]
    return columns


def get_price_ts(start_time: int, end_time: int, zip_codes: List[int]):
    """
    Get the price time series for the zip codes
    """
    zip_codes_in_df = df_data["RegionName"].values
    zip_codes = list(set(zip_codes).intersection(set(zip_codes_in_df)))
    price_df = df_data.set_index("RegionName").loc[zip_codes].reset_index()
    price_df = price_df[price_df.columns[9:]]
    price_df = price_df.fillna(method="ffill", axis="columns")
    columns = get_columns(start_time, end_time)
    price_df = price_df[columns]
    return price_df.values, zip_codes


def datetime_to_int(dt: date | datetime) -> int:
    """
    Convert datetime to int
    """
    return int(dt.strftime("%Y%m%d"))


if __name__ == "__main__":
    event = 62008
    zips, vals, start_time, end_time = get_all_zip_event(event)
    price_ts, zips = get_price_ts(start_time, end_time, list(zips))
    census_features = get_census_features(list(zips))
