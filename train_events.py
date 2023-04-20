"""Train models for SingleEvent TS"""

import os

import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPRegressor
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVR

from datasets import SingleEventDataset
from utils import STORM_TIME_FILE, census_cols

# Get the list of hurricane events
storm_time_df = pd.read_csv(STORM_TIME_FILE)
events = storm_time_df["Event"].values


def train_pred_model(model, X_train, y_train, X_test, y_test):
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)
    rmse = np.sqrt(mean_squared_error(y_test, y_pred))
    return y_pred, rmse


scores = {}
scores["LR"] = {}
scores["LR_Coeff"] = {}
for event in events:
    print("Event: ", event)

    dataset = SingleEventDataset(event)
    X = dataset.features
    X = np.nan_to_num(X)
    y = dataset.labels
    y = np.nan_to_num(y)

    try:
        # Split the data into train and test
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=0.2, random_state=42
        )
    except ValueError:
        continue

    # Scale the data
    scaler = StandardScaler()
    X_train = scaler.fit_transform(X_train)
    X_test = scaler.transform(X_test)
    # scale y based on the first feature
    label_scaler = StandardScaler()
    y_train = label_scaler.fit_transform(y_train.reshape(-1, 1)).flatten()
    y_test = label_scaler.transform(y_test.reshape(-1, 1)).flatten()

    model = LinearRegression()
    y_pred, rmse = train_pred_model(model, X_train, y_train, X_test, y_test)
    print("Linear Regression RMSE: ", rmse)
    if np.abs(rmse) > 100:
        continue
    scores["LR"][event] = rmse

    # Plot the coefficients
    import matplotlib.pyplot as plt

    plt.figure(figsize=(10, 10))
    plt.barh(census_cols, model.coef_[-len(census_cols) :])
    os.makedirs("LR", exist_ok=True)
    plt.savefig(f"LR/{event}.png")

    model_coeffs = model.coef_[-len(census_cols) :]
    max_coeff = np.max(model_coeffs)
    min_coeff = np.min(model_coeffs)
    model_coeffs = (model_coeffs - min_coeff) / (max_coeff - min_coeff)

    scores["LR_Coeff"][event] = model_coeffs

# Average all coeffs
coeffs = np.array(list(scores["LR_Coeff"].values()))
avg_coeffs = np.mean(coeffs, axis=0)

# Plot the average coefficients
import matplotlib.pyplot as plt

plt.figure(figsize=(10, 24))
plt.barh(census_cols, avg_coeffs)
# incease the font size
plt.rcParams.update({"font.size": 22})
plt.savefig("LR/avg.png", bbox_inches="tight")


import pickle

# Save the scores
with open("LR_scores.pkl", "wb") as f:
    pickle.dump(scores, f)
