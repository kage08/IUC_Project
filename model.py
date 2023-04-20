import numpy as np
import pandas as pd
import torch
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPRegressor
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVR

from datasets import SingleEventDataset
from utils import STORM_TIME_FILE, census_cols


class Model(torch.nn.Module):
    """
    A RNN + MLP model that takes in a sequence and fixed-length feature vector and outputs an embedding
    """

    def __init__(self, input_size_seq, input_size, hidden_size, output_size):
        super(Model, self).__init__()
        self.rnn = torch.nn.GRU(input_size_seq, hidden_size, batch_first=True)
        self.mlp = torch.nn.Linear(input_size, hidden_size)
        self.output = torch.nn.Linear(hidden_size * 2, output_size)

    def forward(self, x, seq):
        """
        x: (batch_size, input_size)
        seq: (batch_size, seq_len, input_size_seq)
        """
        # RNN
        h, _ = self.rnn(seq)
        h = h[:, -1, :]
        # MLP
        x = self.mlp(x)
        # Concat
        x = torch.cat([x, h], dim=1)
        # Output
        x = self.output(x)
        return x


def train_model(model: Model, X_train, X_seq_train, y_train):
    opt = torch.optim.Adam(model.parameters(), lr=0.001)
    loss_fn = torch.nn.MSELoss()
    for epoch in range(100):
        y_pred = model(X_train, X_seq_train).squeeze()
        loss = loss_fn(y_pred, y_train)
        opt.zero_grad()
        loss.backward()
        opt.step()
        if epoch % 10 == 0:
            print(f"Epoch {epoch}: {loss.item()}")

    return model


def predict_model(model: Model, X_test, X_seq_test, y_test):
    y_pred = model(X_test, X_seq_test).squeeze()
    loss_fn = torch.nn.MSELoss()
    loss = loss_fn(y_pred, y_test)
    return y_pred, loss.item()


# Get the list of hurricane events
storm_time_df = pd.read_csv(STORM_TIME_FILE)
events = storm_time_df["Event"].values

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

    model = Model(1, X.shape[1], 32, 1)
    model = train_model(
        model,
        torch.tensor(X_train),
        torch.tensor(X_train).unsqueeze(1),
        torch.tensor(y_train),
    )
    y_pred, loss = predict_model(
        model,
        torch.tensor(X_test),
        torch.tensor(X_test).unsqueeze(1),
        torch.tensor(y_test),
    )
    scores["LR"][event] = loss
    print("Loss: ", loss)
