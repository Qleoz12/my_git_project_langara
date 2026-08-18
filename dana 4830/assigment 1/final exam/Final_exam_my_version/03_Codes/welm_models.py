"""Weighted ELM (WELM) and boosted WELM (BWELM) for imbalance-aware classification."""
from __future__ import annotations

import numpy as np


class WELM:
  """Single hidden-layer ELM with inverse-frequency sample weights."""

  def __init__(self, n_hidden: int = 200, C: float = 1.0, random_state: int = 42):
    self.n_hidden = n_hidden
    self.C = C
    self.random_state = random_state
    self.W_in: np.ndarray | None = None
    self.b: np.ndarray | None = None
    self.beta: np.ndarray | None = None

  def _sigmoid(self, x: np.ndarray) -> np.ndarray:
    return 1 / (1 + np.exp(-np.clip(x, -30, 30)))

  def fit(self, X: np.ndarray, y: np.ndarray):
    rng = np.random.RandomState(self.random_state)
    X = np.asarray(X, dtype=float)
    y = np.asarray(y)

    self.W_in = rng.normal(size=(X.shape[1], self.n_hidden))
    self.b = rng.normal(size=self.n_hidden)

    H = self._sigmoid(X @ self.W_in + self.b)

    classes, counts = np.unique(y, return_counts=True)
    weight_map = {c: 1.0 / count for c, count in zip(classes, counts)}
    weights = np.array([weight_map[label] for label in y])
    T = np.where(y == 1, 1, -1).reshape(-1, 1)

    HtW = H.T * weights
    A = np.eye(self.n_hidden) / self.C + HtW @ H
    self.beta = np.linalg.solve(A, HtW @ T)
    return self

  def decision_function(self, X: np.ndarray) -> np.ndarray:
    H = self._sigmoid(np.asarray(X, dtype=float) @ self.W_in + self.b)
    return (H @ self.beta).ravel()

  def predict(self, X: np.ndarray) -> np.ndarray:
    return (self.decision_function(X) > 0).astype(int)

  def predict_proba(self, X: np.ndarray) -> np.ndarray:
    p = self._sigmoid(self.decision_function(X))
    return np.column_stack([1 - p, p])


class BWELM:
  """AdaBoost-style ensemble of WELM base learners with class-balanced weights."""

  def __init__(
    self,
    n_estimators: int = 5,
    n_hidden: int = 200,
    C: float = 1.0,
    random_state: int = 42,
  ):
    self.n_estimators = n_estimators
    self.n_hidden = n_hidden
    self.C = C
    self.random_state = random_state
    self.models: list[tuple[np.ndarray, np.ndarray, np.ndarray]] = []
    self.alphas: list[float] = []

  def _sigmoid(self, x: np.ndarray) -> np.ndarray:
    return 1 / (1 + np.exp(-np.clip(x, -30, 30)))

  def _fit_welm(
    self, X: np.ndarray, y: np.ndarray, sample_weight: np.ndarray, seed: int
  ):
    rng = np.random.RandomState(seed)
    W_in = rng.normal(size=(X.shape[1], self.n_hidden))
    b = rng.normal(size=self.n_hidden)
    H = self._sigmoid(X @ W_in + b)
    T = np.where(y == 1, 1, -1).reshape(-1, 1)
    HtW = H.T * sample_weight
    A = np.eye(self.n_hidden) / self.C + HtW @ H
    beta = np.linalg.solve(A, HtW @ T)
    return W_in, b, beta

  def _predict_welm(self, model: tuple[np.ndarray, np.ndarray, np.ndarray], X: np.ndarray):
    W_in, b, beta = model
    H = self._sigmoid(X @ W_in + b)
    return (H @ beta).ravel()

  def fit(self, X: np.ndarray, y: np.ndarray):
    X = np.asarray(X, dtype=float)
    y = np.asarray(y)
    classes, counts = np.unique(y, return_counts=True)
    m = len(classes)
    count_map = {c: count for c, count in zip(classes, counts)}
    D = np.array([1.0 / (m * count_map[label]) for label in y])

    self.models = []
    self.alphas = []
    for t in range(self.n_estimators):
      model = self._fit_welm(X, y, D, self.random_state + t)
      pred = (self._predict_welm(model, X) > 0).astype(int)
      correct = pred == y
      error = D[~correct].sum() / D.sum()
      error = np.clip(error, 1e-10, 1 - 1e-10)
      alpha = 0.5 * np.log((1 - error) / error)
      self.models.append(model)
      self.alphas.append(alpha)
      for c in classes:
        idx = y == c
        D[idx & ~correct] *= np.exp(alpha)
        D[idx & correct] *= np.exp(-alpha)
        class_sum = D[idx].sum()
        if class_sum > 0:
          D[idx] = D[idx] / class_sum / m
    return self

  def decision_function(self, X: np.ndarray) -> np.ndarray:
    X = np.asarray(X, dtype=float)
    total = np.zeros(X.shape[0])
    for model, alpha in zip(self.models, self.alphas):
      total += alpha * np.sign(self._predict_welm(model, X))
    return total

  def predict(self, X: np.ndarray) -> np.ndarray:
    return (self.decision_function(X) > 0).astype(int)

  def predict_proba(self, X: np.ndarray) -> np.ndarray:
    decision = self.decision_function(X)
    scale = sum(abs(a) for a in self.alphas)
    p = self._sigmoid(decision / (scale + 1e-10) * 4)
    return np.column_stack([1 - p, p])
