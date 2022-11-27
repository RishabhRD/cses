#include <algorithm>
#include <array>
#include <bitset>
#include <cmath>
#include <deque>
#include <iostream>
#include <iterator>
#include <limits>
#include <map>
#include <numeric>
#include <optional>
#include <queue>
#include <set>
#include <stack>
#include <unordered_map>
#include <unordered_set>
#include <vector>

template <typename T> T read() {
  T t;
  std::cin >> t;
  return t;
}

template <typename T> std::vector<T> read_vec(int n) {
  std::vector<T> vec(n);
  for (auto &ele : vec)
    std::cin >> ele;
  return vec;
}

template <typename T> std::vector<T> read_matrix(int m, int n) {
  std::vector<std::vector<T>> vec(m, std::vector<T>(n));
  for (int i = 0; i < m; ++i) {
    for (int j = 0; j < n; ++j) {
      std::cin >> vec[i][j];
    }
  }
  return vec;
}

using ll = int;

constexpr ll mod = 1e9 + 7;

ll solve(std::vector<ll> &coins, ll k) {
  auto const n = std::size(coins);
  std::vector dp(n + 1, std::vector(k + 1, 0));
  dp[0][0] = 1;
  for (ll i = 1; i <= n; ++i) {
    for (ll j = 0; j <= k; ++j) {
      dp[i][j] = dp[i - 1][j];
      if (j - coins[i - 1] >= 0) {
        dp[i][j] += dp[i][j - coins[i - 1]];
      }
      dp[i][j] %= mod;
    }
  }
  return dp[n][k];
}

int main() {
  auto const n = read<ll>();
  auto const k = read<ll>();
  auto coins = read_vec<ll>(n);
  std::cout << solve(coins, k) << std::endl;
}
